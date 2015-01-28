(in-package :cl-wolf)

;;;;; Basic Hello World
;; ---> greeter ---> counter ---> printer
;;              \_____________|

(defun mk-printer (&key (stream *standard-output*) (template "PRINTER -- ~s : ~s~%"))
  (reactor (format stream template tag message)))

(defun mk-greeter (&key (template "Hello there, ~a!"))
  (reactor (out! :out (format nil template message))))

(defun mk-counter (&key (initial 0))
  (let ((ct initial))
    (reactor (out! :out (incf ct)))))

(defun mk-test ()
  (container
      ((greeter (mk-greeter))
       (counter (mk-counter))
       (printer (mk-printer)))
    ((self :in) -> (greeter :in))
    ((greeter :out) -> (counter :in) (printer :in))
    ((counter :out) -> (printer :in))))

(defparameter *test* (mk-test))

(send! *test* :in "inaimathi")
(send! *test* :in "dxnn")
(send! *test* :in "guitarvydas")

;;;;; More elaborate Hello World
;; ---> splitter ---> pairer ---> printer
;;               \____________||
;;               \___ counter _|

(defun splitter (&key (chunk-size 1) (send-remainder? t))
  (reactor
    (loop with len = (length message)
       for i from 0 by chunk-size
       for j from chunk-size by chunk-size 
       while (>= len j)
       do (out! :out (subseq message i j))
       finally (when (and send-remainder? (>= j len) (> len i))
		 (out! :out (subseq message i))))))

(defun pairer ()
  (let ((cache nil))
    (reactor
      (if cache
	  (progn 
	    (out! :out (cons (first cache) message))
	    (setf cache nil))
	  (setf cache (list message))))))

(defun mk-test2 ()
  (container
      (splitter
       pairer
       (counter (mk-counter))
       (printer (mk-printer)))
    ((self :in) -> (splitter :in))
    ((splitter :out) -> (printer :in) (pairer :in) (counter :in))
    ((pairer :out) -> (printer :in))
    ((counter :out) -> (printer :in))))

(defparameter *test2* (mk-test2))

(send! *test2* :in "This is a test message")
(send! *test2* :in "Blahs")
(send! *test2* :in "Blahs")

;;;;; Pull-based Hello World
(defun pull-pairer ()
  (reactor (out! :out (cons (get! :a) (get! :b)))))

(defun mk-pull-test ()
  (container
      ((a (pull-pairer))
       (printer (mk-printer)))
    ((self :a) -> (a :a))
    ((self :b) -> (a :b))
    ((a :out) -> (printer :in))))

(defparameter *pull-test* (mk-pull-test))

(send! *pull-test* :a 56)
(send! *pull-test* :a 57)
(send! *pull-test* :a 58)
(send! *pull-test* :b 59)
(send! *pull-test* :b 60)
(send! *pull-test* :b 61)

;;;;; Basic HTTP server
;; ---> buffer ---> parser ---> router ---> writer
;;           \________\___________\____________\__---> printer

(defun http-listener (&key (ip usocket:*wildcard-host*) (port 4848) (reuse-address t))
  (reactor
    (let ((conns (list (socket-listen ip port :reuse-address reuse-address))))
      (unwind-protect
	   (loop (loop for ready in (wait-for-input conns :ready-only t)
		    do (if (typep ready 'stream-server-usocket)
			   (push (socket-accept ready) conns)
			   (loop with s = (socket-stream ready)
			      with msg = (list)
			      for ct from 1 for char = (read-char-no-hang s nil :eof)
			      until (or (eq char :eof) (null char)) do (push char msg)
			      finally (progn (setf conns (remove ready conns))
					     (out! :out (list ready msg ct)))))))
	(mapc (lambda (sock)
		#+lispworks (loop for res = (socket-close sock)
			       until (or (null res) (eql -1 res)))
		#-lispworks (loop while (socket-close sock)))
	      conns)))))

(defun http-buffer ()
  (let ((buffer-table (make-hash-table)))
    (flet ((get-buf (sock) (gethash sock buffer-table))
	   (new-buf! (sock init len) (setf (gethash sock buffer-table) (cons init len)))
	   (kill-buf! (sock) (remhash sock buffer-table))
	   (complete? (str)
	     (alexandria:starts-with-subseq (list #\linefeed #\return #\linefeed #\return) str)))
      (reactor
	(destructuring-bind (sock buffered len) message
	  (let ((buf (get-buf sock)))
	    (if buf
		(setf (car buf) (append buffered (car buf))
		      (cdr buf) (+ (cdr buf) len))
		(setf buf (new-buf! sock buffered len)))
	    (when (complete? (car buf))
	      (out! :out (list sock (coerce (reverse (car buf)) 'string)))
	      (kill-buf! sock))))))))

(defun http-parser ()
  (flet ((to-key (str) (intern (string-upcase str) :keyword)))
    (reactor
      (handler-case
	  (destructuring-bind (sock raw) message
	    (let ((lines (cl-ppcre:split "\\r?\\n" raw)))
	      (destructuring-bind (req-type path http-version) (cl-ppcre:split " " (pop lines))
		(declare (ignore http-version))
		(let* ((path-pieces (cl-ppcre:split "\\?" path))
		       (resource (first path-pieces))
		       (parameters (loop for pair in (cl-ppcre:split "&" (second path-pieces))
				      for (name val) = (cl-ppcre:split "=" pair)
				      collect (cons (to-key name) (or val ""))))
		       (headers (loop for header = (pop lines) for (name value) = (cl-ppcre:split ": " header)
				   until (null name) collect (cons (to-key name) value) )))
		  (out! :out (list sock (to-key req-type) resource parameters headers))))))
	(error () (out! :error (list :error self message)))))))

(defun http-hello ()
  (reactor
    (destructuring-bind (sock &rest stuff) message
      (declare (ignore stuff))
      (out! :out (list sock
		       (list "HTTP/1.1 200 OK"
			     "Server: k"
			     "Content-Type: text/html"
			     ""
			     "<html><body>Hello, world!</body></html>"
			     "")
		       t)))))

(defun http-writer ()
  (reactor
    (destructuring-bind (sock response close?) message
      (ignore-errors
	(loop with s = (socket-stream sock) for ln in response
	   do (progn (write-string ln s)
		     (write-char #\return s)
		     (write-char #\linefeed s))
	   finally (force-output s)))
      (when close?
	(ignore-errors (socket-close sock))))))

(defun mk-test3 ()
  (container
      (http-listener
       http-buffer 
       http-parser
       http-hello
       http-writer
       (tap (mk-printer)))
    ((self :in) -> (http-listener :in) (tap :in))
    ((http-listener :out) -> (http-buffer :in) (tap :in))
    ((http-buffer :out) -> (http-parser :in) (tap :in))
    ((http-parser :out) -> (http-hello :in) (tap :in))
    ((http-hello :out) -> (http-writer :in) (tap :in))))

(defparameter *server* (mk-test3))

;; The below are test messages from the incremental work of building this.
;; They no longer apply though (except for partial testing, which I should probably also write an example of)
;; If you want to see this in action, you'll need to start it and send it a proper HTTP request at the target port.

;; (let ((get-req "GET /index.html HTTP/1.1
;; Host: www.example.com
;; 
;; "))
;;   (send! *test3* :in (list :sock-tag (coerce (reverse get-req) 'list) (length get-req))))

;; (let ((get-req "GET /index.html?a=1 HTTP/1.1
;; Host: www.example.com
;; 
;; "))
;;   (send! *test3* :in (list :sock-tag (coerce (reverse get-req) 'list) (length get-req))))

;; ;;;;;;;;;; Test Rig
;; (defun test-rig (part &rest port/payload-pairs)
;;   (let ((c (container 
;; 	       ((subject part)
;; 		(tap (mk-printer)))
;; 	     (self -> subject (tap :in))
;; 	     (subject -> (tap :in)))))
;;     (loop for (k v) on port/payload-pairs by #'cddr
;;        do (send! c k v))))

;; (test-rig 
;;  (mk-greeter)
;;  :in "A"
;;  :in "B"
;;  :in "C")
