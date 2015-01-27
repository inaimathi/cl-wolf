(in-package :cl-wolf)

;; ---> greeter ---> counter ---> printer
;;              \_____________|

(defun mk-printer (&key (stream *standard-output*) (template "PRINTER -- ~s : ~s~%"))
  (make-reactor
    (format stream template tag message)))

(defun mk-greeter (&key (template "Hello there, ~a!"))
  (make-reactor
    (out! :out (format nil template message))))

(defun mk-counter (&key (initial 0))
  (let ((ct initial))
    (make-reactor (out! :out (incf ct)))))

(defun mk-test ()
  (make-container
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

;; ---> splitter ---> pairer ---> printer
;;               \____________||
;;               \___ counter _|

(defun splitter (&key (chunk-size 1) (send-remainder? t))
  (make-reactor
    (loop with len = (length message)
       for i from 0 by chunk-size
       for j from chunk-size by chunk-size 
       while (>= len j)
       do (out! :out (subseq message i j))
       finally (when (and send-remainder? (>= j len) (> len i))
		 (out! :out (subseq message i))))))

(defun pairer ()
  (let ((cache nil))
    (make-reactor
      (if cache
	  (progn 
	    (out! :out (cons (first cache) message))
	    (setf cache nil))
	  (setf cache (list message))))))

(defun mk-test2 ()
  (make-container
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

;; ---> buffer ---> parser ---> router ---> writer

(defun http-listener ()
  (make-reactor
    (let ((conns (list (socket-listen usocket:*wildcard-host* 4848 :reuse-address t))))
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
      (make-reactor
	(destructuring-bind (sock buffered len) message
	  (let ((buf (get-buf sock)))
	    (if buf
		(setf (car buf) (append buffered (car buf))
		      (cdr buf) (+ (cdr buf) len))
		(setf buf (new-buf! sock buffered len)))
	    (when (complete? (car buf))
	      (out! :out (list sock (coerce (reverse (car buf)) 'string)))
	      (kill-buf! sock))))))))

(defmethod to-key ((str string)) (intern (string-upcase str) :keyword))

(defun http-parser ()
  (make-reactor
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
      (error () (out! :error (list :error self message))))))

(defun http-hello ()
  (make-reactor
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
  (make-reactor
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
  (make-container
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

(defparameter *test3* (mk-test3))

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

(defun pull-pairer ()
  (make-deactor 
    (out! :out (cons (get! :a) (get! :b)))))

(defun mk-pull-test ()
  (make-container
      ((a (pull-pairer))
       (printer (mk-printer)))
    ((self :a) -> (a :a))
    ((self :b) -> (a :b))
    ((a :out) -> (printer :in))))

(defparameter *pull-test* (mk-pull-test))
