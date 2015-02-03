(in-package :cl-wolf)

;;;;; Basic Hello World
;; ---> greeter ---> (counter (mk-counter)) ---> printer
;;               \___________________________|

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
    ((counter :out) -> (printer :in) (self :out))))

(defparameter *test* (mk-test))

(send! *test* :in "inaimathi")
(send! *test* :in "dxnn")

;;;;; A demonstration of nesting
;; ---> mk-test ---> doubler ----------------> 
;;               \            \___---> prn
;;                \_____________|

(defun nest-test ()
  (container
      (mk-test
       (prn (reactor (format t "Blahdiblah: ~a~%" message)))
       (doubler (reactor (out! :out (* 2 message)))))
    ((self :in) -> (mk-test :in))
    ((mk-test :out) -> (prn :in) (doubler :in))
    ((doubler :out) -> (prn :in) (self :out))))

(defparameter *nest-test* (nest-test))

(send! *nest-test* :in "guitarvydas")

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
;; :a ---> :a (pairer (pull-pairer)) :out ---> :in printer
;; :b ---> :b pairer

(defun pull-pairer ()
  (reactor (out! :out (cons (in! :a) (in! :b)))))

(defun mk-pull-test ()
  (container
      ((pairer (pull-pairer))
       (printer (mk-printer)))
    ((self :a) -> (pairer :a))
    ((self :b) -> (pairer :b))
    ((pairer :out) -> (printer :in))))

(defparameter *pull-test* (mk-pull-test))

(send! *pull-test* :a 56)
(send! *pull-test* :a 57)
(send! *pull-test* :a 58)
(send! *pull-test* :b 59)
(send! *pull-test* :b 60)
(send! *pull-test* :b 61)

;;;;; Basic loop test
;; ---> countdown ---> printer
;;  |          \___--> decrement
;;  |_____________________/

(defun countdown (&key (til 0))
  (reactor 
    (when (> message til)
      (out! :out message))))

(defun decrement (&key (by 1))
  (reactor (out! :out (- message by))))

(defun mk-loop ()
  (container
      (countdown
       (dec (decrement))
       (tap (mk-printer)))
    ((self :in) -> (countdown :in))
    ((countdown :out) -> (dec :in) (tap :in))
    ((dec :out) -> (countdown :in))))

(defparameter *loop-test* (mk-loop))

(send! *loop-test* :in 10)

;;;;; Basic HTTP server
;; ---> buffer ---> parser ---> router ---> http-response ---> writer _
;;              \___________\___________\__________________\___________\__---> printer

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
      (out! :out (list sock (list "200 OK" "text/html" "<html><body>Hello, world!</body></html>"))))))

(defun http-response ()
  (flet ((cat (a b) (concatenate 'string a b)))
    (reactor
      (destructuring-bind (sock (http-code content-type body)) message
	(out! :out (list sock
			 (list (cat "HTTP/1.0 " http-code)
			       "Server: k"
			       (cat "Content-Type: " content-type)
			       ""
			       body
			       "")
			 t))))))

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
       (tap (mk-printer))
       (res (http-response)))
    ((self :in) -> (http-listener :in) (tap :in))
    ((http-listener :out) -> (http-buffer :in) (tap :in))
    ((http-buffer :out) -> (http-parser :in) (tap :in))
    ((http-parser :out) -> (http-hello :in) (tap :in))
    ((http-hello :out) -> (res :in) (tap :in))
    ((res :out) -> (http-writer :in) (tap :in))))

(defparameter *server* (mk-test3))
