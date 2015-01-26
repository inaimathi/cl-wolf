(in-package :cl-wolf)

;; ---> greeter ---> counter ---> printer
;;              \_____________|

(defun mk-printer (&key (stream *standard-output*) (template "PRINTER -- ~s : ~s"))
  (make-reactor
   (lambda (tag message)
     (format stream template tag message))))

(defun mk-greeter (&key (template "Hello there, ~a!"))
  (make-reactor
   (lambda (tag message)
     (declare (ignore tag))
     (out! :out (format nil template message)))))

(defun mk-counter (&key (initial 0))
  (make-reactor
   (let ((ct initial))
     (lambda (tag messsage)
       (declare (ignore tag messsage))
       (out! :out (incf ct))))))

(defparameter *test*
  (make-container
      ((greeter (mk-greeter))
       (counter (mk-counter))
       (printer (mk-printer)))
    ((self :in) -> (greeter :in))
    ((greeter :out) -> (counter :in) (printer :in))
    ((counter :out) -> (printer :in))))

(send! *test* :in "inaimathi")
(send! *test* :in "dxnn")
(send! *test* :in "guitarvydas")

;; ---> splitter ---> pairer ---> printer
;;               \____________||
;;               \___ counter _|

(defun splitter (&key (chunk-size 1) (send-remainder? t))
  (make-reactor
   (lambda (tag message)
     (declare (ignore tag))
     (loop with len = (length message)
	for i from 0 by chunk-size
	for j from chunk-size by chunk-size 
	while (>= len j)
	do (out! :out (subseq message i j))
	finally (when (and send-remainder? (>= j len) (> len i))
		  (out! :out (subseq message i)))))))

(defun pairer ()
  (make-reactor
   (let ((cache nil))
     (lambda (tag message)
       (declare (ignore tag))
       (if cache
	   (progn 
	     (out! :out (cons (first cache) message))
	     (setf cache nil))
	   (setf cache (list message)))))))

(defparameter *test2*
  (make-container
      (splitter
       pairer
       (counter (mk-counter))
       (printer (mk-printer)))
    ((self :in) -> (splitter :in))
    ((splitter :out) -> (printer :in) (pairer :in) (counter :in))
    ((pairer :out) -> (printer :in))
    ((counter :out) -> (printer :in))))

(send! *test2* :in "This is a test message")
(send! *test2* :in "Blahs")
(send! *test2* :in "Blahs")
