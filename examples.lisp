(in-package :cl-wolf)


;; ---> greeter ---> counter ---> printer
;;              --------------|

(defun mk-printer (&key (stream *standard-output*) (prefix "PRINTER -- "))
  (make-instance
   'reactor
   :body (lambda (tag msg)
	   (format stream "~a~s : ~s~%" prefix tag msg))))

(defun mk-greeter (&key (template "Hello there, ~a!"))
  (let ((g (make-instance 'reactor)))
    (setf (body g)
	  (lambda (tag msg)
	    (broadcast! g (msg :out (format nil template msg)))))
    g))

(defun mk-counter (&key (initial 0))
  (let ((ct initial)
	(g (make-instance 'reactor)))
    (setf (body g)
	  (lambda (tag msg)
	    (declare (ignore tag msg))
	    (broadcast! g (msg :out (incf ct)))))
    g))

(defparameter *test*
  (let ((c (make-instance 'container :input (queue)))
	(greeter (mk-greeter))
	(counter (mk-counter))
	(printer (mk-printer)))
    (connect! (incoming c) :in greeter :in)
    (connect! greeter :out counter :in)
    (connect! greeter :out printer :in)
    (connect! counter :out printer :in)
    (add-part! c printer)
    (add-part! c greeter)
    (add-part! c counter)
    c))

(send! *test* :in "Leo")
