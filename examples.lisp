(in-package :cl-wolf)


;; ---> greeter ---> counter ---> printer
;;              \_____________|

(defun mk-printer (&key (stream *standard-output*) (prefix "PRINTER -- "))
  (make-instance
   'reactor
   :body (lambda (tag message)
	   (format stream "~a~s : ~s~%" prefix tag message))))

(defun mk-greeter (&key (template "Hello there, ~a!"))
  (let ((g (make-instance 'reactor)))
    (setf (body g)
	  (lambda (tag message)
	    (broadcast! g (msg :out (format nil template message)))))
    g))

(defun mk-counter (&key (initial 0))
  (let ((ct initial)
	(g (make-instance 'reactor)))
    (setf (body g)
	  (lambda (tag messsage)
	    (declare (ignore tag messsage))
	    (broadcast! g (msg :out (incf ct)))))
    g))

(defparameter *test*
  (let ((c (make-instance 'container))
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

;; ---> splitter ---> pairer ---> printer
;;               \____________||
;;               \___ counter _|

(defun mk-splitter (&key (chunk-size 1) (send-remainder? t))
  (let ((g (make-instance 'reactor)))
    (setf (body g)
	  (lambda (tag message)
	    (declare (ignore tag))
	    (loop with len = (length message)
	       for i from 0 by chunk-size
	       for j from chunk-size by chunk-size 
	       while (>= len j)
	       do (broadcast! g (msg :out (subseq message i j)))
	       finally (when (and send-remainder? (>= j len) (> len i))
			 (broadcast! g (msg :out (subseq message i)))))))
    g))

(defun mk-pairer ()
  (let ((g (make-instance 'reactor)))
    (setf (body g)
	  (let ((cache nil))
	    (lambda (tag message)
	      (declare (ignore tag))
	      (if cache
		  (progn 
		    (broadcast! g (msg :out (cons (first cache) message)))
		    (setf cache nil))
		  (setf cache (list message))))))
    g))

(defparameter *test2*
  (let ((c (make-instance 'container))
	(splitter (mk-splitter))
	(pairer (mk-pairer))
	(counter (mk-counter))
	(printer (mk-printer)))
    (connect! (incoming c) :in splitter :in)
    (connect! splitter :out printer :in)
    (connect! splitter :out pairer :in)
    (connect! splitter :out counter :in)
    (connect! pairer :out printer :in)
    (connect! counter :out printer :in)
    (add-part! c splitter)
    (add-part! c pairer)
    (add-part! c counter)
    (add-part! c printer)
    c))

(send! *test2* :in "This is a test message")
(send! *test2* :in "Blahs")
(send! *test2* :in "Blahs")
