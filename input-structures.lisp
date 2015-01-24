(in-package :cl-wolf)

(defclass queue ()
  ((messages :accessor messages :initform nil)
   (last-cons :accessor last-cons :initform nil)))

(defmethod push! (msg (q queue))
  (let ((m (list msg)))
    (if (empty? q)
	(setf (messages q) m
	      (last-cons q) (messages q))
	(setf (cdr (last-cons q)) m
	      (last-cons q) m))
    nil))

(defmethod pull! ((q queue))
  (if (empty? q)
      (values (setf (last-cons q) nil) nil)
      (values (pop (messages q)) t)))

(defmethod empty? ((q queue))
  (null (messages q)))

(defun queue () (make-instance 'queue))
