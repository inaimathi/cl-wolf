(in-package :cl-wolf)

(defclass queue ()
  ((messages :accessor messages :initform nil)
   (last-cons :accessor last-cons :initform nil)
   (len :accessor len :initform 0)))

(defmethod push! (msg (q queue))
  (let ((m (list msg)))
    (if (empty? q)
	(setf (messages q) m
	      (last-cons q) (messages q))
	(setf (cdr (last-cons q)) m
	      (last-cons q) m))
    (incf (len q))
    nil))

(defmethod pop! ((q queue))
  (if (empty? q)
      (values (setf (last-cons q) nil) nil)
      (progn (decf (len q))
	     (values (pop (messages q)) t))))

(defmethod empty? ((q queue))
  (null (messages q)))

(defun queue () (make-instance 'queue))

(defclass queue-table ()
  ((table :accessor table :initform (make-hash-table :test 'equal))))

(defmethod push! ((msg message) (qt queue-table))
  (let ((q (gethash (tag msg) (table qt))))
    (when q (push! msg q))))

(defmethod pull! ((qt queue-table) tag)
  (let ((q (gethash tag (table qt))))
    (when q (pop! q))))

(defmethod length-of ((qt queue-table) tag)
  (let ((q (gethash tag (table qt))))
    (if q (len q) 0)))

(defun queue-table (keys)
  (let ((qt (make-instance 'queue-table)))
    (loop for k in keys
       do (setf (gethash k (table qt)) (queue)))
    qt))
