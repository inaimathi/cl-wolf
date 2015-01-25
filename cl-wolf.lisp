;;;; cl-wolf.lisp
(in-package #:cl-wolf)

(defclass part () 
  ((origin :accessor origin :initarg :origin :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (input-ports :accessor input-ports :initarg :input-ports :initform '(:in))
   (input :accessor input :initarg :input :initform (queue))
   (output-ports :accessor output-ports :initarg :output-ports :initform '(:out :error))
   (outgoing :accessor outgoing :initarg :outgoing :initform (make-instance 'connection-table))))

(defclass container (part) 
  ((parts :accessor parts :initarg :parts :initform nil)
   (incoming :accessor incoming :initarg :incoming :initform (make-instance 'connection-table))))
(defmethod add-part! ((self container) (new-child part))
  (push new-child (parts self))
  (setf (parent new-child) self)
  nil)
(defmethod run! ((self container))
  (let ((msg (pull! (input self))))
    (when msg
      (mapc 
       (lambda (c) 
	 (broadcast! c msg))
       (dispatch (incoming self) (tag msg))))))

(defclass reactor (part)
  ((body :accessor body :initarg :body :initform nil)))
(defmethod run! ((self reactor))
  (let ((msg (pull! (input self))))
    (when msg (run-with! (body self) (tag msg) (payload msg)))))

(defclass connection-table ()
  ((connections :accessor connections :initform nil)))
(defclass connection ()
  ((origin :accessor origin :initarg :origin :initform nil)
   (tag :accessor tag :initarg :tag)
   (target :accessor target :initarg :target)
   (target-tag :accessor target-tag :initarg :target-tag)))
(defun conn (tag target-tag target)
  (make-instance 'connection :tag tag :target-tag target-tag :target target))

(defclass message ()
  ((tag :accessor tag :initarg :tag)
   (payload :accessor payload :initarg :payload)))
(defun msg (tag payload)
  (make-instance 'message :tag tag :payload payload))

(defmethod connect! ((from connection-table) src-tag (target part) target-tag)
  (push (conn src-tag target-tag target) (connections from))
  nil)
(defmethod connect! ((src part) src-tag (target part) target-tag)
  (connect! (outgoing src) src-tag target target-tag)
  nil)

(defmethod dispatch ((self part) tag)
  (dispatch (outgoing self) tag))
(defmethod dispatch ((conns connection-table) tag)
  (remove-if-not (lambda (s) (equal tag s)) (connections conns) :key #'tag))

(defmethod broadcast! ((self part) (m message))
  (mapc (lambda (conn) (broadcast! conn m)) (dispatch self (tag m))))
(defmethod broadcast! ((conn connection) (m message))
  (push! 
   (if (eq (tag conn) (target-tag conn))
       m 
       (msg (target-tag conn) (payload m)))
   (input (target conn)))
  (run! (target conn)))

(defmethod run-with! ((fn function) tag payload)
  (funcall fn tag payload))

(defmethod send! ((self part) tag payload)
  (push! (msg tag payload) (input self))
  (run! self)
  nil)
