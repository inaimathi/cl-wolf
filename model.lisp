(in-package #:cl-wolf)

;;;;;;;;;; Basic parts
(defclass part () 
  ((origin :accessor origin :initarg :origin :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (input-ports :accessor input-ports :initarg :input-ports :initform '(:in))
   (inbox :accessor inbox :initarg :inbox :initform (queue))
   (output-ports :accessor output-ports :initarg :output-ports :initform '(:out :error))
   (outgoing :accessor outgoing :initarg :outgoing :initform (make-instance 'connection-table))))

(defclass container (part) 
  ((parts :accessor parts :initarg :parts :initform nil)
   (incoming :accessor incoming :initarg :incoming :initform (make-instance 'connection-table))))

(defclass reactor (part)
  ((body :accessor body :initarg :body :initform nil)))

(defclass deactor (part)
  ((body :accessor body :initarg :body :initform nil)
   (inbox :initform nil)
   (expecting :accessor expecting :initarg :expecting :initform nil)
   (guard :accessor guard :initarg :ready-fn :initform (constantly nil))))

;;;;;;;;;; Connection and dispatch
(defclass connection-table ()
  ((connections :accessor connections :initform nil)))
(defclass connection ()
  ((origin :accessor origin :initarg :origin :initform nil)
   (tag :accessor tag :initarg :tag)
   (target :accessor target :initarg :target)
   (target-tag :accessor target-tag :initarg :target-tag)))
(defun conn (tag target-tag target)
  (make-instance 'connection :tag tag :target-tag target-tag :target target))

;;;;;;;;;; Messages
(defclass message ()
  ((tag :accessor tag :initarg :tag)
   (payload :accessor payload :initarg :payload)))
(defun msg (tag payload)
  (make-instance 'message :tag tag :payload payload))
