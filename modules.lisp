(in-package #:cl-wolf)

(defparameter *strap-wolf* 
  (make-fact-base))

(defmacro module (name args &body body)
  `(defun ,name ,args
     ,@body))

(defun register-module (package name fn args body)
  (multi-insert! 
   *strap-wolf*
   `((:name ,name) (:parameters ,args) (:factory ,fn) 
     (:hash ,(sha256 (list args body)))
     (:package ,(intern (package-name package) :keyword)))))

;;;;;;;;;; Hashing utility
(defmethod digest ((thing sequence) digest-type)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    (ironclad:make-digest digest-type)
    thing)))

(defmethod digest ((thing string) digest-type)
  (digest (ironclad:ascii-string-to-byte-array thing) digest-type))

(defmethod sha256 (thing)
  (digest (format nil "~a" thing) :sha256))
