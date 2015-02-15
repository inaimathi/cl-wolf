(in-package #:cl-wolf)

(defun alias-function (new-name old-name)
  (setf (symbol-function new-name) (symbol-function old-name)))

;;;;;;;;;; Hashing utility
(defun module-hash (args body) (sha256 (cons args body)))

(defmethod sha256 (thing)
  (digest (format nil "~a" thing) :sha256))

(defmethod digest ((thing string) digest-type)
  (digest (ironclad:ascii-string-to-byte-array thing) digest-type))

(defmethod digest ((thing sequence) digest-type)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    (ironclad:make-digest digest-type)
    thing)))
