(in-package #:cl-wolf)

(defparameter *strap-wolf* 
  (make-fact-base))

(defun alias-function (new-name old-name)
  (setf (symbol-function new-name) (symbol-function old-name)))

(defun module-hash (args body) (sha256 (cons args body)))

(defmacro module (name args &body body)
  (assert (and (listp (car body)) (null (cdr body))) nil "A module may only contain one term.")
  (let ((alias (for-all `(and (?id :hash ,(module-hash args body)) (?id :name ?name)) :in *strap-wolf* :do (return ?name))))
    `(progn 
       ,(if alias
	    `(alias-function ',name ',alias)
	    `(defun ,name ,args
	       ,@body))
       (register-module! ',name ',args ',body)
       nil)))

(defun register-module! (name args body)
  "Registers a module in the *strap-wolf* structure.
If there's already a module with the same name and hash, this is a no-op
If there's a module with the same name, but a different hash, we add a new version
If there's a module with the same hash, but a different name, we add an alias for that module
Otherwise, we register a new module"
  (let ((hash (module-hash args body)))
    (flet ((register-version! ()
	     (multi-insert!
	      *strap-wolf*
	      `((:name ,name) (:parameters ,args) (:source ,body) (:factory ,(symbol-function name)) 
		(:hash ,hash) (:registered ,(get-universal-time))))))
      (cond ((for-all `(and (?id :name ,name) (?id :hash ,hash)) :in *strap-wolf* :do (return t))
	     :duplicate-module)
	    ((for-all `(?id :hash ,hash) :in *strap-wolf* :do (return t))
	     (let ((id (for-all `(?id :hash ,hash) :in *strap-wolf* :do (return ?id))))
	       (insert! *strap-wolf* (list id :alias name))))
	    ;; ((for-all `(and (?id :name ,name)) :in *strap-wolf* :do (return t))
	    ;;  (register-version!))
	    (t (register-version!))))))

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
