(in-package #:cl-wolf)

(defparameter *strap-wolf* 
  (if (cl-fad:file-exists-p *module-base*)
      (load! *module-base*)
      (make-fact-base :file-name *module-base*)))

(defmethod module-exists? ((name symbol))
  (for-all `(?id :name ,name) :in *strap-wolf* :do (return ?id)))

(defmacro module (name args &body body)
  (assert (and (listp (car body)) (null (cdr body))) nil "A module may only contain one term.")
  (assert (lick (car body)) nil "A module must be a REACTOR or CONTAINER surrounded by local-definition form.")
  (let ((alias (for-all `(and (?id :hash ,(module-hash args body)) (?id :name ?name)) :in *strap-wolf* :do (return ?name))))
    (unless (eq alias name)
      `(progn 
	 ,(if alias
	      `(alias-function ',name ',alias)
	      `(defun ,name ,args
		 ,@body))
	 (register-module! ',name ',args ',body)
	 nil))))

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
		(:hash ,hash) (:registered ,(get-universal-time))
		,@(mapcar (lambda (h) `(:depends-on ,h)) (find-dependencies (car body)))))
	     (write! *strap-wolf*)))
      (cond ((for-all `(and (?id :name ,name) (?id :hash ,hash)) :in *strap-wolf* :do (return t))
	     :duplicate-module)
	    ((for-all `(?id :hash ,hash) :in *strap-wolf* :do (return t))
	     (let ((id (for-all `(?id :hash ,hash) :in *strap-wolf* :do (return ?id))))
	       (insert! *strap-wolf* (list id :name name))))
	    ;; ((for-all `(and (?id :name ,name)) :in *strap-wolf* :do (return t))
	    ;;  (register-version!))
	    (t (register-version!))))))

(defun part-form? (thing)
  (and (listp thing) 
       (member (car thing) 
	       '(reactor container))))

(defun lick (module-body)
  "A body can be either a reactor or container contained within an arbitrary number and order of local definition terms.
These include `let`, `let*`, `flet`, `labels`, `macrolet` and `symbol-macrolet`.
`lick` takes a module body, and either returns the chewy center (reactor/container) form, or NIL if no such form exists.
Additionally, `lick` returns lists of local-vars, local-functions and local-macros (innermost first each) as additional values.

For example, 

    (lick 
      (let ((a 1))
        (flet ((b () 2))
          (labels ((c () 3))
            (macrolet ((blah () 4)) 
	      (symbol-macrolet ((mumble 5))
	        (reactor (write :hello))))))))
    => (REACTOR (WRITE :HELLO))
       (a)
       (c b)
       (mumble blah)

    (lick (lambda (a) b))
    => NIL
       NIL
       NIL
       NIL

There may also be container forms we'll want to handle in the future (for instance `with-open-file`, or similar)."
  (let ((local-vars) (local-fns) (local-macros))
    (labels ((recur (rest)
	       (when (consp rest)
		 (cond ((part-form? rest) rest)
		       ((member (car rest) '(let let* flet labels macrolet symbol-macrolet))
			(push-names (car rest) (second rest))
			(recur (caddr rest))))))
	     (push-names (bind-name bindings)
	       (loop for (k v) in bindings
		  do (cond ((member bind-name '(let let*))
			    (push k local-vars))
			   ((member bind-name '(flet labels))
			    (push k local-fns))
			   ((member bind-name '(macrolet symbol-macrolet))
			    (push k local-macros))))))
      (let ((res (recur module-body)))
	(when res 
	  (values res local-vars local-fns local-macros))))))

(defun find-dependencies (module-body)
  (multiple-value-bind (licked-body local-vars local-fns local-macros) (lick module-body)
    (flet ((single-dep (name)
	     (unless (or (member name local-vars)
			 (member name local-fns)
			 (member name local-macros))
	       (first 
		(for-all 
		 `(and (?id :name ,name) (?id :hash ?hash)) 
		 :in *strap-wolf* 
		 :collect ?hash)))))
      (when (eq (car licked-body) 'container)
	(loop for term in (second licked-body)
	   for dep = (cond ((atom term) (single-dep term))
			   ((or (atom (second term)) 
				(part-form? (second term)))
			    nil)
			   (t (single-dep (caadr term))))
	   when dep collect dep)))))
