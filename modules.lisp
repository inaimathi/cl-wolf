(in-package #:cl-wolf)

(defparameter *strap-wolf* 
  (if (cl-fad:file-exists-p *module-base*)
      (load! *module-base*)
      (make-fact-base :file-name *module-base*)))

(defun install-module (name &key server)
  ;; check install servers for given module. Install if they existz
  nil)

(defun upgrade-module (name &key server) 
  ;; check install servers for new versions. Install them if they exist.
  nil)

(defun load-module (name)
  (let ((mod (first
	      (sort 
	       (for-all 
		`(and (?id :name ,(->wolf-name name))
		      (?id :parameters ?args) (?id :source ?body)
		      (?id :registered ?stamp))
		:in *strap-wolf* 
		:collect (list ?stamp ?args ?body))
	       #'> :key #'first))))
    (if mod 
	(eval `(module ,name ,(second mod) ,@(third mod)))
	(error "TODO - check install servers here. Fail if not found."))))

(defmethod module-exists? ((name symbol))
  (for-all 
   `(?id :name ,(->wolf-name name))
   :in *strap-wolf* :do (return ?id)))

(defmethod ->wolf-name ((name symbol))
  (list (package-name (symbol-package name)) (symbol-name name)))

(defmacro module (name args &body body)
  (assert (and (listp (car body)) (null (cdr body))) nil "A module may only contain one term.")
  (assert 
   (or (eq 'container (caar body)) (lick (car body))) nil
   "A module must either be a naked CONTAINER, or a REACTOR surrounded by local-definition forms.")
  ;; TODO think about whether this actually works properly.
  ;;   - if there's already a bound alias, just alias it, and don't bother registering
  ;;   - if there is an alias, but it isn't bound, define the function as NAME
  ;;   - if there's no alias, both define the function and register it
  ;;   - if NAME is not already an alias of the appropriate module, register it as such
  (let ((alias (for-all `(and (?id :hash ,(module-hash args body)) (?id :name ?wolf-name)) :in *strap-wolf* :do (return ?wolf-name))))
    (cond ((and (equal alias (->wolf-name name))
		(not (fboundp name)))
	   `(defun ,name ,args ,@body))
	  ((null alias)
	   `(progn
	      (defun ,name ,args ,@body)
	      (register-module! ',name ',args ',body)))
	  (t
	   `(progn
	      (alias-function ',name ',alias)
	      (register-module! ',name ',args ',body))))))

(defun register-module! (name args body)
  "Registers a module in the *strap-wolf* structure.
If there's already a module with the same name and hash, this is a no-op
If there's a module with the same name, but a different hash, we add a new version
If there's a module with the same hash, but a different name, we add an alias for that module
Otherwise, we register a new module"
  (let ((hash (module-hash args body))
	(pkg (package-name (symbol-package name)))
	(nm (symbol-name name)))
    (flet ((register-version! ()
	     (multi-insert!
	      *strap-wolf*
	      `((:name ,(->wolf-name name))
		(:parameters ,args) (:source ,body) (:hash ,hash) (:registered ,(get-universal-time))
		,@(mapcar (lambda (h) `(:depends-on ,h)) (find-dependencies (car body)))))
	     (write! *strap-wolf*)))
      (cond ((for-all `(and (?id :name ,(->wolf-name name)) (?id :hash ,hash))
		      :in *strap-wolf* :do (return t))
	     :duplicate-module)
	    ((for-all `(?id :hash ,hash) :in *strap-wolf* :do (return t))
	     (let ((id (for-all `(?id :hash ,hash) :in *strap-wolf* :do (return ?id))))
	       (insert! *strap-wolf* (list id :name (->wolf-name name)))))
	    (t (register-version!))))))

(defun part-form? (thing)
  (and (listp thing) 
       (member (car thing) 
	       '(reactor container))))

(defun lick (module-body)
  "A body can be either a REACTOR or contained within an arbitrary number and order of local definition terms.
These include `let`, `let*`, `flet`, `labels`, `macrolet` and `symbol-macrolet`.
`lick` takes a module body, and either returns the chewy center (reactor) form, or NIL if no such form exists.

For example, 

    (lick 
      '(let ((a 1))
         (flet ((b () 2))
           (labels ((c () 3))
             (macrolet ((blah () 4)) 
	       (symbol-macrolet ((mumble 5))
	         (reactor (write :hello))))))))
    => (REACTOR (WRITE :HELLO))

    (lick (lambda (a) b))
    => NIL

There may also be container forms we'll want to handle in the future (for instance `with-open-file`, or similar)."
  (cond ((atom module-body) nil)
	((eq 'reactor (car module-body))
	 module-body)
	((member (car module-body) '(let let* flet labels macrolet symbol-macrolet))
	 (lick (caddr module-body)))))

(defun find-dependencies (module-body)  
  (flet ((single-dep (name)
	   (first 
	    (for-all 
	     `(and (?id :name ,(->wolf-name name)) (?id :hash ?hash)) 
	     :in *strap-wolf* 
	     :collect ?hash))))
    (when (eq (car module-body) 'container)
      (loop for term in (second module-body)
	 for dep = (cond ((atom term) (single-dep term))
			 ((or (atom (second term)) 
			      (part-form? (second term)))
			  nil)
			 (t (single-dep (caadr term))))
	 when dep collect dep))))
