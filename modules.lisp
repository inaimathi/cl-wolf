(in-package #:cl-wolf)

(defparameter *strap-wolf* 
  (if (cl-fad:file-exists-p *module-base*)
      (load! *module-base*)
      (make-fact-base :file-name *module-base*)))

(defparameter *resu-wolf* "inaimathi")

(defun install-module (name &key server)
  ;; check install servers for given module. Install if they existz
  nil)

(defun upgrade-module (name &key server) 
  ;; check install servers for new versions. Install them if they exist.
  nil)

(defun load-by-hash (hash)
  (let ((mod (for-all 
	      `(and (?id :name (?package ?name))
		    (?id :hash ,hash)
		    (?id :parameters ?args) (?id :source ?body))
	      :in *strap-wolf* :do (return `(module ,(intern ?name) ,?args ,@?body))))
	(deps (dependencies-of hash)))
    (cond ((and mod deps)
	   (loop for d in deps 
	      do (format t "Loading dependency ~a ...~%" d)
	      do (load-by-hash d))
	   (eval mod))
	  (mod (eval mod))
	  (t (error "TODO - check install servers here. Fail if not found.")))))

(defun load-by-name (name &key at by)
  (let ((res (sort 
	      (for-all 
	       `(and (?id :name ,(->wolf-name name))
		     ,@(when by `(?id :author ,by))
		     (?id :hash ?hash) (?id :registered ?stamp))
	       :in *strap-wolf* 
	       :collect (list ?stamp ?hash))
	      #'> :key #'first)))
    ;; If matching modules found, and designated version exists, install it
    ;; If matching module found, and designated version doesn't exist, try to upgrade the module, then attempt installing ONCE more
    ;; If no matching module found, try installing it
    (cond ((and res at)
	   (let ((loaded? nil))
	     (loop for (stamp hash) in res
		when (>= at stamp) do (setf loaded? (load-by-hash hash))
		until (>= at stamp))
	     (unless loaded? 
	       (error "No version available at ~s for module ~s..." at name))))
	  (res
	   (load-by-hash (cadar res)))
	  (t
	   (error "TODO - check install servers, fail if not found")))))

(defmethod dependencies-of ((hash string))
  (for-all `(and (?id :hash ,hash)
		 (?id :depends-on ?hash))
	   :in *strap-wolf* :collect ?hash))

(defmethod dependencies-of ((name symbol))
  (for-all `(and (?id :name ,(->wolf-name name))
		 (?id :depends-on ?hash))
	   :in *strap-wolf* :collect ?hash))

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
	      (alias-function ',name ',(intern (second alias) (first alias)))
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
