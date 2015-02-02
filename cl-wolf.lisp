;;;; cl-wolf.lisp
(in-package #:cl-wolf)

;;;;;;;;;; Basic parts
(defmethod add-part! ((self container) (new-child part))
  (push new-child (parts self))
  (setf (parent new-child) self)
  nil)
(defmethod run! ((self container))
  (let ((msg (pop! (inbox self))))
    (when msg
      (mapc 
       (lambda (c) 
	 (deepcast! c msg))
       (dispatch (incoming self) (tag msg))))))

(defmethod run! ((self reactor))
  (let ((msg (pop! (inbox self))))
    (when msg (funcall (body self) (tag msg) (payload msg)))))

(defmethod run! ((self deactor))
  (when (funcall (guard self) self)
    (apply (body self) 
	   (loop for m in (expecting self) 
	      collect (payload (pull! (inbox self) m))))))

;;;;;;;;;; The Scheduler
(defparameter *work* (queue))
(defmethod run-system! ((q queue))
  (loop until (empty? q) do (run! (pop! q))))

(defmethod send! ((self part) tag payload)
  (push! (msg tag payload) (inbox self))
  (push! self *work*)
  (run-system! *work*)
  nil)

;;;;;;;;;; Connection and dispatch
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

;;;;;;;;;; Messages
(defmethod deepcast! ((self part) (m message))
  (mapc (lambda (conn) (deepcast! conn m)) (dispatch self (tag m))))
(defmethod deepcast! ((conn connection) (m message))
  (push! 
   (if (eq (tag conn) (target-tag conn))
       m 
       (msg (target-tag conn) (payload m)))
   (inbox (target conn)))
  (push! (target conn) *work*))

;;;;;;;;;; Sugar
;;; Basic containers
(defun process-connections (conns)
  (flet ((single (clause)
	   (destructuring-bind (sources targets) (split-sequence '-> clause)
	     (loop for (part-name tag) in sources
		for src = (if (eq part-name 'self) `(incoming ,part-name) part-name)
		append (loop for (target-name target-tag) in targets
			  for tgt = (if (eq target-name 'self) `(outgoing ,self) target-name)
			  collect `(connect! ,src ,tag ,tgt ,target-tag))))))
    (loop for c in conns append (single c))))

(defun find-container-ports (conns)
  (flet ((ports-in (lst)
	   (loop for (part port) in lst
	      when (eq part 'self) collect port)))
    (loop for c in conns for (sources targets) = (split-sequence '-> c)
       append (ports-in sources) into ins
       append (ports-in targets) into outs
       finally (return (values (remove-duplicates ins) 
			       (remove-duplicates outs))))))

(defmacro container ((&rest label/part-pairs) &body connections)
  (multiple-value-bind (ins outs) (find-container-ports connections)
    `(let ((self (make-instance 'container))
	   ,@(mapcar (lambda (pair)
		       (if (symbolp pair)
			   `(,pair (,pair))
			   `(,(first pair) ,(second pair))))
		     label/part-pairs))
       (setf (input-ports self) (list ,@ins)
	     (output-ports self) (list ,@outs))
       ,@(mapcar (lambda (pair) `(add-part! self ,(if (symbolp pair) pair (car pair))))
		 label/part-pairs)
       ,@(process-connections connections)
       self)))

;;; Push and pull reactors
(defun find-reactor-out-ports (body)
  (let ((ports nil))
    (labels ((recur (thing)
	       (cond ((atom thing) nil)
		     ((eq 'out! (car thing))
		      (push (second thing) ports))
		     (t (recur (car thing))
			(recur (cdr thing))))))
      (recur body)
      (remove-duplicates ports))))

(defun process-in!-calls (tree)
  (let ((syms nil)
	(port-list nil)
	(counts (make-hash-table)))
    (labels ((recur (thing)
	       (cond ((null thing) nil)
		     ((atom thing) thing)
		     ((eq 'in! (car thing))
		      (let ((new (gensym)))
			(push (list new thing) syms)
			(push (cadr thing) port-list)
			(incf (gethash (cadr thing) counts 0))
			new))
		     (t (cons (recur (car thing))
			      (recur (cdr thing)))))))
      (let ((processed (recur tree)))
	(values
	 `(lambda (self)
	    (and ,@(loop for k being the hash-keys of counts
		      for v being the hash-values of counts
		      collect `(>= (length-of (inbox self) ,k) ,v))))
	 `(lambda ,(mapcar #'car (reverse syms))
	    ,@processed)
	 (reverse port-list))))))

(defun with-self (self body)
  `(let ((self ,self))
     (flet ((out! (tag payload)
	      (deepcast! self (msg tag payload))))
       (declare (ignorable #'out!))
       ,body)
     self))

(defun reactor-template (body)
  (with-self '(make-instance 'reactor)
    `(setf (body self) 
	   (lambda (tag message)
	     (declare (ignorable tag message))
	     ,@body)
	   (output-ports self)
	   (list ,@(find-reactor-out-ports body)))))

(defun deactor-template (body)
  (multiple-value-bind (guard final-body ports) (process-in!-calls body)
    (with-self '(make-instance 'deactor)
      `(setf (body self) ,final-body
	     (inbox self) (queue-table (list ,@ports))
	     (guard self) ,guard
	     (expecting self) (list ,@ports)
	     (input-ports self) (list ,@(remove-duplicates ports))
	     (output-ports self) (list ,@(find-reactor-out-ports body))))))

(defun tree-find (elem tree &key (test #'eq))
  (subst-if 
   nil (lambda (thing) 
	 (when (funcall test elem thing)
	   (return-from tree-find t)))
   tree)
  nil)

(defmacro reactor (&body body)
  (if (tree-find 'in! body)
      (deactor-template body)
      (reactor-template body)))
