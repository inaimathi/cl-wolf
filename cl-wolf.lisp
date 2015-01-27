;;;; cl-wolf.lisp
(in-package #:cl-wolf)

;;;;;;;;;; Basic parts
(defmethod add-part! ((self container) (new-child part))
  (push new-child (parts self))
  (setf (parent new-child) self)
  nil)
(defmethod run! ((self container))
  (let ((msg (pop! (input self))))
    (when msg
      (mapc 
       (lambda (c) 
	 (broadcast! c msg))
       (dispatch (incoming self) (tag msg))))))

(defmethod run! ((self reactor))
  (let ((msg (pop! (input self))))
    (when msg (funcall (body self) (tag msg) (payload msg)))))

(defmethod run! ((self deactor))
  (when (funcall (guard self) self)
    (apply (body self) 
	   (loop for m in (expecting self) 
	      collect (payload (pull! (input self) m))))))

;;;;;;;;;; Connection and dispatch
(defun conn (tag target-tag target)
  (make-instance 'connection :tag tag :target-tag target-tag :target target))

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
(defun msg (tag payload)
  (make-instance 'message :tag tag :payload payload))

(defmethod broadcast! ((self part) (m message))
  (mapc (lambda (conn) (broadcast! conn m)) (dispatch self (tag m))))
(defmethod broadcast! ((conn connection) (m message))
  (push! 
   (if (eq (tag conn) (target-tag conn))
       m 
       (msg (target-tag conn) (payload m)))
   (input (target conn)))
  (run! (target conn)))

;;;;;;;;;; Sugar
;;;;; Runtime/debugging sugar
(defmethod send! ((self part) tag payload)
  (push! (msg tag payload) (input self))
  (run! self)
  nil)

;;;;; Definition sugar
;;; Basic reactors
(defmacro make-reactor (&body body)
  `(let ((self (make-instance 'reactor)))
     (flet ((out! (tag payload)
	      (broadcast! self (msg tag payload))))
       (declare (ignorable #'out!))
       (setf (body self) 
	     (lambda (tag message)
	       (declare (ignorable tag message))
	       ,@body)))
     self))

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

(defmacro make-container ((&rest label/part-pairs) &body connections)
  `(let ((self (make-instance 'container))
	 ,@(mapcar (lambda (pair)
		     (if (symbolp pair)
			 `(,pair (,pair))
			 `(,(first pair) ,(second pair))))
		   label/part-pairs))
     ,@(mapcar (lambda (pair) `(add-part! self ,(if (symbolp pair) pair (car pair))))
	       label/part-pairs)
     ,@(process-connections connections)
     self))

;;; Deactors (faux-blocking reactors)
(defun process-get!-calls (tree)
  (let ((syms nil)
	(port-list nil)
	(counts (make-hash-table)))
    (labels ((recur (thing)
	       (cond ((null thing) nil)
		     ((atom thing) thing)
		     ((eq 'get! (car thing))
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
		      collect `(>= (length-of (input self) ,k) ,v))))
	 `(lambda ,(mapcar #'car (reverse syms))
	    ,@processed)
	 (reverse port-list))))))

(defmacro make-deactor (&body body)
  (multiple-value-bind (guard final-body ports) (process-get!-calls body)
    `(let ((self (make-instance 'deactor)))
       (flet ((out! (tag payload)
		(broadcast! self (msg tag payload))))
	 (declare (ignorable #'out!))
	 (setf (body self) ,final-body
	       (input self) (queue-table (list ,@ports))
	       (guard self) ,guard
	       (expecting self) (list ,@ports)))
       self)))
