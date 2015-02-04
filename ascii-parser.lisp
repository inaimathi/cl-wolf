(in-package #:cl-wolf)

;;;;;;;;;; Sugar-level: DIABEETUS
(defun read-ascii-graph (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil) until (eql c #\#) 
       do (write-char c out))))

(defun ascii-graph (stream char arg)
  (declare (ignore char arg))
  (parse-ascii-graph (read-ascii-graph stream)))

(set-dispatch-macro-character #\# #\> #'ascii-graph)

(defun find-roots (lines)
  (remove nil
	  (loop for y from 0 for ln across lines
	     collect (loop for x from 0 for chr across ln
			do (cond ((member chr '(#\: #\- #\> #\_)) (return (list x y)))
				 ((eql chr #\space) nil)
				 (t (return nil)))))))

(defun arrow? (thing) (and (listp thing) (eq :arrow (car thing))))
(defun port? (thing) (and (listp thing) (eq :port (car thing))))
(defun part? (thing) (and (listp thing) (eq :part (car thing))))

;; no, you really need connections (some arrows connect to multiple things)
(defun walk-graph-from (x y lines)
  (let ((explored (make-hash-table :test 'equal))
	(facts nil))
    (labels ((recur (x y &optional prev)
	       (let ((char (ix x y)))
		 (cond ((ws? char) 
			(skip-whitespace x y prev))
		       ((arrow-char? char)
			(get-arrow x y :prev prev :conn? t))
		       ((and (null char) (arrow? prev))
			(fact! prev (list :part 'self)))
		       ((null char) nil)
		       (t (get-form x y prev)))))

	     (get-arrow (x y &key (arr (list :arrow (gensym))) prev (xd 1) conn?)
	       (unless (and conn? (explored? x y))
		 (explored! x y)
		 (fact! prev arr)
		 (when (eql #\| (ix x (- y 1)))
		   (connect-up x (- y 1) arr))
		 (let ((y2 (+ y 1)))
		   (when (ix x y2)
		     (case (ix x y2)
		       (#\\ (connect-down-right x y2 arr))
		       (#\/ (connect-down-left x y2 arr)))))
		 (let ((char (ix (+ x xd) y)))
		   (cond ((or (arrow-char? char) (member char '(#\\ #\/)))
			  (get-arrow (+ x xd) y :arr arr :xd xd))
			 ((eql char #\|) 
			  (connect-up (+ x xd) y arr))
			 (t (recur (+ x xd) y arr))))))

	     (connect-down (x y arr xd char)
	       (when (eql char (ix x (+ y 1)))
		 (connect-down x (+ y 1) arr xd char))
	       (when (eql char (ix (+ x xd) (+ y 1)))
		 (connect-down (+ x xd) (+ y 1) arr xd char))
	       (let ((char (ix (+ x xd) y)))
		 (cond ((arrow-char? char)
			(get-arrow (+ x xd) y :arr arr :xd xd))
		       ((eql #\space char) nil)
		       (t (recur (+ x xd) y arr)))))
	     (connect-down-right (x y arr)
	       (connect-down x y arr 1 #\\))
	     (connect-down-left (x y arr)
	       (connect-down x y arr -1 #\/))

	     (connect-up (x y arr)
	       (let ((char (ix x (- y 1))))
		 (cond ((eql #\| char)
			(connect-up x (- y 1) arr))
		       ((arrow-char? char)
			(get-arrow x (- y 1) :arr arr)))))
	     
	     (get-form (x y prev)
	       (multiple-value-bind (form ends-at) (read-from-string (aref lines y) nil nil :start x)
		 (let ((f (if (keywordp form) (list :port form) (list :part form))))
		   (fact! prev f)
		   (recur ends-at y f))))

	     (skip-whitespace (x y prev)
	       (if (ws? (ix (+ x 1) y))
		   (skip-whitespace (+ x 1) y prev)
		   (recur (+ x 1) y prev)))

	     (arrow-char? (char) (member char '(#\- #\> #\_)))
	     (ws? (char) (member char '(#\space #\tab)))
	     (ix (x y) (ignore-errors (char (aref lines y) x)))
	     (explored! (x y) (setf (gethash (cons x y) explored) t))
	     (explored? (x y) (gethash (cons x y) explored))
	     (fact! (a b) (when (and a b) (push (list a :to b) facts))))
      (recur x y)
      (reverse facts))))

(defun find-parts (walk)
  (loop for (a _ b) in walk
     when (part? a) collect (second a)
     when (part? b) collect (second b)))

(defun desugar (walk)
  (flet ((self? (p) (and (part? p) (eq 'self (second p)))))
    (loop for conn in (if (or (arrow? (caar walk)) (port? (caar walk)))
			  (cons `((:part self) :to ,(caar walk)) walk)
			  walk)
       if (and (arrow? (first conn)) (part? (third conn)))
       append (let ((port-name (if (self? (third conn)) :out :in)))
		`((,(first conn) :to (:port ,port-name)) ((:port ,port-name) :to ,(third conn))))
       else if (and (part? (first conn)) (arrow? (third conn)))
       append (let ((port-name (if (self? (first conn)) :in :out)))
		`((,(first conn) :to (:port ,port-name)) ((:port ,port-name) :to ,(third conn))))
       else collect conn)))

(defun merge-ports (desugared-walk)
  (flet ((name-of (p) (if (consp (second p)) (car (second p)) (second p))))
    (let ((input desugared-walk))
      (loop for conn = (pop input) while conn
	 if (and (part? (first conn)) (port? (third conn)))
	 collect `((,(name-of (first conn)) ,(second (third conn))) :to ,(third (pop input)))
	 else if (and (arrow? (first conn)) (port? (third conn)))
	 collect `(,(first conn) :to (,(name-of (third (pop input))) ,(second (third conn))))))))

(defun assemble-connections (merged-walk)
  (loop for conn in merged-walk
     when (arrow? (third conn))
     append (loop for c2 in merged-walk
	       when (equal (first c2) (third conn))
	       collect `(,(first conn) -> ,(third c2)))))

(defun dedupe-parts (parts)
  (remove 'self
	  (remove-duplicates
	   (sort parts #'> :key (lambda (p) (if (symbolp p) 1 0)))
	   :key (lambda (p) (if (consp p) (car p) p)))))

(defun condense-connections (conns)
  (let ((res (make-hash-table :test 'equal)))
    (loop for (src _ dst) in conns
       do (push dst (gethash src res nil)))
    (loop for k being the hash-keys of res
       for v being the hash-values of res
       collect (cons k (cons '-> v)))))

(defun mappend (fn lst) 
  (loop for elem in lst append (funcall fn elem)))

(defun parse-ascii-graph (str)
  (let* ((lines (coerce (split-sequence #\newline str) 'vector))
	 (roots (find-roots lines))
	 (walks (loop for (x y) in roots
		   collect (walk-graph-from x y lines)))
	 (final `(container
		     ,(dedupe-parts (mappend #'find-parts walks))
		   ,@(condense-connections 
		      (mappend 
		       (lambda (w)
			 (assemble-connections 
			  (merge-ports (desugar w))))
		       walks)))))
    (format t "Compiled~%===============~a~%===============~%to ~a~%" str final)
    final))

#>
 ---> mk-greeter ---> mk-counter ---> mk-printer
                 \________________|
#

(container
    (mk-greeter mk-counter mk-printer)
  ((self :in) -> (mk-greeter :in))
  ((mk-greeter :out) -> (mk-counter :in) (mk-printer :in))
  ((mk-counter :out) -> (mk-printer :in)))

#>
---> splitter ---> pairer ---> printer
                \___________||
                 \_ counter _|
#

#>
self ---- splitter ---- pairer ---- printer
                    \-----------||
                     \- counter -|
#

":a ---- :a (a (pull-pairer)) :out ---- :in printer
:b ---- :b a"

#(":a ---- :a (a (pull-pairer)) :out ---- :in printer"
  ":b ---- :b a")

#("----- countdown ---- printer"
  "  |              \\------- decrement --"
  "  |----------------------------------/")

#>
:a ---> :a (a (pull-pairer)) :out ---> :in printer
:b ---> :b a
#

#>
 ---> buffer ---> parser ---> router ---> http-response ---> writer 
             \___________\___________\__________________\__________\___---> printer
#

#(" ---> buffer ---> parser ---> router ---> http-response ---> writer "
  "             \\___________\\___________\\__________________\\__________\\___---> printer")

#>
 ---> countdown ---> printer
  |             \_-> decrement
  |_____________________/
#
