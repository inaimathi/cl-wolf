(in-package #:cl-wolf)

;;;;;;;;;; Sugar-level: DIABEETUS
(defun parse-ascii-graph (str) str)

(defun read-ascii-graph (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil)
       until (eql c #\#) do (write-char c out))))

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
		   (cond ((arrow-char? char)
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

	     (ws? (char) (member char '(#\space #\tab)))
	     (ix (x y) (ignore-errors (char (aref lines y) x)))
	     (explored! (x y) (setf (gethash (cons x y) explored) t))
	     (explored? (x y) (gethash (cons x y) explored))
	     (fact! (a b) (when (and a b) (push (list a :to b) facts))))
      (recur x y)
      (reverse facts))))

;; (defun desugar (walk)
;;   (let ((res walk))
;;     (cond ((arrow? (caar res))
;; 	   (let ((arr (caar res)))
;; 	     (push `((:port :in) :to ,arr) res)
;; 	     (push `((:part self) :to ,arr) res)))
;; 	  ((port? (caar res))
;; 	   (push `((:part self) :to ,(caar res)))))
;;     (loop for (conn-a conn-b) on walk
;;        for (src-a _ dst-a) = conn-a
;;        for (src-b _1 dst-b) = conn-b
;;        append (cond ((and (arrow? (car conn-a)))))))
;;   (let ((desugared)
;; 	(parts nil))
    
;;     (loop for (a b) on walk
;;        for (src-a _0 dst-a) = a
;;        for (src-b _1 dst-b) = b
;;        append (list a b))))

(defun parse-ascii (str)
  (let* ((lines (coerce (split-sequence #\newline str) 'vector))
	 (roots (find-roots lines)))
    (loop for (x y) in roots
       collect (condense-connections (walk-graph-from x y lines)))))

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

#>
 ---> countdown ---> printer
  ^             \_-> decrement
  |_____________________/
#
