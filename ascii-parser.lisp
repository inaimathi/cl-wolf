(in-package #:cl-wolf)

;;;;;;;;;; Sugar-level: DIABEETUS
(defun parse-ascii-graph (str)
  (coerce (split-sequence:split-sequence #\newline str :remove-empty-subseqs t) 'vector))

(defun read-ascii-graph (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil)
       until (eql c #\#) do (write-char c out))))

(defun ascii-graph (stream char arg)
  (declare (ignore char arg))
  (parse-ascii-graph (read-ascii-graph stream)))

(set-dispatch-macro-character #\# #\> #'ascii-graph)

(defun root-finder ()
  (reactor
    (out! :out
	  (remove 
	   nil (loop for y from 0 for ln across message
		  collect (loop for x from 0 for chr across ln
			     do (case chr
				  (#\: (return (list x y :pin)))
				  (#\- (return (list x y :arrow)))
				  (#\_ (return (list x y :arrow)))
				  (#\s (return (list x y :self-part)))
				  (#\space nil)
				  (t (return nil)))))))))

(defun ascii-graph-walker ()
  (reactor
    (let ((lines (in! :lines))
	  (roots (in! :roots)))
      (labels ((recur (x y state)
		 (funcall state x y))))
      (out! :out (list lines roots)))))

(defun part-finder ()
  (reactor 
    (out! 
     :out (remove-duplicates
	   (sort
	    (loop for y from 0 for ln across message
	       append (let ((state :looking)
			    (res nil)
			    (jump-to 0))
			(loop for x from 0 for chr across ln
			   do (case state
				(:looking
				 (cond ((eql chr #\space) nil)
				       ((member chr '(#\: #\- #\_ #\> #\\ #\/ #\|))
					(setf state :skipping))
				       (t
					(multiple-value-bind (term ends-at) (read-from-string ln t nil :start x)
					  (push term res)
					  (setf state :jumping
						jump-to ends-at)))))
				(:jumping
				 (when (>= x jump-to) (setf state :looking)))
				(:skipping
				 (when (eql chr #\space) (setf state :looking))))
			   finally (return res))))
	    #'> :key (lambda (p) (if (atom p) 1 0)))
	   :key (lambda (p) (if (atom p) p (car p)))))))

(defun line-tracer ()
  (reactor
    (let ((roots (in! :roots))
	  (lines (in! :lines)))
      ;; TODO
      )))


(defun container-generator ()
  (reactor
    (let ((parts (in! :parts))
	  (conns (in! :conns)))
      (out! :out `(container ,parts ,@conns)))))


(defun ascii-graph-parser ()
  (container
      ((tap (mk-printer))
       (roots (root-finder))
       (walker (ascii-graph-walker)))
    ((self :in) -> (roots :in) (walker :lines))
    ((roots :out) -> (walker :roots))
    ((walker :out) -> (tap :in))))

(defparameter *parser* (ascii-graph-parser))

(send! *parser* :in #(":a ---> :a (a (pull-pairer)) :out ---> :in printer" ":b ---> :b a"))

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
self ---> splitter ---> pairer ---> printer
                    \___________||
                     \_ counter _|
#

#>
self ---- splitter ---- pairer ---- printer
                    \-----------||
                     \- counter -|
#

(let ((lns #("----- countdown ---- printer"
	     "  |              \\------- decrement --"
	     "  |----------------------------------/"))
      (explored (make-hash-table :test 'equal))
      (facts nil))
  (labels ((recur (x y &optional prev)
	     (when (ix x y)
	       (case (ix x y)
		 (#\- (get-arrow x y :prev (or prev (list :part 'self)) :conn? t))
		 (#\: (get-pin x y :prev prev))
		 (t (get-part x y :prev prev)))))

	   (get-arrow (x y &key (arr (list :arrow (gensym))) prev (xd 1) conn?)
	     (unless (and conn? (explored? x y))
	       (explored! x y)
	       (prev! prev arr)
	       (when (eql #\| (ix x (- y 1)))
		 (connect-up x (- y 1) arr))
	       (let ((y2 (+ y 1)))
		 (when (ix x y2)
		   (case (ix x y2)
		     (#\\ (connect-down-right x y2 arr))
		     (#\/ (connect-down-left x y2 arr)))))
	       (let ((x2 (+ x xd)))
		 (case (ix x2 y)
		   (#\- (get-arrow x2 y :arr arr :xd xd))
		   (#\| (connect-up x2 y arr))
		   (t (recur x2 y arr))))))

	   (connect-down (x y arr xd char)
	     (when (eql char (ix x (+ y 1)))
	       (connect-down x (+ y 1) arr xd char))
	     (when (eql char (ix (+ x xd) (+ y 1)))
	       (connect-down (+ x xd) (+ y 1) arr xd char))
	     (when (ix (+ x xd) y)
	       (case (ix (+ x xd) y)
		 (#\- (get-arrow (+ x xd) y :arr arr :xd xd))
		 (#\space nil)
		 (t (recur (+ x xd) y arr)))))

	   (connect-down-right (x y arr)
	     (when (eql #\\ (ix x (+ y 1)))
	       (connect-down-right x (+ y 1) arr))
	     (when (eql #\\ (ix (+ x 1) (+ y 1)))
	       (connect-down-right (+ x 1) (+ y 1) arr))
	     (when (ix (+ x 1) y)
	       (case (ix (+ x 1) y)
		 (#\- (get-arrow (+ x 1) y :arr arr))
		 (#\space nil)
		 (t (recur (+ x 1) y arr)))))

	   (connect-down-left (x y arr)
	     (format t "Connecting /...~%")
	     (when (eql #\/ (ix x (+ y 1)))
	       (connect-down-left x (+ y 1) arr))
	     (when (eql #\/ (ix (+ x -1) (+ y 1)))
	       (connect-down-left (+ x -1) (+ y 1) arr))
	     (when (ix (+ x -1) y)
	       (case (ix (+ x -1) y)
		 (#\- (get-arrow (+ x -1) y :arr arr :xd -1))
		 (#\space nil)
		 (t (recur (+ x -1) y arr)))))

	   (connect-up (x y arr)
	     (case (ix x (- y 1))
	       (#\| (connect-up x (- y 1) arr))
	       (#\- (get-arrow x (- y 1) :arr arr))))
	   
	   (get-pin (x y &key prev)
	     (multiple-value-bind (form ends-at) (read-from-string (aref lns y) nil nil :start x)
	       (prev! prev (list :pin form))
	       (recur ends-at y (list :pin form))))

	   (get-part (x y &key prev)
	     (multiple-value-bind (form ends-at) (read-from-string (aref lns y) nil nil :start x)
	       (prev! prev (list :part form))
	       (recur ends-at y (list :part form))))
	   (ix (x y) (ignore-errors (char (aref lns y) x)))
	   (explored! (x y) (setf (gethash (cons x y) explored) t))
	   (explored? (x y) (gethash (cons x y) explored))
	   (prev! (a b) (when (and a b) (push (list a :to b) facts))))
    (recur 0 0)
    facts))

;; (let ((a self) (b splitter) (c pairer) (d counter) (e printer))
;;   (a :in) :connects-to (b :in)
;;   (b :out) :connects-to (c :in)
;;   (b :out) :connects-to (d :in)
;;   (b :out) :connects-to (e :in)
;;   (c :out) :connects-to (e :in)
;;   (d :out) :connects-to (e :in))


;; (container
;;     (splitter pairer printer)
;;   ((self :in) -> (splitter :in))
;;   ((splitter :out) -> (pairer :in) (counter :in) (printer :in))
;;   ((pairer :out) -> (printer :in))
;;   ((counter :out) -> (printer :in)))

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
