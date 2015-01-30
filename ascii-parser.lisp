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

(defparameter *test-res* 
  (let ((lns #("self ---- splitter ---- pairer ---- printer"
	       "                    \\-----------||"
	       "                    \\-- counter -|"))
	(coord-map (make-hash-table :test 'equal))
	(facts nil))
    (labels ((recur (x y &optional prev)
	       (format t "Recurring (~s)...~%" (ix x y))
	       (when (ix x y)
		 (case (ix x y)
		   (#\- (get-arrow x y :prev prev))
		   (#\: (get-pin x y :prev prev))
		   (#\\ #'connect-down-right)
		   (#\/ #'connect-down-left)
		   (#\| #'connect-up)	       
		   (t (get-part x y :prev prev)))))

	     (get-arrow (x y &key (arr (list :arrow (gensym))) prev)
	       (format t "Getting arrow ...~%")
	       (when prev (prev! prev arr))
	       (set! x y arr)
	       (when (eql #\| (ix x (- y 1)))
		 (connect-up x (- y 1) arr))
	       (when (ix x (+ y 1))
		 (case (ix x (+ y 1))
		   (#\\ (connect-down-right x (+ y 1) arr))
		   (#\/ (connect-down-left x (+ y 1) arr))))
	       (case (ix (+ x 1) y)
		 (#\- (get-arrow (+ x 1) y :arr arr))
		 (#\| (connect-up (+ x 1) y arr))
		 (t (recur (+ x 1) y arr))))

	     (connect-down-right (x y arr)
	       (format t "Connecting \\...~%")
	       (set! x y arr)
	       (when (eql #\\ (ix x (+ y 1)))
		 (connect-down-right x (+ y 1) arr))
	       (when (ix (+ x 1) y)
		 (case (ix (+ x 1) y)
		   (#\- (get-arrow (+ x 1) y :arr arr))
		   (#\space nil)
		   (t (recur (+ x 1) y arr)))))

	     (connect-down-left (x y arr)
	       (format t "Connecting /...~%")
	       (set! x y arr))

	     (connect-up (x y arr)
	       (format t "Connecting |...~%")
	       (set! x y arr)
	       (when (eql #\| (ix x (- y 1)))
		 (connect-up x (- y 1) arr)))
	     
	     (get-pin (x y &key prev)
	       (format t "Getting PIN...~%")
	       (multiple-value-bind (form ends-at) (read-from-string (aref lns y) nil nil :start x)
		 (when prev (prev! prev form))
		 (loop for i from x to ends-at do (set! i y (list :pin #\> form)))
		 (recur ends-at y (list :pin form))))

	     (get-part (x y &key prev)
	       (format t "Getting PART...~%")
	       (multiple-value-bind (form ends-at) (read-from-string (aref lns y) nil nil :start x)
		 (when prev (prev! prev form))
		 (loop for i from x to ends-at do (set! i y (list :part #\M form)))
		 (recur ends-at y (list :part form))))
	     (ix (x y) (ignore-errors (char (aref lns y) x)))
	     (set! (x y thing) (setf (gethash (cons x y) coord-map) thing))
	     (prev! (a b) (push (list a :to b) facts)))
      (recur 0 0)
      (values coord-map facts))))

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
