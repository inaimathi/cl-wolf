(in-package #:cl-wolf)

;;;;;;;;;; Sugar-level: DIABEETUS
(defun parse-ascii-graph (str)
  (let ((lines (coerce (split-sequence:split-sequence #\newline str :remove-empty-subseqs t) 'vector))
	(left 0) (top 0))
    (loop for i from left
       while (every (lambda (ln) (eql #\space (char ln left))) lines)
       do (incf left))
    (let ((parts '(mk-greeter mk-counter mk-printer))
	  (conns (make-hash-table :test 'equal)))
      (push '(mk-greeter :in) (gethash '(self :in) conns nil))
      (push '(mk-counter :in) (gethash '(self :in) conns nil))
      `'(container ,parts ,@(loop for k being the hash-keys of conns
			       for v being the hash-values of conns
			       collect `(,k -> ,@v))))
    lines))

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

(defun read-ascii-graph (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil)
       until (eql c #\#) do (write-char c out))))

(defun ascii-graph (stream char arg)
  (declare (ignore char arg))
  (parse-ascii-graph (read-ascii-graph stream)))

(set-dispatch-macro-character #\# #\> #'ascii-graph)

(defun chunk-line (ln)
  (let ((res nil)
	(ix 0))
    (labels ((skip-whitespace ()
	       (if (eql #\space (char ln ix))
		   (progn (incf ix)
			  (skip-whitespace))
		   ix))
	     (get-arrow ()
	       (case (char ln ix)
		 (#\- (incf ix) (get-arrow))
		 (#\> ix)
		 (t (error "Parse error")))))
      (skip-whitespace)
      (push (list ix (get-arrow) :arrow) res)
      res)))

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
                    \__ counter _|
#

(container
    (splitter pairer printer)
  ((self :in) -> (splitter :in))
  ((splitter :out) -> (pairer :in) (counter :in) (printer :in))
  ((pairer :out) -> (printer :in))
  ((counter :out) -> (printer :in)))

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



;; divide into lines and tokens
;; 
