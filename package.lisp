;;;; package.lisp

(defpackage #:cl-wolf
  (:use #:cl #:usocket)
  (:import-from #:split-sequence :split-sequence)
  (:export #:container #:reactor
	   #:in! #:out! #:->))

