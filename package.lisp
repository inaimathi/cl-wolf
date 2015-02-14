;;;; package.lisp

(defpackage #:cl-wolf
  (:use #:cl #:usocket #:fact-base)
  (:import-from #:split-sequence :split-sequence)
  (:import-from #:alexandria :with-gensyms)
  (:export #:container #:reactor
	   #:in! #:out! #:->))

