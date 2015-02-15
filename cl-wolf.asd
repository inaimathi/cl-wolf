;;;; cl-wolf.asd

(asdf:defsystem #:cl-wolf
  :serial t
  :description "Describe cl-wolf here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:cl-fad #:cl-ppcre #:fact-base #:ironclad #:split-sequence #:usocket)
  :components ((:file "package")
	       (:file "util")
	       (:file "model")
	       (:file "input-structures")
	       (:file "modules")
               (:file "cl-wolf")))

