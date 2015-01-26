;;;; cl-wolf.asd

(asdf:defsystem #:cl-wolf
  :serial t
  :description "Describe cl-wolf here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:split-sequence)
  :components ((:file "package")
	       (:file "input-structures")
               (:file "cl-wolf")))

