;;;; alg-design.asd

(asdf:defsystem #:alg-design
  :description "Describe alg-design here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:parse-number
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "alg-design")))

