;;;; cl-latin-hc.asd

(asdf:defsystem #:cl-latin-hc
  :description "Describe cl-latin-hc here"
  :author "Caleb Marshall"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:select)
  :components ((:file "package")
               (:file "cl-latin-hc")))
