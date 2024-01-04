;;;; basic-htmx.asd

(asdf:defsystem #:basic-htmx
  :description "Describe basic-htmx here"
  :author "Alberto Lerda"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:ningle #:clack #:spinneret)
  :components ((:file "package")
               (:file "basic-htmx")))
