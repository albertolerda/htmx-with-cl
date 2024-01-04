;;;; htmx-examples.asd

(asdf:defsystem #:htmx-examples
  :description "Describe htmx-examples here"
  :author "Alberto Lerda"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack #:ningle #:spinneret)
  :components ((:file "package")
               (:file "htmx-examples")))
