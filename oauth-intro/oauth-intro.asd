;;;; oauth-intro.asd

(asdf:defsystem #:oauth-intro
  :description "Describe oauth-intro here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:ningle #:clack #:spinneret #:ironclad #:qbase64 #:dexador)
  :components ((:file "package")
               (:file "oauth-intro")))
