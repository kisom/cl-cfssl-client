;;;; cl-cfssl-client.asd

(asdf:defsystem #:cl-cfssl-client
  :description "Common Lisp CFSSL API client."
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT license"
  :depends-on (#:cl-fad
	       #:drakma
               #:fare-mop
               #:flexi-streams
               #:ironclad
               #:local-time
               #:s-base64
               #:split-sequence
               #:yason)
  :serial t
  :components ((:file "package")
	       (:file "api")
               (:file "errors")
               (:file "utils")
               (:file "requests")
               (:file "auth")
               (:file "client")
               (:file "groups")
               (:file "config")))

