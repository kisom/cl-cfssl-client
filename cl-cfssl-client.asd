;;;; cl-cfssl-client.asd

(asdf:defsystem #:cl-cfssl-client
  :description "Describe cl-cfssl-client here"
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT license"
  :depends-on (#:drakma
               #:fare-mop
               #:flexi-streams
               #:ironclad
               #:local-time
               #:s-base64
               #:split-sequence
               #:yason)
  :serial t
  :components ((:file "package")
               (:file "errors")
               (:file "utils")
               (:file "requests")
               (:file "auth")
               (:file "client")
               (:file "groups")))

