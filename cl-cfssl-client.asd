;;;; cl-cfssl-client.asd

(asdf:defsystem #:cl-cfssl-client
  :description "Describe cl-cfssl-client here"
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT license"
  :depends-on (#:drakma
               #:split-sequence
               #:flexi-streams
               #:yason
               #:fare-mop
               #:ironclad
               #:s-base64)
  :serial t
  :components ((:file "package")
               (:file "errors")
               (:file "utils")
               (:file "auth")
               (:file "requests")
               (:file "cl-cfssl-client")
               (:file "groups")))

