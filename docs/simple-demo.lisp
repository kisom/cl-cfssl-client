(ql:quickload :cl-cfssl-client)
(defvar *local-server* (cfssl-client:new-server "127.0.0.1"))
(defvar *remote-server* (cfssl-client:new-server "ca.example.org"))

(defvar *certificate-request*
  (cfssl-client:new-certificate-request
   '("www.example.org" "mail.example.org" "example.org")
   (cfssl-client:new-subject-name :country "US"
				  :org "Example Organisation")
   :common-name "example.org"
   :key-spec cfssl-client:*rsa-2048-key*))

(defvar *key-and-csr*
  (cfssl-client:new-key-and-csr *local-server*
				*certificate-request*))

(format t "Certificate request: ~A~%"
	(gethash "certificate-request" *key-and-csr*))

(defvar *signing-request*
  (cfssl-client:new-sign-request
   (cfssl-client:hosts-of *certificate-request*)
   (gethash "certificate-request" *key-and-csr*)
   (cfssl-client:subject-of *certificate-request*)))

(defvar *certificate*
  (cfssl-client:sign *remote-server* *signing-request*))

(format t "Certificate: ~A~%" *certificate*)

(defvar *remote-ca-cert*
  (cfssl-client:info *remote-server*))
(format t "Remote CA's certificate: ~A~%" *remote-ca-cert*)

(defvar *remote-info*
  (cfssl-client:info *remote-server* :extra t))

(maphash (lambda (k v)
              (format t "~A: ~A~%" k v))
           *remote-info*)

