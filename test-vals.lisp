(in-package :cfssl-client)

(defconstant +ca-data-path+ #P"/home/kyle/tmp/ca/")
(defvar *csr-path* (merge-pathnames +ca-data-path+
                                    "cert.csr"))
(defvar *test-sn*
  (make-instance 'subject-name
                 :country "US"
                 :state "California"
                 :locality "San Francisco"))
(defvar *test-req*
  (make-instance 'sign-request
                 :hosts '("public.kyleisom.net" "ca.kyleisom.net")
                 :certificate-request (read-file-string
                                       (merge-pathnames
                                        +ca-data-path+
                                        "cert.csr"))
                 :subject (make-instance 'subject
                                         :common-name "public CA"
                                         :names (list *test-sn*))
                 :profile ""))
(defvar *local-server* (make-instance 'server :host "127.0.0.1")) ;

(new-sign-request-from-file "ca.example.org"
                            *csr-path*
                            (new-simple-subject :common-name "ca"
                                                :country "US"))
