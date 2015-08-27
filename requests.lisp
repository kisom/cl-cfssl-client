(in-package #:cfssl-client)

(defgeneric clos-to-map (obj &key converter)
  (:documentation "Take a CLOS object and return it as a hash table."))

(defmethod clos-to-map (obj &key (converter #'keyword-to-downcase))
  "Given a CLOS object, create a hash table from the object."
  (with-new-hash-table (ht)
    (labels ((add-entries (lst)
               (unless (null lst)
                 (sethash (funcall converter (car lst)) (cadr lst) ht)
                 (add-entries (cddr lst)))))
      (add-entries (fare-mop:collect-slots obj)))))

(defclass subject-name ()
  ((country :initarg :country
            :type string
            :reader country-of)
   (state :initarg :state
          :type string
          :reader state-of)
   (locality :initarg :locality
             :type string
             :reader locality-of)
   (organization :initarg :org
                 :type string
                 :reader org-of)
   (org-unit :initarg :org-unit
             :type string
             :reader org-unit-of))
  (:documentation "subject-name specifies a single set of subject names."))

(defmethod clos-to-map ((subject-name subject-name) &key (converter #'keyword-to-downcase))
  "When presented with subject information, specific key values should be used."
  (with-new-hash-table (ht)
    (let ((alst (list-to-alist (fare-mop:collect-slots subject-name))))
      (if-present-set :country "C" alst ht)
      (if-present-set :state "ST" alst ht)
      (if-present-set :locality "L" alst ht)
      (if-present-set :org "O" alst ht)
      (if-present-set :org-unit "OU" alst ht))))

(defclass subject ()
  ((common-name :initarg :common-name
                :type string
                :reader common-name-of
                :documentation "The common name that should be used in the subject info.")
   (names :initarg :names
          :type (vector subject-name)
          :reader names-of
          :documentation "The list of subject names to use for the certificate."))
  (:documentation "subject represents certificate subject information."))

(defun new-simple-subject (&key common-name country  state city org org-unit)
  "Create a simple (most commonly used) subject for use in a request."
  (make-instance 'subject
                 :common-name common-name
                 :names
                 (to-list
                  (make-instance 'subject-name
                                 :country country
                                 :state state
                                 :locality city
                                 :org org
                                 :org-unit org-unit))))

(defmethod clos-to-map ((subject subject) &key (converter #'keyword-to-downcase))
  "When presented with subject information, #'clos-to-map should capitalise
the common name."
  (with-new-hash-table (ht)
    (sethash "CN" (common-name-of subject) ht)
    (sethash "names" (mapcar #'clos-to-map (names-of subject)) ht)))

(defclass sign-request ()
  ((hosts :initarg :hosts
          :type (vector string)
          :reader sign-request-hosts-of
          :documentation "Specifies the hosts that should be used as SANs.")
   (certificate-request :initarg :certificate-request
                        :type string
                        :reader sign-request-csr-of
                        :documentation "The PKCS #10 certificate
signing request that the CA should sign.")
   (subject :initarg :subject
            :reader sign-request-subject-of
            :type subject
            :documentation "The subject is an instance of the subject class describing the end holder of the certificate.")
   (profile :initarg :profile
            :initform ""
            :type string
            :reader sign-request-profile-of
            :documentation "The CA profile that should be used for signing.")
   (label :initarg :label
          :initform ""
          :type string
          :reader sign-request-label-of
          :documentation "A label is used to identify which signer should sign the request when talking to a multiroot CA."))
  (:documentation "sign-request contains a request for a certificate
signature. It should contain the relevant subject information, the
hosts for which to generate the certificate, the certificate request, and other data required by the CA."))

(defun new-sign-request (hosts csr subject
                               &key (profile "") (label ""))
  "Create a new CFSSL signing request from the hosts, PKCS #10
certificate signing request, and subject information. Additionally,
the label and profile may be specified, if appropriate."
  (make-instance 'sign-request
                 :hosts (to-list hosts :validator #'stringp)
                 :certificate-request csr
                 :subject subject
                 :profile profile
                 :label label))

(defun new-sign-request-from-file (hosts csr-path subject
                                         &key (profile "") (label ""))
  "Create a new CFSSL signing request from the hosts, PKCS #10
certificate signing request that is found at csr-path, and subject
information. Additionally, the label and profile may be specified, if
appropriate."
  (let ((csr (read-file-string csr-path)))
    (new-sign-request hosts csr subject
                      :profile profile :label label)))

(defmethod clos-to-map ((request sign-request) &key (converter #'keyword-to-downcase))
  "When serialising a signature request, the subject must be handled specially."
  (let ((ht (call-next-method request :key converter)))
    (sethash "subject" (clos-to-map (sign-request-subject-of request)) ht)
    (sethash "certificate_request" (sign-request-csr-of request) ht)
    (remhash "certificate-request" ht)
    ht))

(defclass auth-sign-request ()
  ((token :initarg :token
          :type string
          :reader token-of
          :documentation "Base64-encoded authentication token.")
   (request :initarg :request
            :type (or string sign-request)
            :reader request-of
            :documentation "Signing request: may be already encoded into JSON or an instance of 'sign-request'.")
   (timestamp :initarg :timestamp
              :type (or integer nil)
              :reader timestamp-of
              :documentation "Optional timestamp to accompany the
request; its optionality is dependent on the policies of the upstream
server.")
   (remote-address :initarg :remote-address
                   :type (or string nil)
                   :reader remote-address-of
                   :documentation "Optional remote address to
accompany the request; its optionality is depdendent on the policies
of the upstream server."))
  (:documentation "An authenticated signing request."))

(defun authenticate-signing-request (provider request)
  "Generate an authenticated signing request from an unauthenticated
signing request and an authentication provider."
  (assert (providerp provider) (provider)
    "~A is not an authentication provider."
    (type-of provider))
  (make-instance 'auth-sign-request
                 :token (auth-token provider request)
                 :request request
                 :timestamp (unix-timestamp)))

(defmethod clos-to-map ((request auth-sign-request)
                        &key (converter #'keyword-to-downcase))
  "Implementation of #'clos-to-map for authenticated signing requests."
  (with-new-hash-table (ht)
    (sethash "token" (token-of request) ht)
    (sethash "request" (base64 (to-json (request-of request))) ht)
    (when (slot-boundp request 'timestamp)
      (sethash "timestamp" (timestamp-of request) ht))
    (when (slot-boundp request 'remote-address)
      (sethash "remote_address" (remote-address-of request)))))

