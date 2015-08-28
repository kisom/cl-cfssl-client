(in-package #:cfssl-client)

;;; Define the request data types and provide functions for converting
;;; them to a format suitable to outputting to JSON.

(defmethod ->hash-table (obj &key (converter #'keyword-to-downcase))
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

(defun new-subject-name (&key country state locality org org-unit)
  "Produce a new subject-name entry. At least one of the members of
the lambda list must be non-nil."
  (assert (some #'identity (list country state locality org org-unit))
	  (list country state locality org org-unit)
	  "No subject data present.")
  (make-instance 'subject-name
		 :country country
		 :state state
		 :locality locality
		 :org org
		 :org-unit org-unit))

(defmethod ->hash-table ((subject-name subject-name) &key (converter #'keyword-to-downcase))
  "When presented with subject information, specific key values should be used."
  (with-new-hash-table (ht)
    (let ((alst (list-to-alist (fare-mop:collect-slots subject-name))))
      (if-present-set :country "C" alst ht)
      (if-present-set :state "ST" alst ht)
      (if-present-set :locality "L" alst ht)
      (if-present-set :org "O" alst ht)
      (if-present-set :org-unit "OU" alst ht))))

(defclass key-specification ()
  ((algorithm :initarg :algo
	      :reader :algorithm-of
	      :type string
	      :documentation "This specifies the public key algorithm for the key; it must be one of \"rsa\" or \"ecdsa\".")
   (size :initarg :size
	 :reader size-of
	 :type integer
	 :documentation "The key size (in bits) as relevant to the algorithm."))
  (:documentation "A key specification contains information about a specific type of key that should be generated."))

(defun make-rsa-keyspec (size)
  "Helper function for defining the RSA key types."
  (make-instance 'key-specification :algo "rsa" :size size))

(defun make-ecdsa-keyspec (size)
  "Helper function for defining the ECDAS key types."
  (make-instance 'key-specification :algo "ecdsa" :size size))

;;; The key specification class shouldn't be used directly; one of the
;;; following predefined specifications should be used.

(defvar *rsa-2048-key* (make-rsa-keyspec 2048)
  "Key specification for a 2048-bit RSA key.")
(defvar *rsa-3072-key* (make-rsa-keyspec 3072)
  "Key specification for a 3072-bit RSA key.")
(defvar *rsa-4096-key* (make-rsa-keyspec 4096)
  "Key specification for a 4096-bit RSA key.")
(defvar *ec-p256-key*  (make-ecdsa-keyspec 256)
  "Key specification for a 256-bit ECDSA key.")
(defvar *ec-p384-key*  (make-ecdsa-keyspec 384)
  "Key specification for a 384-bit ECDSA key.")
(defvar *ec-p521-key*  (make-ecdsa-keyspec 521)
  "Key specification for a 521-bit ECDSA key." )
(export (list'*ec-p521-key*
	 '*ec-p384-key*
	 '*ec-p256-key*
	 '*rsa-4096-key*
	 '*rsa-3072-key*
	 '*rsa-2048-key*))

#|
type CertificateRequest struct {
    CN         string
    Names      []Name      `json:"names"`
    Hosts      []string    `json:"hosts"`
    KeyRequest *KeyRequest `json:"key,omitempty"`
    CA         *CAConfig   `json:"ca,omitempty"`
}
|#

(defclass certificate-request ()
  ((common-name :initarg :common-name
		:reader common-name-of
		:type string
		:documentation "The common name that should be used to refer to the server. This does not need to be a hostname, although some TLS clients will expect it to be.")
   (names :initarg :names
	  :reader names-of
	  :type (vector subject-name)
	  :documentation "Subject names that should be used for the certificate.")
   (hosts :initarg :hosts
	  :reader hosts-of
	  :type (vector string)
	  :documentation "A list of subject alternative names, which are hostnames the certificate is valid for.")
   (key-spec :initarg :key-spec
	     :initform nil
	     :type key-specification
	     :reader key-spec-of
	     :documentation "If not nil, this is used to request a specific type of private key from the server.")
   ;; TODO: support the CA config section for bootstrapping a new
   ;; CA. This isn't immediately useful, though.
   )
  (:documentation "A certificate is used to request a new private key
  and CSR from a CFSSL API server."))

(defun new-certificate-request (hosts names &key (common-name "") (key-spec *ec-p256-key*))
  "Create a new certificate request from the specifed `hosts` and
`names`. An optional common name may be provided, and the key
specifier overridden."
  (make-instance 'certificate-request
		 :common-name common-name
		 :names (to-list names)
		 :hosts (to-list hosts)
		 :key-spec key-spec))

(defmethod ->hash-table ((request certificate-request) &key (converter #'keyword-to-downcase))
  "A certificate request needs the common-name field to be sent as the JSON \"CN\" key, and the names to be parsed."
  (declare (ignore converter))
  (with-new-hash-table (ht)
    (sethash "CN" (common-name-of request) ht)
    (sethash "names" (mapcar #'->hash-table (names-of request)) ht)
    (when (and (slot-boundp request 'key-spec)
	       (key-spec-of request)))
    (sethash "key" (->hash-table (key-spec-of request)) ht)
    (sethash "hosts" (hosts-of request) ht)))

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

(defmethod ->hash-table ((subject subject) &key (converter #'keyword-to-downcase))
  "When presented with subject information, #'->hash-table should capitalise
the common name."
  (with-new-hash-table (ht)
    (sethash "CN" (common-name-of subject) ht)
    (sethash "names" (mapcar #'->hash-table (names-of subject)) ht)))

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

(defmethod ->hash-table ((request sign-request) &key (converter #'keyword-to-downcase))
  "When serialising a signature request, the subject must be handled specially."
  (let ((ht (call-next-method request :key converter)))
    (sethash "subject" (->hash-table (sign-request-subject-of request)) ht)
    (sethash "certificate_request" (sign-request-csr-of request) ht)
    (remhash "certificate-request" ht)
    ht))

(defclass generate-and-sign-request ()
  ((request :initarg :request
	    :type certificate-request
	    :reader request-of
	    :documentation "A certificate-request containing the request details.")
   (profile :initarg :profile
	    :type string
	    :reader profile-of
	    :documentation "A string identifying the CA profile to use.")
   (label :initarg :label
	  :initform ""
	  :type string
	  :reader label-of
	  :documentation "A string identifying the label of the signer
that should be used to sign the certificate."))
  (:documentation "Contains the information needed by a CFSSL CA to generate a new private key and sign a certificate for that private key."))

(defun cert-request->gen-request (request profile &key (label ""))
  "Given a certificate request and profile (and optionally, a label),
produce a generate-and-sign-request. This contains the information needed by the CA when requesting a new private key and certificate be generated."
  (make-instance 'generate-and-sign-request
		 :request request
		 :profile profile
		 :label label))

(defmethod ->hash-table ((req generate-and-sign-request) &key converter)
  (declare (ignore converter))
  (with-new-hash-table (ht)
    (sethash "request" (->hash-table (request-of req)) ht)
    (sethash "profile" (profile-of req) ht)
    (set-if-bound req 'label "label" ht)))
