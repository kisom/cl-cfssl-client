(in-package :cfssl-client)

;;; Implement the standard signer from CFSSL. The full Provider
;;; interface isn't implemented; this package does not need to
;;; verify tokens.

(defgeneric auth-token (provider request)
  (:documentation "Generate an authentication token."))

(defmethod auth-token (provider request)
  "By default, an auth-token can't be generated."
  (declare (ignore request))
  (error "~A is not a valid authentication provider." (type-of provider)))

(defgeneric providerp (provider)
  (:documentation "Does 'provider' represent a valid authentication
provider?"))

(defmethod providerp (x)
  "Most objects are not authentication providers."
  (declare (ignore x))
  nil)

(defclass standard-auth ()
  ((key :initarg :key
        :type string
        :reader key-of
        :documentation "The key is expected to be a hex-encoded string representing an HMAC-SHA-256 key."))
  (:documentation "standard-auth implements the CFSSL 'Standard' provider, which uses HMAC-SHA-256 to generate an authentication token."))

(defmethod providerp ((p standard-auth))
  "standard-auth is an authentication provider."
  (declare (ignore p))
  t)

(defun new-standard-auth (key)
  "Create a new standard authentication provider from the provided key."
  (make-instance 'standard-auth :key key))

(defun hmac-sha-256 (key data)
  "Compute the HMAC-SHA-256 digest of the data using key."
  (let* ((hmac-key (ironclad:hex-string-to-byte-array key))
         (hmac     (ironclad:update-hmac
                    (ironclad:make-hmac hmac-key :sha256)
                    data)))
    (base64 (ironclad:hmac-digest hmac))))

(defmethod auth-token ((provider standard-auth) (data string))
  "Produce an authentication token directly from an already-encoded
string."
  (hmac-sha-256 (key-of provider) (flexi-streams:string-to-octets data)))

(defmethod auth-token ((provider standard-auth) (request sign-request))
  "Produce an authentication token from a signing request."
  (hmac-sha-256 (key-of provider)
                (flexi-streams:string-to-octets
                 (to-json request))))
