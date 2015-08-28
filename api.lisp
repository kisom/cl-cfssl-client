(in-package :cfssl-client)

;;; This file contains the API definitions.

;;; auth-sign performs an authenticated signature request.
(defgeneric auth-sign (remote req &optional provider id)
  (:documentation "Sending a singing request to the remote, asking it
to sign a certificate to be generated from the PKCS #10 certificate
signing request and accompanying metadata contained in the request. If
the request is not an `auth-sign-request`, the provider must be
non-NIL and a valid authentication provider that will be used to
authenticate the request."))

;;; sign requests that the server sign a certificate request.
(defgeneric sign (remote sign-request)
  (:documentation "Send a request to the remote, asking it to sign a
certificate to be generated from the PKCS #10 certificate signing
request and accompanying metadata contained in `sign-request`."))

;;; info returns the CA's certificate for the given label and profile.
;;; if usages is not nil, it will return a hash table containing the
;;; certificate and usages; otherwise, it returns the CA's certificate
;;; as a string.
(defgeneric info (remote &key profile label extra)
  (:documentation "Request information about the CA. If extra is nil,
it will return a string containing the CA's certificate; otherwise, a
hash-table containing the certificate and extra information will be
returned."))

;;; new-key-and-csr asks the server to generate a new key and
;;; PKCS #10 certificate request.
(defgeneric new-key-and-csr (remote request &optional provider)
  (:documentation "Request a new private key and PKCS #10 certificate
request from a CFSSL server. This is intended to be used with a local
instance of CFSSL, and should not be sent over the network: the
private key is transmitted in cleartext."))

;; new-key-and-cert asks the server to generate a new key and sign a
;; certificate for this key. As explained in the CFSSL docs, this
;; functionality is intended for local CFSSL instances set to forward
;; signature requests to a remote CFSSL.
(defgeneric new-key-and-cert (remote request &optional provider)
  (:documentation "Request a new private key and accompanying
certificate signed by the CA from a CFSSL server. This is intended to
be used with a local instance of CFSSL, and should not be sent over
the network: the private key is transmitted in cleartext."))

(defgeneric remote-uri (remote endpoint)
  (:documentation "Return the URI appropriate for a given remoteection
type and endpoint."))

(defmethod remote-uri (remote endpoint)
  "remote-uri should fail with a bad remote."
  (error "The method #'remote-uri is inappropriate for an object of type ~A"
         (type-of remote)))

