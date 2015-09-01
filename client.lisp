;;;; client.lisp

(in-package #:cfssl-client)

;;; "cfssl-client" goes here. Hacks and glory await!

;;; Define the basic 'Remote' interfaces from the CFSSL client and
;;; implement a single-server 'Remote'.

(defclass server ()
  ((host :initarg :host 
         :reader server-host-of
         :type string
         :documentation "The hostname or IP address of the server.")
   (port :initarg :port
         :initform 8888
         :type integer
         :reader server-port-of
         :documentation "The port the API server is listening on."))
  (:documentation "A server represents a single API server."))

(defun new-server (host &key port)
  "Set up a new handle to a CFSSL server. If no port is given, the
default CFSSL port will be assumed. The port may be specified either
by host:port (e.g. 'ca.example.org:8080') or via the :port keyword."
  (let ((host-port (split-host-port host)))
    (cond ((and (null host-port)
                (null port))
           (make-instance 'server :host host))
          ((and host-port port)
           (error "Specify the port either by the standard host:port
notation or via the :port keyword, not both."))
          (host-port (make-instance 'server
                                    :host (first host-port)
                                    :port (rest host-port)))
          (t (make-instance 'server :host host :port port)))))
                            
(defmethod remote-uri ((server server) (endpoint string))
  "Return the endpoint URI for the server."
  (format nil "http://~A:~A/api/v1/cfssl/~A"
          (server-host-of server)
          (server-port-of server)
          endpoint))

(defun post-request (uri contents)
  "Send a POST request to the server and return the HTTP status code,
body, and response URI. The response URI is used to respond to 404
codes."
  (multiple-value-bind (body status headers response-uri)
      (drakma:http-request uri
                           :content-type "application/json"
                           :method :post
                           :want-stream nil
                           :content contents)
    (list (cons :status status)
          (cons :body body)
          (cons :uri response-uri))))
    
(defun post-api-request (request conn endpoint keys)
  "Build a POST request for the CFSSL server and parse the response
received from the server."
  (parse-api-response
   (post-request (remote-uri conn endpoint)
                 (to-json request))
    keys))

(defmethod sign ((conn server) (request sign-request))
  "Send a request for a signed certificate to the CFSSL server."
  (post-api-request request conn "sign" "certificate"))

(defun info-request (label profile)
  "Produce a hash table with the relevant info request parameters."
  (with-new-hash-table (ht)
    (unless (emptyp label)
      (sethash "label" label ht))
    (unless (emptyp profile)
      (sethash "profile" profile ht))))

(defmethod info ((conn server) &key (profile "") (label "") extra)
  "Request a CFSSL CA's certificate, and optionally additional
information."
  (post-api-request (info-request label profile)
                    conn "info" (if extra nil "certificate")))

(defmethod auth-sign ((remote server) (req sign-request)
                      &optional provider id)
  "Use the provider to authenticate the request, then send the request
to the CFSSL server."
  (post-api-request (authenticate-request provider req)
                    remote "authsign" "certificate"))
  
(defmethod auth-sign ((remote server) (req auth-sign-request)
                      &optional provider id)
  "Send an authenticated request to the CFSSL server."
  (post-api-request req remote "authsign" "certificate"))

(defmethod new-key-and-csr ((conn server) (req certificate-request) &optional provider)
  "Send an unauthenticated request for a new private key and corresponding CSR."
  (let ((req (if provider
		 (authenticate-request provider req)))))
  (post-api-request req conn "newkey" '("private_key" "certificate_request")))

(defmethod new-key-and-cert ((conn server) (req certificate-request)
			     &optional provider)
  "Send a request for a new private key and accompanying certificate from the information present in the certificate request. If provider is present, it will be used to authenticate the request."
  (let ((req (if provider
		 (authenticate-request provider req)
		 req)))
    (post-api-request req conn "newcert"
		      '("private_key" "certificate" "certificate_request"))))

(defmethod new-key-and-cert ((conn server) (req certificate-request)
			     &optional provider)
  "Send a request for a new private key and accompanying certificate. If provider is present, it will be used to authenticate the request."
  (let ((req (if provider
		 (authenticate-request provider req)
		 req)))
    (post-api-request req conn "newcert"
		      '("private_key" "certificate" "certificate_request"))))
