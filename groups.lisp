(in-package :cfssl-client)

;;; Implement the fallback groups currently in the CFSSL client API.

(defclass server-group ()
  ((servers :initarg :servers
            :reader servers-of
            :type (vector server)
            :documentation "The list of servers in the group.")
   (strategy :initarg :strategy
             :initform :fallback
             :reader strategy-of
             :type keyword
             :documentation "The strategy for selecting which host to use for a given request."))
  (:documentation "A server group contains multiple servers for higher
availability. Currently, the only strategy for selecting which server
gets used is the fallback method; this is also the case with the CFSSL
Go client. The order of servers specified matters; the head of the
list will always be attempted first for a given request, moving on to
the next server only in the case where the server could not be
reached. Servers may be specified either as a host name (in which
case, the default port will be used), or as a host:port."))

(defun new-server-group (&rest hosts)
  "Set up a new handle to a server group."
  (labels ((build-server (srv)
             (cond ((stringp srv) (new-server srv))
                   ((equal (type-of srv) 'server) srv)
                   (t (error "Invalid server.")))))
    (make-instance 'server-group
                   :servers (mapcar #'build-server hosts))))

(defmacro with-fallback (opts &rest body)
  (let ((f (gensym))
	(group (first opts))
	(bindsym (first (rest opts))))
    `(labels ((,f (,bindsym)
		(if (null ,bindsym)
		    (error "No server could complete the request.")
		    (let ((response
			   (ignore-errors
			     ,@body)))
		      (if (null response)
			  (,f (rest ,bindsym))
			  response)))))
       (,f (servers-of ,group)))))

(defmethod sign ((group server-group) (request sign-request))
  "Request a signed certificate from the server group."
  (with-fallback (group servers)
    (sign (first servers) request)))

(defmethod info ((group server-group) &key (profile "") (label "") extra)
  "Request information about the first CA that can be reach in the
group. If usages is nil, it will return a string containing the CA's
certificate; otherwise, a hash-table containing the certificate and
usages will be returned."
  (with-fallback (group servers)
    (info (first servers)
	  label profile :extra extra)))

(defmethod auth-sign ((group server-group) (req sign-request)
                      &optional provider id)
  "Request a certificate signing using an authenticated request."
  (let ((auth-request (authenticate-request provider req)))
    (with-fallback (group servers)
      (auth-sign (first servers)
		 auth-request provider))))

(defmethod new-key-and-csr ((group server-group) req &optional provider)
  "Using a fallback group to generate a new private key and CSR is not supported."
  (error "new-key-and-csr is not supported for server groups."))


(defmethod new-key-and-cert ((group server-group) req &optional provider)
  "Using a fallback group to generate a new private key and CSR is not supported."
  (error "new-key-and-cert is not supported for server groups."))
