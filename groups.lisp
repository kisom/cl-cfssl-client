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
  (make-instance 'server-group
                 :servers (mapcar #'new-server hosts)))

;;; todo: write a macro `with-fallback` to reduce some of the common
;;; elements below.

(defmethod sign ((group server-group) (request sign-request))
  "Request a signed certificate from the server group."
  (labels ((group-sign (servers)
             (if (null servers)
                 (error "No server could complete the request.")
               (let ((response
                      (ignore-errors
                        (sign (first servers) request))))
                 (if (null response)
                     (group-sign (rest servers))
                   response)))))
    (group-sign (servers-of group))))

(defmethod info ((group server-group) (label string) (profile string)
                 &key extra)
  "Request information about the first CA that can be reach in the
group. If usages is nil, it will return a string containing the CA's
certificate; otherwise, a hash-table containing the certificate and
usages will be returned."
  (labels ((group-info (servers)
             (if (null servers)
                 (error "No server could complete the request.")
               (let ((response
                      (ignore-errors
                        (info (first servers)
                              label profile :extra extra))))
                 (if (null response)
                     (group-info (rest servers))
                   response)))))
    (group-info (servers-of group))))

(defmethod auth-sign ((group server-group) (req sign-request)
                      &optional provider id)
  "Request a certificate signing using an authenticated request."
  (labels ((auth-request (authenticate-signing-request provider req))
           (group-auth-sign (servers)
             (if (null servers)
                 (error "No server could complete the request.")
               (let ((response
                      (ignore-errors
                        (auth-sign (first servers)
                                   auth-request provider))))
                 (if (null response)
                     (group-auth-sign (rest servers))
                   response)))))
    (group-auth-sign (servers-of group))))
               
