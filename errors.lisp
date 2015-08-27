(in-package :cfssl-client)

(define-condition http-error (error)
  ((status :initarg :status
           :reader status-of
           :documentation "An HTTP status code corresponding to the
error.")
   (message :initarg :message
            :reader message-of
            :documentation "The message that describes the error."))
  (:documentation "An HTTP error is signaled when an HTTP request
fails.")
  (:report (lambda (condition stream)
             (format stream "The HTTP request failed with status ~A.~A"
                     (status-of condition)
                     (when (slot-boundp condition 'message)
                       (format nil " The server replied: ~A"
                               (message-of condition)))))))

(defun page-not-found (uri)
  "Build an HTTP 404 error."
  (make-condition 'http-error
                  :status 404
                  :message (format nil "~A was not found on the server." uri)))

(define-condition api-error (error)
  ((code :initarg :code
         :reader api-error-code-of)
   (message :initarg :message
            :reader api-error-message-of))
  (:documentation "An API error is returned by CloudFlare's API servers when a request fails.")
  (:report (lambda (condition stream)
             (format stream "Request failed (~A). The server replied with ~A."
                     (api-error-code-of condition)
                     (api-error-message-of condition)))))

(defun new-api-error (ht)
  "Given a hash table returned by the JSON decoder which contains an API error message, construct a new API error condition."
  (make-condition 'api-error
                  :code (gethash "code" ht)
                  :message (gethash "message" ht)))
                  

