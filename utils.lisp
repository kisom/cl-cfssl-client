(in-package #:cfssl-client)

(defun sethash (k v m)
  "Convenience notation for setting a value in a hash table."
  (setf (gethash k m) v))

(defun hash-table-to-alist (m)
  "Converts the hash-table given to an alist of (key . value) pairs."
  (let ((alist '()))
    (maphash (lambda (k v)
	       (let ((elt (cons k v)))
		 (setf alist (cons elt alist))))
	     m)
    alist))

(defun alist-to-hash-table (alist)
  "Converts the alist to a hash-table."
  (let ((m (make-hash-table :test 'equal)))
    (dolist (elt alist)
      (sethash (car elt) (cdr elt) m))
    m))

(defun snake-case (s)
  "Substitute Lisp-style hyphenation to underscores, as is the
standard in JSON key names."
  (substitute #\_ #\- s))

(defun keyword-to-downcase (kw)
  "Convert a keyword to a string, downcasing the result."
  (string-downcase
   (snake-case
    (subseq
     (with-output-to-string (s)
       (write kw :stream s))
     1))))

(defun to-json (obj)
  "JSON encode obj to a string."
  (with-output-to-string (s)
    (cond ((hash-table-p obj) (yason:encode obj s))
          (t (yason:encode (clos-to-map obj) s)))))

(defun new-hash-table ()
  "Create a new hash table with the #'equal function as its test."
  (make-hash-table :test #'equal))

(defun alist-cons (lst)
  "alist-cons expects a list of at least two items or a null list. If
the list is not null, it returns the cons of the first two items in
the list."
  (unless (null lst)
    (cons (first lst) (first (rest lst)))))

(defun list-to-alist (lst)
  "Produce an alist from a list."
  (unless (null lst)
    (cons (alist-cons lst) (list-to-alist (rest (rest lst))))))

(defmacro if-present-set (k k% alst ht)
  "If k has a non-NIL value in the alist alst, set the key k% in the
hash-table ht to that value."
  `(let ((v (rest (assoc ,k ,alst))))
    (unless (null v)
       (sethash ,k% v ,ht))))

;;; read-file-string is used for testing this with CSR files on disk.
(defun read-file-string (path)
  (with-open-file (s path)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      data)))

(defmacro with-new-hash-table (htsyms &body body)
  `(let ,(mapcar (lambda (sym)
                   (list sym (list 'new-hash-table))) htsyms)
     ,@body
     ht))

(defun emptyp (v)
  "Returns true if v is nil or an empty string."
  (cond ((null v) t)
        ((and (stringp v) (string-equal "" v)) t)
        (t nil)))

(defun n-string-p (v)
  "Is v a string or a nil value, or is it something else?"
  (or (stringp v) (null v)))

(defun aval (k alst)
  "Return the value associated with k in the alst."
  (rest (assoc k alst)))

(defun partial (fn &rest initial-args)
  "partial provides partial function application. It returns a lambda
that will call the function given with the intial args and any
additional args provided to the lambda."
  (lambda (&rest args)
    (apply fn (append initial-args args))))

(defun to-string (v)
  "Return v as a string. The input to certain functions may either be
a binary value or a string, and this ensures that in either case,
a sane value is returned."
  (cond ((stringp v) v)
        (t (flexi-streams:octets-to-string v))))

(defun extract-keys (ht keys)
  "Return the value of the keys that have been requested. If keys is
NIL, return the entire result hash table."
  (cond ((null keys) ht)
        ((listp keys) 
         (with-new-hash-table (new-ht)
           (mapcar (lambda (k)
                     (sethash k (gethash k ht) new-ht)))))
        (t (gethash keys ht))))

;;; The CloudFlare API standard, which CFSSL follows, returns a JSON
;;; object with a few relevant keys.  The first, "success", contains a
;;; boolean indicating whether the request was successful. If it's T,
;;; the "result" key contains the JSON object with the useful response
;;; data that the client wanted.  If it's NIL, then the "errors" key
;;; contains a list of JSON objects with error information. These
;;; objects have two keys: "code" has a numeric code that be of use in
;;; debugging the error, and "message" contains a string with the
;;; "human-readable" (*) error message.
;;;
;;; (*) for certain instances of the human class.
(defun parse-api-response (response keys)
  "Given a response to the CFSSL API directly from drakma, parse the
result. If an error has occurred, signal a condition to the user.
Otherwise, return the requested data. If keys is nil, the response data
will be directly returned. If it's a single string key, the relevant
entry in the response hash table will be returned. If it's a list of
keys, an new hash table of only the requested keys will be returned."
  (case (aval :status response)
    (404 (error (page-not-found (aval :uri response))))
    (t (let ((rht (yason:parse (to-string (aval :body response)))))
         (if (gethash "success" rht)
             (extract-keys (gethash "result" rht) keys)
           (error (new-api-error (first (gethash "errors" rht)))))))))

(defun split-host-port (s)
  "Split a host:port specification into an improper list containing
the host and port. If only a host is specified, returns nil."
  (let ((host-port (split-sequence:split-sequence #\: s)))
    (when (= (length host-port) 2)
      (cons (car host-port) (parse-integer (cadr host-port))))))

(defun unix-timestamp ()
  "Return a Unix timestamp for the current time."
  (local-time:timestamp-to-unix (local-time:now)))

(defun base64 (data)
  "Base64 encode data flexibly whether it's a string or binary."
  (with-output-to-string (s)
    (if (stringp data)
        (s-base64:encode-base64-bytes
         (flexi-streams:string-to-octets data) s)
      (s-base64:encode-base64-bytes data s))))

(defun to-list (v &key validator)
  "If v is not a list, make it a list. If validator is not nil, it
should be a predicate for elements in the resulting list; it will be
used in #'every. If the predicate fails, an error will be returned."
  (labels ((validate (x)
             (unless (funcall validator x)
               (error "Validation failed for ~A of ~A."
                      x (type-of x))))
           (maybe-validate (lst)
             (when validator
               (every validator lst))
             lst))
    (if (listp v)
        (maybe-validate v)
      (maybe-validate (list v)))))
        
        

