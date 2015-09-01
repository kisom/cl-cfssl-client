(in-package #:cfssl-client)

;;; Various utility functions used throughout the code.

;;; Yason requires that JSON objects come from hash tables; however,
;;; most of the objects used in this package are CLOS objects. This
;;; provides a common interface for producing the necessary hash
;;; table.
(defgeneric ->hash-table (obj &key converter)
  (:documentation "Convert the object to a hash table."))

(defun sethash (k v m)
  "Convenience notation for setting a value in a hash table."
  (setf (gethash k m) v))

(defun hash-table-to-alist (m)
  "Converts the hash-table given to an alist of (key . value) pairs."
  (let ((alist '()))
    (maphash (lambda (k v)
	       (let ((elt (cons k v)))
		 (push elt alist)))
	     m)
    alist))

(defun hash-table-keys (ht)
  "Returns a list of the keys in the hash table."
  (let ((keys '()))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k keys))
	     ht)
    keys))

(defun alist-to-hash-table (alist)
  "Converts the alist to a hash-table."
  (let ((m (make-hash-table :test 'equal)))
    (dolist (elt alist)
      (sethash (car elt) (cdr elt) m))
    m))

(defun snake-case (s)
  "Substitute Lisp-style hyphenation to underscores, as is the
standard in JSON key names."
  (nsubstitute #\_ #\- s))

(defun lisp-case (s)
  "Substitute underscores to Lisp-style hyphentation, e.g. to translate JSON names to Lisp names."
  (nsubstitute #\- #\_ s))

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
          (t (yason:encode (->hash-table obj) s)))))

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

(defun if-present-set (k k% alst ht)
  "If k has a non-NIL value in the alist alst, set the key k% in the
hash-table ht to that value."
  (let ((v (rest (assoc k alst))))
    (unless (null v)
      (sethash k% v ht))))

(defun set-if-bound (obj slot k ht)
  (when (slot-boundp obj slot)
    (sethash k (slot-value obj slot) ht)))

(defun read-file-string (path)
  "Read the contents of the file at path as a string."
  (with-open-file (s path)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      data)))

(defmacro with-new-hash-table (htsyms &body body)
  "with-new-hash-table creates and binds a new hash table for each of
the symbols in htsyms, executing inside a let form, and returns the
hash table(s). If only one hash table is provided, return it as a
single element; otherwise, return an list of the hash tables."
  `(let ,(mapcar (lambda (sym)
                   (list sym (list 'new-hash-table))) htsyms)
     ,@body
     ,(if (null (rest htsyms))
         (first htsyms)
        `(list ,@htsyms))))

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
        ((consp keys) 
         (with-new-hash-table (new-ht)
           (mapcar (lambda (k)
		     (let ((v (gethash k ht)))
		       (sethash (lisp-case k) v new-ht)))
		   keys)))
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
    (200 (let ((rht (yason:parse (to-string (aval :body response)))))
           (if (gethash "success" rht)
               (extract-keys (gethash "result" rht) keys)
             (error (new-api-error (first (gethash "errors" rht)))))))
    (t (let ((rht (yason:parse (to-string (aval :body response)))))
         (error (new-api-error (first (gethash "errors" rht))))))))


(defun split-host-port (s)
  "Split a host:port specification into an improper list containing
the host and port. If only a host is specified, returns nil."
  (let ((host-port (split-sequence:split-sequence #\: s)))
    (unless (or (null (rest host-port))
		(rest (rest host-port)))
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

(defun validate (pred x)
  "If pred does not hold for x, throw an error."
  (format t "Validating ~A~%")
  (if (funcall validator x)
      (format t "~A is valid.~%" x)
    (error "Validation failed for ~A of ~A."
           x (type-of x))))

(defun maybe-validate (pred vect)
  "If pred is not NIL, call @c(validate) over the elements of the
vector."
  (format t "validator: ~A~%" pred)
  (when (functionp pred)
    (format t "Will validate~%")
    (dolist (x vect)
      (funcall validate pred x)))
  vect)

(defun to-vector (v &key validator)
  "If v is not a vector, make it a vector. If validator is not nil, it
should be a predicate for elements in the resulting list; it will be
used in #'every. If the predicate fails, an error will be returned."
    (cond ((vectorp v) (maybe-validate validator v))
          ((listp v)   (maybe-validate validator (apply #'vector v)))
          (t (maybe-validate validator (funcall #'vector v)))))
        
(defun hash-table-key-to-disk (ht k path)
  "Write the value of the hash table HT for the key K to PATH."
  (when (gethash k ht)
    (ensure-directories-exist path)
    (with-open-file (out path
			 :direction :output
			 :if-exists :rename)
      (with-standard-io-syntax
	(write-sequence (gethash k ht) out)))
    t))

(defun build-pathname (base suffix)
  "Given a suffix, such as \"key.pem\", attempt to build a logical pathname starting at base."
  (cond ((fad:directory-pathname-p base) (concatenate 'string base suffix))
	((fad:directory-exists-p base) (concatenate 'string base "/" suffix))
	(t (concatenate 'string base "-" suffix))))

(defmacro did-any-p (&body body)
  "Did any of the forms in the body return T after executing all of
them?"
  `(reduce (lambda (x y) (or x y))
	   (list ,@body)))

(defun write-response-to-file (base-path response)
  "Given a CFSSL CA response (e.g. from @c(#'new-key-and-csr)), write
the keys that are present to disk. If base-path is a directory, the
files will be written to key.pem, csr.pem, and cert.pem, respectively.
Otherwise, they will be written as -key.pem, -csr.pem, and -cert.pem
concatenated onto the base path. Returns T if any of the keys were
written."
  (did-any-p
    (hash-table-key-to-disk response "private-key"
			    (build-pathname base-path "key.pem"))
    (hash-table-key-to-disk response "certificate-request"
			    (build-pathname base-path "csr.pem"))
    (hash-table-key-to-disk response "certificate"
			    (build-pathname base-path "cert.pem"))))
