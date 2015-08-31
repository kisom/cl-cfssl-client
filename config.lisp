;;;; config.lisp

(in-package #:cfssl-client)

;;; Contains code for parsing a CFSSL configuration file.

(defvar *valid-usages*
  (with-new-hash-table (ht)
    (sethash "any" t ht)
    (sethash "server auth" t ht)
    (sethash "client auth" t ht)
    (sethash "code signing" t ht)
    (sethash "email protection" t ht)
    (sethash "s/mime" t ht)
    (sethash "ipsec end system" t ht)
    (sethash "ipsec tunnel" t ht)
    (sethash "ipsec user" t ht)
    (sethash "timestamping" t ht)
    (sethash "ocsp signing" t ht)
    (sethash "microsoft sgc" t ht)
    (sethash "netscape sgc" t ht)
    (sethash "signing" t ht)
    (sethash "digital signature" t ht)
    (sethash "content committment" t ht)
    (sethash "key encipherment" t ht)
    (sethash "key agreement" t ht)
    (sethash "data encipherment" t ht)
    (sethash "cert sign" t ht)
    (sethash "crl sign" t ht)
    (sethash "encipher only" t ht)
    (sethash "decipher only" t ht)))

(defun config-standard-auth-p (arg)
  (string-equal (gethash "type" arg) "standard"))

(defun parse-standard-auth (config)
  (let ((auth-keys (gethash "auth_keys" config)))
    (when auth-keys
      (maphash (lambda (k v)
                 (when (config-standard-auth-p v)
                   (sethash k (new-standard-auth
                               (gethash "key" v)) auth-keys)))
               auth-keys))))

(defun load-config-file (path)
  "Attempt to load the configuration file at @c(path). This will
process the authentication keys, creating providers where possible."
  (let* ((config (read-file-string path))
         (confht (yason:parse config)))
    (parse-standard-auth confht)
    confht))

(defun get-profile (config profile)
  "Extract the signing profile named by @c(profile)."
  (let ((profiles (gethash "signing" config)))0
    (if (emptyp profile)
        (gethash "default" signing)
          (gethash profile (gethash "profiles" profiles)))))

(defun get-auth-provider-for-profile (config profile)
  (let* ((sign-profile (get-profile config profile))
         (auth-key-name (and sign-profile
                             (gethash "auth_key" sign-profile))))
    (when auth-key-name
      (gethash auth-key-name (gethash "auth_keys" config)))))

    