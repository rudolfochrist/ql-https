;;;; ql-https.lisp

(defpackage #:ql-https
  (:use :cl)
  (:import-from #:ql-gunzipper #:gunzip)
  (:export
   #:fetcher
   #:*quietly-use-https*
   #:register-fetch-scheme-functions
   #:no-https-error))

(in-package #:ql-https)

(define-condition no-https-error (error)
  ((url :initarg :url
        :reader no-https-url))
  (:report (lambda (c stream)
             (format stream "We don't use HTTP here!~&URL: ~A" (no-https-url c)))))

(defvar *quietly-use-https* nil
  "If non-nil quietly use HTTPS.")

(defun fetcher (url file &rest args)
  "Fetch URL and safe it to FILE."
  (declare (ignorable args))
  (if (uiop:string-prefix-p "https://" url)
      ;; Convert the file path to a string with any leading "~" replaced by the
      ;; HOME directory, and then download.
      (let* ((file-namestring (namestring file))
             (file-namestring-full (if (uiop:string-prefix-p "~" file-namestring)
                                       (concatenate 'string
                                                    (namestring (user-homedir-pathname))
                                                    (subseq file-namestring 1))
                                       file-namestring))
             (output (uiop:run-program (list "curl" "-fsSL" url "-o" file-namestring-full)
                                       :force-shell nil
                                       :output '(:string :stripped t)
                                       :error-output :output))
             (file (and file (probe-file file)))
             (release (url-to-release url)))
        (when release
          (verify-download file release))
        (values output file))
      (restart-case
          (handler-bind ((no-https-error (lambda (c)
                                           (declare (ignore c))
                                           (when *quietly-use-https*
                                             (invoke-restart 'use-https)))))
            (error 'no-https-error :url url))
        (use-https ()
          :report "Retry with HTTPS."
          (apply #'fetcher
                 (format nil "https~A" (subseq url 4))
                 file
                 args))
        (use-https-session ()
          :report "Retry with HTTPS and save decision for this session."
          (setf *quietly-use-https* t)
          (apply #'fetcher url file args)))))

(defun url-to-release (url)
  "extracts name of release from URL"
  (when (search "/archive/" url)
    (let* ((start (+ (search "/archive/" url) (length "/archive/")))
           (end (position #\/ url :start start)))
      (subseq url start end))))

#+sbcl
(defun md5 (file)
  "Returns md5sum of FILE"
  (format nil "~{~2,'0x~}" (coerce (sb-md5:md5sum-file file) 'list)))

(defun extract-openssl-digest (output)
  "Extracts digest from output of `openssl dgst'"
  (let ((space-pos (position #\Space output)))
    (subseq output (1+ space-pos))))    ; exclude space itself

#-sbcl
(defun md5 (file)
  "Returns md5sum of FILE"
  (extract-openssl-digest
   (uiop:run-program (list "openssl" "dgst" "-md5" (namestring file))
                     :output '(:string :stripped t))))

(defun file-size (file)
  "Returns the size of FILE in bytes"
  (with-open-file (f file)
    (file-length f)))

(defun verify-download (file name)
  "Checks that the md5 and size of FILE are as expected from the quicklisp
dist."
  (let ((release (ql-dist:find-release name)))
    (unless (string-equal (ql-dist:archive-md5 release) (md5 file))
      (error "md5 mismatch for ~A" name))
    (unless (string-equal (ql-dist:archive-content-sha1 release) (content-hash file))
      (error "sha1 mismatch for ~A" name))
    (unless (= (ql-dist:archive-size release) (file-size file))
      (error "file size mismatch for ~A" name))))

#+ql-https/ultralisp-check-sha1
(defun content-hash-ultralisp (tarfile)
  (let* ((octets (babel-streams:with-output-to-sequence (buffer)
                   (uiop:run-program
                    (apply #'concatenate
                           'string (list "tar -xOf" (namestring tarfile)))
                    :output buffer)))

         ;; (openssl-sha1
         ;;   (uiop:launch-program "openssl dgst -sha1"
         ;;                        :input (copy-seq octets)
         ;;                        :element-type '(unsigned-byte 8)
         ;;                        :output :stream))
         )
    ;; (print
    ;;  (extract-openssl-digest
    ;;   (read-binary-line
    ;;    (uiop:process-info-output openssl-sha1))))

    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha1 (copy-seq octets)))))

(defmethod ql-dist:check-local-archive-file :after ((release ql-dist:release))
  "Checks that the md5 and size of FILE are as expected from the quicklisp
dist."
  (let ((name (ql-dist:name release))
        (file (ql-dist:local-archive-file release)))

    (unless (string-equal (ql-dist:archive-md5 release) (md5 file))
      (error "md5 mismatch for ~A" name))

    (let* ((archive-sha1 (ql-dist:archive-content-sha1 release))
           (ql-dist-name (slot-value (slot-value release 'ql-dist:dist) 'ql-dist:name))
           (expected (case ql-dist-name
                       #+ql-https/quicklisp-check-sha1
                       ("quicklisp" (content-hash file))
                       #+ql-https/ultralisp-check-sha1
                       ("ultralisp" (content-hash-ultralisp file))
                       (t nil))))
      (when (and expected (not (string-equal archive-sha1 expected)))
        (error "sha1 mismatch for ~A" name)))))

(defun register-fetch-scheme-functions ()
  (setf ql-http:*fetch-scheme-functions*
        (list (cons "http" 'fetcher)
              (cons "https" 'fetcher))))
