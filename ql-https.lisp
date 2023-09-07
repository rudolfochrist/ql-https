;;;; ql-https.lisp

(defpackage #:ql-https
  (:use :cl)
  (:export
   #:fetcher
   #:*quietly-use-https*
   #:register-fetch-scheme-functions))

(in-package #:ql-https)

(defvar *quietly-use-https* nil
  "If non-nil quietly use HTTPS.")

(defun fetcher (url file &rest args)
  "Fetch URL and safe it to FILE."
  (declare (ignorable args))
  (if (uiop:string-prefix-p "https://" url)
      (let ((output (uiop:run-program (format nil "curl -fsSL ~A -o ~A" url file)
                                      :output '(:string :stripped t)
                                      :error-output :output))
            (file (and file (probe-file file))))
        (verify-download file (url-to-release url))
        (values output file))
      (restart-case
          (handler-bind ((error (lambda (c)
                                  (declare (ignore c))
                                  (when *quietly-use-https*
                                    (invoke-restart 'use-https)))))
            (error "We don't use HTTP here!~&URL: ~A" url))
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
  (let* ((start (+ (search "/archive/" url) (length "/archive/")))
         (end (position #\/ url :start start)))
    (subseq url start end)))

(defun md5 (file)
  "Returns md5sum of FILE"
  (uiop:run-program (format nil "md5sum \"~A\" | cut -d' ' -f 1" file)
                    :output '(:string :stripped t)))

(defun file-size (file)
  "Returns the size of FILE in bytes"
  (with-open-file (f file)
    (file-length f)))

(defun verify-download (file name)
  "Checks that the md5 and size of FILE are as expected from the quicklisp
dist."
  (let ((release (ql-dist:find-release name)))
    (unless (string= (ql-dist:archive-md5 release) (md5 file))
      (error "md5 mismatch for ~A" name))
    (unless (= (ql-dist:archive-size release) (file-size file))
      (error "file size mismatch for ~A" name))))

(defun register-fetch-scheme-functions ()
  (setf ql-http:*fetch-scheme-functions*
        (list (cons "http" 'fetcher)
              (cons "https" 'fetcher))))
