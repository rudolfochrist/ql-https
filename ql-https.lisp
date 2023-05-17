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
      (uiop:run-program (format nil "curl -fsSL ~A -o ~A" url file)
                        :output '(:string :stripped t)
                        :error-output :output)
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


(defun register-fetch-scheme-functions ()
  (setf ql-http:*fetch-scheme-functions*
        (list (cons "http" 'fetcher)
              (cons "https" 'fetcher))))
