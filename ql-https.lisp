;;;; ql-https.lisp

(defpackage #:ql-https
  (:use :cl)
  (:export
   #:fetcher
   #:*quietly-use-https*))

(in-package #:ql-https)

(defvar *quietly-use-https* nil)


(defun fetcher (url file &rest args)
  (declare (ignorable args))
  (if (uiop:string-prefix-p "https://" url)
      (uiop:run-program (format nil "curl -fsSL ~A -o ~A" url file)
                        :output '(:string :stripped t)
                        :error-output :output)
      (restart-case
          (handler-bind ((error (lambda (c)
                                  (declare (ignore c))
                                  (when *quietly-use-https*
                                    (invoke-restart 'use-http)))))
            (error "We don't use HTTP here!"))
        (use-http ()
          :report "Retry with HTTPS."
          (apply #'fetcher
                 (format nil "https~A" (subseq url 4))
                 file
                 args)))))

(setf ql-http:*fetch-scheme-functions*
      (list (cons "http" 'fetcher)
            (cons "https" 'fetcher)))
