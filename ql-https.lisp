;;;; ql-https.lisp

(defpackage #:ql-https
  (:use :cl)
  (:export
   #:fetcher))

(in-package #:ql-https)


(defun fetcher (url file &rest args)
  (declare (ignorable args))
  (if (uiop:string-prefix-p "https://" url)
      (uiop:run-program (format nil "curl -fsSL ~A -o ~A" url file)
                        :output '(:string :stripped t)
                        :error-output :output)
      (restart-case (error "We don't use HTTP here!")
        (use-http ()
          :report "Retry with HTTPS."
          (apply #'fetcher
                 (format nil "https~A" (subseq url 4))
                 file
                 args)))))

(setf ql-http:*fetch-scheme-functions*
      (list (cons "http" 'fetcher)
            (cons "https" 'fetcher)))
