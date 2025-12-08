;;;; ql-https.asd

(dolist (dist (ql-dist:all-dists))
  (let ((dist-name (slot-value dist 'ql-dist::name)))
    (cond
      ((string= dist-name "quicklisp")
       (pushnew :ql-https/quicklisp-check-sha1 *features*))
      ((string= dist-name "ultralisp")
       (pushnew :ql-https/ultralisp-check-sha1 *features*)))))

(defsystem "ql-https"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MIT"
  :homepage "https://github.com/rudolfochrist/ql-https"
  :bug-tracker "https://github.com/rudolfochrist/ql-https/issues"
  :source-control (:git "https://github.com/rudolfochrist/ql-https.git")
  :version (:read-file-line "version")
  :depends-on ((:require "uiop")
               #+ql-https/ultralisp-check-sha1 (:require "ironclad")
               #+ql-https/ultralisp-check-sha1 (:require "babel-streams")
               (:feature :sbcl :sb-md5))
  :components ((:file "ql-https")
               (:file "content-hash"))
  :description "Enable HTTPS in Quicklisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :perform (load-op :after (o c)
                    (uiop:symbol-call :ql-https :register-fetch-scheme-functions)
                    (pushnew :ql-https *features*))
  :in-order-to ((test-op (test-op "ql-https/test"))))


(defsystem "ql-https/test"
  :description "Tests for ql-https"
  :depends-on ((:require "uiop")
               "fiasco"
               "ql-https")
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :fiasco :run-package-tests :package :ql-https/test)
                      #+(not (or :swank :slynk))
                      (error "Tests failed."))))



(loop for f in (list :ql-https/quicklisp-check-sha1 :ql-https/ultralisp-check-sha1)
      do (setf *features* (delete f *features*)))


