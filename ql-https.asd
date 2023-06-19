;;;; ql-https.asd

(defsystem "ql-https"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MIT"
  :homepage "https://github.com/rudolfochrist/ql-https"
  :bug-tracker "https://github.com/rudolfochrist/ql-https/issues"
  :source-control (:git "https://github.com/rudolfochrist/ql-https.git")
  :version (:read-file-line "version")
  :depends-on ((:require "uiop"))
  :components ((:file "ql-https"))
  :description "Enable HTTPS in Quicklisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :perform (load-op :after (o c)
                    (uiop:symbol-call :ql-https :register-fetch-scheme-functions)
                    (pushnew :ql-https *features*)))


