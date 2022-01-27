;;;; ql-https.asd

(defsystem "ql-https"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MIT"
  :version (:read-file-line "version")
  :depends-on ((:require "uiop"))
  :components ((:file "ql-https"))
  :description "Enable HTTPS in Quicklisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt")))
