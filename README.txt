1 NAME
======

  ql-https --- HTTPS support for Quicklisp via curl


2 VERSION
=========

  ,----
  | 0.5.0
  `----


3 SYNOPSIS
==========

  ,----
  | (asdf:load-system "ql-https")
  `----


4 DESCRIPTION
=============

4.1 PREREQUISITES
~~~~~~~~~~~~~~~~~

  - [Quicklisp]
  - curl (refer to your system package manager)


[Quicklisp] <https://www.quicklisp.org/beta/>


4.2 INSTALLATION
~~~~~~~~~~~~~~~~

  1. `mkdir ~/quicklis' and `cd ~/quicklisp'
  2. Go to <https://beta.quicklisp.org/client/quicklisp.sexp> and lookup
     `:client-tar' URL, download it, verify hash and untar.
  3. Clone ql-https from <https://github.com/rudolfochrist/ql-https.git>
     to to `~/common-lisp/ql-https'
  4. Disconnect internet. (Prevent that anything leaks over HTTP during
     the installation)
  5. Start a fresh REPL and (require 'asdf)
  6. Load `~/common-lisp/ql-https/ql-setup.lisp'
  7. Eval `(asdf:load-system "ql-https")'
  8. Inspect `ql-http:*scheme-functions*' and verify everything was
     registered properly.
  9. Connect internet.
  10. Eval `(quicklisp:setup)' - use the USE-HTTPS restart if you hit
      the network.

  Removing the /Missing client-info.sexp, using mock info/ warning.

  1. Eval `(ql:update-client)'
  2. move `~/quicklisp/tmp/client-info.sexp' to `~/quicklisp'


4.3 STARTUP
~~~~~~~~~~~

  ,----
  | (let ((quicklisp-init #p"~/common-lisp/ql-https/ql-setup.lisp"))
  |   (when (probe-file quicklisp-init)
  |     (load quicklisp-init)
  |     (asdf:load-system "ql-https")
  |     (uiop:symbol-call :quicklisp :setup)))
  | 
  | ;; optional
  | #+ql-https
  | (setf ql-https:*quietly-use-https* t)
  `----


5 AUTHOR
========

  Sebastian Christ (<mailto:rudolfo.christ@pm.me>)


6 COPYRIGHT
===========

  Copyright (c) 2022 Sebastian Christ (rudolfo.christ@pm.me)


7 LICENSE
=========

  Released under the MIT license.
