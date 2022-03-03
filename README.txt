1 NAME
======

  ql-https --- HTTPS support for Quicklisp via curl


2 VERSION
=========

  ,----
  | 0.1
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

  Clone the repository somewhere ASDF finds it, like `~/common-lisp' and
  add following lines to your lisp init/startup file (e.g. `~/.sbclrc')

  ,----
  | (require 'asdf)
  | #+quicklisp (asdf:load-system "ql-https")
  | #+ql-https (setf ql-https:*quietly-use-https* t)  ;; optional
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
