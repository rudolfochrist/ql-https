;; Copyright (c) 2014 Zachary Beane <zach@quicklisp.org>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:
;;
;; Copied to get rid the `(qicklisp:setup)' call.
;; Whenever the client is updated the setup.lisp is replaced
;; and contains the call to setup.

(defpackage #:ql-setup
  (:use #:cl)
  (:export #:*quicklisp-home*
           #:qmerge
           #:qenough))

(in-package #:ql-setup)

(unless *load-truename*
  (error "This file must be LOADed to set up quicklisp."))

(defvar *quicklisp-home*
  (make-pathname :name nil :type nil
                 :defaults (let ((qlhome "~/quicklisp/"))
                             (if (probe-file qlhome)
                                 qlhome
                                 (error "Quicklisp not installed to
                                 default location. Please set
                                 *quicklisp-home* manually and
                                 retry")))))

(defun qmerge (pathname)
  "Return PATHNAME merged with the base Quicklisp directory."
  (merge-pathnames pathname *quicklisp-home*))

(defun qenough (pathname)
  (enough-namestring pathname *quicklisp-home*))

;;; ASDF is a hard requirement of quicklisp. Make sure it's either
;;; already loaded or load it from quicklisp's bundled version.

(defvar *required-asdf-version* "3.0")

;;; Put ASDF's fasls in a separate directory

(defun implementation-signature ()
  "Return a string suitable for discriminating different
implementations, or similar implementations with possibly-incompatible
FASLs."
  ;; XXX Will this have problems with stuff like threads vs
  ;; non-threads fasls?
  (let ((*print-pretty* nil))
    (format nil "lisp-implementation-type: ~A~%~
                 lisp-implementation-version: ~A~%~
                 machine-type: ~A~%~
                 machine-version: ~A~%"
            (lisp-implementation-type)
            (lisp-implementation-version)
            (machine-type)
            (machine-version))))

(defun dumb-string-hash (string)
  "Produce a six-character hash of STRING."
  (let ((hash #xD13CCD13))
    (loop for char across string
          for value = (char-code char)
          do
          (setf hash (logand #xFFFFFFFF
                             (logxor (ash hash 5)
                                     (ash hash -27)
                                     value))))
    (subseq (format nil "~(~36,6,'0R~)" (mod hash 88888901))
            0 6)))

(defun asdf-fasl-pathname ()
  "Return a pathname suitable for storing the ASDF FASL, separated
from ASDF FASLs from incompatible implementations. Also, save a file
in the directory with the implementation signature, if it doesn't
already exist."
  (let* ((implementation-signature (implementation-signature))
         (original-fasl (compile-file-pathname (qmerge "asdf.lisp")))
         (fasl
          (qmerge (make-pathname
                   :defaults original-fasl
                   :directory
                   (list :relative
                         "cache"
                         "asdf-fasls"
                         (dumb-string-hash implementation-signature)))))
         (signature-file (merge-pathnames "signature.txt" fasl)))
    (ensure-directories-exist fasl)
    (unless (probe-file signature-file)
      (with-open-file (stream signature-file :direction :output)
        (write-string implementation-signature stream)))
    fasl))

(defun ensure-asdf-loaded ()
  "Try several methods to make sure that a sufficiently-new ASDF is
loaded: first try (require \"asdf\"), then loading the ASDF FASL, then
compiling asdf.lisp to a FASL and then loading it."
  (let ((source (qmerge "asdf.lisp")))
    (labels ((asdf-symbol (name)
               (let ((asdf-package (find-package '#:asdf)))
                 (when asdf-package
                   (find-symbol (string name) asdf-package))))
             (version-satisfies (version)
               (let ((vs-fun (asdf-symbol '#:version-satisfies))
                     (vfun (asdf-symbol '#:asdf-version)))
                 (when (and vs-fun vfun
                            (fboundp vs-fun)
                            (fboundp vfun))
                   (funcall vs-fun (funcall vfun) version)))))
      (block nil
        (macrolet ((try (&body asdf-loading-forms)
                     `(progn
                        (handler-bind ((warning #'muffle-warning))
                          (ignore-errors
                            ,@asdf-loading-forms))
                        (when (version-satisfies *required-asdf-version*)
                          (return t)))))
          (try)
          (try (require "asdf"))
          (let ((fasl (asdf-fasl-pathname)))
            (try (load fasl :verbose nil))
            (try (load (compile-file source :verbose nil :output-file fasl))))
          (error "Could not load ASDF ~S or newer" *required-asdf-version*))))))

(ensure-asdf-loaded)

;;;
;;; Quicklisp sometimes must upgrade ASDF. Ugrading ASDF will blow
;;; away existing ASDF methods, so e.g. FASL recompilation :around
;;; methods would be lost. This config file will make it possible to
;;; ensure ASDF can be configured before loading Quicklisp itself via
;;; ASDF. Thanks to Nikodemus Siivola for pointing out this issue.
;;;

(let ((asdf-init (probe-file (qmerge "asdf-config/init.lisp"))))
  (when asdf-init
    (with-simple-restart (skip "Skip loading ~S" asdf-init)
      (load asdf-init :verbose nil :print nil))))

(push (qmerge "quicklisp/") asdf:*central-registry*)

(let ((*compile-print* nil)
      (*compile-verbose* nil)
      (*load-verbose* nil)
      (*load-print* nil))
  (asdf:oos 'asdf:load-op "quicklisp" :verbose nil))
