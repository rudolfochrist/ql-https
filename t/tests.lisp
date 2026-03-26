;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:ql-https/test
  (:use :cl :fiveam))

(in-package #:ql-https/test)

(def-suite* :ql-https)

#+(and sbcl sb-thread)
(defmacro wait ((&key (timeout 20)) &body body)
  (let ((gthread (gensym "thread")))
    `(let ((,gthread (sb-thread:make-thread (lambda ()
                                              (progn ,@body)))))
       (sb-thread:join-thread ,gthread :timeout ,timeout))))

#+ecl
(defmacro wait ((&key (timeout 20)) &body body)
  (decalre (ignore timeout))
  (let ((gthread (gensym "thread")))
    `(let ((,gthread (mp:process-run-function 'wait-thread (lambda () (progn ,@body)))))
       (mp:process-join ,gthread))))

#-(or sbcl ecl)
(defmacro wait ((&key (timeout 20)) &body body)
  (declare (ignore timeout))
  `(progn ,@body))

(test test-ql-https-is-initialized-correctly
  (let ((http-function (cdr (assoc "http" ql-http:*fetch-scheme-functions* :test #'string=)))
        (https-function (cdr (assoc "https" ql-http:*fetch-scheme-functions* :test #'string=))))
    (is (eq 'ql-https:fetcher http-function) "HTTP doesn't use `ql-https:fetch'")
    (is (eq 'ql-https:fetcher https-function) "HTTP doesn't use `ql-https:fetch'")))

(test test-downloading-system
  (ql:uninstall "str")
  (let ((ql-https:*quietly-use-https* nil))
    (signals ql-https:no-https-error
      (ql:quickload "str" :silent t))))

(test test-quicklisp-download
  (ql:uninstall "str")
  (wait ()
        (let ((ql-https:*quietly-use-https* t))
          (ql:quickload "str" :silent t)))
  (is (find-package :str)))

(test test-ultralisp-download
  (ql:uninstall "hyperdoc")
  (wait ()
        (let ((ql-https:*quietly-use-https* t))
          ;; install ultralisp
          (ql-dist:install-dist "https://dist.ultralisp.org/" :prompt nil :replace t)
          (ql:quickload "hyperdoc" :silent t)))
  (is (find-package :hyperdoc)))



