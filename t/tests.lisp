;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(fiasco:define-test-package :ql-https/test)
(in-package #:ql-https/test)

(deftest test-ql-https-is-initialized-correctly ()
  (let ((http-function (cdr (assoc "http" ql-http:*fetch-scheme-functions* :test #'string=)))
        (https-function (cdr (assoc "https" ql-http:*fetch-scheme-functions* :test #'string=))))
    (is (eq 'ql-https:fetcher http-function) "HTTP doesn't use `ql-https:fetch'")
    (is (eq 'ql-https:fetcher https-function) "HTTP doesn't use `ql-https:fetch'")))

(deftest test-downloading-system ()
  (let ((ql-https:*quietly-use-https* nil))
    (signals ql-https:no-https-error
      (ql:quickload "str" :silent t))))



