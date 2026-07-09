;;; SPDX-License-Identifier: MPL-2.0
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(require 'asdf)

(handler-bind ((error (lambda (c)
                        (let ((restart
                                (find-if (lambda (restart-name)
                                           (string-equal "USE-HTTPS-SESSION"
                                                         (symbol-name restart-name)))
                                         (compute-restarts c)
                                         :key #'restart-name)))
                          (cond
                            (restart
                             (invoke-restart restart))
                            (t
                             (format *error-output* "~%Installation failed~%~A~%" c)
                             (uiop:quit 1)))))))
  (uiop:symbol-call :ql-setup :setup))

(assert (equalp ql-http:*fetch-scheme-functions* '(("http" . ql-https:fetcher) ("https" . ql-https:fetcher))))

(ql-util:without-prompting
    (ql:add-to-init-file))
