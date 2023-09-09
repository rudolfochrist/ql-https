#!/usr/bin/env bash

set -euo pipefail

LISP=${LISP=sbcl}

echo "Downloading quicklisp metadata..."
mkdir -p ~/quicklisp
meta=$( curl -s https://beta.quicklisp.org/client/quicklisp.sexp | \
        awk '/:client-tar/,/)/' | tr '\n' ' ' | sed -e's/\s\+/ /g' )

url=$( grep -oP '(?<=:url ")[^"]*' <<< "$meta" )
sha256=$( grep -oP '(?<=:sha256 ")[^"]*' <<< "$meta" )

echo "Downloading quicklisp client..."
curl -s "$url" -o ~/quicklisp/quicklisp.tar

if [ "$sha256" != "$(sha256sum ~/quicklisp/quicklisp.tar  | cut -d' ' -f 1)" ]
then
   echo "sha mismatch" >&2
   exit 1
fi

tar xf ~/quicklisp/quicklisp.tar -C ~/quicklisp
rm ~/quicklisp/quicklisp.tar

echo "Cloning ql-https..."
git clone https://github.com/rudolfochrist/ql-https ~/common-lisp/ql-https

echo "Running setup code..."
$LISP <<EOF
(require 'asdf)
(load "~/common-lisp/ql-https/ql-setup.lisp")
(asdf:load-system "ql-https")
(assert (equalp ql-http:*fetch-scheme-functions* '(("http" . ql-https:fetcher) ("https" . ql-https:fetcher))))
(setf ql-https:*quietly-use-https* t)
(quicklisp:setup)
(ql-util:without-prompting
  (ql:add-to-init-file))
EOF

cat > ~/quicklisp/setup.lisp <<EOF
(require 'asdf)
(let ((quicklisp-init #p"~/common-lisp/ql-https/ql-setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)
    (asdf:load-system "ql-https")
    (uiop:symbol-call :quicklisp :setup)))

;; optional
#+ql-https
(setf ql-https:*quietly-use-https* t)
EOF

echo "All done!"
