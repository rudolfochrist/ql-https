#!/usr/bin/env bash

set -euo pipefail

LISP=${LISP=sbcl}
CLFLAGS=${CLFLAGS=--non-interactive --no-userinit}

# For testers
QL_TOPDIR="${QL_TOPDIR-$HOME/quicklisp}"
CLDIR="${CLDIR-$HOME/common-lisp}"
SKIP_USERINIT="${SKIP_USERINIT-no}"

if test -d "$QL_TOPDIR"; then
    echo "Cannot install Quicklisp because it seems it is already installed!"
    echo "Please check $QL_TOPDIR"
    exit 1
fi

echo "Downloading quicklisp metadata..."
mkdir -p "$QL_TOPDIR"
meta=$( curl -s https://beta.quicklisp.org/client/quicklisp.sexp | \
            awk '/:client-tar/,/)/' | tr '\n' ' ' | tr -s ' ' )

url=$( perl -nle 'print $& if m{(?<=:url ")[^"]*}g' <<< "$meta" )
[[ "$url" =~ ^http:// ]] && url="https${url#http}"
sha256=$( perl -nle 'print $& if m{(?<=:sha256 ")[^"]*}g' <<< "$meta" )

echo "Downloading quicklisp client..."
curl -s "$url" -o "$QL_TOPDIR"/quicklisp.tar

if [ "$sha256" != "$(openssl dgst -sha256 "$QL_TOPDIR"/quicklisp.tar  | cut -d' ' -f 2)" ]
then
    echo "sha mismatch" >&2
    exit 1
fi

tar xf "$QL_TOPDIR"/quicklisp.tar -C "$QL_TOPDIR"
rm "$QL_TOPDIR"/quicklisp.tar

echo "Cloning ql-https..."
git clone https://github.com/rudolfochrist/ql-https "$CLDIR"/ql-https

if test "$SKIP_USERINIT" = no; then
    echo "Running setup code..."
    $LISP $CLFLAGS \
          --eval "(require 'asdf)" \
          --load "$CLDIR/ql-https/ql-setup.lisp" \
          --load "$CLDIR/ql-https/install.lisp"
    cat > "$QL_TOPDIR"/setup.lisp <<EOF
(require 'asdf)
(let ((quicklisp-init #p"~/common-lisp/ql-https/ql-setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)
    (uiop:symbol-call :ql-setup :setup)))

;; optional
#+ql-https
(setf ql-https:*quietly-use-https* t)
EOF
fi

echo "All done!"
