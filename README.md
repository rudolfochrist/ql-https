

# NAME

ql-https &#x2014; HTTPS support for Quicklisp via curl


# VERSION

    0.5.0


# SYNOPSIS

    (asdf:load-system "ql-https")


# DESCRIPTION


## PREREQUISITES

-   [Quicklisp](https://www.quicklisp.org/beta/)
-   curl (refer to your system package manager)


## AUTOMATIC INSTALLATION

The default implementation is sbcl, if you are using another then set the `LANG`
environment variable, for example to use Clozure common lisp:

    export LANG=ccl

now run the installer script:

    curl https://raw.githubusercontent.com/rudolfochrist/ql-https/install-script/install.sh | bash


## MANUAL INSTALLATION

1.  `mkdir ~/quicklisp` and `cd ~/quicklisp`
2.  Go to <https://beta.quicklisp.org/client/quicklisp.sexp> and lookup `:client-tar` URL, download it, verify
    hash and untar.
3.  Clone ql-https from <https://github.com/rudolfochrist/ql-https.git> to
    to `~/common-lisp/ql-https`
4.  Disconnect internet. (Prevent that anything leaks over HTTP during the installation)
5.  Start a fresh REPL and (require 'asdf)
6.  Load `~/common-lisp/ql-https/ql-setup.lisp`
7.  Eval `(asdf:load-system "ql-https")`
8.  Inspect `ql-http:*fetch-scheme-functions*` and verify everything was registered properly. Both `http` and
    `https` have `ql-https:fetcher` registered.
9.  Connect internet.
10. Eval `(quicklisp:setup)` - use the USE-HTTPS restart if you hit the network.

Removing the *Missing client-info.sexp, using mock info* warning.

1.  Eval `(ql:update-client)`
2.  move `~/quicklisp/tmp/client-info.sexp` to `~/quicklisp`

Watch ASCIInema:

[![asciicast](https://asciinema.org/a/585361.svg)](https://asciinema.org/a/585361)


## STARTUP

    (let ((quicklisp-init #p"~/common-lisp/ql-https/ql-setup.lisp"))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)
        (asdf:load-system "ql-https")
        (uiop:symbol-call :quicklisp :setup)))
    
    ;; optional
    #+ql-https
    (setf ql-https:*quietly-use-https* t)


# AUTHOR

Sebastian Christ (<mailto:rudolfo.christ@pm.me>)


# COPYRIGHT

Copyright (c) 2022 Sebastian Christ (rudolfo.christ@pm.me)


# LICENSE

Released under the MIT license.

