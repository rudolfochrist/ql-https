# NAME


ql-https &#x2014; HTTPS support for Quicklisp via curl


# SYNOPSIS

    (asdf:load-system "ql-https")


# DESCRIPTION


## PREREQUISITES

-   [Quicklisp](https://www.quicklisp.org/beta/)
-   curl


## AUTOMATIC INSTALLATION

The default implementation is SBCL, if you are using another then set the `LISP`
environment variable, for example to use Clozure Common Lisp:

    export LISP=ccl

If you change the implementation, adjusting the `CLFLAGS` is almost
always necessary as well, for example for CCL:

    export CLFLAGS=-Q -b -n

Now run the installer script:

    curl https://raw.githubusercontent.com/rudolfochrist/ql-https/master/install.sh | bash


## MANUAL INSTALLATION

1. `mkdir ~/quicklisp` and `cd ~/quicklisp`
2. Go to <https://beta.quicklisp.org/client/quicklisp.sexp> and lookup
   `:client-tar` URL, download it, verify hash and untar.
3. Clone ql-https from <https://github.com/rudolfochrist/ql-https.git>
   to to `~/common-lisp/ql-https`
4. Start a fresh REPL and (require 'asdf)
5. Load `~/common-lisp/ql-https/ql-setup.lisp`
6. Eval `(uiop:symbol-call :ql-setup :setup)`. Invoke `ql-https`
   restarts as necessary. If you use an alternative install location
   for quicklisp eval `(uiop:symbol-call :ql-setup :setup
   #p"/path/to/quicklisp")`

Removing the *Missing client-info.sexp, using mock info* warning.

1.  Eval `(ql:update-client)`
2.  move `~/quicklisp/tmp/client-info.sexp` to `~/quicklisp`


## STARTUP

```lisp
(let ((quicklisp-init #p"~/common-lisp/ql-https/ql-setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)
	;; Alternatively: (uiop:symbol-call :ql-setup :setup #p"/path/to/quicklisp/")
    (uiop:symbol-call :ql-setup :setup)))

;; optional
#+ql-https
(setf ql-https:*quietly-use-https* t)
```

# AUTHOR

Sebastian Christ (<mailto:rudolfo.christ@pm.me>)


# COPYRIGHT

Copyright (c) 2022 Sebastian Christ (rudolfo.christ@pm.me)


# LICENSE

Released under the MIT license.
