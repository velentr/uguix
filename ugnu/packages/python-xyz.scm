;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ugnu packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public python-crossenv
  (package
    (name "python-crossenv")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "crossenv" version))
              (sha256
               (base32
                "0n1rrvg3krwj5lfhwcaki4z82hgjrngz8k70804bgvbrb881cb34"))))
    (build-system python-build-system)
    (arguments
     ;; tests are not included in the archive
     `(#:tests? #f))
    (home-page "https://crossenv.readthedocs.io/en/latest/")
    (synopsis "Cross-compiling virtualenv for Python")
    (description
     "This package is a tool for cross-compiling extension modules.  It
creates a special virtual environment such that pip or setup.py will cross
compile packages for you, often with no further work on your part.")
    (license license:expat)))
