;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (uguix deb-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:export (url-fetch/deb))

;;; Commentary:
;;;
;;; An <origin> method that downloads and unpacks a .deb package. This is only
;;; really useful for nonfree packages that distribute .deb packages instead of
;;; source.
;;;
;;; Code:

(define* (url-fetch/deb url hash-algo hash
                        #:optional name
                        #:key (system (%current-system))
                        (guile (default-guile)))
  "Similar to 'url-fetch' but unpack the deb archive as well."
  (define file-name
    (match url
           ((head _ ...)
            (basename head))
           (_
            (basename url))))
  (define binutils
    (module-ref (resolve-interface '(gnu packages base)) 'binutils))
  (define zstd
    (module-ref (resolve-interface '(gnu packages compression)) 'zstd))
  (define tar
    (module-ref (resolve-interface '(gnu packages base)) 'tar))

  (mlet %store-monad ((drv (url-fetch url hash-algo hash
                                      (string-append "deb-"
                                                     (or name file-name))
                                      #:system system
                                      #:guile guile))
                      (guile (package->derivation guile system)))
    (gexp->derivation (or name file-name)
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (mkdir #$output)
            (chdir #$output)
            (invoke (string-append #+binutils "/bin/ar")
                    "xf" #$drv)
            (setenv "PATH" (string-append #+zstd "/bin"))
            (invoke (string-append #+tar "/bin/tar")
                    "xf" (string-append #$output "/data.tar.zst"))))
      #:system system
      #:guile-for-build guile
      #:graft? #f
      #:local-build? #t)))

;;; deb-download.scm ends here
