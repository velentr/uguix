;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (uguix git-download)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (git-multi-reference
            git-multi-reference?
            git-multi-reference-urls
            git-multi-reference-commits
            git-multi-reference-paths
            git-multi-fetch))

;;; Commentary:
;;;
;;; An <origin> method that fetches specific commits from multiple Git
;;; repositories and merges the trees together. The repository URLs and commit
;;; hashes ares specified with a <git-multi-reference> object.
;;;
;;; Code:

(define (git-package)
  "Return the default Git package."
  (let ((distro (resolve-interface '(gnu packages version-control))))
    (module-ref distro 'git-minimal)))

(define-record-type* <git-multi-reference>
  git-multi-reference make-git-multi-reference
  git-multi-reference?
  (urls    git-multi-reference-urls)
  (commits git-multi-reference-commits)
  (paths   git-multi-reference-paths))

(define* (git-multi-fetch ref hash-algo hash
                          #:optional name
                          #:key (system (%current-system))
                          (guile (default-guile))
                          (git (git-package)))
  (define build
    (with-imported-modules
     (source-module-closure '((guix build git)
                              (guix build utils)))
     #~(begin
         (use-modules (guix build git)
                      (guix build utils)
                      (srfi srfi-1))

         (every (lambda (reference)
                  (let ((url    (car   reference))
                        (commit (cadr  reference))
                        (path   (caddr reference)))
                    (git-fetch url commit (string-append #$output "/" path)
                               #:git-command (string-append #+git "/bin/git"))))
                '#$(sexp->gexp (zip (git-multi-reference-urls ref)
                                    (git-multi-reference-commits ref)
                                    (git-multi-reference-paths ref)))))))
  (mlet %store-monad ((guile (package->derivation guile system)))
        (gexp->derivation (or name "git-multi-checkout") build
                          #:leaked-env-vars '("http_proxy" "https_proxy"
                                              "LC_ALL" "LC_MESSAGES" "LANG"
                                              "COLUMNS")
                          #:system system
                          #:hash-algo hash-algo
                          #:hash hash
                          #:recursive? #t
                          #:local-build? #t
                          #:guile-for-build guile)))

;;; git-download.scm ends here
