;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ugnu packages linux)
  #:use-module (uguix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

;;; Commentary:
;;;
;;; Tegra Linux kernel from NVIDIA's L4T sources, built with nonfree firmware
;;; and blobs.
;;;
;;; Code:

(define-public tegra-linux
  (define (nv-tegra-repo path)
    (string-append "https://nv-tegra.nvidia.com/r/" path))
  (let* ((kernel-version "4.9")
         (kernel-root (string-append "kernel/kernel-" kernel-version)))
    (package
      (name "tegra-linux")
      (version "32.7.1")
      (source
       (origin
         (method git-multi-fetch)
         (uri
          (git-multi-reference
           (urls
            (list
             (nv-tegra-repo (string-append "linux-" kernel-version))
             (nv-tegra-repo "linux-nvgpu")
             (nv-tegra-repo "linux-nvidia")
             (nv-tegra-repo "device/hardware/nvidia/platform/t19x/common")
             (nv-tegra-repo "device/hardware/nvidia/platform/t19x/jakku-dts")
             (nv-tegra-repo "device/hardware/nvidia/platform/tegra/common")
             (nv-tegra-repo "device/hardware/nvidia/soc/t19x")
             (nv-tegra-repo "device/hardware/nvidia/soc/tegra")))
           (commits (make-list 8 (string-append "tegra-l4t-r"
                                                version)))
           (paths `(,kernel-root "kernel/nvgpu"
                                 "kernel/nvidia"
                                 "hardware/nvidia/platform/t19x/common"
                                 "hardware/nvidia/platform/t19x/jakku/kernel-dts"
                                 "hardware/nvidia/platform/tegra/common"
                                 "hardware/nvidia/soc/t19x"
                                 "hardware/nvidia/soc/tegra"))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1nsa4blyjc7in25m6yzp2vxf18a79fh1xfpvavp5xpcfdbkwkqgh"))
         (patches
          (parameterize
              ((%patch-path
                (map (lambda (directory)
                       (string-append directory "/ugnu/packages/patches"))
                     %load-path)))
            (search-patches "tegra-linux-build-fixes.patch")))))
      (supported-systems '("aarch64-linux"))
      (build-system gnu-build-system)
      (native-inputs
       `(("perl" ,perl)
         ("python" ,python-wrapper)
         ("bc" ,bc)
         ("openssl" ,openssl)
         ("elfutils" ,elfutils)
         ("flex" ,flex)
         ("bison" ,bison)
         ("gmp" ,gmp)
         ("mpfr" ,mpfr)
         ("mpc" ,mpc)
         ("xxd" ,xxd)))
      (arguments
       (list
        #:modules
        '((guix build gnu-build-system) (guix build utils)
          (srfi srfi-1)
          (srfi srfi-26)
          (ice-9 ftw)
          (ice-9 match))
        #:make-flags
        #~(list "-C" #$kernel-root "Image" "modules")
        #:phases
        ;; copied almost verbatim from gnu/packages/linux.scm
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-buildfiles
              (lambda _
                (substitute* (find-files "." "^Makefile(\\.include)?$")
                  (("/bin/pwd") "pwd"))))
            (replace 'configure
              (lambda* (#:key inputs target #:allow-other-keys)
                (setenv "KCONFIG_NOTIMESTAMP" "1")
                (setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))
                (setenv "KBUILD_BUILD_USER" "guix")
                (setenv "KBUILD_BUILD_HOST" "guix")
                (let ((arch #$(system->linux-architecture
                               (or (%current-target-system)
                                   (%current-system)))))
                  (setenv "ARCH" arch))
                (when target
                  (setenv "CROSS_COMPILE" (string-append target "-")))
                (invoke
                 "make" "-C" #$kernel-root "tegra_gnu_linux_defconfig")))
            (replace 'install
              (lambda* (#:key inputs native-inputs #:allow-other-keys)
                (let ((moddir (string-append #$output "/lib/modules")))
                  ;; install kernel image, kernel configuration, and link map
                  (for-each (lambda (file)
                              (install-file file #$output))
                            (find-files "." "^(\\.config|Image|System\\.map|Module\\.symvers)$"))
                  ;; install kernel modules
                  (mkdir-p moddir)
                  (invoke "make" "-C" #$kernel-root
                          ;; disable depmod because the guix system's
                          ;; module directory is an union of potentially
                          ;; multiple packages; it is not possible to use
                          ;; depmod to usefully calculate a dependency
                          ;; graph while building only one of them
                          "DEPMOD=true"
                          (string-append "MODULE_DIR=" moddir)
                          (string-append "INSTALL_PATH=" #$output)
                          (string-append "INSTALL_MOD_PATH=" #$output)
                          "INSTALL_MOD_STRIP=1"
                          "modules_install")
                  (let* ((versions (filter (lambda (name)
                                             (not (string-prefix? "." name)))
                                           (scandir moddir)))
                         (version (match versions
                                         ((x) x))))
                    ;; there are symlinks to the build and source
                    ;; directory; both will point to target
                    ;; /tmp/guix-build* and thus not be useful in a
                    ;; profile
                    (false-if-file-not-found
                     (delete-file
                      (string-append moddir "/" version "/build")))
                    (false-if-file-not-found
                     (delete-file
                      (string-append moddir "/" version "/source"))))))))))
      (home-page "https://nv-tegra.nvidia.com")
      (synopsis "Linux kernel for the NVIDIA Tegra")
      (description "Linux kernel for the NVIDIA Tegra.")
      (license license:gpl2))))

;;; linux.scm ends here
