;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ugnu system images jetson)
  #:use-module (ugnu packages linux)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader extlinux)
  #:use-module (gnu image)
  #:use-module (gnu packages admin)
  #:use-module (gnu platforms arm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image))

;;; Commentary:
;;;
;;; Base OS definition and system image for NVIDIA's xavier-nx SoC.
;;;
;;; Code:

;; TODO: actually implement the bootloader here
(define jetson-bootloader
  (bootloader
   (inherit extlinux-bootloader-gpt)
   (installer #f)))

(define-public jetson-barebones-os
  (operating-system
    (host-name "jetson-xavier-nx")
    (timezone "US/Pacific")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader jetson-bootloader)
                 (targets '("/dev/mmcblk0p1"))))
    (initrd-modules '())
    (kernel tegra-linux)
    (file-systems (cons (file-system
                          (device (file-system-label "jetson-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    ;; shepherd-0.9 depends on guile-fibers, which does not cross-compile (see
    ;; #54793); point at shepherd-0.8 instead
    (essential-services
     (modify-services (operating-system-default-essential-services
                       this-operating-system)
       (shepherd-root-service-type config => (shepherd-configuration
                                              (inherit config)
                                              (shepherd shepherd-0.8)))))))

(define-public jetson-image-type
  (image-type
   (name 'jetson-raw)
   (constructor (lambda (os)
                  (image-with-os (raw-with-offset-disk-image) os)))))

(define jetson-barebones-raw-image
  (image
   (inherit
    (os+platform->image jetson-barebones-os aarch64-linux
                        #:type jetson-image-type))
   (name 'jetson-barebones-raw-image)))

jetson-barebones-raw-image

;;; xavier-nx.scm ends here
