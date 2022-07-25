;;; ffi.lisp - shm foreign function interface
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

;; Using uiop's define-package here, because defpackage warns of all symbols
;; that autowrap exports for us.
(uiop:define-package #:xyz.shunter.posix-shm.ffi
  (:use)
  (:import-from #:cl
                #:defpackage
                #:in-package)
  (:export #:*errno*))

(in-package #:xyz.shunter.posix-shm.ffi)



(autowrap:c-include '(#:posix-shm #:spec "shm.h")
                    :spec-path '(#:posix-shm #:spec)
                    :exclude-definitions (".*")
                    :include-definitions (;; types
                                          "^__"
                                          "^stat$"
                                          "^timespec$"
                                          "_t$"

                                          ;; Constants
                                          "^O_" ;; open flags
                                          "^S_" ;; permission bits
                                          "^PROT_" ;; map protections
                                          "^MAP_SHARED$"
                                          "^EEXIST$"
                                          "^EINTR$"
                                          "^ENOENT$"

                                          ;; syscalls
                                          "^ftruncate$"
                                          "^mmap$"
                                          "^munmap$"
                                          "^close$"
                                          "^fstat$"
                                          "^fchown$"
                                          "^fchmod$"

                                          ;; libc
                                          "^strerror$"

                                          ;; librt
                                          "^shm_open$"
                                          "^shm_unlink$"))

(cffi:defcvar "errno" :int)

(cffi:define-foreign-library libc
  (:default "libc"))

(cffi:define-foreign-library librt
  (:unix "librt.so")
  (:default "librt"))

(cffi:use-foreign-library libc)
(cffi:use-foreign-library librt)
