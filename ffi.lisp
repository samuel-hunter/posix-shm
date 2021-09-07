;;; ffi.lisp - shm-buffers foreign function interface
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

(defpackage #:xyz.shunter.shm-buffers.ffi
  (:export #:*errno*))

(in-package #:xyz.shunter.shm-buffers.ffi)



(autowrap:c-include '(#:shm-buffers #:spec "shm.h")
                    :spec-path '(#:shm-buffers #:spec)
                    :exclude-definitions (".*")
                    :include-definitions ("size_t"
                                          "mode_t"
                                          "off_t"
                                          "__LOFF_T"
                                          "__OFF64_T"

                                          "^O_*" ;; open flags
                                          "^S_*" ;; permission bits
                                          "^PROT_*" ;; map permissions
                                          "^MAP_*" ;; map flags
                                          "^EEXIST$"
                                          "^EINTR$"

                                          "^shm_*"
                                          "^mmap$"
                                          "^munmap$"
                                          "^ftruncate$"
                                          "close"
                                          "^errno$"
                                          "^strerror$"))

(cffi:defcvar "errno" :int)

(cffi:define-foreign-library libc
  (:default "libc"))

(cffi:define-foreign-library librt
  (:unix "librt.so")
  (:default "librt"))

(cffi:use-foreign-library libc)
(cffi:use-foreign-library librt)
