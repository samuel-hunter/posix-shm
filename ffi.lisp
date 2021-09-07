;;; ffi.lisp - shm foreign function interface
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

(defpackage #:xyz.shunter.posix-shm.ffi
  (:import-from #:cl
                #:defpackage
                #:in-package)
  (:export #:*errno*))

(in-package #:xyz.shunter.posix-shm.ffi)



(autowrap:c-include '(#:posix-shm #:spec "shm.h")
                    :spec-path '(#:posix-shm #:spec)
                    :exclude-definitions (".*")
                    :include-definitions ("_t$"
                                          "^__"
                                          "stat"

                                          "^O_*" ;; open flags
                                          "^S_*" ;; permission bits
                                          "^PROT_*" ;; map protections
                                          "^MAP_SHARED$"
                                          "^EEXIST$"
                                          "^EINTR$"

                                          "^shm_open$"
                                          "^ftruncate$"
                                          "^mmap$"
                                          "^munmap$"
                                          "^shm_unlink$"
                                          "^close$"
                                          "^fstat$"
                                          "^fchown$"
                                          "^fchmod$"

                                          "^strerror$"))

(cffi:defcvar "errno" :int)

(cffi:define-foreign-library libc
  (:default "libc"))

(cffi:define-foreign-library librt
  (:unix "librt.so")
  (:default "librt"))

(cffi:use-foreign-library libc)
(cffi:use-foreign-library librt)
