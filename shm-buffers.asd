;;; shm-buffers.asd - shm-buffers system definition
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

(asdf:defsystem #:shm-buffers
  :description "POSIX shared memory buffers"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"

  :depends-on (#:alexandria
               #:cl-autowrap)
  :serial t
  :components ((:module #:spec
                :pathname "spec"
                :components ((:static-file "shm.h")))
               (:file "ffi")
               (:file "shm-buffers")))
