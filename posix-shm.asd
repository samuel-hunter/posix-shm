;;; posix-shm.asd - posix-shm system definition
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

(asdf:defsystem #:posix-shm
  :description "POSIX shared memory"
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
               (:file "shm"))

  :in-order-to ((asdf:test-op (asdf:test-op :posix-shm/test))))

(asdf:defsystem #:posix-shm/test
  :description "Test suite for posix-shm"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.1"

  :depends-on (#:posix-shm
               #:parachute)
  :components ((:file "test"))

  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.posix-shm.test)))
