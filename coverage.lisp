;;; coverage.lisp - Generate coverage statistics
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

(require :sb-cover)

(defpackage #:xyz.shunter.posix-shm.coverage
  (:use #:cl)
  (:export #:report))

(in-package #:xyz.shunter.posix-shm.coverage)



(defun report (directory)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op :posix-shm :force t)
  (asdf:test-system :posix-shm)
  (prog1
    (sb-cover:report directory)
    (declaim (optimize (sb-cover:store-coverage-data 0)))))

(report #P"/tmp/posix-shm-coverage/")
