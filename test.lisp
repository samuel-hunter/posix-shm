;;; test.lisp - posix-shm test suite
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

(defpackage #:xyz.shunter.posix-shm.test
  (:use #:cl #:parachute)
  (:local-nicknames (#:shm #:xyz.shunter.posix-shm)))

(in-package #:xyz.shunter.posix-shm.test)



(defun random-name ()
  ;; Steal util function from shm internal
  (shm::random-name))

(defparameter +shm-name+
  "/xyz.shunter.posix-shm.test")

(define-test shm-open
  (let ((shm (shm:shm-open +shm-name+ :open-flags '(:read-write :create)
                           :permissions '(:all-user))))
    (unwind-protect
      (of-type (integer 1 *) shm)
      (shm:shm-close shm)))

  (shm:with-open-shm (shm +shm-name+ :open-flags '(:read-write :create)
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm)))

(define-test can-use-various-open-flags
  :parent shm-open
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:read-write :create)
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm))

  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdwr :creat)
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm))

  (shm:with-open-shm (shm +shm-name+ :open-flags '(:read-only :truncate)
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm))

  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdonly :trunc)
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:read-write :create :exclusive)
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdwr :creat :excl)
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm)))

(define-test fail-without-creat-on-noexist
  :parent shm-open
  (ignore-errors (shm:shm-unlink +shm-name+))
  (fail (shm:shm-open +shm-name+ :open-flags '(:rdonly)
                      :permissions '(:all-user))))

(define-test fail-with-excl-on-exist
  :parent shm-open
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdonly :create)
                          :permissions '(:all-user)))

  (fail (shm:shm-open +shm-name+ :open-flags '(:rdonly :create :excl)
                      :permissions '(:all-user))))

(define-test can-use-various-permissions
  :parent shm-open
  (ignore-errors (shm:shm-unlink +shm-name+))
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdonly :create :excl)
                          :permissions '(:exec-user :write-user :read-user
                                         :exec-group :write-group :read-group
                                         :exec-other :write-other :read-other))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdonly :create :excl)
                          :permissions '(:xusr :wusr :rusr
                                         :xgrp :wgrp :rgrp
                                         :xoth :woth :roth))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdonly :create :excl)
                          :permissions '(:all-user :all-group :all-other))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdonly :create :excl)
                          :permissions '(:rwxu :rwxg :rwxo))
    (of-type (integer 1 *) shm)))

(define-test cant-open-names-with-multiple-slashes
  :parent shm-open
  (fail (shm:shm-open "/bad/name" :open-flags '(:rdwr :create)
                      :permissions '(:all-user))))

(define-test shm-ftruncate
  :depends-on (shm-open)
  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdwr :create)
                          :permissions '(:all-user))
    (is eq nil
        (multiple-value-list (shm:shm-ftruncate shm 100))))

  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdonly :create)
                          :permissions '(:all-user))
    (fail (shm:shm-ftruncate shm 100)))

  (shm:with-open-shm (shm +shm-name+ :open-flags '(:rdwr :create)
                          :permissions '(:all-user))
    (fail (shm:shm-ftruncate shm -1))))

(define-test mmap
  :depends-on (shm-open shm-ftruncate)
  )

(define-test fstat
  :depends-on (shm-open))

(define-test fchown
  :depends-on (shm-open))

(define-test fchmod
  :depends-on (shm-open))
