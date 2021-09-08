;;; test.lisp - posix-shm test suite
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause

(defpackage #:xyz.shunter.posix-shm.test
  (:use #:cl #:parachute)
  (:local-nicknames (#:shm #:xyz.shunter.posix-shm)))

(in-package #:xyz.shunter.posix-shm.test)



(defparameter +shm-name+
  "/xyz.shunter.posix-shm.test")

(defparameter +int-size+
  (cffi:foreign-type-size :int))

(define-test shm-open
  (ignore-errors (shm:shm-unlink +shm-name+))
  (let ((shm (shm:shm-open +shm-name+ :if-does-not-exist :create
                           :permissions '(:all-user))))
    (of-type (integer 1 *) shm)
    (shm:shm-close shm))

  (let ((shm (shm:shm-open*)))
    (of-type (integer 1 *) shm)
    (shm:shm-close shm))

  (shm:with-open-shm (shm +shm-name+)
    (of-type (integer 1 *) shm))
  (shm:shm-unlink +shm-name+))

(define-test shm-open.can-use-various-options
  :parent shm-open
  (shm:with-open-shm (shm +shm-name+ :if-exists nil
                          :if-does-not-exist :create
                          :permissions '(:all-user)
                          )
    (of-type (integer 1 *) shm))

  (fail (shm:shm-open +shm-name+ :if-exists :error
                      :if-does-not-exist :create))

  (shm:with-open-shm (shm +shm-name+ :if-exists :supersede
                          :if-does-not-exist :create
                          :permissions '(:all-user)
                          )
    (of-type (integer 1 *) shm))

  (shm:with-open-shm (shm +shm-name+ :if-exists :truncate)
    (of-type (integer 1 *) shm))

  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists :error :if-does-not-exist :create
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm))
  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists nil :if-does-not-exist :create
                          :permissions '(:all-user))
    (of-type (integer 1 *) shm))

  (shm:shm-unlink +shm-name+)
  (fail (shm:shm-open +shm-name+ :if-does-not-exist :error))
  (shm:with-open-shm (shm +shm-name+ :if-does-not-exist nil)
    (is eq nil shm)))

(define-test shm-open.can-use-various-permissions
  :parent shm-open
  (ignore-errors (shm:shm-unlink +shm-name+))
  (shm:with-open-shm (shm +shm-name+ :if-exists :error :if-does-not-exist :create
                          :permissions '(:exec-user :write-user :read-user
                                         :exec-group :write-group :read-group
                                         :exec-other :write-other :read-other))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists :error :if-does-not-exist :create
                          :permissions '(:xusr :wusr :rusr
                                         :xgrp :wgrp :rgrp
                                         :xoth :woth :roth))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists :error :if-does-not-exist :create
                          :permissions '(:all-user :all-group :all-other))
    (of-type (integer 1 *) shm))


  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists :error :if-does-not-exist :create
                          :permissions '(:rwxu :rwxg :rwxo))
    (of-type (integer 1 *) shm))

  ;; Cleanup
  (shm:shm-unlink +shm-name+))

(define-test cant-open-names-with-multiple-slashes
  :parent shm-open
  (fail (shm:shm-open "/xyz.shunter.posix-shm.test/bad/name///"
                      :if-does-not-exist :create)))

(define-test shm-ftruncate
  :depends-on (shm-open)
  (shm:with-open-shm (shm +shm-name+ :direction :io :if-does-not-exist :create
                          :permissions '(:all-user))
    (finish (shm:shm-ftruncate shm 100)))

  (shm:with-open-shm (shm +shm-name+)
    (fail (shm:shm-ftruncate shm 100)))

  (shm:with-open-shm (shm +shm-name+ :direction :input)
    (fail (shm:shm-ftruncate shm 100)))

  (shm:with-open-shm (shm +shm-name+ :direction :io)
    (fail (shm:shm-ftruncate shm -1)))

  ;; Cleanup
  (shm:shm-unlink +shm-name+))

(define-test mmap
  :depends-on (shm-open shm-ftruncate)

  (shm:with-open-shm (shm +shm-name+ :direction :io :if-does-not-exist :create
                          :permissions '(:all-user))
    (shm:shm-ftruncate shm 100)
    (let ((ptr (shm:mmap (cffi:null-pointer) 100 '(:read) shm 0)))
      (true (cffi:pointerp ptr))
      (of-type integer
               (cffi:mem-aref ptr :int))
      (shm:munmap ptr 100)))

  (shm:with-open-shm (shm +shm-name+ :direction :io)
    (shm:shm-ftruncate shm 100)
    (shm:with-mmap (ptr (cffi:null-pointer) 100 '(:read) shm 0)
      (true (cffi:pointerp ptr))
      (of-type integer
               (cffi:mem-aref ptr :int))))

  (shm:with-mmapped-shm (shm ptr (+shm-name+ :direction :io)
                             ((cffi:null-pointer) 100 '(:read) 0))
    (of-type (integer 1 *) shm)
    (true (cffi:pointerp ptr))
    (of-type integer
             (cffi:mem-aref ptr :int)))

  ;; mmapping a read-only shm with read-write protections
  (shm:with-open-shm (shm +shm-name+)
    (fail (shm:mmap (cffi:null-pointer) 10 '(:read :write) shm 0)))

  ;; writing to and reading from a read-write shm object
  (shm:with-mmapped-shm (shm ptr (+shm-name+ :direction :io)
                             ((cffi:null-pointer)
                              (* +int-size+ 5) '(:read :write) 0))
    (loop :for i :upto 5 :do (setf (cffi:mem-aref ptr :int i) (* 10 i)))
    (loop :for i :upto 5
          :do (is = (* i 10)
                  (cffi:mem-aref ptr :int i))))

  ;; Memory fault on reading with no read protection
  (shm:with-mmapped-shm (shm ptr (+shm-name+ :direction :io)
                             ((cffi:null-pointer) +int-size+ nil 0))
    (fail (cffi:mem-aref ptr :int)))

  ;; Memory fault on writing with no write protection
  (shm:with-mmapped-shm (shm ptr (+shm-name+ :direction :io)
                             ((cffi:null-pointer) +int-size+ nil 0))
    (fail (cffi:mem-aref ptr :int)))

  ;; Two ptr's mmapped from the same shm object should share values
  (shm:with-open-shm (shm +shm-name+ :direction :io)
    (shm:shm-ftruncate shm +int-size+)
    (shm:with-mmap (write-ptr (cffi:null-pointer)
                              +int-size+
                              '(:write) shm 0)
      (shm:with-mmap (read-ptr (cffi:null-pointer)
                               +int-size+
                               '(:read) shm 0)
        (loop :repeat 3
              :with the-int := (random 1000)
              :do (setf (cffi:mem-ref write-ptr :int) the-int)
                  (is = the-int (cffi:mem-ref read-ptr :int))))))

  ;; Two mmaps from two shm objects from the same path should share values
  (shm:shm-unlink +shm-name+)
  (shm:with-mmapped-shm (shm1 write-ptr
                              (+shm-name+ :direction :io
                                          :if-exists :error
                                          :if-does-not-exist :create
                                          :permissions '(:all-user))
                              ((cffi:null-pointer)
                               +int-size+
                               '(:write) 0))
    (posix-shm:with-mmapped-shm (shm2 read-ptr
                                      (+shm-name+)
                                      ((cffi:null-pointer)
                                       +int-size+
                                       '(:read) 0)
                                      :truncate nil)
      (loop :repeat 3
            :with the-int := (random 1000)
            :do (setf (cffi:mem-ref write-ptr :int) the-int)
                (is = the-int (cffi:mem-ref read-ptr :int)))))

  ;; Cleanup
  (shm:shm-unlink +shm-name+))

(define-test fstat
  :depends-on (shm-open))

(define-test fchown
  :depends-on (shm-open))

(define-test fchmod
  :depends-on (shm-open))
