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
                           :permissions '(:user-all))))
    (of-type (integer 1 *) shm)
    (shm:shm-close shm))

  (let ((shm (shm:shm-open*)))
    (of-type (integer 1 *) shm)
    (shm:shm-close shm))

  (fail (shm:shm-open* :attempts 0)
        'shm:shm-error)

  (shm:with-open-shm (shm +shm-name+)
    (of-type (integer 1 *) shm))

  (shm:with-open-shm* (shm)
    (of-type (integer 1 *) shm))

  (fail (shm:shm-close -1)
        'shm:shm-error)

  (fail (shm:shm-open +shm-name+ :permissions '(:flag-that-doesnt-exist)))

  ;; Cleanup
  (shm:shm-unlink +shm-name+))

(define-test shm-open.can-use-various-options
  :parent shm-open
  (shm:with-open-shm (shm +shm-name+ :if-exists nil
                          :if-does-not-exist :create
                          :permissions '(:user-all))
    (of-type (integer 1 *) shm))

  (fail (shm:shm-open +shm-name+ :if-exists :error
                      :if-does-not-exist :create)
        'shm:shm-error)

  (shm:with-open-shm (shm +shm-name+ :if-exists :supersede
                          :if-does-not-exist :create
                          :permissions '(:user-all)
                          )
    (of-type (integer 1 *) shm))

  (shm:with-open-shm (shm +shm-name+ :if-exists :truncate)
    (of-type (integer 1 *) shm))

  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists :error :if-does-not-exist :create
                          :permissions '(:user-all))
    (of-type (integer 1 *) shm))
  (shm:shm-unlink +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists nil :if-does-not-exist :create
                          :permissions '(:user-all))
    (of-type (integer 1 *) shm))

  (shm:shm-unlink +shm-name+)
  (fail (shm:shm-open +shm-name+ :if-does-not-exist :error)
        'shm:shm-error)
  (shm:with-open-shm (shm +shm-name+ :if-does-not-exist nil)
    (is eq nil shm)))

(define-test shm-open.can-use-various-permissions
  :parent shm-open
  (shm:with-open-shm* (shm :permissions '(:user-exec :user-write :user-read
                                          :group-exec :group-write :group-read
                                          :other-exec :other-write :other-read))
    (of-type (integer 1 *) shm))


  (shm:with-open-shm* (shm :permissions '(:xusr :wusr :rusr
                                          :xgrp :wgrp :rgrp
                                          :xoth :woth :roth))
    (of-type (integer 1 *) shm))


  (shm:with-open-shm* (shm :permissions '(:user-all :group-all :other-all))
    (of-type (integer 1 *) shm))


  (shm:with-open-shm* (shm :permissions '(:rwxu :rwxg :rwxo))
    (of-type (integer 1 *) shm)))

(define-test cant-open-names-with-multiple-slashes
  :parent shm-open
  (fail (shm:shm-open "/xyz.shunter.posix-shm.test/bad/name///"
                      :if-does-not-exist :create)
        'shm:shm-error))

(define-test shm-ftruncate
  :depends-on (shm-open)
  (shm:with-open-shm (shm +shm-name+ :direction :io :if-does-not-exist :create
                          :permissions '(:user-all))
    (finish (shm:shm-ftruncate shm 100)))

  (shm:with-open-shm (shm +shm-name+)
    (fail (shm:shm-ftruncate shm 100)
          'shm:shm-error))

  (shm:with-open-shm (shm +shm-name+ :direction :input)
    (fail (shm:shm-ftruncate shm 100)
          'shm:shm-error))

  (shm:with-open-shm (shm +shm-name+ :direction :io)
    (fail (shm:shm-ftruncate shm -1)
          'shm:shm-error))

  ;; Cleanup
  (shm:shm-unlink +shm-name+))

(define-test mmap
  :depends-on (shm-open shm-ftruncate)

  (shm:with-open-shm* (shm :direction :io)
    (shm:shm-ftruncate shm 100)
    (let ((ptr (shm:mmap (cffi:null-pointer) 100 '(:read) shm 0)))
      (true (cffi:pointerp ptr))
      (of-type integer
               (cffi:mem-aref ptr :int))
      (shm:munmap ptr 100)))

  (shm:with-open-shm* (shm :direction :io)
    (shm:shm-ftruncate shm 100)
    (shm:with-mmap (ptr (cffi:null-pointer) 100 '(:read) shm 0)
      (true (cffi:pointerp ptr))
      (of-type integer
               (cffi:mem-aref ptr :int))))

  (shm:with-mmapped-shm (shm ptr (+shm-name+ :direction :io :if-does-not-exist :create
                                             :permissions '(:user-all))
                             ((cffi:null-pointer) 100 '(:read) 0))
    (of-type (integer 1 *) shm)
    (true (cffi:pointerp ptr))
    (of-type integer
             (cffi:mem-aref ptr :int)))
  (shm:shm-unlink +shm-name+)

  (shm:with-mmapped-shm* (shm ptr (:direction :io)
                              ((cffi:null-pointer) 100 '(:read) 0))
    (of-type (integer 1 *) shm)
    (true (cffi:pointerp ptr))
    (of-type integer
             (cffi:mem-aref ptr :int)))

  ;; mmapping a read-only shm with read-write protections
  (shm:with-open-shm* (shm)
    (fail (shm:mmap (cffi:null-pointer) 10 '(:read :write) shm 0)
          'shm:shm-error))

  ;; writing to and reading from a read-write shm object
  (shm:with-mmapped-shm* (shm ptr (:direction :io)
                              ((cffi:null-pointer)
                               (* +int-size+ 5) '(:read :write) 0))
    (loop :for i :upto 5 :do (setf (cffi:mem-aref ptr :int i) (* 10 i)))
    (loop :for i :upto 5
          :do (is = (* i 10)
                  (cffi:mem-aref ptr :int i))))

  ;; Memory fault on reading with no read protection
  (shm:with-mmapped-shm* (shm ptr (:direction :io)
                              ((cffi:null-pointer) +int-size+ nil 0))
    (fail (cffi:mem-aref ptr :int)))

  ;; Memory fault on writing with no write protection
  (shm:with-mmapped-shm* (shm ptr (:direction :io)
                              ((cffi:null-pointer) +int-size+ nil 0))
    (fail (cffi:mem-aref ptr :int)))

  ;; Two ptr's mmapped from the same shm object should share values
  (shm:with-open-shm* (shm :direction :io)
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
  (ignore-errors (shm:shm-unlink +shm-name+))
  (shm:with-mmapped-shm (shm1 write-ptr
                              (+shm-name+ :direction :io
                                          :if-exists :error
                                          :if-does-not-exist :create
                                          :permissions '(:user-all))
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

  (fail (shm:munmap (cffi:null-pointer) 0)
        'shm:shm-error)

  ;; Cleanup
  (shm:shm-unlink +shm-name+))

(define-test fstat
  :depends-on (shm-open))

(define-test fchown
  :depends-on (shm-open))

(define-test fchmod
  :depends-on (shm-open)
  (shm:with-open-shm* (shm)
    (finish (shm:fchmod shm '(:user-all))))

  (fail (shm:fchmod -1 ())))
