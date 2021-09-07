;;; shm.lisp - POSIX shared memory

(defpackage #:xyz.shunter.posix-shm
  (:nicknames #:posix-shm)
  (:local-nicknames (#:ffi #:xyz.shunter.posix-shm.ffi)
                    (#:a #:alexandria))
  (:use #:cl)
  (:export #:shm-open
           #:shm-ftruncate
           #:mmap
           #:munmap
           #:shm-unlink
           #:shm-close
           #:fstat
           #:fchown
           #:fchmod
           #:with-open-shm
           #:with-mmap
           #:with-mmapped-shm))

(in-package #:xyz.shunter.posix-shm)



(define-condition shm-error (error)
  ((errno :initarg :errno))
  (:report (lambda (c s)
             (princ (ffi:strerror (slot-value c 'errno)) s))))

(defun raise-shm-error ()
  (error 'shm-error :errno ffi:*errno*))

(defparameter +open-flags+
  (a:plist-hash-table
    (list :rdonly ffi:+o-rdonly+
          :rdwr ffi:+o-rdwr+
          :creat ffi:+o-creat+
          :excl ffi:+o-excl+
          :trunc ffi:+o-trunc+

          :read-only ffi:+o-rdonly+
          :read-write ffi:+o-rdwr+
          :create ffi:+o-creat+
          :exclusive ffi:+o-excl+
          :truncate ffi:+o-trunc+)))

(defparameter +permissions+
  (a:plist-hash-table
    (list :xoth ffi:+s-ixoth+
          :woth ffi:+s-iwoth+
          :roth ffi:+s-iroth+
          :rwxo ffi:+s-irwxo+

          :exec-other ffi:+s-ixoth+
          :write-other ffi:+s-iwoth+
          :read-other ffi:+s-iroth+
          :all-other ffi:+s-irwxo+

          :xgrp ffi:+s-ixgrp+
          :wgrp ffi:+s-iwgrp+
          :rgrp ffi:+s-irgrp+
          :rwxg ffi:+s-irwxg+

          :exec-group ffi:+s-ixgrp+
          :write-group ffi:+s-iwgrp+
          :read-group ffi:+s-irgrp+
          :all-group ffi:+s-irwxg+

          :xusr ffi:+s-ixusr+
          :wusr ffi:+s-iwusr+
          :rusr ffi:+s-irusr+
          :rwxu ffi:+s-irwxu+

          :exec-user ffi:+s-ixusr+
          :write-user ffi:+s-iwusr+
          :read-user ffi:+s-irusr+
          :all-user ffi:+s-irwxu+)))

(defparameter +protections+
  (a:plist-hash-table
    (list :exec ffi:+prot-exec+
          :read ffi:+prot-read+
          :write ffi:+prot-write+
          :none ffi:+prot-none+)))

(defparameter +map-failed+
  (1- (ash 1 (* 4 (cffi:foreign-type-size :pointer)))))

(defun flag (keyword hash-table)
  (or (gethash keyword hash-table)
      (error "Cannot find flag for keyword ~S" keyword)))

(defun to-flags (keywords hash-table)
  (reduce #'logior (mapcar (a:rcurry #'flag hash-table) keywords)))

(defun compile-flags-if-possible (form flag-table environment)
  (if (constantp form environment)
      (to-flags (eval form) flag-table)
      form))

(defun %shm-open (name oflag mode)
  (let ((fd (ffi:shm-open name oflag mode)))
    (if (minusp fd)
        (raise-shm-error)
        fd)))

(defun shm-open (name &key open-flags permissions)
  (let ((oflag (to-flags open-flags +open-flags+))
        (mode (to-flags permissions +permissions+)))
    (%shm-open name oflag mode)))

(defun random-name ()
  (loop :repeat 10
        :collect (code-char (+ #x41 (random 26)
                               (* #x20 (random 2))))
          :into suffix
        :finally (return (concatenate 'string "/xyz.shunter.shm-" suffix))))

(defun %shm-open* (oflag mode attempts)
  (assert (= (logior ffi:+o-creat+ ffi:+o-excl+)
             (logand oflag (logior ffi:+o-creat+ ffi:+o-excl+))))
  (loop :repeat attempts
        :for name := (random-name)
        :for fd := (ffi:shm-open name oflag mode)
        :unless (minusp fd)
          :do (shm-unlink name)
              (return fd)
        :unless (= ffi:*errno* ffi:+eexist+)
          :do (raise-shm-error)
        :finally ;; out of attempts
          (raise-shm-error)))

(defun shm-open* (&key open-flags permissions (attempts 100))
  (let ((oflag (to-flags open-flags +open-flags+))
        (mode (to-flags permissions +permissions+)))
    (%shm-open* oflag mode attempts)))

(defun shm-ftruncate (fd size)
  (loop :while (minusp (ffi:ftruncate fd size))
        :unless (= ffi:*errno* ffi:+eintr+)
          :do (raise-shm-error))
  (values))

(defun %mmap (ptr length prot fd offset)
  (let ((ptr (ffi:mmap ptr length prot ffi:+map-shared+ fd offset)))
    (if (= +map-failed+ (cffi:pointer-address ptr))
        (raise-shm-error)
        ptr)))

(defun mmap (ptr length protections fd offset)
  (let ((prot (to-flags protections +protections+)))
    (mmap ptr length prot fd offset)))

(defun munmap (ptr length)
  (when (minusp (ffi:munmap ptr length))
    (raise-shm-error))

  (values))

(defun shm-unlink (name)
  (when (minusp (ffi:shm-unlink name))
    (raise-shm-error))

  (values))

(defun shm-close (fd)
  (when (minusp (ffi:close fd))
    (raise-shm-error))

  (values))

(defun fstat (fd)
  (declare (ignore fd))
  (error "TODO"))

;; XXX: cffi can't find fchown().
;(defun fchown (fd owner-id group-id)
;  (when (minusp (ffi:fchown fd owner-id group-id))
;    (raise-shm-error))
;
;  (values))

(defun %fchmod (fd mode)
  (when (minusp (ffi:fchmod fd mode))
    (raise-shm-error))

  (values))

(defun fchmod (fd permissions)
  (let ((mode (to-flags permissions +permissions+)))
    (%fchmod fd mode)))

(defmacro with-open-shm ((var &rest options) &body body)
  `(let ((,var (shm-open ,@options)))
     (unwind-protect
       (progn ,@body)
       (shm-close ,var))))

(defmacro with-mmap ((var length protections fd offset) &body body)
  (a:once-only (length)
    `(let ((,var (mmap ,length ,protections ,fd ,offset)))
       (unwind-protect
         (progn ,@body))
       (munmap ,var ,length))))

(defmacro with-mmapped-shm ((fd mmap shm-options (length protections offset)) &body body)
  (a:once-only (length)
    `(with-open-shm (,fd ,@shm-options)
       (shm-ftruncate ,fd ,length)
       (with-mmap (,mmap ,length ,protections ,fd ,offset)
         ,@body))))
