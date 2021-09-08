;;; shm.lisp - POSIX shared memory

(defpackage #:xyz.shunter.posix-shm
  (:nicknames #:posix-shm)
  (:local-nicknames (#:ffi #:xyz.shunter.posix-shm.ffi)
                    (#:a #:alexandria))
  (:use #:cl)
  (:export #:shm-open
           #:shm-open*
           #:shm-ftruncate
           #:mmap
           #:munmap
           #:shm-unlink
           #:shm-close
           #:fstat
           #:fchown
           #:fchmod
           #:with-open-shm
           #:with-open-shm*
           #:with-mmap
           #:with-mmapped-shm
           #:with-mmapped-shm*))

(in-package #:xyz.shunter.posix-shm)



(define-condition shm-error (error)
  ((errno :initarg :errno))
  (:report (lambda (c s)
             (princ (ffi:strerror (slot-value c 'errno)) s))))

(defun raise-shm-error ()
  (error 'shm-error :errno ffi:*errno*))

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
          :write ffi:+prot-write+
          :read ffi:+prot-read+)))

(defparameter +map-failed+
  (1- (ash 1 (* 8 (cffi:foreign-type-size :pointer)))))

(defun flag (keyword hash-table)
  (or (gethash keyword hash-table)
      (error "Cannot find flag for keyword ~S" keyword)))

(defun to-flags (keywords hash-table)
  (reduce #'logior (mapcar (a:rcurry #'flag hash-table) keywords)))

(defun %shm-open (name oflag mode if-exists if-does-not-exist)
  (let ((fd (ffi:shm-open name oflag mode))
        (errno ffi:*errno*))
    (cond
      ((>= fd 0) fd)
      ;; shm_open failed because the object already exists.
      ((= errno ffi:+EEXIST+)
       (ecase if-exists
         (:error (raise-shm-error))
         ((nil) nil)
         (:supersede
           (shm-unlink name)
           (%shm-open name oflag mode :error if-does-not-exist))))
      ;; shm_open failed because the object doesn't exist.
      ((= errno ffi:+ENOENT+)
       (ecase if-does-not-exist
         (:error (raise-shm-error))
         ((nil) nil)))
      (t (raise-shm-error)))))

(defun open-options-to-oflag (direction if-exists if-does-not-exist)
  (logior
    (ecase direction
      (:input ffi:+O-RDONLY+)
      (:io ffi:+O-RDWR+))
    (ecase if-exists
      (:overwrite 0)
      (:truncate ffi:+O-TRUNC+)
      ((:error :supersede nil) ffi:+O-EXCL+))
    (ecase if-does-not-exist
      (:create ffi:+O-CREAT+)
      ((:error nil) 0))))

(defun default-dne-option (direction if-exists)
  (if (or (eq direction :input)
          (member if-exists '(:overwrite :truncate)))
      :error
      :create))

(defun shm-open (name &key (direction :input) (if-exists :overwrite)
                      (if-does-not-exist (default-dne-option direction if-exists))
                      permissions)
  (let ((mode (to-flags permissions +permissions+))
        (oflag (open-options-to-oflag direction if-exists if-does-not-exist)))
    (%shm-open name oflag mode if-exists if-does-not-exist)))

(defun random-name ()
  (loop :repeat 10
        :collect (code-char (+ #x41 (random 26)
                               (* #x20 (random 2))))
          :into suffix
        :finally (return (concatenate 'string "/xyz.shunter.shm-" suffix))))

(defun %shm-open* (oflag mode attempts)
  (setf oflag (logior oflag ffi:+O-CREAT+ ffi:+O-EXCL+))
  (loop :repeat attempts
        :for name := (random-name)
        :for fd := (%shm-open name oflag mode :error :create)
        :when fd
          :do (shm-unlink name)
              (return fd)

        :finally ;; out of attempts
          (raise-shm-error)))

(defun shm-open* (&key (direction :input) permissions (attempts 100))
  (%shm-open* (ecase direction
                (:input ffi:+O-RDONLY+)
                (:io ffi:+O-RDWR+))
              (to-flags permissions +permissions+) attempts))

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
  (let ((prot (if protections
                  (to-flags protections +protections+)
                  ffi:+prot-none+)))
    (%mmap ptr length prot fd offset)))

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
  (error "TODO: Unimplemented"))

;; XXX: cffi can't find fchown().
(defun fchown (fd owner-id group-id)
  (declare (ignore fd owner-id group-id))
  (error "TODO: Unimplemented"))

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
       (when ,var
         (shm-close ,var)))))

(defmacro with-open-shm* ((var &rest options) &body body)
  `(let ((,var (shm-open* ,@options)))
     (unwind-protect
       (progn ,@body)
       (shm-close ,var))))

(defmacro with-mmap ((var ptr length protections fd offset) &body body)
  (a:once-only (length)
    `(let ((,var (mmap ,ptr ,length ,protections ,fd ,offset)))
       (unwind-protect
         (progn ,@body)
         (munmap ,var ,length)))))

(defmacro with-mmapped-shm ((fd mmap shm-options (ptr length protections offset)
                                &key (truncate t))
                            &body body)
  (a:once-only (length offset)
    `(with-open-shm (,fd ,@shm-options)
       ,(when truncate `(shm-ftruncate ,fd (+ ,length ,offset)))
       (with-mmap (,mmap ,ptr ,length ,protections ,fd ,offset)
         ,@body))))

(defmacro with-mmapped-shm* ((fd mmap shm-options (ptr length protections offset)
                                 &key (truncate t))
                             &body body)
  (a:once-only (length offset)
    `(with-open-shm* (,fd ,@shm-options)
       ,(when truncate `(shm-ftruncate ,fd (+ ,length ,offset)))
       (with-mmap (,mmap ,ptr ,length ,protections ,fd ,offset)
         ,@body))))
