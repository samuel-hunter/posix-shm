;;; shm.lisp - POSIX shared memory

(defpackage #:xyz.shunter.shm
  (:nicknames #:shm)
  (:local-nicknames (#:ffi #:xyz.shunter.shm.ffi)
                    (#:a #:alexandria))
  (:use #:cl)
  (:export #:shm-buffer
           #:shm-buffer-p
           #:shm-buffer-type
           #:shm-buffer-fd
           #:shm-buffer-ptr
           #:shm-aref
           #:open-shm-buffer
           #:close-shm-buffer
           #:delete-shm
           #:with-shm-buffer))

(in-package #:xyz.shunter.shm)



(defstruct (shm-buffer (:copier nil))
  type length fd ptr)

(define-condition posix-error (error)
  ((while :initarg :while)
   (message :initarg :message))
  (:report (lambda (c s)
             (format s "Error while ~A: ~A"
                     (slot-value c 'while)
                     (slot-value c 'message)))))

(defparameter +open-flags+
  (a:plist-hash-table
    (list :rdonly ffi:+o-rdonly+
          :rdwr ffi:+o-rdwr+
          :creat ffi:+o-creat+
          :excl ffi:+o-excl+

          :read-only ffi:+o-rdonly+
          :read-write ffi:+o-rdwr+
          :create ffi:+o-creat+
          :exclusive ffi:+o-excl+)))

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

(defparameter +mmap-types+
  (a:plist-hash-table
    (list :shared ffi:+map-shared+
          :private ffi:+map-private+)))

(defparameter +map-failed+
  (1- (ash 1 (* 4 (cffi:foreign-type-size :pointer)))))

(defun to-flags (keywords hash-table)
  (reduce #'logior (mapcar (a:rcurry #'gethash hash-table) keywords)))

(defun delete-shm (name)
  (when (minusp (ffi:shm-unlink name))
    (error 'posix-error :while "unlinking shm"
        :message (ffi:strerror ffi:*errno*)))

  (values))

(defun shm-open (name oflag mode)
  (let ((fd (ffi:shm-open name oflag mode)))
    (if (minusp fd)
        (error 'posix-error
               :while "opening shm"
               :message (ffi:strerror ffi:*errno*))
        fd)))

(defun random-name ()
  (loop :repeat 10
        :collect (code-char (+ #x41 (random 26)
                               (* #x20 (random 2))))
          :into suffix
        :finally (return (concatenate 'string "/xyz.shunter.shm-" suffix))))

(defun shm-open* (oflag mode attempts)
  (assert (= (logior ffi:+o-creat+ ffi:+o-excl+)
             (logand oflag (logior ffi:+o-creat+ ffi:+o-excl+))))
  (loop :repeat attempts
        :for name := (random-name)
        :for fd := (ffi:shm-open name oflag mode)
        :when (plusp fd)
          :do (delete-shm name)
              (return fd)
        :unless (= ffi:*errno* ffi:+eexist+)
          :do (error 'posix-error
                     :while "opening shm"
                     :message (ffi:strerror ffi:*errno*))))

(defun close-fd (fd)
  (when (minusp (ffi:close fd))
    (error 'posix-error
           :while "closing shm"
           :message (ffi:strerror ffi:*errno*)))
  (values))

(defun allocate-shm (fd size)
  (loop :while (minusp (ffi:ftruncate fd size))
        :unless (= ffi:*errno* ffi:+eintr+)
          :do (ffi:close fd)
              (error 'posix-error
                     :while "sizing buffer"
                     :message (ffi:strerror ffi:*errno*)))
  fd)

(defun make-shm-buffer* (fd type count prot flags)
  (let* ((size (* (cffi:foreign-type-size type)
                  count))
         (ptr (ffi:mmap (cffi:null-pointer)
                       size prot flags (allocate-shm fd size) 0)))
    (when (= (cffi:pointer-address ptr) +map-failed+)
      (ffi:close fd)
      (error 'posix-error
             :while "memory-mapping shm"
             :message (ffi:strerror ffi:*errno*)))
    (make-shm-buffer :type type :length count :fd fd :ptr ptr)))

(defun open-shm-buffer (name type count &key open-flags permissions protections mmap-type)
  (let ((oflag (to-flags open-flags +open-flags+))
        (mode (to-flags permissions +permissions+))
        (prot (to-flags protections +protections+))
        (flags (gethash mmap-type +mmap-types+)))
    (make-shm-buffer*
      (shm-open name oflag mode)
      type count prot flags)))

(defun open-shm-buffer* (type count &key (attempts 100) open-flags permissions protections mmap-type)
  (let ((oflag (to-flags open-flags +open-flags+))
        (mode (to-flags permissions +permissions+))
        (prot (to-flags protections +protections+))
        (flags (gethash mmap-type +mmap-types+)))
    (make-shm-buffer*
      (shm-open* oflag mode attempts)
      type count prot flags)))

(defun close-shm-buffer (shm-buffer)
  (when (minusp (ffi:munmap (shm-buffer-ptr shm-buffer)
                            (* (cffi:foreign-type-size
                                 (shm-buffer-type shm-buffer))
                               (shm-buffer-length shm-buffer))))
    (error 'posix-error :while "unmapping shm"
           :message (ffi:strerror ffi:*errno*)))

  (close-fd (shm-buffer-fd shm-buffer))
  (setf (shm-buffer-fd shm-buffer) -1
        (shm-buffer-ptr shm-buffer) (cffi:null-pointer)
        (shm-buffer-length shm-buffer) 0)

  (values))

(defmacro with-shm-buffer ((var &rest options) &body body)
  `(let ((,var (open-shm-buffer ,@options)))
     (unwind-protect (progn ,@body)
       (close-shm-buffer ,var))))

(defmacro with-shm-buffer* ((var &rest options) &body body)
  `(let ((,var (open-shm-buffer* ,@options)))
     (unwind-protect (progn ,@body)
       (close-shm-buffer ,var))))



(defun shm-aref (shm-buffer index)
  (assert (< index (shm-buffer-length shm-buffer))
          (index)
          "Invalid index ~D for ~S, should be a non-negative integer below ~D."
          index 'shm-buffer (shm-buffer-length shm-buffer))
  (cffi:mem-aref (shm-buffer-ptr shm-buffer)
                 (shm-buffer-type shm-buffer)
                 index))

(defun (setf shm-aref) (new-value shm-buffer index)
  (assert (< index (shm-buffer-length shm-buffer))
          (index)
          "Invalid index ~D for ~S, should be a non-negative integer below ~D."
          index 'shm-buffer (shm-buffer-length shm-buffer))
  (setf (cffi:mem-aref (shm-buffer-ptr shm-buffer)
                       (shm-buffer-type shm-buffer)
                       index)
        new-value))
