# posix-shm
[![builds.sr.ht status](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml.svg)](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml)
[![Quicklisp status](https://quickdocs.org/badge/posix-shm.svg)](https://quickdocs.org/posix-shm)

Common Lisp bindings to the POSIX shared memory API.

The POSIX shared memory (or `shm`) API "allows processes to communicate information by sharing a region of memory." (`shm_overview(7)`).
This library provides a wrapper over the POSIX shm API, all POSIX commands that interact with shm file descriptors, and a small handful of utility functions and macros.

Features include:

- Bindings for `shm_open`, `ftruncate`, `mmap`, `munmap`, `shm_unlink`, `close`, and `fchmod`.
- **shm-open** appears more like **open** from the CL standard.
- **shm-open\***, for creating anonymous shm objects.
- **with-open-shm**, **with-mmap** and similar **with-** macros for safely accessing resources with dynamic extent.

## Installation and Usage

Install posix-shm with Quicklisp:

```lisp
(ql:quickload :posix-shm)
(use-package :posix-shm)

(defparameter +shm-size+
  (* 10 (cffi:foreign-type-size :int)))

(defparameter *shm*
  (shm-open "/foobar-shm" :direction :io :if-does-not-exist :create
            :permissions '(:user-read :user-write)))

(shm-ftruncate *shm* +shm-size+)

(defparameter *ptr1*
  (mmap (cffi:null-pointer) +shm-size+ '(:write) *shm* 0))

(defparameter *ptr2*
  (mmap (cffi:null-pointer) +shm-size+ '(:read) *shm* 0))

(dotimes (i 10)
  (setf (cffi:mem-aref *ptr1* :int i) (* i 10)))
(dotimes (i 10)
  (print (cffi:mem-aref *ptr2* :int i)))

(munmap *ptr1* +shm-size+)
(munmap *ptr2* +shm-size+)
(shm-close *shm*)
(shm-unlink "/foobar-shm")
```

Use **with-open-shm** and **with-mmap** to automatically close and munmap when the program leaves scope:

```lisp
(with-open-shm (shm "/foobar-shm" :direction :io)
  (shm-ftruncate shm 100)
  (with-mmap (ptr (cffi:null-pointer) 100 '(:read :write) shm 0)
    ;; do your thing...
    ))
```

**with-mmapped-shm** opens a shm, truncates it, and then maps it to a pointer all in one:

```lisp
(with-mmapped-shm (shm ptr ("/foobar-shm" :direction :io)
	               ((cffi:null-pointer) 100 '(:read :write) 0))
  ;; do your thing...
  )
```

Use **shm-open\*** to create anonymous shm objects:

```lisp
(defvar *anon-shm* (shm-open* :direction :io :permissions '(:user-all))

;; do your thing...

(shm-close *anon-shm*)
```

## Contributing

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/posix-shm/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/posix-shm).

## API

### [Enum] **permission**

Used for setting the permissions of the shm on `shm-open` and `fchmod`, and correspond to whether a user, group, or all users can read, write, or execute the shm (or perform any three):

- `:rusr`, `:user-read` - `O_RUSR`.
- `:wusr`, `:user-write` - `O_WUSR`.
- `:xusr`, `:user-exec` - `O_XUSR`.
- `:rwxu`, `:user-all` - `O_RWXU`.
- `:rgrp`, `:group-read` - `O_RGRP`.
- `:wgrp`, `:group-write` - `O_WGRP`.
- `:xgrp`, `:group-exec` - `O_XGRP`.
- `:rwxg`, `:group-all` - `O_RWXG`.
- `:roth`, `:other-read` - `O_ROTH`.
- `:woth`, `:other-write` - `O_WOTH`.
- `:xoth`, `:other-exec` - `O_XOTH`.
- `:rwxo`, `:other-all` - `O_RWXO`.

### [Condition] **shm-error** *(error)*

Raised whenever any POSIX API function fails.
The errno is accessed by the reader function **shm-error-errno**, and is reported as the errno's `strerror(3)` value.

### [Function] **shm-open** *name &key (direction :input) if-exists if-does-not-exist permissions* => *fd*

Creates and opens a new, or opens an existing, POSIX shared memory object.
It can then be used by separate processes to **mmap** the same region of shared memory.

*name* -- a string designator.

*direction* -- one of `:input` or `:io`. The default is `:input`.

*if-exists* -- one of `:error`, `:overwrite`, `:truncate`, `:supersede`, or `nil`.
The default is `:overwrite`.

*if-does-not-exist* -- one of `:error`, `:create`, or `nil`.
The default is `:error` if *direction* is `:input` or *if-exists* is `:overwrite` or `:truncate`, or `:create` otherwise.

*permissions* -- a list of **permission** keywords.

If *direction* is `:input`, it opens the shm object for read access (in C, this would be the `O_RDONLY` flag).
If it is `:io`, it it opened for read-write access (`O_RDWR`).

*if-exists* specifies the action to be taken if a shm object of the name *name* already exists.
If *direction* is `:input`, it is ignored:

- `:error` - An error of type **shm-error** is signaled.
  If *if-does-not-exist* is not `:create`, then this is ignored.
- `:overwrite` - Output operations on the shm object destructively modify the shared memory.
- `:truncate` - Like `:overwrite`, but the shm object is first truncated to 0 bytes first (in C, this would be `O_TRUNC`).
- `:supersede` - The existing shm object is superseded; that is, the old shm object is unlinked, and then a new shm object is created.
  If *if-does-not-exist* is not `:create`, then this is ignored.
- `nil` - No shm object is created; instead, `nil` is returned to indicate failure.
  If *if-does-not-exist* is not `:create`, then this is ignored.

*if-does-not-exist* specifies the action to be taken if a shm object of the name *name* does not exist.

- `:error` - An error of type **shm-error** is signaled.
- `:create` - An empty shm object is created (in C, this would be `O_CREAT`).
- `nil` - No shm object is created; instead, `nil` is returned to indicate failure.

If you're translating from C, each combination of `oflag` would map to:

- `O_RDONLY` - `` (no keyword arguments)
- `O_RDONLY | O_CREAT` - `:if-does-not-exist :create`
- `O_RDONLY | O_CREAT | O_EXCL` - `:if-exists :error :if-does-not-exist :create`
- `O_RDONLY | O_TRUNC` - `:if-exists :truncate`
- `O_RDONLY | O_CREAT | O_TRUNC` - `:if-exists :truncate :if-does-not-exist :create`
- `O_RDONLY | O_CREAT | O_EXCL | O_TRUNC` - `:if-exists :error :if-does-not-exist :create`
- `O_RDWR` - `:direction :io`
- `O_RDWR | O_CREAT` - `:direction :io :if-does-not-exist :create`
- `O_RDWR | O_CREAT | O_EXCL` - `:direction :io :if-exists :error`
- `O_RDWR | O_TRUNC` - `:direction :io :if-exists :truncate`
- `O_RDWR | O_CREAT | O_TRUNC` - `:direction :io :if-exists :truncate :if-does-not-exist :create`
- `O_RDWR | O_CREAT | O_EXCL | O_TRUNC` - `:direction :io :if-exists :error`

**shm-open** signals a **shm-error** on failure.

### [Function] **shm-open\*** *&key (direction :input) permissions (attempts 100)* => *fd*

Creates and opens, and then unlinks, a new POSIX shared memory object.
This shm object may be shared with another UNIX process by sending its
file descriptor over a UNIX domain socket via `SCM_RIGHTS`.

*direction* -- one of `:input` or `:io`. The default is `:input`.

*permissions* -- a list of **permission** keywords.

*attempts* -- the number of times **shm-open\*** tries to open a shm object before giving up.

**shm-open\*** signals a **shm-error** on failure.

### [Function] **shm-ftruncate** *fd length*

Cause the shm object specified by *FD* to be truncated to a size of exactly *length* bytes.

**shm-ftruncate** signals a **shm-error** on failure.

### [Function] **mmap** *addr length protections fd offset* => *ptr*

Creates a new mapping in the virtual address space of the calling process.

*addr* -- a CFFI pointer.

*length* -- a positive length of the mapping, up to the length of the shm object.

*protections* -- a list of `:exec`, `:read`, `:write`, or nil.

*fd* -- a valid file descriptor.

*offset* -- a nonnegative integer.

*protections* describes the desired memory protection of the mapping. It is either `nil`, or a list of the following keywords:

- `:exec` - pages may be executed.
- `:read` - pages may be read.
- `:write` - pages may be written.

**mmap** signals a **shm-error** on failure.

Note: Unlike C's `mmap(2)`, **mmap** does not have a *flags* parameter.
The only POSIX-compliant, or otherwise widely-supported, flag available that makes sense in context with shm objects is `MAP_SHARED`.
Therefore, **mmap** removes the *flags* parameter and calls `mmap(2)` with the constant value `MAP_SHARED`.

### [Function] **munmap** *addr size*

Deletes the mappings for the specified address range, and cause further references to addresses within the range to generate invalid memory references.
Closing the shm file descriptor may not automatically un-map the region.

**mmap** signals a **shm-error** on failure.

### [Function] **shm-unlink** *name*

Removes a shm object previously created by **shm-open**.

*name* -- a string designator.

If the shm object does not exist, or the caller does not have permission to unlink the object, a **shm-error** is signaled.

### [Function] **shm-close** *fd*

Closes a file descriptor, so that it no longer refers to any shm object and may be reused.

*fd* -- a valid file descriptor.

If *fd* is a bad file descriptor, an I/O error occurs, or the `close()` call was interrupted by a signal, a **shm--error** is signaled.

### [Function] **fchmod** *fd permissions*

Changes the file mode bits of the open shm object.

*permissions* -- a list of **permission** keywords.

**fchmod** signals a **shm-error** on failure.

### [Function] **fstat** *fd* => *stats*

Returns information about a file as a 10-item list with the following integer fields:

- The ID of device containing the file
- The Inode number
- The file type and mode
- The number of hard links
- The user ID of the owner
- The group ID of the owner
- The device ID of the owner
- The total size, in bytes
- The block size for filesystem I/O
- The number of 512B blocks allocated

**fstat** signals a **shm-error** on failure.

### [Function] **fchown** *fd owner-id group-id*

Changes the ownership of the open shm object to the given owner and
group ID's (mapped to users and groups in their respective `passwd(5)`
and `group(5)` files).

If OWNER-ID or GROUP-ID is specified as NIL, then that ID is not
changed.

### [Macro] **with-open-shm** *(var &rest options) &body body* => *result*

Runs *BODY* under lexical scope of *VAR*, an open shm object.
*VAR* is closed (but not unlinked) when the program exits scope.

### [Macro] **with-open-shm\*** *(var &rest options) &body body* => *result*

Like **with-open-shm**, but uses **shm-open\*** instead of **shm-open** to open a shm object.

### [Macro] **with-mmap** *(var &rest options) &body body* => *result*

Runs *BODY* under lexical scope of *VAR*, a memory-mapped pointer.
*VAR* is unmapped when the program exits scope.

### [Macro] **with-mmapped-shm** *(shm-var mmap-var shm-options (ptr length protections offset) :key (truncate t)) &body body* => *result*

Opens a shm object *SHM*, truncates it to `(+ LENGTH OFFSET)` (unless otherwise configured), and maps it to *MMAP*.
Runs *BODY* under lexical scope of *SHM* and *MMAP*, unmapping and closing both when the program exits scope.

### [Macro] **with-mmapped-shm\*** *(shm-var mmap-var shm-options (ptr length protections offset) :key (truncate t)) &body body* => *result*

Like **with-mmapped-shm**, but uses **shm-open\*** instead of **shm-open** to open a shm object.
