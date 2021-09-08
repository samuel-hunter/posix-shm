# posix-shm
[![builds.sr.ht status](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml.svg)](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml)

Common Lisp bindings to the POSIX shared memory API.

This library is currently in its embryonic stages, and is not yet ready for public consumption.

According to the Linux Programmer's Manual, the POSIX shared memory API (or `shm`) "allows processes to communicate information by sharing a region of memory." -- `shm_overview(7)`.
This library provides a slim wrapper over the POSIX shm interface.

Features include:

- `shm_open`, `shm_unlink`, `mmap`
- A utility function, `shm-open*`, for opening anonymous shm objects that are immediately unlinked.
- `shm-open` appears more like `open` from the CL standard.

Features to provide in the future:

- `fchown`, `fchmod`, `fstat`
- Pre-compiling flags to their foreign integer values when they're `constantp`

## Usage

Posix-shm is not yet available on Quicklisp:

```sh
$ cd ~/common-lisp/ # Or wherever you store your systems
$ git clone https://git.sr.ht/~shunter/posix-shm
```

```lisp
(ql:quickload :posix-shm)
(use-package :posix-shm)

(defparameter +shm-size+
  (* 10 (cffi:foreign-type-size :int)))

(defparameter *shm*
  (shm-open "/foobar-shm" :direction :io :if-does-not-exist :create
            :permissions '(:read-user :write-user)))

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

Use `with-open-shm` and `with-mmap` to automatically close and unmap when the program leaves scope:

```lisp
(with-open-shm (shm "/foobar-shm" :direction :io)
  (shm-ftruncate shm 100)
  (with-mmap (ptr (cffi:null-pointer) 100 '(:read :write) shm 0)
    ;; do your thing...
    ))
```

`with-mmapped-shm` opens a shm, truncates it, and then maps it to a pointer all in one:

```lisp
(with-mmapped-shm (shm ptr ("/foobar-shm" :direction :io)
	               ((cffi:null-pointer) 100 '(:read :write) 0))
  ;; do your thing...
  )
```

Use `shm-open*` to create anonymous shm objects:

```lisp
(defvar *anon-shm* (shm-open* :direction :io :permissions '(:all-user))

;; do your thing...

(shm-close *anon-shm*)
```

## Contributing

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/posix-shm/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/posix-shm).

## API

### [Enum] **permission**

Used for setting the permissions of the shm on `shm-open` and `fchmod`, and correspond to whether a user, group, or all users can read, write, or execute the shm (or perform any three):

- `:rusr`, `:read-user` - `O_RUSR`.
- `:wusr`, `:write-user` - `O_WUSR`.
- `:xusr`, `:exec-user` - `O_XUSR`.
- `:rwxu`, `:all-user` - `O_RWXU`.
- `:rgrp`, `:read-group` - `O_RGRP`.
- `:wgrp`, `:write-group` - `O_WGRP`.
- `:xgrp`, `:exec-group` - `O_XGRP`.
- `:rwxg`, `:all-group` - `O_RWXG`.
- `:roth`, `:read-other` - `O_ROTH`.
- `:woth`, `:write-other` - `O_WOTH`.
- `:xoth`, `:exec-other` - `O_XOTH`.
- `:rwxo`, `:all-other` - `O_RWXO`.

### [Enum] **protection**

Used by `mmap` to set whether pages may be executed, read, written, or none at all.
If `:none` is provided, no other flags may be included:

- `:exec` - `PROT_EXEC`.
- `:read` - `PROT_READ`.
- `:write` - `PROT_WRITE`.
- `:none` - `PROT_NONE`.

### [Condition] **shm-error** *(error)*

Raised whenever any of the shm API functions fail.

### [Function] **shm-open** *name &key (direction :input) if-exists if-does-not-exist permissions* => *fd*

Creates and opens a new, or opens an existing, POSIX shared memory object.
It can then be used by unrelated processes to **mmap** the same region of shared memory.

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

### [Function] **shm-open\*** *&key (direction :input) permissions (attempts 100)* => *fd*

Creates and opens, and then unlinks, a new POSIX shared memory object.

*attempts* -- the number of times **shm-open\*** tries to open a shm object before giving up.

### [Function] **shm-ftruncate** *fd length* => `(values)`

Cause the shm object specified by *FD* to be truncated to a size of exactly *length* bytes.

### [Function] **mmap** *fd addr length protections fd offset* => *ptr*

Creates a new mapping in the virtual address space of the calling process.

Only `MAP_SHARED`, `MAP_PRIVATE` `MAP_FIXED`, and `MAP_ANONYMOUS` is POSIX, or otherwise is supported widely.
Out of these, only `MAP_SHARED` makes sense to use with shared memory.
Therefore, in place of providing the *flags* parameter in `mmap(2)`, it will always be `MAP_SHARED`.

### [Function] **munmap** *addr size* => `(values)`

Deletes the mappings for the specified address range, and cause further references to addresses within the range to generate invalid memory references.
Closing the shm file descriptor may not automatically un-map the region.

### [Function] **shm-unlink** *name* => `(values)`

Removes a shm object previously created by **shm-open**.

### [Function] **shm-close** *fd* => `(values)`

Closes a file descriptor, so that it no longer refers to any shm object and may be reused.

### [Function] **fstat** *fd* => *stat*

**TODO** fstat is not yet implemented.

Retrieve information about the file pointed to by the fd.

### [Function] **fchmod** *fd mode* => *stat*

**TODO** fchmod is not yet implemented.

Changes the mode of the shm object.

### [Function] **fchown** *fd owner group* => *stat*

**TODO** fchown is not yet implemented.

Changes the ownership of the shm object

### [Macro] **with-open-shm** *(var &rest options) &body body*

Runs *BODY* under lexical scope of *VAR*, an open shm object.
*VAR* is closed (but not unlinked) when the program exits scope.

### [Macro] **with-open-shm\*** *(var &rest options) &body body*

Like **with-open-shm**, but uses **shm-open\*** to open a shm object instead.

### [Macro] **with-mmap** *(var &rest options) &body body*

Runs *BODY* under lexical scope of *VAR*, a memory-mapped pointer.
*VAR* is unmapped when the program exits scope.

### [Macro] **with-mmapped-shm** *(shm mmap shm-options (ptr length protections offset) :key (truncate t)) &body body*

Opens a shm object *SHM*, truncates it to `(+ LENGTH OFFSET)` (unless otherwise configured), and maps it to *MMAP*.
Runs *BODY* under lexical scope of *SHM* and *MMAP*, unmapping and closing both when the program exits scope.

### [Macro] **with-mmapped-shm\*** *(shm mmap shm-options (ptr length protections offset) :key (truncate t)) &body body*

Like **with-mmapped-shm**, but uses **shm-open\*** instead of **shm-open** to open a shm object.
