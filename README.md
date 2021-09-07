# posix-shm
[![builds.sr.ht status](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml.svg)](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml)

POSIX shared memory for Common Lisp.

This library is currently in its embryonic stages.

According to the Linux Programmer's Manual, the POSIX shared memory API (or `shm`) "allows processes to communicate information by sharing a region of memory." -- `shm_overview(7)`.
This library provides a slim wrapper over the POSIX shm interface.

Features include:

- `shm_open`, `shm_unlink`, `mmap`
- A utility function, `shm-open*`, for opening anonymous shms that are immediately unlinked.
- Flags provided as keywords, with more readable variations.

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
  (shm-open "/foobar-shm" :open-flags '(:create :read-write)
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
(with-open-shm (shm "/foobar-shm" :open-flags '(:create :read-write)
                    :permissions '(:all-user))
  (shm-ftruncate shm 100)
  (with-mmap (ptr (cffi:null-pointer) 100 '(:read :write) shm 0)
    ;; do your thing...
    ))
```

`with-mmapped-shm` opens a shm, truncates it, and then maps it to a pointer all in one:

```lisp
(with-mmapped-shm (shm ptr ("/foobar-shm" :open-flags '(:create :read-write)
                                          :permissions '(:all-user))
	               ((cffi:null-pointer) 100 '(:read :write) 0))
  ;; do your thing...
  )
```

Use `shm-open*` to create anonymous shms:

```lisp
;; shm-open* automatically tacks on (:create :exclusive) to the open flags
(defvar *anon-shm* (shm-open* :open-flags '(:read-write) :permissions '(:all-user)))

;; Do your thing...

(shm-close *anon-shm*)
```

## Contributing

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/posix-shm/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/posix-shm).

## API

### [Enum] **open-flag**

Used with `shm-open` and `shm-open*`:

- `:rdonly`, `:read` - `O_RDONLY`.
  The shm object is opened in read-only mode.
- `:rdwr`, `:read-write` - `O_RDWR`.
  The shm object is opened in read-write mode.
- `:creat`, `:create` - `O_CREAT`.
  The shm object is created if it doesn't yet exist.
- `:excl`, `:exclusive` - `O_EXCL`.
  Alongside `:creat`, `shm-open` fails to create a shm object when it already exists.
- `:trunc`, `:truncate` - `O_TRUNC`.
  The shm object truncates the file to 0 bytes when it opens.

`shm-open` already provides the flags `:create` and `:exclusive`.

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

### [Function] **shm-open** *name open-flags permissions* => *fd*

Creates and opens a new, or opens an existing, POSIX shared memory object.
It can then be used by unrelated processes to **mmap** the same region of shared memory.

*NAME* specifies the shared memory object to be created or opened. For portable use, a shm object should be identified by a name of the form `/somename`; that is, a string up to **NAME_MAX** (i.e. 255) characters consisting of an initial slash, followed by non-slash characters.

*OPEN-FLAGS* is a list of **open-flag** keywords, putting together exactly one of `:rdonly`/`:read-only` or `:rdwr`/`:read-write` and any of the rest.

*PERMISSIONS* specifies a list of file permissions set by the shm object if it is created.

### [Function] **shm-open\*** *name open-flags permissions &optional (attempts 100)* => *fd*

Creates and opens, and then unlinks, a new POSIX shared memory object.

*ATTEMPTS* specifies the number of attempted openings before giving up.

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

### [Macro] **with-mmap** *(var &rest options) &body body*

Runs *BODY* under lexical scope of *VAR*, a memory-mapped pointer.
*VAR* is unmapped when the program exits scope.

### [Macro] **with-mmapped-shm** *(shm mmap shm-options (length protections offset) :key (truncate t)) &body body*

Opens a shm object *SHM*, truncates it to `(+ LENGTH OFFSET)` (unless otherwise configured), and maps it to *MMAP*.
Runs *BODY* under lexical scope of *SHM* and *MMAP*, unmapping and closing both when the program exits scope.
