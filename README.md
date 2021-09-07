# shm

POSIX shared memory for Common Lisp.

This library is currently in its embryonic stages.

According to the Linux Programmer's Manual, the POSIX shared memory API (or `shm`) "allows processes to communicate information by sharing a region of memory." -- `shm_overview(7)`.
I've found working with the C library directly unwieldy in Lisp.
`shm` intends to provide a more comfortable interface instead.

Features include (or will include):

- A more natural interface for `shm`-related functions
- A utility function, `shm-open*`, for opening an "anonymous" shm without a name.
  It attempts to create a new shm, and then immediately unlinks the name before returning the file descriptor.
- Flags are provided as a list of keywords, with aliases for readability.
  If a flag list can be constantly at runtime, it will transform into a constant number.

## Usage

TODO

## Contributing

TODO

## API

### [Enum] **open-flag**

Used on `shm-open`:

- `:rdonly`, `:read` - `O_RDONLY`.
- `:rdwr`, `:read-write` - `O_RDWR`.
- `:creat`, `:create` - `O_CREAT`.
- `:excl`, `:exclusive` - `O_EXCL`.
- `:trunc`, `:truncate` - `O_TRUNC`.

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

TODO

### [Function] **shm-open\*** *name open-flags permissions &optional (attempts 100)* => *fd*

TODO

### [Function] **posix-ftruncate** *fd size* => `(values)`

TODO

### [Function] **mmap** *fd addr length protections fd offset* => *ptr*

Creates a new mapping in the virtual address space of the calling process.
Only `MAP_SHARED`, `MAP_PRIVATE` `MAP_FIXED`, and `MAP_ANONYMOUS` is POSIX, or otherwise is supported widely.
Out of these, only `MAP_SHARED` makes sense to use with shared memory.
Therefore, in place of providing the *flags* parameter in `mmap(2)`, it will always be `MAP_SHARED`.

### [Function] **munmap** *addr size* => `(values)`

TODO

### [Function] **shm-unlink** *name* => `(values)`

TODO

### [Function] **close** *fd* => `(values)`

TODO

### [Function] **fstat** *fd* => *stat*

TODO
