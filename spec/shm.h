/*
 * shm.h - POSIX shm includes
 * Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
 * BSD 3-Clause
 */
#define _POSIX_C_SOURCE 200112L

#include <sys/mman.h> /* shm_open(), shm_unlink(), mmap(), munmap() */
#include <sys/stat.h> /* S_* constants, fstat() */
#include <sys/types.h>
#include <fcntl.h> /* O_* constants, fchmod() */
#include <unistd.h> /* ftruncate(), close(), fchown() */

/* error handling */
#include <errno.h> /* EEXIST, EINT */
#include <string.h> /* strerror() */
