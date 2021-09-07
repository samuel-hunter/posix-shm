/*
 * shm.h - POSIX shm includes
 * Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
 * BSD 3-Clause
 */
#define _POSIX_C_SOURCE 200112L

#include <sys/mman.h> /* shm_open()(), shm_unlink(), mmap(), munmap(), etc */
#include <sys/stat.h> /* S_* constants */
#include <fcntl.h> /* O_* constants */
#include <unistd.h> /* close() */

/* error handling */
#include <errno.h> /* errno */
#include <string.h> /* strerror() */
