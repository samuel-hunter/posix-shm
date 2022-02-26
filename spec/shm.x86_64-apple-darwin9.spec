[
{ "tag": "function", "name": "fcntl", "ns": 0, "location": "/usr/include/fcntl.h:148:12", "variadic": true, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }, { "tag": "parameter", "name": "__cmd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "open", "ns": 0, "location": "/usr/include/fcntl.h:168:12", "variadic": true, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__file", "type": { "tag": ":pointer", "type": { "tag": ":char", "bit-size": 8, "bit-alignment": 8 } } }, { "tag": "parameter", "name": "__oflag", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "openat", "ns": 0, "location": "/usr/include/fcntl.h:192:12", "variadic": true, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }, { "tag": "parameter", "name": "__file", "type": { "tag": ":pointer", "type": { "tag": ":char", "bit-size": 8, "bit-alignment": 8 } } }, { "tag": "parameter", "name": "__oflag", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "posix_fallocate", "ns": 0, "location": "/usr/include/fcntl.h:282:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }, { "tag": "parameter", "name": "__offset", "type": { "tag": "off_t" } }, { "tag": "parameter", "name": "__len", "type": { "tag": "off_t" } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "close", "ns": 0, "location": "/usr/include/unistd.h:353:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "sleep", "ns": 0, "location": "/usr/include/unistd.h:453:21", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__seconds", "type": { "tag": ":unsigned-int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":unsigned-int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "pause", "ns": 0, "location": "/usr/include/unistd.h:478:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "extern", "name": "__environ", "ns": 0, "location": "/usr/include/unistd.h:553:15", "type": { "tag": ":pointer", "type": { "tag": ":pointer", "type": { "tag": ":char", "bit-size": 8, "bit-alignment": 8 } } } },
{ "tag": "function", "name": "_exit", "ns": 0, "location": "/usr/include/unistd.h:613:13", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__status", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":void" } },
{ "tag": "function", "name": "getlogin", "ns": 0, "location": "/usr/include/unistd.h:860:14", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [], "return-type": { "tag": ":pointer", "type": { "tag": ":char", "bit-size": 8, "bit-alignment": 8 } } },
{ "tag": "function", "name": "fsync", "ns": 0, "location": "/usr/include/unistd.h:967:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "fdatasync", "ns": 0, "location": "/usr/include/unistd.h:1128:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fildes", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "const", "name": "S_IRGRP", "ns": 0, "location": "/usr/include/fcntl.h:111:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IRWXU", "ns": 0, "location": "/usr/include/fcntl.h:109:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IXUSR", "ns": 0, "location": "/usr/include/fcntl.h:107:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IWOTH", "ns": 0, "location": "/usr/include/fcntl.h:118:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "__GLIBC_USE_ISOC2X", "ns": 0, "location": "/usr/include/features.h:235:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "__GLIBC_USE_DEPRECATED_GETS", "ns": 0, "location": "/usr/include/features.h:422:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "S_IROTH", "ns": 0, "location": "/usr/include/fcntl.h:117:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IXGRP", "ns": 0, "location": "/usr/include/fcntl.h:113:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IRWXG", "ns": 0, "location": "/usr/include/fcntl.h:115:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "__USE_FORTIFY_LEVEL", "ns": 0, "location": "/usr/include/features.h:414:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "S_IWGRP", "ns": 0, "location": "/usr/include/fcntl.h:112:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "__USE_XOPEN2K", "ns": 0, "location": "/usr/include/features.h:332:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "S_ISUID", "ns": 0, "location": "/usr/include/fcntl.h:97:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IWUSR", "ns": 0, "location": "/usr/include/fcntl.h:106:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IRUSR", "ns": 0, "location": "/usr/include/fcntl.h:105:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "__USE_XOPEN2K8", "ns": 0, "location": "/usr/include/features.h:340:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "S_ISGID", "ns": 0, "location": "/usr/include/fcntl.h:98:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "__USE_ISOC95", "ns": 0, "location": "/usr/include/features.h:334:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_ISOC11", "ns": 0, "location": "/usr/include/features.h:241:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_ISOC99", "ns": 0, "location": "/usr/include/features.h:336:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX199309", "ns": 0, "location": "/usr/include/features.h:324:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX199506", "ns": 0, "location": "/usr/include/features.h:328:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX", "ns": 0, "location": "/usr/include/features.h:316:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX2", "ns": 0, "location": "/usr/include/features.h:320:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "SEEK_END", "ns": 0, "location": "/usr/include/unistd.h:313:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "_POSIX_C_SOURCE", "ns": 0, "location": "/home/anon/Documents/Programs/posix-shm/spec/shm.h:6:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "SEEK_SET", "ns": 0, "location": "/usr/include/unistd.h:311:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "SEEK_CUR", "ns": 0, "location": "/usr/include/unistd.h:312:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "S_IXOTH", "ns": 0, "location": "/usr/include/fcntl.h:119:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IRWXO", "ns": 0, "location": "/usr/include/fcntl.h:121:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "_FCNTL_H", "ns": 0, "location": "/usr/include/fcntl.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_FEATURES_H", "ns": 0, "location": "/usr/include/features.h:19:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_STDC_PREDEF_H", "ns": 0, "location": "/usr/include/stdc-predef.h:19:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__GNU_LIBRARY__", "ns": 0, "location": "/usr/include/features.h:459:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 6 },
{ "tag": "const", "name": "__GLIBC__", "ns": 0, "location": "/usr/include/features.h:463:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "__STDC_IEC_559_COMPLEX__", "ns": 0, "location": "/usr/include/stdc-predef.h:49:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__STDC_ISO_10646__", "ns": 0, "location": "/usr/include/stdc-predef.h:58:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 201706 },
{ "tag": "const", "name": "_ATFILE_SOURCE", "ns": 0, "location": "/usr/include/features.h:342:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__GLIBC_MINOR__", "ns": 0, "location": "/usr/include/features.h:464:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 33 },
{ "tag": "const", "name": "S_IFCHR", "ns": 0, "location": "/usr/include/fcntl.h:82:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IFMT", "ns": 0, "location": "/usr/include/fcntl.h:80:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IFREG", "ns": 0, "location": "/usr/include/fcntl.h:84:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IFDIR", "ns": 0, "location": "/usr/include/fcntl.h:81:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "S_IFBLK", "ns": 0, "location": "/usr/include/fcntl.h:83:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "__GLIBC_USE_DEPRECATED_SCANF", "ns": 0, "location": "/usr/include/features.h:445:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "F_OK", "ns": 0, "location": "/usr/include/unistd.h:284:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "W_OK", "ns": 0, "location": "/usr/include/unistd.h:282:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "X_OK", "ns": 0, "location": "/usr/include/unistd.h:283:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "R_OK", "ns": 0, "location": "/usr/include/unistd.h:281:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 4 },
{ "tag": "const", "name": "_XOPEN_XPG3", "ns": 0, "location": "/usr/include/unistd.h:104:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_XOPEN_XPG4", "ns": 0, "location": "/usr/include/unistd.h:105:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_XOPEN_UNIX", "ns": 0, "location": "/usr/include/unistd.h:108:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_XOPEN_ENH_I18N", "ns": 0, "location": "/usr/include/unistd.h:112:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_XOPEN_LEGACY", "ns": 0, "location": "/usr/include/unistd.h:115:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "STDIN_FILENO", "ns": 0, "location": "/usr/include/unistd.h:210:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "STDERR_FILENO", "ns": 0, "location": "/usr/include/unistd.h:212:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "STDOUT_FILENO", "ns": 0, "location": "/usr/include/unistd.h:211:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_POSIX2_VERSION", "ns": 0, "location": "/usr/include/unistd.h:67:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "_POSIX2_C_VERSION", "ns": 0, "location": "/usr/include/unistd.h:70:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "_POSIX2_C_BIND", "ns": 0, "location": "/usr/include/unistd.h:74:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "_POSIX2_C_DEV", "ns": 0, "location": "/usr/include/unistd.h:78:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "_POSIX2_SW_DEV", "ns": 0, "location": "/usr/include/unistd.h:82:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "_POSIX2_LOCALEDEF", "ns": 0, "location": "/usr/include/unistd.h:86:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "_XOPEN_VERSION", "ns": 0, "location": "/usr/include/unistd.h:90:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 700 },
{ "tag": "const", "name": "_XOPEN_XCU_VERSION", "ns": 0, "location": "/usr/include/unistd.h:100:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 4 },
{ "tag": "const", "name": "_XOPEN_XPG2", "ns": 0, "location": "/usr/include/unistd.h:103:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_UNISTD_H", "ns": 0, "location": "/usr/include/unistd.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_POSIX_VERSION", "ns": 0, "location": "/usr/include/unistd.h:34:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "__POSIX2_THIS_VERSION", "ns": 0, "location": "/usr/include/unistd.h:53:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200809 },
{ "tag": "const", "name": "_ERRNO_H", "ns": 0, "location": "/usr/include/errno.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "errno", "ns": 0, "location": "/usr/include/errno.h:38:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "_STRING_H", "ns": 0, "location": "/usr/include/string.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "strerror_r", "ns": 0, "location": "/usr/include/string.h:423:12", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "__STDC_IEC_559__", "ns": 0, "location": "/usr/include/stdc-predef.h:41:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 }
]
