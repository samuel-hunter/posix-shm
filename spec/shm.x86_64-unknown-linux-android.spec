[
{ "tag": "function", "name": "fcntl", "ns": 0, "location": "/usr/include/fcntl.h:148:12", "variadic": true, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }, { "tag": "parameter", "name": "__cmd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "posix_fallocate", "ns": 0, "location": "/usr/include/fcntl.h:282:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }, { "tag": "parameter", "name": "__offset", "type": { "tag": "off_t" } }, { "tag": "parameter", "name": "__len", "type": { "tag": "off_t" } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "typedef", "ns": 0, "name": "size_t", "location": "/usr/lib/llvm-11/lib/clang/11.0.1/include/stddef.h:46:23", "type": { "tag": ":unsigned-long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "function", "name": "close", "ns": 0, "location": "/usr/include/unistd.h:353:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "sleep", "ns": 0, "location": "/usr/include/unistd.h:444:21", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__seconds", "type": { "tag": ":unsigned-int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":unsigned-int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "pause", "ns": 0, "location": "/usr/include/unistd.h:469:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "extern", "name": "__environ", "ns": 0, "location": "/usr/include/unistd.h:543:15", "type": { "tag": ":pointer", "type": { "tag": ":pointer", "type": { "tag": ":char", "bit-size": 8, "bit-alignment": 8 } } } },
{ "tag": "function", "name": "_exit", "ns": 0, "location": "/usr/include/unistd.h:603:13", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__status", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":void" } },
{ "tag": "function", "name": "getlogin", "ns": 0, "location": "/usr/include/unistd.h:848:14", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [], "return-type": { "tag": ":pointer", "type": { "tag": ":char", "bit-size": 8, "bit-alignment": 8 } } },
{ "tag": "function", "name": "fsync", "ns": 0, "location": "/usr/include/unistd.h:954:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fd", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "function", "name": "fdatasync", "ns": 0, "location": "/usr/include/unistd.h:1115:12", "variadic": false, "inline": false, "storage-class": "extern", "parameters": [{ "tag": "parameter", "name": "__fildes", "type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } }], "return-type": { "tag": ":int", "bit-size": 32, "bit-alignment": 32 } },
{ "tag": "const", "name": "_XOPEN_ENH_I18N", "ns": 0, "location": "/usr/include/unistd.h:112:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_XOPEN_LEGACY", "ns": 0, "location": "/usr/include/unistd.h:115:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "STDIN_FILENO", "ns": 0, "location": "/usr/include/unistd.h:210:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "STDOUT_FILENO", "ns": 0, "location": "/usr/include/unistd.h:211:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "STDERR_FILENO", "ns": 0, "location": "/usr/include/unistd.h:212:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "_ERRNO_H", "ns": 0, "location": "/usr/include/errno.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_POSIX2_C_BIND", "ns": 0, "location": "/usr/include/unistd.h:74:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "_POSIX2_C_DEV", "ns": 0, "location": "/usr/include/unistd.h:78:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "_POSIX2_SW_DEV", "ns": 0, "location": "/usr/include/unistd.h:82:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "_STRING_H", "ns": 0, "location": "/usr/include/string.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_POSIX2_LOCALEDEF", "ns": 0, "location": "/usr/include/unistd.h:86:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "_XOPEN_VERSION", "ns": 0, "location": "/usr/include/unistd.h:92:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 600 },
{ "tag": "const", "name": "errno", "ns": 0, "location": "/usr/include/errno.h:38:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "_XOPEN_XCU_VERSION", "ns": 0, "location": "/usr/include/unistd.h:100:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 4 },
{ "tag": "const", "name": "_XOPEN_XPG2", "ns": 0, "location": "/usr/include/unistd.h:103:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_XOPEN_XPG3", "ns": 0, "location": "/usr/include/unistd.h:104:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_XOPEN_XPG4", "ns": 0, "location": "/usr/include/unistd.h:105:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX199506", "ns": 0, "location": "/usr/include/features.h:328:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX199309", "ns": 0, "location": "/usr/include/features.h:324:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX2", "ns": 0, "location": "/usr/include/features.h:320:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_POSIX", "ns": 0, "location": "/usr/include/features.h:316:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_XOPEN2K", "ns": 0, "location": "/usr/include/features.h:332:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_FEATURES_H", "ns": 0, "location": "/usr/include/features.h:19:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_ISOC95", "ns": 0, "location": "/usr/include/features.h:334:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_ISOC99", "ns": 0, "location": "/usr/include/features.h:336:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__USE_ISOC11", "ns": 0, "location": "/usr/include/features.h:241:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "NULL", "ns": 0, "location": "/usr/lib/llvm-11/lib/clang/11.0.1/include/stddef.h:89:11", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "__GLIBC__", "ns": 0, "location": "/usr/include/features.h:452:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "__GNU_LIBRARY__", "ns": 0, "location": "/usr/include/features.h:448:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 6 },
{ "tag": "const", "name": "__GLIBC_MINOR__", "ns": 0, "location": "/usr/include/features.h:453:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 31 },
{ "tag": "const", "name": "__STDC_IEC_559__", "ns": 0, "location": "/usr/include/stdc-predef.h:41:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "__STDC_ISO_10646__", "ns": 0, "location": "/usr/include/stdc-predef.h:58:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 201706 },
{ "tag": "const", "name": "__STDC_IEC_559_COMPLEX__", "ns": 0, "location": "/usr/include/stdc-predef.h:49:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_STDC_PREDEF_H", "ns": 0, "location": "/usr/include/stdc-predef.h:19:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_FCNTL_H", "ns": 0, "location": "/usr/include/fcntl.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_POSIX_C_SOURCE", "ns": 0, "location": "/home/anon/Documents/Programs/shm/spec/shm.h:6:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "R_OK", "ns": 0, "location": "/usr/include/unistd.h:281:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 4 },
{ "tag": "const", "name": "W_OK", "ns": 0, "location": "/usr/include/unistd.h:282:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "__GLIBC_USE_DEPRECATED_SCANF", "ns": 0, "location": "/usr/include/features.h:434:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "__USE_FORTIFY_LEVEL", "ns": 0, "location": "/usr/include/features.h:403:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "__GLIBC_USE_ISOC2X", "ns": 0, "location": "/usr/include/features.h:235:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "__GLIBC_USE_DEPRECATED_GETS", "ns": 0, "location": "/usr/include/features.h:411:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "SEEK_END", "ns": 0, "location": "/usr/include/unistd.h:313:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 2 },
{ "tag": "const", "name": "SEEK_CUR", "ns": 0, "location": "/usr/include/unistd.h:312:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "SEEK_SET", "ns": 0, "location": "/usr/include/unistd.h:311:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "F_OK", "ns": 0, "location": "/usr/include/unistd.h:284:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 0 },
{ "tag": "const", "name": "X_OK", "ns": 0, "location": "/usr/include/unistd.h:283:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 },
{ "tag": "const", "name": "_POSIX2_C_VERSION", "ns": 0, "location": "/usr/include/unistd.h:70:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "_POSIX2_VERSION", "ns": 0, "location": "/usr/include/unistd.h:67:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "__POSIX2_THIS_VERSION", "ns": 0, "location": "/usr/include/unistd.h:57:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "strerror_r", "ns": 0, "location": "/usr/include/string.h:416:12", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 } },
{ "tag": "const", "name": "_POSIX_VERSION", "ns": 0, "location": "/usr/include/unistd.h:37:10", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 200112 },
{ "tag": "const", "name": "_UNISTD_H", "ns": 0, "location": "/usr/include/unistd.h:23:9", "type": { "tag": ":long", "bit-size": 64, "bit-alignment": 64 }, "value": 1 }
]
