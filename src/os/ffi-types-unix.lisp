;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign types related to posix_spawn(3)
;;;

(in-package :iolib.os)

(include "unistd.h" "fcntl.h" "spawn.h")

(constant (+stdin+  "STDIN_FILENO"))
(constant (+stdout+ "STDOUT_FILENO"))
(constant (+stderr+ "STDERR_FILENO"))

(constant (fd-cloexec "FD_CLOEXEC"))

(cstruct posix-spawnattr-t "posix_spawnattr_t")

(cstruct posix-spawn-file-actions-t "posix_spawn_file_actions_t")

(constant (posix-spawn-resetids      "POSIX_SPAWN_RESETIDS"))
(constant (posix-spawn-setpgroup     "POSIX_SPAWN_SETPGROUP"))
(constant (posix-spawn-setsigdef     "POSIX_SPAWN_SETSIGDEF"))
(constant (posix-spawn-setsigmask    "POSIX_SPAWN_SETSIGMASK"))
(constant (posix-spawn-setschedparam "POSIX_SPAWN_SETSCHEDPARAM"))
(constant (posix-spawn-setscheduler  "POSIX_SPAWN_SETSCHEDULER"))
