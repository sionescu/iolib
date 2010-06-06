;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- posix_spawn(3) and its minions
;;;

(in-package :iolib.os)

(defsyscall (posix-spawn "posix_spawn")
    (:int :restart t)
  (pid          :pointer)
  (path         :string)
  (file-actions :pointer)
  (attributes   :pointer)
  (arguments    :pointer)
  (environment  :pointer))

(defsyscall (posix-spawnp "posix_spawnp")
    (:int :restart t)
  (pid          :pointer)
  (file         :string)
  (file-actions :pointer)
  (attributes   :pointer)
  (arguments    :pointer)
  (environment  :pointer))

(defsyscall (posix-spawnattr-init "posix_spawnattr_init")
    :int
  (attributes :pointer))

(defsyscall (posix-spawnattr-destroy "posix_spawnattr_destroy")
    :int
  (attributes :pointer))

(defsyscall (posix-spawnattr-getsigdefault "posix_spawnattr_getsigdefault")
    :int
  (attributes :pointer)
  (sigdefault :pointer))

(defsyscall (posix-spawnattr-setsigdefault "posix_spawnattr_setsigdefault")
    :int
  (attributes :pointer)
  (sigdefault :pointer))

(defsyscall (posix-spawnattr-getsigmask "posix_spawnattr_getsigmask")
    :int
  (attributes :pointer)
  (sigmask    :pointer))

(defsyscall (posix-spawnattr-setsigmask "posix_spawnattr_setsigmask")
    :int
  (attributes :pointer)
  (sigmask    :pointer))

(defsyscall (posix-spawnattr-getflags "posix_spawnattr_getflags")
    :int
  (attributes :pointer)
  (flags      :pointer))

(defsyscall (posix-spawnattr-setflags "posix_spawnattr_setflags")
    :int
  (attributes :pointer)
  (flags      :pointer))

(defsyscall (posix-spawnattr-getpgroup "posix_spawnattr_getpgroup")
    :int
  (attributes :pointer)
  (pgroup     :pointer))

(defsyscall (posix-spawnattr-setpgroup "posix_spawnattr_setpgroup")
    :int
  (attributes :pointer)
  (pgroup     :pointer))

(defsyscall (posix-spawnattr-getschedpolicy "posix_spawnattr_getschedpolicy")
    :int
  (attributes  :pointer)
  (schedpolicy :pointer))

(defsyscall (posix-spawnattr-setschedpolicy "posix_spawnattr_setschedpolicy")
    :int
  (attributes  :pointer)
  (schedpolicy :pointer))

(defsyscall (posix-spawnattr-getschedparam "posix_spawnattr_getschedparam")
    :int
  (attributes :pointer)
  (schedparam :pointer))

(defsyscall (posix-spawnattr-setschedparam "posix_spawnattr_setschedparam")
    :int
  (attributes :pointer)
  (schedparam :pointer))

(defsyscall (posix-spawn-file-actions-init "posix_spawn_file_actions_init")
    :int
  (file-actions :pointer))

(defsyscall (posix-spawn-file-actions-destroy "posix_spawn_file_actions_destroy")
    :int
  (file-actions :pointer))

(defsyscall (posix-spawn-file-actions-addopen "posix_spawn_file_actions_addopen")
    :int
  (file-actions :pointer)
  (fd           :int)
  (path         :string)
  (flags        :int)
  (mode         mode-t))

(defsyscall (posix-spawn-file-actions-addclose "posix_spawn_file_actions_addclose")
    :int
  (file-actions :pointer)
  (fd           :int))

(defsyscall (posix-spawn-file-actions-adddup2 "posix_spawn_file_actions_adddup2")
    :int
  (file-actions :pointer)
  (fd           :int)
  (newfd        :int))
