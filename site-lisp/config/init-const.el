;; init-const.el --- Define constants.	-*- lexical-binding: t -*-

;; 定义一些启动目录，方便下次迁移修改
(defconst my-emacs-root-dir (file-name-directory user-init-file)
    "Directory of site-lisp")

(defconst my-emacs-config-dir (concat my-emacs-root-dir "config")
    "Directory of config")

(defconst my-emacs-extension-dir (concat my-emacs-root-dir "extensions")
    "Directory of extensions")

;;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-const.el
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here