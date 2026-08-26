;; -*- coding: utf-8; lexical-binding: t; -*-
;; URL: https://github.com/seagle0128/.emacs.d
;; 需要拷贝到.emacs.d目录中

;; PERF: 将垃圾收集推迟到启动过程后期
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; PERF: 启动时不检查 .elc 的 mtime，由 `assistant.py sync` 保证正确性。
;; Ref: Doom Emacs early-init.el:45
(setq load-prefer-newer nil)

;; PERF: 缓存 `load-path' 各目录的文件列表，避免 `load' 每次逐目录探测 .elc/.el。
;; 新增的 .el 需重启 Emacs 才能被发现。
(when (fboundp 'load-path-filter-cache-directory-files)
  (setq load-path-filter-function #'load-path-filter-cache-directory-files))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; PERF: 即使设了 `inhibit-startup-screen'，Emacs 仍会初始化启动画面（文件 IO 和位图操作）。
;; 用 advice 直接跳过，省 50-100ms。
;; Ref: Doom Emacs early-init.el:193-198
(advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; PERF: `tool-bar-setup' 在禁用 toolbar 前仍会做非平凡工作，启动期间先拦截。
;; Ref: Doom Emacs early-init.el:244
(advice-add #'tool-bar-setup :override #'ignore)
