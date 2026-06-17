;; -*- coding: utf-8; lexical-binding: t; -*-
;; init-accelerate.el --- Accelerate the start Emacs

(require 'init-const)

(setq initial-major-mode 'fundamental-mode ; ;; 默认用最简单的模式
      package-enable-at-startup nil        ; ;; 不要自动启用package
      package--init-file-ensured t)

;; Improve the performance of rendering long lines.
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Scrolling.html
;; https://www.reddit.com/r/emacs/comments/gaub11/poor_scrolling_performance_in_doom_emacs/fp392eh/

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; 1MB：agent-shell 跑 Claude Code 等大输出进程时减少读次数（项目无 LSP，按最大输出场景设）。
(setq read-process-output-max (* 1 1024 1024))  ; 1MB
;; 增加IO性能
(setq process-adaptive-read-buffering nil)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when sys/windows-p
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless sys/mac-p   (setq command-line-ns-option-alist nil))
(unless sys/linux-p (setq command-line-x-option-alist nil))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
;; Speed up startup
(setq auto-mode-case-fold nil)

;; Ref: Doom Emacs modules/doom/init.el:303-307
;; 将 gcmh 激活延迟到首次打开文件时，减少启动开销。
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold #x1000000) ; 16MB

(defun my-enable-gcmh-once ()
  "首次打开文件时启用 gcmh，然后移除此 hook。"
  (require 'gcmh)
  (gcmh-mode 1)
  (remove-hook 'find-file-hook #'my-enable-gcmh-once))
(add-hook 'find-file-hook #'my-enable-gcmh-once)

;; Ref: Doom Emacs doom.el:555-558
;; 启动时 GC 恢复：early-init 把 gc-cons-threshold 设为 most-positive-fixnum 加速启动，
;; 而 gcmh 被延迟到首次 find-file 才激活，故此处（emacs-startup-hook）必须先把阈值
;; 降回 16MB 作为安全默认，gcmh 随后接管并自行管理阈值。这是主路径，非"兜底"。
(defun my-restore-gc-after-startup ()
  "启动后恢复 GC 阈值；若 gcmh 已意外激活则不干预。"
  (cond
   ;; gcmh 已生效（理论上首次 find-file 前不应发生），交由 gcmh 管理
   ((bound-and-true-p gcmh-mode))
   ;; gcmh 尚未激活，启动期阈值仍为 most-positive-fixnum → 降回 16MB
   ((>= gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold (* 16 1024 1024)
          gc-cons-percentage 0.1))
   ;; 阈值已被别处恢复，设保守默认（防御性，正常不触发）
   (t
    (setq gc-cons-threshold 800000
          gc-cons-percentage 0.1))))

(add-hook 'emacs-startup-hook #'my-restore-gc-after-startup 100) ; 低优先级，靠后执行

(provide 'init-accelerate)
