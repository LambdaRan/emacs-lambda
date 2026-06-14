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
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb
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
;; GC 安全网：如果 gcmh 加载失败或 GC 值在启动后仍极高，强制恢复到合理值以防止内存膨胀。
(add-hook 'emacs-startup-hook
          #'(lambda ()
              "启动后恢复 GC 值，含安全网。"
              (cond
               ;; gcmh 已生效，不干预，由 gcmh 自行管理 GC 阈值
               ((bound-and-true-p gcmh-mode))
               ;; gcmh 未加载且 GC 阈值仍极高，降回 16MB 安全值
               ((>= gc-cons-threshold most-positive-fixnum)
                (setq gc-cons-threshold (* 16 1024 1024)
                      gc-cons-percentage 0.1))
               ;; gcmh 未加载，GC 阈值已恢复，设置合理默认值
               (t
                (setq gc-cons-threshold 800000
                      gc-cons-percentage 0.1))))
          100) ; 低优先级，最后执行

(provide 'init-accelerate)
