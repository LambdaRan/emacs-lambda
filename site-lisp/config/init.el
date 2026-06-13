;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'init-const)
(require 'init-accelerate)

;; 清空避免加载远程文件的时候分析文件。
(let ((file-name-handler-alist nil))

  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            #'(lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  (with-temp-message ""
    ;; 按需加载插件
    (require 'lazy-load)
    (require 'init-font)
    (require 'init-generic)
    (require 'init-theme)
    (when (featurep 'cocoa)
      (require 'cache-path-from-shell)
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize))

    (require 'init-awesome-tab)
    (require 'init-awesome-tray)
    (require 'init-auto-save)
    (require 'init-mode)
    (require 'init-treesit)
    (require 'init-key)
    (require 'init-ivy)
    (require 'init-startup)

    ;; 第一批延迟：视觉和轻量模块 (0.5s)
    (run-with-idle-timer
     0.5 nil
     #'(lambda ()
         (require 'one-key)
         (require 'init-fingertip)
         (require 'init-dired)
         (require 'init-one-key)
         (require 'init-vi-navigate)
         (require 'init-line-number)
         (require 'init-highlight-parentheses)
         (require 'init-treesit-fold)))

    ;; 第二批延迟：重型模块 (1.5s)
    (run-with-idle-timer
     1.5 nil
     #'(lambda ()
         (require 'init-agent-shell)
         (require 'init-ghostel)
         (require 'init-idle)
         (require 'init-ffip)
         (require 'init-color-rg)
         (require 'init-fastctags)
         (require 'init-tempbuf)
         (require 'init-indent)
         (require 'init-company-mode)
         (require 'init-info)
         (require 'init-c)
         (require 'ran-toolkit)

         ;; Restore session at last.
         (require 'init-session)
         (emacs-session-restore)))
    ) ;; end with-temp-message
  ) ;; end let

(add-hook 'emacs-startup-hook
          #'(lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'init)
