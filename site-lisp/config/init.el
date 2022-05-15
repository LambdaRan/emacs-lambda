;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'init-const)

(let (;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; 统计启动时间
  (with-temp-message ""                 ;抹掉插件启动的输出
    (require 'benchmark-init-modes)
    (require 'benchmark-init)
    (benchmark-init/activate)

    (require 'init-accelerate)
    ;; 按需加载插件
    (require 'lazy-load)    
    (require 'init-font)
    (require 'init-generic)
    (require 'init-theme)
    (when (featurep 'cocoa)
      (require 'cache-path-from-shell))

    (require 'one-key)
    (require 'init-awesome-pair)
    (require 'init-awesome-tab)
    (require 'init-awesome-tray)
    (require 'init-auto-save)
    (require 'init-mode)
    (require 'init-dired)
    (require 'init-one-key)
    (require 'init-key)
    ;; 只读模式下使用vi样式的单按键操作
    (require 'init-vi-navigate)
    (require 'init-ivy)
    ;; (require 'init-vertico)
    (require 'init-startup)
    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
         ;; 显示行号
         (require 'init-line-number)
         (require 'init-highlight-parentheses)
         (require 'init-idle)
         ;; 后台自动删除不用的buffer
         (require 'init-tempbuf)
         ;; minibuf中参数提示
         (require 'init-eldoc)
         (require 'init-indent)
         ;; 模板
         (require 'init-yasnippet)
         ;; 自动补全
         (require 'init-corfu)         
         (require 'init-corfu-tabnine)

         ;; (require 'init-company-mode)
         ;; (require 'init-company-tabnine)
         (require 'init-info)
         (require 'init-c)
         (require 'init-flycheck)
         (require 'init-diff-hl)

         ;; Restore session at last.
         (require 'init-session)
         (emacs-session-restore)

         (server-start)            ;为emacsclient准备使用场景，比如git
         )))
  ) ;; end let

(provide 'init)
