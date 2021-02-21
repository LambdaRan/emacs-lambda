;; 加速配置。
(require 'init-accelerate)

;; 字体设置
(require 'init-font)

(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

;; 定义一些启动目录，方便下次迁移修改
(defvar lazycat-emacs-root-dir (file-truename "~/emacs-lambda/site-lisp"))
(defvar lazycat-emacs-config-dir (concat lazycat-emacs-root-dir "/config"))
(defvar lazycat-emacs-extension-dir (concat lazycat-emacs-root-dir "/extensions"))

;; 统计启动时间
  (with-temp-message ""                 ;抹掉插件启动的输出
     ;; (require 'benchmark-init-modes)
     ;; (require 'benchmark-init)
     ;; (benchmark-init/activate)

     (require 'init-startup)
     (require 'init-generic)
     (require 'init-theme)
     (when (featurep 'cocoa)
       (require 'cache-path-from-shell))

     ;; 按需加载插件
     (require 'lazy-load)
     (require 'one-key)
     ;; 显示行号
     (require 'display-line-numbers)
     (require 'basic-toolkit)
     (require 'redo)
     (require 'init-highlight-parentheses)
     (require 'init-awesome-pair)
     (require 'init-awesome-tab)
     (require 'init-awesome-tray)
     ;; 不要自动备份
     (require 'init-backup)
     (require 'init-line-number)
     (require 'init-auto-save)
     (require 'init-mode)
     (require 'init-dired)
     (require 'init-indent)
     (require 'init-one-key)
     (require 'init-key)
     ;; 只读模式下使用vi样式的单按键操作
     (require 'init-vi-navigate)
     (require 'init-performance)
     (require 'init-ivy)

     ;; 可以延后加载的
     (run-with-idle-timer
      1 nil
      #'(lambda ()

          ;; 后台自动删除不用的buffer
          (require 'init-tempbuf)
          ;; minibuf中参数提示
          (require 'init-eldoc)
          ;; 模板
          (require 'init-yasnippet)
          ;; 自动补全
          (require 'init-company-mode)
          (require 'init-company-tabnine)
          ;; 平滑滚动，不知道干啥的
          (require 'init-smooth-scrolling)
          ;; 保存窗口位置与内容，下次启动时直接恢复到上次窗口状态
          (require 'init-winpoint)
          (require 'init-info)
          (require 'init-c)
          (require 'init-flycheck)
          (require 'init-idle)
          ;; (require 'init-nox)
          (require 'init-diff-hl)
          (require 'init-ffip)

          (require 'init-etags)
          (require 'init-doxymacs)
          (require 'init-ibuffer)
          (require 'init-hydra)
          (require 'init-autorevert)
          (require 'init-ediff)
          (require 'init-vdiff)
          (require 'ran-toolkit)

          ;; Restore session at last.
          (require 'init-session)
          (emacs-session-restore)

          (server-start)            ;为emacsclient准备使用场景，比如git
          ))))

(provide 'init)
