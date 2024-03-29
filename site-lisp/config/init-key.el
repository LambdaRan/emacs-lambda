;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'init-const)

;; Super = windows
;; Hyper =
;; Meta = Alt
;; Key Modifiers

;; Mac平台下交换Ctrl与Command
(when sys/mac-p
  (setq mac-control-modifier 'super)    ;; ctrl -> commnd
  (setq mac-command-modifier 'control)  ;; command -> ctrl
  )

(when sys/windows-p
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper)

  (setq w32-pass-alt-to-system nil)
  (setq w32-recognize-altgr nil)

  (w32-register-hot-key [s-])
  (w32-register-hot-key [M-])
  (w32-register-hot-key [C-])
  )

;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
 '("C-x C-f" "C-z" "C-q" "s-W" "M-h" "C-\\" "C-/" "s-x" "C-x d"))

;;; --- 工具函数
(lazy-load-set-keys
 '(
   ("<f2>" . make_fullscreen)               ; 全屏模式
   ("<f5>" . emacs-session-save)            ; 退出emacs
   ("<M-s-return>" . toggle-debug-on-error) ; 切换调试模式
   ("s-[" . eval-expression)                ; 执行表达式
   ("M-h" . set-mark-command)               ; Instead C-Space for Chinese input method

   ("C-z i" . display-fill-column-indicator-mode) ; 120字符限制线
   ("C-z l" . display-line-numbers-mode)          ; 行号模式切换
   ("C-c C-k". kill-whole-line)                   ; 在当前行任何位置删除整行
   ))

;; 搜索
(lazy-load-global-keys
 '(
   ("s-R" . re-builder)                 ;可视化构建正则表达式
   )
 "init-rebuilder")

(lazy-load-global-keys
 '(("M-s" . symbol-overlay-put)         ;懒惰搜索
   )
 "init-symbol-overlay")

;;; ### Color-Rg ###
(lazy-load-global-keys
 '(
   ("u" . color-rg-search-symbol)
   ("i" . color-rg-search-input)
   ("j" . color-rg-search-symbol-in-project)
   ("k" . color-rg-search-input-in-project)
   ("," . color-rg-search-symbol-in-current-file)
   ("." . color-rg-search-input-in-current-file)
   ("[" . color-rg-search-symbol-with-type)
   ("]" . color-rg-search-project-with-type)
   )
 "init-color-rg"
 "C-c")

(lazy-load-set-keys
 '(
   ("C-z k" . beginning-of-buffer)      ;缓存开始
   ("C-z j" . end-of-buffer)            ;缓存结尾
   ("M-a" . beginning-of-defun)         ;函数开头
   ("M-e" . end-of-defun)               ;函数结尾
   ))

(lazy-load-global-keys
 '(("M-g" . goto-line-preview))
 "goto-line-preview")

(lazy-load-global-keys
 '(
   ("C-s-n" . comment-dwim-next-line)   ;移动到上一行并注释
   ("C-s-p" . comment-dwim-prev-line)   ;移动到下一行并注释
   ("M-2" . indent-buffer)              ;动格式化当前Buffer
   ("M-z" . upcase-char)                ;Upcase char handly with capitalize-word
   ("C-x l" . mark-line)                ;选中整行
   ("C-z m" . kill-and-join-forward)    ;在缩进的行之间删除
   ("C->" . remember-init)              ;记忆初始函数
   ("C-<" . remember-jump)              ;记忆跳转函数
   ("M-s-," . point-stack-pop)          ;buffer索引跳转
   ("M-s-." . point-stack-push)         ;buffer索引标记
   ("M-G" . goto-column)                ;到指定列
   ("s-g" . goto-percent)               ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-I" . backward-indent)            ;向后移动4个字符

   ("M-j" . scroll-up-one-line)         ;向上滚动一行
   ("M-k" . scroll-down-one-line)       ;向下滚动一行
   )
 "basic-toolkit")

(lazy-load-global-keys
 '(
   ("C-o" . open-newline-above)         ;在上面一行新建一行
   ("C-l" . open-newline-below)         ;在下面一行新建一行
   )
 "open-newline")

;; 快速删除光标左右的内容
(lazy-load-global-keys
 '(
   ("M-N" . delete-block-backward)
   ("M-M" . delete-block-forward))
 "delete-block")

(lazy-load-set-keys
 '(
   ("M-o" . backward-delete-char-untabify) ;向前删除字符
   ("C-M-h" . mark-defun)                  ;选中函数
   ("M-SPC" . just-one-space)              ;只有一个空格在光标处
   ))

(lazy-load-set-keys
 '(
   ("C-/" . undo)                       ;撤销
   ("C-?" . undo-redo)                  ;重做
   ))

 ;;; ### Multi-Scratch
(lazy-load-global-keys
 '(("s-Q" . multi-scratch-new))
 "multi-scratch")

;;; ### Window Operation ###
(lazy-load-global-keys
 '(("C-j" . ace-window))
 "init-ace-window")

;;; --- 窗口操作
(lazy-load-set-keys
 '(
   ("C-c v" . split-window-vertically)   ;纵向分割窗口
   ("C-c h" . split-window-horizontally) ;横向分割窗口
   ("C-c w" . kill-this-buffer)          ;关闭当前buffer
   ("C-c o" . delete-other-windows)      ;关闭其它窗口
   ))

;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("M-J" . watch-other-window-up)        ;向下滚动其他窗口
   ("M-K" . watch-other-window-down)      ;向上滚动其他窗口
   ("M-<" . watch-other-window-up-line)   ;向下滚动其他窗口一行
   ("M->" . watch-other-window-down-line) ;向上滚动其他窗口一行
   )
 "watch-other-window")

;;; --- 临时最大化当前窗口
(lazy-load-global-keys
 '(
   ("C-c '" . toggle-one-window)              ;切换一个窗口
   )
 "toggle-one-window")

;; xwidget webkit
(lazy-load-global-keys
 '(
   ("C-c c w" . browse-url-at-point)
   ("C-c c W" . xwidget-webkit-browse-url)
   ("C-c c n" . ran-xwidget-webkit-browse-url-at-point-new-session)
   ("C-c c N" . ran-xwidget-webkit-browse-url-at-point-old-session)
   )
 "init-xwidget")

;;; ### Awesome-Tab ###
;;; --- 多标签浏览
(lazy-load-set-keys
 '(
   ("M-i" . awesome-tab-ace-mp)                  ;Ace jump
   ("M-7" . awesome-tab-backward-tab)              ;移动到后一个标签
   ("M-8" . awesome-tab-forward-tab)               ;移动到前一个标签
   ("M-9" . awesome-tab-backward-group)            ;移动到后一个标签组
   ("M-0" . awesome-tab-forward-group)             ;移动到前一个标签组
   ))

(lazy-load-global-keys
 '(
   ("M-&" . awesome-tab-backward-tab-other-window)
   ("M-*" . awesome-tab-forward-tab-other-window)
   ("M-s-7" . awesome-tab-select-beg-tab)
   ("M-s-8" . awesome-tab-select-end-tab)
   ("M-s-9" . awesome-tab-move-current-tab-to-beg)
   ("C-c Q" . awesome-tab-kill-other-buffers-in-current-group)
   ("C-c q" . awesome-tab-kill-all-buffers-in-current-group)
   ("s-w" . awesome-tab-keep-match-buffers-in-current-group)
   ("s-W" . awesome-tab-kill-match-buffers-in-current-group)
   )
 "awesome-tab")

;;; ### Help ###
;;; --- 帮助模式
(lazy-load-global-keys
 '(("C-h". one-key-menu-help)           ;帮助菜单
   )
 "init-help-mode")

;;; ### Thingh-edit ###
;;; --- 增强式编辑当前光标的对象
(lazy-load-global-keys
 '(("C-c x" . one-key-menu-thing-edit)  ;thing-edit 菜单
   )
 "init-thing-edit")

;; ### Dired;;  ###
(lazy-load-global-keys
 '(
   ("C-x d" . dired-jump)
   ("C-x C-f" . find-file)
   )
 "init-dired")

;;; ### Avy jump ###
(lazy-load-global-keys
 '(
   ("C-c n" . avy-goto-word-or-subword-1)
   ("C-c p" . avy-goto-char-2)
   ("C-c l" . avy-goto-line)
   )
 "avy")

(autoload 'ielm-map "ielm")
;全局按键的卸载
(lazy-load-unset-keys
 '("C-x e"))
(lazy-load-global-keys
 '(
   ("s-o" . insert-changelog-date)      ;插入日志时间 (%Y/%m/%d)
   ("s-p" . insert-standard-date)       ;
   ("C-&" . switch-to-messages)         ;跳转到 *Messages* buffer

   ("M-s-n" . ran-comment-line-next-line) ;向下移动注释
   ("M-s-p" . ran-comment-line-prev-line) ;向上移动注释
   ("M-s-i" . ielm-toggle)                ;切换ielm,Emacs Lisp 解释模式

   ("C-\\" . comment-or-uncomment-region+) ;注释当前行

   ("C-\"" . delete-current-buffer-and-window) ;关闭当前buffer, 并关闭窗口
   ("C-'" . delete-current-buffer-window)      ;删除当前buffer的窗口
   )
 "ran-toolkit")

(eval-after-load 'ielm-mode
  '(lambda ()
    (progn
      (lazy-load-unset-keys
       '("M-p" "M-n")
       ielm-map)                        ;卸载按键
      (lazy-load-set-keys
       '(
         ("C-s-p" . comint-previous-input) ;上一个输入
         ("C-s-n" . comint-next-input)     ;下一个输入
         )
       ielm-map)
      )))

;;; Elisp
(lazy-load-set-keys
 '(
   ("RET" . comment-indent-new-line)    ;自动换行并注释
   )
 emacs-lisp-mode-map)

;;; ### Man ###
;;; --- Man
(lazy-load-global-keys
 '(
   ("<f1>" . woman))
 "init-woman")

;; Dash.
(lazy-load-global-keys
 '(("y" . dash-at-point)
   )
 "dash-at-point"
 "C-x")

;;; ### expand-region ###
(lazy-load-global-keys
 '(
   ("C-=" . er/expand-region))
 "expand-region")

;;; ### Magit ###
(lazy-load-global-keys
 '(
   ("C-c m" . magit-status+))
 "init-git")

;;; ### Aweshell ###
;;; --- 多标签式的shell
(lazy-load-global-keys
 '(
   ("C-`" . aweshell-new)
   ("s-h" . aweshell-toggle)
   ("s-x s-x" . aweshell-dedicated-toggle)
   )
 "init-aweshell")

(lazy-load-set-keys
 '(
   ("DEL" . isearch-del-char)
   ("M-o" . isearch-del-char))
 isearch-mode-map)

(lazy-load-global-keys
 '(
   ("C-}" . counsel-etags-find-tag)
   ("C-]" . counsel-etags-find-tag-at-point)
   ("C-c M-i" . ran-counsel-imenu))
 "init-etags")

(lazy-load-global-keys
 '(
   ("e" . find-file-in-project-at-point)
   ("f" . find-file-in-project-by-selected)
   ;; ("F" . find-file-in-project)
   ("s" . find-file-with-similar-name)
   ("d" . find-directory-in-project-by-selected)
   ;; ("D" . find-directory-in-project)
   ("t" . find-file-in-current-directory-by-selected)
   ;; ("C" . find-file-in-current-directory)
   ("r" . ffip-find-files-resume)
   )
 "init-ffip"
 "C-c")

;;; ### Unset key ###
(lazy-load-unset-keys                   ; 全局按键卸载
 '("C-x C-f" "M-x" "C-x b" "M-y"))

(lazy-load-global-keys
 '(("C-c c z" . zlua-jump-to-directory))
 "zlua")
(with-eval-after-load 'zlua
  ;; 设置zlua脚本路径
  (setq zlua-path "~/ransysconf/zlua/z.lua"))

(lazy-load-global-keys
 '(
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-c M-l" . counsel-locate)
   ("C-c M-d" . counsel-dired-jump)
   ("C-c M-e" . counsel-find-file-extern)
   ("C-c M-f" . counsel-recentf)
   ;; ("C-c M-i" . counsel-semantic-or-imenu)
   ("C-x b" . ivy-switch-buffer)
   ("C-c c s" . swiper-isearch)
   )
 "init-ivy")

(provide 'init-key)
