

;; Mac平台下交换 Option 和 Command 键。
;; key bindings
;;(when (eq system-type 'darwin) ;; mac specific settings
;;  (setq mac-control-modifier 'super)
;;  (setq mac-command-modifier 'control)
;;  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
;;  )

;; https://emacs.stackexchange.com/questions/26616/how-to-use-a-macs-command-key-as-a-control-key
;; Mac平台下交换 Ctrol 和 Command 键。
(when (featurep 'cocoa)
  (setq mac-control-modifier 'super)
  (setq mac-command-modifier 'control))

;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
                      '("C-x C-f" "C-z" "C-q" "s-W" "s-z" "M-h" "C-x C-c" "C-\\" "s-c" "s-x" "s-v"))
;;  '("C-x C-f" "C-z" "C-q" "s-W" "M-h" "C-x C-c" "C-\\" "s-c" "s-x"))


(lazy-load-global-keys
 '(
   ("<f2>" . make_fullscreen) ; 全屏模式
   )
 "init-startup")

;;; ### Toolkit ###
;;; --- 工具函数
(lazy-load-set-keys
 '(
   ("s-c o" . one-key-menu-directory)       ;目录打开菜单
   ("s-," . bury-buffer)                    ;隐藏当前buffer
   ("s-." . unbury-buffer)                  ;反隐藏当前buffer
   ("s-&" . killall)                        ;杀掉进程
   ("<M-s-return>" . toggle-debug-on-error) ;切换调试模式
   ("s-[" . eval-expression)                ;执行表达式
   ("C-s-q" . quoted-insert)                ;读取系一个输入字符并插入
   ("M-h" . set-mark-command) ;Instead C-Space for Chinese input method
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 搜索
(lazy-load-global-keys
 '(
   ("s-R" . re-builder)                 ;可视化构建正则表达式
   )
 "init-rebuilder")

(lazy-load-global-keys
 '(
   ("M-s" . symbol-overlay-put)         ;懒惰搜索
   )
 "init-symbol-overlay")

;;; ### Color-Rg ###
;;; --- 搜索重构
;; (lazy-load-global-keys
;;  '(
;;    ("s-x g" . color-rg-search-symbol)
;;    ("s-x h" . color-rg-search-input)
;;    ("s-x j" . color-rg-search-symbol-in-project)
;;    ("s-x k" . color-rg-search-input-in-project)
;;    ("s-x ," . color-rg-search-symbol-in-current-file)
;;    ("s-x ." . color-rg-search-input-in-current-file)
;;    ("s-x o" . color-rg-search-symbol-with-type)
;;    ("s-x p" . color-rg-search-project-with-type)
;;    )
;;  "color-rg")
(lazy-load-global-keys
 '(
   ("g" . color-rg-search-symbol)
   ("h" . color-rg-search-input)
   ("j" . color-rg-search-symbol-in-project)
   ("k" . color-rg-search-input-in-project)
   ("," . color-rg-search-symbol-in-current-file)
   ("." . color-rg-search-input-in-current-file)
   ("o" . color-rg-search-symbol-with-type)
   ("p" . color-rg-search-project-with-type)
   )
 "color-rg"
 "C-c c")
;; 模糊搜索框架
(lazy-load-global-keys
 '(
   ("C-c y" . snails)
   ("C-c u" . snails-search-point)
   )
 "snails")

;;; ### Buffer Move ###
;;; --- 缓存移动
(lazy-load-set-keys
 '(
   ("C-z k" . beginning-of-buffer)      ;缓存开始
   ("C-z j" . end-of-buffer)            ;缓存结尾
   ("C-M-f" . forward-paragraph)        ;下一个段落
   ("C-M-b" . backward-paragraph)       ;上一个段落
   ("C-M-y" . backward-up-list)         ;向左跳出 LIST
   ("C-M-o" . up-list)                  ;向右跳出 LIST
   ("C-M-u" . backward-down-list)       ;向左跳进 LIST
   ("C-M-i" . down-list)                ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)       ;函数开头
   ("C-M-e" . end-of-defun)             ;函数末尾
   ))

(lazy-load-global-keys
 '(
   ("M-g" . goto-line-preview))
 "goto-line-preview")

(lazy-load-global-keys
 '(
   ("C-z l" . display-line-numbers-mode) ;行号模式切换
   ("M-s-n" . comment-part-move-down)    ;向下移动注释
   ("M-s-p" . comment-part-move-up)      ;向上移动注释
   ("C-s-n" . comment-dwim-next-line)    ;移动到上一行并注释x
   ("C-s-p" . comment-dwim-prev-line)    ;移动到下一行并注释x
   ("M-2" . indent-buffer)               ;自动格式化当前Buffer
   ("M-z" . upcase-char)      ;Upcase char handly with capitalize-word
   ("C-x u" . mark-line)      ;选中整行
   ("s-k" . kill-and-join-forward)      ;在缩进的行之间删除
   ("C->" . remember-init)              ;记忆初始函数
   ("C-<" . remember-jump)              ;记忆跳转函数
   ("M-s-," . point-stack-pop)          ;buffer索引跳转
   ("M-s-." . point-stack-push)         ;buffer索引标记
   ("M-G" . goto-column)                ;到指定列
   ("s-g" . goto-percent)    ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-I" . backward-indent) ;向后移动4个字符

   ("s-J" . scroll-up-one-line)         ;向上滚动一行
   ("s-K" . scroll-down-one-line)       ;向下滚动一行
   ;; ("<f2>" . refresh-file)              ;自动刷新文件
   ;; ("s-f" . find-file-root)             ;用root打开文件
   ;; ("s-r" . find-file-smb)              ;访问sambao
   )
 "basic-toolkit")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-global-keys
 '(
   ("C-o" . open-newline-above)         ;在上面一行新建一行
   ("C-l" . open-newline-below)         ;在下面一行新建一行
   )
 "open-newline")
(lazy-load-global-keys
 '(
   ("s-N" . move-text-down)      ;把光标所在的整行文字(或标记)下移一行
   ("s-P" . move-text-up)        ;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")
(lazy-load-global-keys
 '(
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-M-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-M-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("C-;" . comment-or-uncomment-region+)     ;注释当前行
   )
 "duplicate-line")
;; 快速删除光标左右的内容
(lazy-load-global-keys
 '(
   ("M-N" . delete-block-backward)
   ("M-M" . delete-block-forward))
 "delete-block")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ### Buffer Edit ###
;;; --- 缓存编辑
(lazy-load-set-keys
 '(
   ("C-x C-x" . exchange-point-and-mark)   ;交换当前点和标记点
   ("M-o" . backward-delete-char-untabify) ;向前删除字符
   ("C-M-S-h" . mark-paragraph)            ;选中段落
   ("M-SPC" . just-one-space)              ;只有一个空格在光标处
   ))
(lazy-load-global-keys
 '(
   ("C-/" . undo-tree-undo)             ;撤销
   ("C-?" . undo-tree-redo)             ;重做)
   )
 "undo-tree")
;;; ### Font ###
;;; --- 字体命令
(lazy-load-set-keys
 '(
   ("s--" . text-scale-decrease)        ;减小字体大小
   ("s-=" . text-scale-increase)        ;增加字体大小
   ))

 ;;; ### Multi-Scratch
(lazy-load-global-keys
 '(
   ("s-Q" . multi-scratch-new)
   ("C-S-s-q" . multi-scratch-new)
   )
 "multi-scratch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Window Operation ###
(lazy-load-global-keys
 '(
   ("s-;" . one-key-menu-window-navigation) ;快速窗口导航
   )
 "init-window")
(lazy-load-global-keys
 '(
   ("C-c j" . ace-window)
   )
 "init-ace-window")
;;; --- 窗口操作
(lazy-load-set-keys
 '(
   ("C-c v" . split-window-vertically)   ;纵向分割窗口
   ("C-c h" . split-window-horizontally) ;横向分割窗口
   ("C-c w" . kill-this-buffer)          ;关闭当前buffer
   ("C-c o" . delete-other-windows)      ;关闭其它窗口
   ))
(lazy-load-global-keys
 '(
   ("C-c V" . delete-other-windows-vertically+)   ;关闭上下的其他窗口
   ("C-c H" . delete-other-windows-horizontally+) ;关闭左右的其他窗口
   ("C-'" . delete-current-buffer-and-window) ;关闭当前buffer, 并关闭窗口
   ("C-\"" . delete-current-buffer-window)    ;删除当前buffer的窗口
   ("C-c i" . toggle-one-window)              ;切换一个窗口
   ("C-x O" . toggle-window-split)
   )
 "window-extension")
;;; ### Watch other window ###
;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("M-J" . watch-other-window-up)        ;向下滚动其他窗口
   ("M-K" . watch-other-window-down)      ;向上滚动其他窗口
   ("M-<" . watch-other-window-up-line)   ;向下滚动其他窗口一行
   ("M->" . watch-other-window-down-line) ;向上滚动其他窗口一行
   )
 "watch-other-window")
;; ### Buffer Name ###
;; --- 缓存名字
(lazy-load-global-keys
 '(
   ("C-M-;" . kill-other-window-buffer) ;关闭其他窗口的buffer
   )
 "buffer-extension")


;; 文件列表 TODO 目录设置
(lazy-load-global-keys
 '(
   ("C-S-s" . sr-speedbar-toggle)
   )
 "init-speedbar")
;;; ### Awesome-Tab ###
;;; --- 多标签浏览
(lazy-load-set-keys
 '(
   ("s-j" . awesome-tab-ace-jump)                  ;Ace jump
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
   ("s-a" . awesome-tab-kill-other-buffers-in-current-group)
   ("s-A" . awesome-tab-kill-all-buffers-in-current-group)
   ("s-w" . awesome-tab-keep-match-buffers-in-current-group)
   ("s-W" . awesome-tab-kill-match-buffers-in-current-group)
   )
 "awesome-tab")

;;; ### Functin key ###
;;; --- 功能函数
(lazy-load-set-keys
 '(
   ("<f5>" . emacs-session-save)        ;退出emacs
   ))
(lazy-load-global-keys
 '(
   ("C-4" . insert-changelog-date)      ;插入日志时间 (%Y/%m/%d)
   ("C-&" . switch-to-messages)         ;跳转到 *Messages* buffer
   )
 "lazycat-toolkit")

(lazy-load-global-keys
 '(
   ("C-7" . find-define-back)           ;返回查找符号的定义之前的位置
   ("C-8" . find-define)                ;查找符号的定义
   ("C-c 7" . dumb-jump-back)           ;dumb-jump 返回之前位置
   )
 "find-define")

;;; ### Awesome-Pair ###
;;; --- 结构化编程
(lazy-load-unset-keys
 '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b" "M-)")
 awesome-pair-mode-map)                 ;卸载按键
(defvar awesome-pair-key-alist nil)
(setq awesome-pair-key-alist
      '(
        ;; 移动
        ("M-n" . awesome-pair-jump-left)
        ("M-p" . awesome-pair-jump-right)
        ;; 符号插入
        ("%" . awesome-pair-match-paren)       ;括号跳转
        ("(" . awesome-pair-open-round)        ;智能 (
        ("[" . awesome-pair-open-bracket)      ;智能 [
        ("{" . awesome-pair-open-curly)        ;智能 {
        (")" . awesome-pair-close-round)       ;智能 )
        ("]" . awesome-pair-close-bracket)     ;智能 ]
        ("}" . awesome-pair-close-curly)       ;智能 }
        ("\"" . awesome-pair-double-quote)     ;智能 "
        ("=" . awesome-pair-equal)             ;智能 =
        ("SPC" . awesome-pair-space)           ;智能 Space
        ;; 删除
        ("M-o" . awesome-pair-backward-delete) ;向后删除
        ("C-d" . awesome-pair-forward-delete)  ;向前删除
        ("C-k" . awesome-pair-kill)            ;向前kill
        ;; 包围
        ("M-\"" . awesome-pair-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
        ("M-[" . awesome-pair-wrap-bracket)       ;用 [ ] 包围对象
        ("M-{" . awesome-pair-wrap-curly)         ;用 { } 包围对象
        ("M-(" . awesome-pair-wrap-round)         ;用 ( ) 包围对象
        ("M-)" . awesome-pair-unwrap)             ;去掉包围对象
        ;; 跳出并换行缩进
        ("M-:" . awesome-pair-jump-out-pair-and-newline) ;跳出括号并换行
        ))
(lazy-load-set-keys awesome-pair-key-alist awesome-pair-mode-map)

;;; ### Help ###
;;; --- 帮助模式
(lazy-load-global-keys
 '(
   ("C-h". one-key-menu-help)           ;帮助菜单
   )
 "init-help-mode")

;;; ### Thingh-edit ###
;;; --- 增强式编辑当前光标的对象
(lazy-load-global-keys
 '(
   ("C-c k" . one-key-menu-thing-edit)  ;thing-edit 菜单
   )
 "init-thing-edit"
 )

;;; ### Aweshell ###
;;; --- 多标签式的shell
(lazy-load-global-keys
 '(
   ("s-n" . aweshell-new)
   ("s-h" . aweshell-toggle)
   ("s-x s-x" . aweshell-dedicated-toggle)
   )
 "init-aweshell")

;;; multi-term`
(lazy-load-global-keys
 '(
   ("C-`" . multi-term)
   ("s-x n" . multi-term-dedicated-toggle)
   )
 "init-multiterm")


;; ### Dired;;  ###
;; --- Dired
(lazy-load-global-keys
 '(
   ("C-c d" . dired-jump)
   ("C-x C-f" . find-file)
   )
 "init-dired")

;; (lazy-load-global-keys
;;  '(
;;    ("C-c ," . grep-dired-dwim)
;;    ("C-c ." . grep-dired)
;;    )
;;  "init-dired")


;;; ### Ace jump ###
(lazy-load-global-keys
 '(
   ("s-<" . ace-jump-word-mode)
   ("s->" . ace-jump-char-mode)
   ("s-?" . ace-jump-line-mode)
   )
 "ace-jump-mode")

;;; ### Ielm ###
;;; --- Emacs Lisp 解释模式
(autoload 'ielm-map "ielm")
(lazy-load-global-keys
 '(
   ("M-s-i" . ielm-toggle)              ;切换ielm
   ("s-p" . insert-standard-date)
   )
 "lazycat-toolkit")

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
       ielm-map
       )
      )))


;;; ### Man ###
;;; --- Man
(lazy-load-global-keys
 '(
   ("<f1>" . woman))
 "init-woman")

;;; ### Ido ###
;;; --- 交互式管理文件和缓存
(lazy-load-set-keys
 '(
   ("C-x C-f" . ido-find-file)          ;交互式查找文件
   ("C-x b" . ido-switch-buffer)        ;交互式切换buffer
   ("C-x i" . ido-insert-buffer)        ;插入缓存
   ("C-x I" . ido-insert-file)          ;插入文件
   ))
(add-hook 'ido-setup-hook
          '(lambda ()
            (interactive)
            (ido-my-keys ido-completion-map)))
(defun ido-my-keys (keymap)
  "Add my keybindings for ido."
  (lazy-load-set-keys
   '(
     ("M-p" . ido-prev-match)              ;上一个匹配
     ("M-n" . ido-next-match)              ;下一个匹配
     ("M-h" . ido-next-work-directory)     ;下一个工作目录
     ("M-l" . ido-prev-work-directory)     ;上一个工作目录
     ("M-o" . backward-delete-char-untabify) ;向前删除字符
     ("M-O" . ido-delete-backward-updir)     ;删除字符或进入上一级目录
     )
   keymap
   ))

;;; Elisp
(lazy-load-set-keys
 '(
   ("RET" . comment-indent-new-line)    ;自动换行并注释
   )
 emacs-lisp-mode-map
 )

;; Dash.
(lazy-load-global-keys
 '(("y" . dash-at-point)
   )
 "dash-at-point"
 "C-x"
 )

;;; ### expand-region ###
(lazy-load-global-keys
 '(
   ("C-=" . er/expand-region))
 "expand-region")

;;; ### Magit ###
(lazy-load-global-keys
 '(
   ("s-x m" . one-key-menu-magit))
 "init-git")

;; M-x enhancement,列出最近、常用、其他命令
(lazy-load-global-keys
 '(
   ("M-x" . smex)
   ("C-c C-c M-x" . execute-extended-command)
   )
 "init-smex")

(provide 'init-key)

