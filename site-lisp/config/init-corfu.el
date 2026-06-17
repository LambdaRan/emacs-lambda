;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-corfu.el --- Corfu + Cape in-buffer completion configuration

(require 'corfu)
(require 'corfu-auto)
(require 'corfu-popupinfo)
(require 'corfu-history)
(require 'corfu-indexed)
(require 'corfu-info)

(require 'cape)
(require 'cape-keyword)

;;; Corfu 基本设置（对标原 company-mode 配置）
(setq corfu-auto t                     ; 自动弹出补全
      corfu-auto-delay 0.2             ; 对标 company-idle-delay 0.2
      corfu-auto-prefix 2              ; 对标 company-minimum-prefix-length 2
      corfu-cycle t                    ; 对标 company-selection-wrap-around
      corfu-preselect 'first           ; 预选第一项，对标 company-tng 行为
      corfu-count 14)                  ; 弹窗显示的候选数量

;; 弹窗宽高
(setq corfu-min-width 40
      corfu-max-width 80)

;;; 启用扩展
(global-corfu-mode 1)
(corfu-popupinfo-mode 1)               ; 文档预览弹窗（悬停显示）
(corfu-history-mode 1)                 ; 历史/频率排序
(corfu-indexed-mode 1)                 ; 数字索引选择（M-1~M-9 对标 company-show-quick-access）

;;; 按键映射（对标 company-tng：TAB 直接插入选中项）
;; 先清空 corfu-map 默认绑定再重绑，避免未覆盖的默认键残留。
;; corfu-map 默认含：M-n/M-p=next/prev, RET=insert, TAB=complete, M-TAB=expand,
;; M-g=info-location, M-h=info-documentation, M-SPC=insert-separator。
;; （corfu-indexed-mode 的 M-1~M-9 在上方已启用，不在此清除。）
(dolist (key '("M-n" "M-p" "RET" "TAB" "M-TAB" "M-g" "M-h" "M-SPC"))
  (define-key corfu-map (kbd key) nil))

(define-key corfu-map (kbd "TAB")   #'corfu-insert)        ; 对标 company-complete-selection
(define-key corfu-map (kbd "<tab>") #'corfu-insert)
(define-key corfu-map (kbd "C-h")   #'corfu-complete)       ; 对标 company-complete-common（补全公共前缀）
(define-key corfu-map (kbd "RET")   #'corfu-insert)        ; RET 同样插入
(define-key corfu-map (kbd "C-n")   #'corfu-next)           ; 对标原 company C-n
(define-key corfu-map (kbd "C-p")   #'corfu-previous)       ; 对标原 company C-p
(define-key corfu-map (kbd "M-w")   #'corfu-info-location)  ; 对标 company-show-location
(define-key corfu-map (kbd "M-SPC") #'corfu-insert-separator) ; Orderless 多词匹配

;;; 排除模式（对标 company-global-modes）
;; global-corfu-mode 没有排除列表，需在特定 mode-hook 中手动禁用
(defun my-corfu-disable-in-modes ()
  "在不需要补全的模式中禁用 corfu。"
  (corfu-mode -1))

(dolist (hook '(shell-mode-hook
                eshell-mode-hook
                comint-mode-hook
                erc-mode-hook
                gud-mode-hook
                rcirc-mode-hook
                text-mode-hook))
  (add-hook hook #'my-corfu-disable-in-modes))

;; minibuffer 中禁用 corfu
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (when (eq (current-buffer) (window-buffer (minibuffer-window)))
              (corfu-mode -1))))

;;; Cape 补全源（替代 company backends）

;; 全局默认 Capf
(setq-default completion-at-point-functions
              (list #'cape-dabbrev
                    #'cape-file
                    #'cape-keyword))

;; Dabbrev 设置（对标原 company-dabbrev 配置）
(setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))

;; 编程模式的 Capf 合并（fastctags + cape-dabbrev）在 init-fastctags.el 中配置，
;; 以确保 fastctags-completion-at-point 一定可用（避免增量加载顺序问题）。

;; cape-line 按需整行补全
;; cape-line 补全起点为行首(pos-bol)，与 cape-dabbrev/fastctags（词/符号起点）不同，
;; 无法并入 cape-capf-super（会被静默丢弃），故绑成全局按键按需触发。
(global-set-key (kbd "C-c M-l") #'cape-line)

(provide 'init-corfu)

;;; init-corfu.el ends here
