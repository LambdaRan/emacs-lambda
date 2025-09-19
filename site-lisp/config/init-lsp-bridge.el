;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Require
(require 'lsp-bridge)
(require 'lazy-load)

;; 关闭代码诊断
(setq lsp-bridge-enable-diagnostics nil)
;; 索引打开文件的单词， 默认打开
;; (setq lsp-bridge-enable-search-words t)
;;  支持函数参数显示， 默认打开
(setq lsp-bridge-enable-signature-help nil)
;; 针对非 LSP 后端提供 capf 补全支持， 默认是关闭的
;; (setq acm-enable-capf t)
;; 补全菜单是否显示帮助文档
(setq acm-enable-doc nil)
;; 补全菜单是否显示图标 
;; (setq acm-enable-icon t)
;; 对补全文档中的 Markdown 内容进行语法着色
(setq acm-enable-doc-markdown-render nil)

(setq acm-enable-ctags nil)
;; 是否打开 tabnine 补全支持， 默认打开
(setq acm-enable-tabnine nil)
;; 是否打开 Codeium 补全支持
(setq acm-enable-codeium t)
;; 是否打开 Copilot 补全支持
(setq acm-enable-copilot nil)
;; 补全菜单是否显示打开文件的单词， 默认打开
(setq acm-enable-search-file-words t)
;; 是否在图标后面显示索引， 通过 Alt + Number 来快速选择候选词， 默认关闭
(setq acm-enable-quick-access t)
;; 是否用数字键快速选择候选词， 默认关闭
(setq acm-quick-access-use-number-select t)
;; yasnippet 补全， 默认打开
(setq acm-enable-yas nil)
;; citre(ctags) 补全， 默认关闭
(setq acm-enable-citre nil)
;; LSP 补全最小的触发字符数, 默认是 0
(setq acm-backend-lsp-candidate-min-length 10)
;; YaSnippet 补全最小的触发字符数, 默认是 0
(setq acm-backend-yas-candidate-min-length 2)
;; Search Words 补全最小的触发字符数, 默认是 0
(setq acm-backend-search-file-words-candidate-min-length 1)
;; Codeium 补全最小的触发字符数, 默认是 0
(setq acm-backend-codeium-candidate-min-length 2)

(setq lsp-bridge-lua-lsp-server nil)
;; 调试相关
(setq lsp-bridge-enable-log nil)
(setq lsp-bridge-enable-debug nil)
;; 按键
(lazy-load-unset-keys
 '("RET" "C-h")
 acm-mode-map)
(lazy-load-set-keys
 '(
   ("TAB" . acm-complete)
   ("C-h" . acm-insert-common)
   ("M-S" . acm-filter)
   ("C-n" . acm-select-next)
   ("C-p" . acm-select-prev)
   ("RET" . acm-complete)
   )
 acm-mode-map)

(global-lsp-bridge-mode)

(provide 'init-lsp-bridge)
