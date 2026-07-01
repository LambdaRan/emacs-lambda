;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Compat — 必须先于 vertico/consult/marginalia 加载
;; site-start 已把 extensions/compat 加入 load-path（末尾）。这里把它前置到
;; load-path 最前，确保 vertico/consult/corfu 等优先 require 到 extensions 版本，
;; 而非任何先于本文件加载的其它 compat。若已有 compat 被加载（feature 标记存在），
;; 先清除标记再强制重载 extensions 版本，保证版本一致。
;; 注：Emacs 并不内置 compat；本块是优先级前置 + 一致性保障，不做版本号比较。
(let ((ext-compat (expand-file-name "extensions/compat" my-emacs-root-dir)))
  (when (file-directory-p ext-compat)
    (add-to-list 'load-path ext-compat)        ; 前置：优先于 load-path 中的其它 compat
    (when (featurep 'compat)                   ; 已加载过 compat：清标记后重载 extensions 版本
      (setq features (delq 'compat features)))
    (load "compat" nil t)))

;;; Vertico — 垂直候选 UI
(require 'vertico)
(vertico-mode 1)
(setq vertico-count 15)
(setq vertico-resize nil)
(setq vertico-cycle t)

;;; Orderless — 空格分隔无序匹配
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))
                                      (buffer (styles partial-completion))))

;;; Savehist — 历史持久化
(require 'savehist)
(savehist-mode 1)
(setq history-length 500)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)

;;; Recentf — 记录最近打开的文件（consult-buffer 依赖）
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 200)

;;; Marginalia — 候选注解
(require 'marginalia)
(marginalia-mode 1)

;;; Consult 配置
(require 'consult)
;; --with-filename / --path-separator / / --search-zip 须与 consult 默认保持一致：
;; consult 的候选解析正则要求每行以 "文件名\0行号" 开头，缺 --with-filename 时单文件/stdin
;; 搜索结果会被静默丢弃；--path-separator / 用于 Windows 路径分隔。-i 始终忽略大小写。
(setq consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=512 --path-separator / --no-heading --with-filename --line-number --search-zip -i")

;; consult-buffer 禁用实时预览，确认选中后才切换
(consult-customize consult-buffer :preview-key nil)

;;; Vertico Multiform — 按命令定制排序等属性
(require 'vertico-multiform)
(require 'vertico-sort)
;; vertico-sort.el 在 vertico.el 之后加载，defcustom 的 fboundp 检查已返回 nil，
;; 必须显式设置排序函数，否则 vertico 不排序（候选按 completion table 原始顺序）。
(setq vertico-sort-function #'vertico-sort-history-length-alpha)
;; consult-buffer 关闭重新排序，保留 buffer 自带的最近访问顺序
(setq vertico-multiform-commands
      '((consult-buffer (vertico-sort-function . nil))))
(vertico-multiform-mode 1)

;;; 外观调整
(custom-set-faces
 ;; 选择高亮：深蓝背景 + distant-foreground 确保所有候选文字在选中时清晰可读
 '(vertico-current ((t (:background "#2257A0" :foreground "#FAFAFA" :distant-foreground "#FAFAFA" :extend nil))))
 ;; 未打开的文件（recentf 历史）：浅灰蓝 + 斜体，与主文字 #383a42 明显区分
 '(consult-file ((t (:foreground "#73797e" :slant italic)))))

(provide 'init-vertico)
