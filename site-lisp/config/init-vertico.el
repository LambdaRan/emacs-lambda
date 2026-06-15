;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Compat — 必须先于 vertico/consult/marginalia 加载
;; 强制使用 extensions/ 下的新版 compat，避免加载 Emacs 内置旧版
(let ((compat-dir (expand-file-name "extensions/compat" my-emacs-root-dir)))
  (when (file-directory-p compat-dir)
    (add-to-list 'load-path compat-dir)
    ;; 清除内置 compat 的 feature 标记，强制重新加载新版
    (setq features (delq 'compat features))
    (require 'compat)))

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
(setq consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=512 --no-heading --line-number -i")

;; M-x 自动添加 ^ 前缀，始终从命令名开头匹配
(defvar my-mx-anchored t "M-x 是否默认从命令名开头匹配。")
(define-advice read-extended-command (:around (orig-fn &rest args) my-anchor-prefix)
  (if my-mx-anchored
      (minibuffer-with-setup-hook (lambda () (insert "^"))
        (apply orig-fn args))
    (apply orig-fn args)))

;; consult-buffer 禁用实时预览，确认选中后才切换
(consult-customize consult-buffer :preview-key nil)

;;; 外观调整
(custom-set-faces
 ;; 选择高亮：深蓝背景，不延伸到窗口右边缘
 '(vertico-current ((t (:background "#2257A0" :foreground "#FAFAFA" :extend nil))))
 ;; 未打开的文件（recentf 历史）显示为深灰 + 斜体
 '(consult-file ((t (:foreground "#595959" :slant italic)))))

(provide 'init-vertico)
