;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-hideshow.el --- 代码折叠

(require 'hideshow)

;; ts 模式折叠边界来自语法树：`treesit-thing-settings' 的 `list' thing
;; → `forward-list-function' → `hs-forward-sexp-function'，覆盖 c/c++/lua/python/
;; json/yaml/go/rust/typescript 等 ts-mode 与 emacs-lisp-mode；web-mode 不支持。
;; 折叠命令作用于当前行所属的最内层块，`hs-hide-block-behavior' 设为 `after-point'
;; 可改为折叠 point 之后的块。

(setq hs-display-lines-hidden t)            ;省略号旁显示隐藏行数
(setq hs-hide-comments-when-hiding-all t)   ;折叠全部时一并折叠注释块

;; 必须保持 nil：非 nil 会 jit-lock 逐行扫描找块首，是 hideshow 唯一的常驻开销
(setq hs-show-indicators nil)
(setq hs-indicator-type 'fringe)            ;`fringe' / `margin' / nil（行尾）

;; 全局开启：`hs-minor-mode' 开启不扫描缓冲区，指示器关闭时不注册常驻钩子，
;; 耗时只在按下折叠键时发生。
;; 守卫不可省：comment-start 为 nil 的 mode（fundamental/text/dired）下 hideshow
;; 会拒绝开启并 message 刷屏；也不能只挂 prog-mode-hook——yaml-ts-mode 派生自
;; text-mode，conf-mode 二者都不派生。
(defun my-hs-enable ()
  "在支持 hideshow 的 buffer 中开启 `hs-minor-mode'。"
  (when (and comment-start comment-end (not (minibufferp)))
    (hs-minor-mode 1)))

(define-globalized-minor-mode my-global-hs-minor-mode
  hs-minor-mode my-hs-enable
  :group 'hideshow)

(my-global-hs-minor-mode 1)

(lazy-load-set-keys
 '(("C-c /"   . hs-toggle-hiding)   ;切换当前块
   ("C-c c c" . hs-hide-all)        ;折叠全部
   ("C-c c o" . hs-show-all)        ;展开全部
   ("C-c c t" . hs-cycle)))         ;父块/仅子块/全展开 三态循环

(provide 'init-hideshow)

;;; init-hideshow.el ends here
