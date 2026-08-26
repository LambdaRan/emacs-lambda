;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-lua.el --- Init lua

(require 'lua-ts-mode)

(setq lua-ts-indent-offset 4)

(add-hook 'lua-ts-mode-hook #'my-lua-ts-indent-fix)

(defun my-lua-ts-indent-fix ()
  "修复函数参数/形参换行后的缩进问题。
内置规则中 first-real-sibling-anchor 会将换行参数对齐到
行首 ( 的位置,当 ( 与函数名同行时导致缩进为 0。
此函数用 standalone-parent 覆盖这些规则,使参数相对于
父语句缩进一个 tab。"
  (let* ((lang-entry (car treesit-simple-indent-rules))
         (lang (car lang-entry))
         (old-rules (cdr lang-entry)))
    ;; 在已有规则之前插入新规则（优先匹配）
    ;; 覆盖所有直接父节点为 arguments/parameters 的情况，
    ;; 统一用 standalone-parent 缩进，避免 first-real-sibling-anchor
    ;; 把参数对齐到 ( 的位置（当 ( 与函数名同行时会导致缩进为 0）
    (setcar treesit-simple-indent-rules
            (cons lang
                  (append
                   `(((parent-is "arguments")
                      standalone-parent lua-ts-indent-offset)
                     ((parent-is "parameters")
                      standalone-parent lua-ts-indent-offset))
                   old-rules)))))

(add-hook 'lua-ts-mode-hook (lambda ()
                              (setq indent-tabs-mode t)
                              (setq tab-width 4)))

;; 通过 face-remap 让 treesit 新增 face 与 lua-mode 风格一致：

(add-hook 'lua-ts-mode-hook #'my-lua-ts-face-remap)

(defun my-lua-ts-face-remap ()
  "Remap treesit faces to match lua-mode color scheme."
  (face-remap-add-relative 'font-lock-operator-face      'font-lock-keyword-face)
  (face-remap-add-relative 'font-lock-bracket-face       'font-lock-keyword-face)
  (face-remap-add-relative 'font-lock-delimiter-face     'font-lock-keyword-face)
  (face-remap-add-relative 'font-lock-punctuation-face   'font-lock-keyword-face)
  (face-remap-add-relative 'font-lock-number-face        'font-lock-constant-face)
  (face-remap-add-relative 'font-lock-function-call-face 'font-lock-function-name-face)
  (face-remap-add-relative 'font-lock-variable-use-face  'font-lock-variable-name-face)
  (face-remap-add-relative 'font-lock-property-name-face 'font-lock-variable-name-face)
  (face-remap-add-relative 'font-lock-property-use-face  'font-lock-variable-name-face))

;; 在 lua buffer 中按 M-x treesit-inspect-mode，光标所在处会显示 AST 节点类型和字段名，据此编写 query。

;; ── 自定义高亮 ─────────────────────────────────────────────────────
;;
;; treesit 高亮由三层控制：
;;   1. treesit-font-lock-settings  — query 规则（AST 节点 → face）
;;   2. treesit-font-lock-feature-list — feature 分层列表（level 1~4）
;;   3. treesit-font-lock-level     — 当前启用的 level（默认 3）
;;
;; 自定义方法：
;;   A. 启用/禁用已有 feature → treesit-font-lock-recompute-features
;;   B. 添加新 query 规则     → treesit-add-font-lock-rules
;;   C. 自定义 face 颜色      → set-face-foreground / custom-set-faces

(add-hook 'lua-ts-mode-hook #'my-lua-ts-font-lock-setup)

(defun my-lua-ts-font-lock-setup ()
  "Customize treesit font-lock for lua-ts-mode."

  ;; ── A. 禁用/启用已有 feature ──────────────────────────────
  ;; 例：level 3 默认不开 operator，手动开启
  ;; (treesit-font-lock-recompute-features
  ;;  '(operator)       ; ADD-LIST: 强制启用
  ;;  nil)              ; REMOVE-LIST: 不禁用任何 feature
  ;; 如果要禁用某个 feature：
  ;; (treesit-font-lock-recompute-features nil '(punctuation))

  ;; ── B. 添加自定义 query 规则 ──────────────────────────────
  ;; 用 treesit-font-lock-rules 生成规则，再用 treesit-add-font-lock-rules 注入
  (treesit-add-font-lock-rules
   (treesit-font-lock-rules
    :default-language 'lua

    ;; 示例1: 高亮 self 关键字（self 在 AST 中是 identifier 节点，用 regexp-opt 匹配）
    :feature 'self
    `(((identifier) @font-lock-type-face
       (:match? ,(regexp-opt '("self") 'symbols)
               @font-lock-type-face)))

    ;; 示例2: 高亮 require 调用
    :feature 'require
    `(((identifier) @font-lock-preprocessor-face
       (:match? ,(regexp-opt '("require") 'symbols)
               @font-lock-preprocessor-face)))

    ;; 示例3: 高亮 method/dot 函数声明中的表名（如 function CServerPlayer:Method）
    :feature 'definition
    :override nil
    '((function_declaration
       name: (method_index_expression
              table: (identifier) @font-lock-type-face))
      (function_declaration
       name: (dot_index_expression
              table: (identifier) @font-lock-type-face)))

    ;; 高亮 goto label（::name::）
    :feature 'label
    :override t
    '((label_statement) @font-lock-constant-face)
    ))

  ;; 把新 feature 加到 feature-list 对应 level 中
  ;; 这里把 require 放 level 2，self/label 放 level 3
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string require)
                (builtin constant function number self label)
                (assignment bracket delimiter escape operator property punctuation variable)))
  ;; 重新计算生效的 feature
  (treesit-font-lock-recompute-features))

(provide 'init-lua)
