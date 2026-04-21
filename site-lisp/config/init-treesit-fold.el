;;; init-treesit-fold.el --- Config for treesit   -*- lexical-binding: t; -*-

(require 'treesit-fold)

;; 修复 Lua tree-sitter 新版 grammar 节点类型不匹配问题
;; 新版 grammar 节点名称: function_definition_statement, elseif_clause,
;; else_clause, for_numeric_statement, for_generic_statement
(with-eval-after-load 'treesit-fold
  ;; 覆盖 Lua 的 fold range alist，适配新版 grammar
  (setf (alist-get 'lua-ts-mode treesit-fold-range-alist)
        '((expression_list              . treesit-fold-range-seq)
          (function_definition_statement . treesit-fold-range-lua-function)
          (if_statement                  . treesit-fold-range-lua-if)
          (elseif_clause                 . treesit-fold-range-lua-elseif)
          (else_clause                   . treesit-fold-range-lua-else)
          (while_statement               . treesit-fold-range-lua-do-loop)
          (for_numeric_statement         . treesit-fold-range-lua-do-loop)
          (for_generic_statement         . treesit-fold-range-lua-do-loop)
          (repeat_statement              . treesit-fold-range-lua-repeat)
          (comment                       . treesit-fold-range-lua-comment)))
  (setf (alist-get 'lua-mode treesit-fold-range-alist)
        (alist-get 'lua-ts-mode treesit-fold-range-alist))

  ;; 修复 treesit-fold-range-lua-function: 新版 grammar 的
  ;; function_definition_statement 可能没有 "parameters" 字段
  (defun treesit-fold-range-lua-function (node offset)
    "Define fold range for Lua `function' declaration.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
    (when-let* ((params (or (treesit-node-child-by-field-name node "parameters")
                            (car (treesit-fold-find-children node ")"))))
                (beg (treesit-node-end params))
                (end (- (treesit-node-end node) 3)))
      (when treesit-fold-on-next-line
        (setq end (treesit-fold--last-eol end)))
      (treesit-fold--cons-add (cons beg end) offset)))

  ;; 修复 treesit-fold-range-lua-if: 子节点类型适配新版 grammar
  (defun treesit-fold-range-lua-if (node offset)
    "Define fold range for Lua `if' statement.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
    (let* ((then (car (treesit-fold-find-children node "then")))
           (beg (treesit-node-end then))
           (next (or (treesit-fold-find-children node "elseif_clause")
                     (treesit-fold-find-children node "else_clause")))
           (end (if next
                    (treesit-node-start (car next))
                  (- (treesit-node-end node) 3))))
      (when treesit-fold-on-next-line
        (setq end (treesit-fold--last-eol end)))
      (treesit-fold--cons-add (cons beg end) offset))))

;; 折叠区域显示行数
(setq treesit-fold-line-count-show t)
;; (setq treesit-fold-line-count-format " <%d lines> ")
(global-treesit-fold-mode 1)

(lazy-load-set-keys
 '(("C-c /" . treesit-fold-toggle)
   ("C-c c c" . treesit-fold-close-all)
   ("C-c c o" . treesit-fold-open-all)))

(provide 'init-treesit-fold)
