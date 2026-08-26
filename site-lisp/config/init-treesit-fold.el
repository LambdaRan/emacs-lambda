;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'treesit-fold)

;; 注：曾经这里覆盖过 Lua 的 fold range alist 与 treesit-fold-range-lua-function/-lua-if，
;; 原因是当时 init-treesit.el 把 lua grammar 钉在 tree-sitter-lua 的 master 提交
;; a24dab1 上，该版本把节点重命名为 function_definition_statement / elseif_clause /
;; else_clause / for_numeric_statement / for_generic_statement。
;; 现在 grammar 已改钉 v0.5.0（经典节点名 function_declaration / elseif_statement /
;; else_statement / for_statement），与上游 `treesit-fold-parsers-lua' 完全一致，
;; 覆盖反而会让 `treesit-fold-close-all' 编译 query 时 signal treesit-query-error。
;; 故整体删除，不要复活。若将来再把 lua grammar 换回 master，需同步处理这里。

;; 折叠区域显示行数
(setq treesit-fold-line-count-show t)
;; (setq treesit-fold-line-count-format " <%d lines> ")
(global-treesit-fold-mode 1)

(lazy-load-set-keys
 '(("C-c /" . treesit-fold-toggle)
   ("C-c c c" . treesit-fold-close-all)
   ("C-c c o" . treesit-fold-open-all)))

(provide 'init-treesit-fold)
