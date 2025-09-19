;;; lua-ts-mode.el --- tree sitter support for Lua  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: John Muhl <jm@pub.pink>
;; Maintainer: John Muhl <jm@pub.pink>
;; Created: June 27, 2023
;; Version: 1.0
;; Keywords: lua languages tree-sitter
;; URL: https://git.sr.ht/~johnmuhl/lua-ts-mode
;; Package-Requires: ((emacs "29.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides ‘lua-ts-mode’ which is a major mode for
;; editing Lua files that uses Tree Sitter to parse the language.
;;
;; This package is compatible with and tested against the grammar
;; for Lua found at https://github.com/MunifTanjim/tree-sitter-lua
;;
;; With Git, a C compiler and linker in PATH you can install it by
;; running:
;;
;;     M-x treesit-install-language-grammar RET lua RET y
;;     https://github.com/MunifTanjim/tree-sitter-lua RET RET RET RET RET
;;
;; To automatically enable it when you open a Lua file add the following
;; to your ‘user-init-file’:
;;
;;     (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

;;; News

;; Version 1.0 <2023-07-11 Tue>
;; Supports font-lock, indentation, navigation, imenu,
;; which-function-mode and outline-minor-mode.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defcustom lua-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `lua-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'lua)

(defvar lua-ts-mode--builtins
  '("assert" "collectgarbage" "coroutine" "debug" "dofile"
    "error" "getmetatable" "io" "ipairs" "load" "loadfile"
    "math" "next" "os" "package" "pairs" "pcall" "print"
    "rawequal" "rawget" "rawlen" "rawset" "require" "select"
    "setmetatable" "string" "table" "tonumber" "tostring"
    "type" "utf8" "warn" "xpcall")
  "Lua built-in functions for tree-sitter font-locking.")

(defvar lua-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+  "."    table)
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?%  "."    table)
    (modify-syntax-entry ?^  "."    table)
    (modify-syntax-entry ?~  "."    table)
    (modify-syntax-entry ?<  "."    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?/  "."    table)
    (modify-syntax-entry ?*  "."    table)
    (modify-syntax-entry ?\n ">"    table)
    (modify-syntax-entry ?\' "\""   table)
    (modify-syntax-entry ?\" "\""   table)
    table)
  "Syntax table for `lua-ts-mode'.")

(defvar lua-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'lua
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'lua
   :feature 'builtin
   `(((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string
                `(seq bol (or ,@lua-ts-mode--builtins) eol))
              @font-lock-builtin-face)))

   :language 'lua
   :feature 'delimiter
   '(["," ";"] @font-lock-delimiter-face)

   :language 'lua
   :feature 'escape
   '((escape_sequence) @font-lock-escape-face)

   :language 'lua
   :feature 'function
   '((function_call name: (identifier) @font-lock-function-call-face)
     (function_call
      name: (method_index_expression
             method: (identifier) @font-lock-function-call-face))
     (function_call
	  name: (dot_index_expression
             table: (identifier) @font-lock-function-call-face)))

   :language 'lua
   :feature 'constant
   '((variable_list
      attribute: (attribute (["<" ">"] (identifier))))
     @font-lock-constant-face)

   :language 'lua
   :feature 'operator
   '(["and" "not" "or" "+" "-" "*" "/" "%" "^"
      "#" "==" "~=" "<=" ">=" "<" ">" "=" "&"
      "~" "|" "<<" ">>" "//" ".."]
     @font-lock-operator-face
     (vararg_expression) @font-lock-operator-face)

   :language 'lua
   :feature 'property
   '((field name: (identifier) @font-lock-property-name-face)
     (dot_index_expression
      field: (identifier) @font-lock-property-use-face))

   :language 'lua
   :feature 'punctuation
   '(["." ":"] @font-lock-punctuation-face)

   :language 'lua
   :feature 'variable
   '((function_call
      arguments: (arguments (identifier))
      @font-lock-variable-use-face)
     (function_call
      name: (method_index_expression
             table: (identifier) @font-lock-variable-use-face))
     (goto_statement (identifier) @font-lock-variable-use-face))

   :language 'lua
   :feature 'assignment
   '((variable_list (identifier) @font-lock-variable-name-face))

   :language 'lua
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'lua
   :feature 'keyword
   '((break_statement) @font-lock-keyword-face
     (true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (nil) @font-lock-constant-face
     ["and" "do" "else" "elseif" "end" "for" "function"
      "goto" "if" "in" "local" "not" "or" "repeat"
      "return" "then" "until" "while"]
     @font-lock-keyword-face)

   :language 'lua
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'lua
   :feature 'comment
   '((comment) @font-lock-comment-face
     (hash_bang_line) @font-lock-comment-face)

   :language 'lua
   :feature 'definition
   '((function_declaration
      name: (identifier) @font-lock-function-name-face)
     (parameters
      name: (identifier) @font-lock-variable-name-face)
     (label_statement) @font-lock-variable-name-face)

   :language 'lua
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `lua-ts-mode'.")

(defvar lua-ts-mode--indent-rules
  `((lua
     ((parent-is "chunk") column-0 0)
     ((node-is "comment_end") column-0 0)
     ((parent-is "block") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "else_statement") parent-bol 0)
     ((node-is "elseif_statement") parent-bol 0)
     ((node-is "end") parent-bol 0)
     ((node-is "until") parent-bol 0)
     ((parent-is "for_statement") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "function_declaration") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "function_definition") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "else_statement") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "repeat_statement") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "while_statement") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "table_constructor") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "parameters") parent-bol lua-ts-mode-indent-offset)
     ((parent-is "ERROR") no-indent 0))))

(defun lua-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "function_declaration" "function_definition")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("variable_declaration"
     (let ((child (treesit-node-child-by-field-name node "name")))
       (if child (treesit-node-text child t)
         (treesit-node-text
          (treesit-node-child-by-field-name
           (treesit-search-subtree node "assignment_statement" nil nil 1)
           "name")
          t))))
    ("field"
     (and (treesit-search-subtree node "function_definition" nil nil 1)
          (treesit-node-text
           (treesit-node-child-by-field-name node "name") t)))))

;;;###autoload
(define-derived-mode lua-ts-mode prog-mode "Lua"
  "Major mode for editing Lua, powered by tree-sitter."
  :group 'lua
  :syntax-table lua-ts-mode--syntax-table

  (when (treesit-ready-p 'lua)
    (treesit-parser-create 'lua)

    (setq-local treesit-defun-prefer-top-level t)

    ;; Comments.
    (setq-local comment-start "--")
    (setq-local comment-start-skip (rx "--" (* (syntax whitespace))))
    (setq-local comment-end "")

    ;; Font-lock.
    (setq-local treesit-font-lock-settings lua-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (builtin keyword string)
                  (assignment constant number)
                  (bracket
                   delimiter
                   escape
                   function
                   operator
                   property
                   punctuation
                   variable)))

    ;; Indent.
    (setq-local treesit-simple-indent-rules lua-ts-mode--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-name-function #'lua-ts-mode--defun-name)
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("function_declaration"
                              "function_definition")))
    (setq-local treesit-sentence-type-regexp
                (regexp-opt '("do_statement"
                              "while_statement"
                              "repeat_statement"
                              "if_statement"
                              "for_statement"
                              "variable_declaration")))
    (setq-local treesit-sexp-type-regexp
                (regexp-opt '("arguments"
                              "comment"
                              "string"
                              "table_constructor")))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Variable" "\\`variable_declaration\\'" nil nil)
                  ("Function" ,(rx bos (or "function_declaration"
                                           "function_definition"
                                           "field")
                                   eos)
                   nil nil)))

    ;; Which-function.
    (setq-local which-func-functions (treesit-defun-at-point))

    ;; Outline.
    (setq-local outline-regexp
                (regexp-opt '("do" "for" "function" "local"
                              "if" "repeat" "while" "--[[")))

    (treesit-major-mode-setup)))

;; (if (treesit-ready-p 'lua)
;;     (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode)))

(provide 'lua-ts-mode)

;;; lua-ts-mode.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: t
;; End:
