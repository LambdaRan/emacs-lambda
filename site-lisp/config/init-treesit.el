;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'treesit)

;; 执行以下函数确定ABI版本，当前是14
;; (treesit-library-abi-version)
;; Grammar 的 `parser.c` 中 `LANGUAGE_VERSION` 必须与此匹配，否则报 `version-mismatch`。

;; Customize treesit grammer load path.
;; (setq treesit-extra-load-path (list (concat lazycat-emacs-root-dir "/treesit-grammer")))

;; Make sure `treesit-install-language-grammar' download library file at `treesit-extra-load-path'
;; (setq treesit--install-language-grammar-out-dir-history treesit-extra-load-path)

;; M-x `treesit-install-language-grammar` to install language grammar.
;; (LANG . (URL REVISION SOURCE-DIR CC C++))
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.24.0"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil)
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.3"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src" nil nil)
        (heex . ("https://github.com/phoenixframework/tree-sitter-heex" "main" "src" nil nil))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "a24dab1"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.0" "tree-sitter-markdown-inline/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.5"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.23.5" "php/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.23.0"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src" nil nil)
        (toml "https://github.com/tree-sitter/tree-sitter-toml" "master" "src" nil nil)
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
        (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
        (mojo . ("https://github.com/HerringtonDarkholme/tree-sitter-mojo"))))

;; ── major-mode-remap-alist ──────────────────────────────────────
;; 所有内置 ts-mode 的 auto-mode-alist 注册都是条件的（treesit-ready-p），
;; 因此必须通过 remap 确保：grammar 可用时用 ts-mode，不可用时回退到原 mode
(setq major-mode-remap-alist
      '(;; --- progmodes/ ---
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (elixir-mode     . elixir-ts-mode)
        (heex-mode       . heex-ts-mode)
        (java-mode       . java-ts-mode)
        (js-json-mode    . json-ts-mode)
        (lua-mode        . lua-ts-mode)
        (ruby-mode       . ruby-ts-mode)
        (typescript-mode . typescript-ts-mode)
        ;; --- textmodes/ ---
        (conf-toml-mode  . toml-ts-mode)
        (html-mode       . html-ts-mode)
        (yaml-mode       . yaml-ts-mode)))

;; ── 高亮级别（1=注释+定义 2=+关键字+字符串 3=+赋值+内置+数字 4=全部）──
(setq treesit-font-lock-level 3)

;; ── 手动创建 treesit parser（无内置 ts-mode 的语言）──────────────
;; 这些语言没有 Emacs 内置的 *-ts-mode，通过 hook 在原 mode 中启用 treesit 语法高亮
(add-hook 'zig-mode-hook #'(lambda () (treesit-parser-create 'zig)))
(add-hook 'mojo-mode-hook #'(lambda () (treesit-parser-create 'mojo)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'haskell-mode-hook #'(lambda () (treesit-parser-create 'haskell)))
(add-hook 'kotlin-mode-hook #'(lambda () (treesit-parser-create 'kotlin)))

;; web-mode 中为 vue/html/php 启用 treesit parser（web-mode 本身无 ts 替代）
(add-hook 'web-mode-hook #'(lambda ()
                             (let ((file-name (buffer-file-name)))
                               (when-let* ((file-name file-name)
                                           (lang (pcase (file-name-extension file-name)
                                                   ("vue" 'vue)
                                                   ("html" 'html)
                                                   ("php" 'php))))
                                 (treesit-parser-create lang)))))

;; ── defun 导航性能修复 ──────────────────────────────────────────
;; Emacs 30 的 treesit--thing-sibling 在深层嵌套/大文件上极慢（Windows 尤甚），
;; 导致 beginning-of-defun / end-of-defun 卡死。
;; 影响: fingertip, expand-region (er/mark-defun), mark-defun 等所有调用
;;       defun 导航的功能。
;; 修复: 用语法表 + 缩进方式替代 treesit 的 defun 遍历。

(defun my-treesit--defun-start-braces ()
  "用语法表找 defun 起点（大括号语言）。
通过 syntax-ppss 的嵌套深度，逐层 up-list 跳出到最外层 {。"
  (let ((forward-sexp-function nil))  ; 强制用语法表，不走 treesit
    (let ((depth (car (syntax-ppss)))
          (moved nil))
      (while (> depth 0)
        (if (ignore-errors (up-list -1))
            (setq moved t depth (car (syntax-ppss)))
          (setq depth 0)))
      (when (and moved (eq (char-after) ?\{))
        (forward-line -1)
        (skip-chars-forward " \t"))
      moved)))

(defun my-treesit--defun-start-indent ()
  "用缩进找 defun 起点（Python / Ruby 等缩进语言）。
从当前位置向前搜索 def/class 关键字，且缩进比当前行浅。"
  (let ((orig-indent (save-excursion
                       (back-to-indentation)
                       (current-column)))
        (found nil))
    (when (> orig-indent 0)
      (while (and (not found)
                  (re-search-backward
                   "^[ \t]*\\(def\\|class\\)\\_>" nil t))
        (let ((ind (save-excursion
                     (back-to-indentation)
                     (current-column))))
          (when (< ind orig-indent)
            (setq found t)))))
    found))

(defun my-treesit-beginning-of-defun (&optional arg)
  "替代 treesit-beginning-of-defun，避免 treesit--thing-sibling 性能问题。
大括号语言用语法表；缩进语言用关键字+缩进检测。"
  (let ((arg (or arg 1)))
    (cond
     ((< arg 0) (my-treesit-end-of-defun (- arg)))
     (t
      (while (> arg 0)
        (let ((orig (point)))
          ;; 先退到行首，避免停留在当前 defun 头部
          (beginning-of-line)
          (unless (or (my-treesit--defun-start-braces)
                      (my-treesit--defun-start-indent))
            ;; 两种方法都没找到，回到原位
            (goto-char orig))
        (setq arg (1- arg))))))))

(defun my-treesit-end-of-defun (&optional arg)
  "替代 treesit-end-of-defun，避免 treesit--thing-sibling 性能问题。
大括号语言用 syntax-ppss + forward-sexp 找匹配 }。"
  (let ((forward-sexp-function nil))  ; 强制用语法表，不走 treesit
    (let ((arg (or arg 1)))
      (cond
       ((< arg 0) (my-treesit-beginning-of-defun (- arg)))
       (t
        (while (> arg 0)
          (let* ((state (syntax-ppss))
                 (depth (car state))
                 (orig (point)))
            (cond
             ((> depth 0)
              ;; 在嵌套结构中：先跳到最外层，再 forward-sexp 到匹配的 }
              (catch 'done
                (while (> (car (syntax-ppss)) 0)
                  (unless (ignore-errors (up-list -1))
                    (throw 'done nil))))
              (or (ignore-errors (forward-sexp 1))
                  (goto-char (point-max))))
             (t
              ;; 顶层：跳到下一个 defun 之前（缩进语言的 def/class）
              (or (re-search-forward "^\\(def\\|class\\)\\_>" nil t)
                  (goto-char (point-max)))
              (when (< (point) (point-max))
                (forward-line -1)
                (end-of-line)))))
          (setq arg (1- arg))))))))

(advice-add 'treesit-beginning-of-defun :override #'my-treesit-beginning-of-defun)
(advice-add 'treesit-end-of-defun :override #'my-treesit-end-of-defun)

(provide 'init-treesit)
