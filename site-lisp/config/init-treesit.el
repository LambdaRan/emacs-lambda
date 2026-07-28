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
        (markdown . ("https://github.com/tree-sitter/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter/tree-sitter-markdown" "v0.4.0" "tree-sitter-markdown-inline/src"))
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
;;
;; 策略：设置 beginning-of-defun-function / end-of-defun-function（buffer-local），
;; 不 advise treesit-beginning/end-of-defun 本身。
;; 这样 treesit 原始函数保持不变，作为安全 fallback，不会递归。
;;
;; 快速路径:
;;   - 大括号语言 ({})：syntax-ppss + up-list
;;   - def/class 缩进语言 (Python/Ruby)：关键字 + 缩进
;; Fallback:
;;   - 其他语言 (Lua function/end, Haskell, ...)：调用 treesit 原始函数

(defun my-treesit--defun-start-braces ()
  "用语法表找 defun 起点（大括号语言）。
处理两种情况：
- 嵌套在 {} 内部：逐层 up-list 到最外层 {
- 顶层 (depth 0)：从当前行向前扫描 {
仅在找到 {} 时返回 t，非大括号语言返回 nil。"
  (let* ((forward-sexp-function nil)     ; 强制用语法表，不走 treesit
         (orig-depth (car (syntax-ppss)))
         (moved nil)
         (depth orig-depth))
    (cond
     ;; 嵌套在 {} 内部：逐层跳出到最外层
     ((> depth 0)
      (while (> depth 0)
        (if (ignore-errors (up-list -1))
            (setq moved t depth (car (syntax-ppss)))
          (setq depth 0)))
      (when (and moved (eq (char-after) ?\{))
        ;; { 可能在声明同行或单独一行，取靠前位置
        (let ((brace-line (line-number-at-pos))
              (brace-col (current-column)))
          (forward-line -1)
          (skip-chars-forward " \t")
          ;; 如果上一行是空行或缩进比 { 深，说明声明与 { 同行
          (when (or (looking-at-p "[ \t]*$")
                    (> (current-column) brace-col))
            (goto-char (line-beginning-position (+ brace-line 1)))))))
     ;; 顶层 (depth 0)：向前扫描找函数声明行附近的 {
     (t
      (let ((brace-pos nil))
        (save-excursion
          (catch 'done
            (dotimes (_ 6)              ; 最多向前扫描 5 行
              (let ((d (car (syntax-ppss))))
                (cond
                 ((> d 0)               ; 进入 {} 了
                  (when (re-search-forward "{" (line-end-position) t)
                    (setq brace-pos (1- (point))))
                  (throw 'done t))
                 ((looking-at-p "[ \t]*$") ; 空行：停止扫描
                  (throw 'done t))))
              (forward-line 1))))
        (when brace-pos
          (goto-char brace-pos)
          (beginning-of-line)
          (skip-chars-forward " \t")
          t))))))

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
  "快速 defun 起点定位 + treesit fallback。
大括号 / def·class 缩进语言用快速 heuristic；
其他语言 (Lua, Haskell, ...) 调用 treesit-beginning-of-defun。"
  (let ((arg (or arg 1)))
    (cond
     ((< arg 0) (my-treesit-end-of-defun (- arg)))
     (t
      (while (> arg 0)
        (let ((orig (point)))
          (beginning-of-line)
          (unless (or (my-treesit--defun-start-braces)
                      (my-treesit--defun-start-indent))
            ;; heuristic 不适用：fallback 到 treesit 原始函数
            ;; （treesit-beginning-of-defun 未被 advise，安全调用）
            (goto-char orig)
            (treesit-beginning-of-defun 1)))
        (setq arg (1- arg)))))))

(defun my-treesit-end-of-defun (&optional arg)
  "快速 defun 终点定位 + treesit fallback。
大括号语言用 syntax-ppss + forward-sexp；其他语言调用 treesit。"
  (let* ((forward-sexp-function nil)     ; 强制用语法表，不走 treesit
         (arg (or arg 1)))
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
            ;; 顶层：先尝试大括号语言（向前找 { 再 forward-sexp 到 }）
            (let ((brace-pos nil))
              (save-excursion
                (catch 'done
                  (dotimes (_ 6)        ; 最多向前扫描 5 行
                    (let ((d (car (syntax-ppss))))
                      (cond
                       ((> d 0)         ; 进入 {} 了
                        (when (re-search-forward "{" (line-end-position) t)
                          (setq brace-pos (1- (point))))
                        (throw 'done t))
                       ((looking-at-p "[ \t]*$") ; 空行：停止
                        (throw 'done t))))
                    (forward-line 1))))
              (if brace-pos
                  ;; 大括号语言：跳到 { 再 forward-sexp 到匹配的 }
                  (progn (goto-char brace-pos)
                         (or (ignore-errors (forward-sexp 1))
                             (goto-char (point-max))))
                ;; 非大括号语言：fallback 到 treesit 原始函数
                ;; （treesit-end-of-defun 未被 advise，安全调用）
                (treesit-end-of-defun 1))))))
        (setq arg (1- arg)))))))

;; 在 treesit 模式的 hook 中设置 buffer-local 的 defun 导航函数。
;; 不 advise treesit-beginning/end-of-defun，避免递归。
(defun my-treesit--setup-defun-nav ()
  "为当前 treesit 模式 buffer 设置快速 defun 导航。"
  (when (and (boundp 'treesit-language) treesit-language)
    (setq-local beginning-of-defun-function #'my-treesit-beginning-of-defun)
    (setq-local end-of-defun-function #'my-treesit-end-of-defun)))

(add-hook 'treesit-mode-hook #'my-treesit--setup-defun-nav)

(provide 'init-treesit)
