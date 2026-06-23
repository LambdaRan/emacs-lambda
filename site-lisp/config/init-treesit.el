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

(provide 'init-treesit)
