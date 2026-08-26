;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'treesit)

;; ── 版本信息（Emacs 31.1）────────────────────────────────────────
;; 运行库：bin/libtree-sitter-0.26.dll → tree-sitter 0.26.x
;;   (treesit-library-abi-version)   => 15   支持的最高 grammar ABI
;;   (treesit-library-abi-version t) => 13   支持的最低 grammar ABI
;; Grammar 的 `parser.c' 中 LANGUAGE_VERSION 必须落在 [13, 15]，
;; 否则动态库加载时报 `version-mismatch'。
;; 注：Emacs 31.1 没有 `treesit-library-version' 函数，库版本以 DLL 文件名为准。

;; ── grammar 安装策略 ────────────────────────────────────────────
;; `treesit-extra-load-path'（31.1 起是 user option）既是 grammar 搜索路径，
;; 也是 `treesit-auto-install-grammar' 的安装目标目录。
;; ~/.emacs.d/tree-sitter 本就是默认位置，显式写出便于查看与迁移。
(setq treesit-extra-load-path (list (locate-user-emacs-file "tree-sitter")))

;; 让 `M-x treesit-install-language-grammar' 的目录提示默认落在上面这个位置。
(setq treesit--install-language-grammar-out-dir-history treesit-extra-load-path)

;; Emacs 31.1 新增 `treesit-auto-install-grammar'，默认 `ask'：打开文件时若缺
;; grammar 会弹窗询问、然后现场 git clone + 编译（Windows 上通常没有 cc）。
;; 本配置走手工安装，所以关掉自动安装：
;;   M-x treesit-install-language-grammar
;; 想改成自动的话：`always'（静默装）/ `ask'（弹窗问）/ `ask-dir'（还问装哪）。
(setq treesit-auto-install-grammar nil)

;; ── grammar 下载配方 ────────────────────────────────────────────
;; 位置格式（Emacs 31.1，向后兼容旧写法）：
;;   (LANG . (URL REVISION SOURCE-DIR CC C++ COMMIT))
;; 关键字格式（31.1 新增）：
;;   (LANG . (URL :revision R :source-dir D :cc CC :c++ CXX
;;                :commit SHA :copy-queries t))
;; 注意两种格式**不能混用**：`treesit--install-language-grammar-1' 只在
;; 第一个可选参数就是关键字时才走关键字分支，否则整串按位置解析。
;; 也就是说 ("URL" "v1.0" :copy-queries t) 会把 :copy-queries 当成 SOURCE-DIR。
;;
;; `:copy-queries t' 会把仓库里的 queries/ 一并装到 grammar 目录，
;; 供下面 `my-treesit-setup' 给「没有内置 ts-mode 的语言」做高亮用。
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.25.1"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.24.2"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git" "v0.23.5"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        ;; elisp/haskell/kotlin/mojo/zig 没有内置 ts-mode，靠 queries 高亮 → :copy-queries
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp" :copy-queries t))
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil)
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.25.0"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"
                    :revision "master" :copy-queries t))
        (heex . ("https://github.com/phoenixframework/tree-sitter-heex" "main" "src" nil nil))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.5.0"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        ;; markdown 特意保留 split 版 v0.4.x：Emacs 31 内置 markdown-ts-mode
        ;; 官方测试版本（v0.4.1 + markdown-inline v0.4.1），上游已合并为单一
        ;; grammar（v0.5+），合并版与内置 mode 不兼容。
        ;; 注：原 tree-sitter/tree-sitter-markdown 仓库已删除，迁移到
        ;; tree-sitter-grammars org（v0.4.x tag 均保留）。
        ;; 另注：Emacs 31.1 的 markdown-ts-mode.el 自带一份 pin 到 commit
        ;; 4132852 的配方，用 (add-to-list ... t) **追加**在末尾；本 alist 是
        ;; `setq' 且在其之前生效，`assq' 取首个匹配 → 下面这两条优先，
        ;; 内置配方不会覆盖它们。
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown-inline/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.25.0"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.24.2" "php/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.24.2"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src" nil nil)
        (toml "https://github.com/tree-sitter/tree-sitter-toml" "master" "src" nil nil)
        (vue . ("https://github.com/tree-sitter-grammars/tree-sitter-vue"))
        (kotlin . ("https://github.com/tree-sitter-grammars/tree-sitter-kotlin" :copy-queries t))
        (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
        (zig . ("https://github.com/tree-sitter-grammars/tree-sitter-zig" :copy-queries t))
        (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
        (mojo . ("https://github.com/HerringtonDarkholme/tree-sitter-mojo" :copy-queries t))))

;; csharp grammar（tree-sitter-c-sharp）的导出符号是 `tree_sitter_c_sharp'
;; （仓库名中的连字符变下划线），与 Emacs 按语言名 `csharp' 推导的
;; `tree_sitter_csharp' 不匹配，动态库加载报 symbol-error。必须 override。
;; （Emacs 31.1 的 `treesit-load-name-override-list' 默认仍是 nil，
;;   csharp-mode.el 也没有内置这条，所以这里仍然必需。）
(add-to-list 'treesit-load-name-override-list
             '(csharp "libtree-sitter-csharp" "tree_sitter_c_sharp"))

;; ── major-mode-remap ────────────────────────────────────────────
;; 内置 ts-mode 的 `auto-mode-alist' 注册都是条件的，所以要靠 remap 来接管。
;;
;; Emacs 31.1 提供了 `treesit-major-mode-remap-alist'（26 条）+ user option
;; `treesit-enabled-modes'，但内置方案**不检查 grammar 是否真的装了**，而部分
;; ts-mode 在 grammar 缺失时是硬报错而非优雅回退：
;;   (java-ts-mode)  => (error "Tree-sitter for Java isn't available")
;;   (ruby-ts-mode)  => (error "Tree-sitter for Ruby isn't available")
;;   (php-ts-mode)   => (error "Tree-sitter for PHP isn't available...")
;; 所以这里按「grammar 装了才 remap」自行装配，缺 grammar 时保持原 mode。
;;
;; 另外用 `add-to-list' 追加而不是 `setq' 覆盖，避免顶掉其它模块写入的 remap。
;; 注意：判定在启动时做一次，事后新装 grammar 需要重启 Emacs 才生效。

(defvar my-treesit-remap-list
  '(;; (原 mode         ts-mode              所需 grammar ...)
    ;; --- progmodes/ ---
    (c-mode             c-ts-mode            c)
    (c++-mode           c++-ts-mode          cpp)
    (cmake-mode         cmake-ts-mode        cmake)
    (dockerfile-mode    dockerfile-ts-mode   dockerfile)
    (elixir-mode        elixir-ts-mode       elixir)
    (heex-mode          heex-ts-mode         heex)
    (java-mode          java-ts-mode         java)
    (js-json-mode       json-ts-mode         json)
    (lua-mode           lua-ts-mode          lua)
    (ruby-mode          ruby-ts-mode         ruby)
    (typescript-mode    typescript-ts-mode   typescript)
    ;; --- textmodes/ ---
    (conf-toml-mode     toml-ts-mode         toml)
    (yaml-mode          yaml-ts-mode         yaml))
  "自定义的 `major-mode-remap-alist' 候选表。
每项形如 (OLD-MODE TS-MODE LANG...)，仅当所有 LANG 的 grammar 都可用时才生效。

已移除的条目：
  (html-mode . html-ts-mode) —— 死条目，init-mode.el 已把 `\\.html?\\='
  指向 `web-mode'，`html-mode' 根本不会被触发。

Emacs 31.1 内置表里还有下面这些，按需取用（同样会被 grammar 可用性过滤）：
  (sh-mode          bash-ts-mode        bash)
  (python-mode      python-ts-mode      python)
  (go-mode          go-ts-mode          go)
  (go-mod-mode      go-mod-ts-mode      gomod)
  (go-work-mode     go-work-ts-mode     gowork)
  (rust-mode        rust-ts-mode        rust)
  (css-mode         css-ts-mode         css)
  (csharp-mode      csharp-ts-mode      csharp)
  (javascript-mode  js-ts-mode          javascript)
  (tsx-mode         tsx-ts-mode         tsx)
  (php-mode         php-ts-mode         php)
  (c-or-c++-mode    c-or-c++-ts-mode    c cpp)
  (mhtml-mode       mhtml-ts-mode       html css javascript)
注：.go/.rs/.lua/.php 在 init-mode.el 的 `auto-mode-alist' 里已直接指向
对应的 ts-mode，不走 remap。")

(dolist (entry my-treesit-remap-list)
  (when (seq-every-p #'treesit-language-available-p (cddr entry))
    (add-to-list 'major-mode-remap-alist (cons (nth 0 entry) (nth 1 entry)))))

;; ── 高亮级别（1=注释+定义 2=+关键字+字符串 3=+赋值+内置+数字 4=全部）──
(setq treesit-font-lock-level 3)

;; ── 没有内置 ts-mode 的语言：在原 major-mode 里挂 treesit ────────
;; 关键点：**裸 `treesit-parser-create' 不产生任何高亮**。高亮需要
;; `treesit-font-lock-settings'，而内置 ts-mode 是把 query 以 Elisp sexp 形式
;; 写死在自己的 .el 里（`treesit-font-lock-rules'），31.1 的 lisp/ 目录中
;; **没有任何一个内置 ts-mode 读 .scm 文件**。只建 parser = 每次编辑白跑一遍解析。
;;
;; 另一条独立通路是 31.1 新增的 treesit-x.el：`treesit-generic-mode-setup'
;; 读 `user-emacs-directory'/tree-sitter/queries/LANG/highlights.scm
;; （即上游 grammar 仓库自带的 nvim-treesitter 风格 query），按
;; `treesit-generic-mode-font-lock-map' 把 @capture 文本替换成 face 名，
;; 再喂给同一个 `treesit-font-lock-rules'。queries/ 由配方里的
;; `:copy-queries t' 在安装时拷贝，因此**已装过的 grammar 需要重新执行一次
;; `M-x treesit-install-language-grammar' 才会有**。
;;
;; 注意 `treesit-generic-mode-font-lock-query' 读的是 `user-emacs-directory'
;; 下的固定路径，而 `:copy-queries' 写的是 `treesit-extra-load-path' 的首个
;; 可写目录——上面把两者都设成 ~/.emacs.d/tree-sitter，正好对上。
;;
;; ⚠ 这条通路对真实上游 .scm 的兼容性有限（Emacs 31.1 实测）：
;;   1. Emacs 只支持 `equal'/`match'/`pred' 三类谓词。上游普遍使用的
;;      `#lua-match?' / `#any-of?' 会让整个 query 编译 signal
;;      `treesit-query-error'（tree-sitter-zig 的 highlights.scm 两个都用了）。
;;   2. `treesit-generic-mode-font-lock-query' 用朴素前缀替换处理 capture name，
;;      且 map 里短名在前，于是 @variable.builtin 先被 @variable 命中，
;;      变成不存在的 face `font-lock-variable-name-face.builtin' → 不上色。
;;      @keyword.return / @function.call / @comment.documentation 同理。
;; 所以 `my-treesit-setup' 对整条通路做了兜底：失败就退回「只建 parser」，
;; 行为与改动前一致，不会让 mode hook 炸掉。

(defvar my-treesit--query-failed nil
  "已经报告过 highlights.scm 不可用的语言，避免每次开文件都刷 echo area。")

(defun my-treesit-setup (lang &optional font-lock)
  "在当前 buffer 为 LANG 建立 tree-sitter parser。

grammar 不可用（或 buffer 超过 `treesit-max-buffer-size'）时静默跳过，
不会像裸 `treesit-parser-create' 那样 signal `treesit-load-language-error'。

FONT-LOCK 非 nil 时，尝试用 LANG 的 highlights.scm 接管本 buffer 的 font-lock
（同时会设置 `transpose-sexps-function' 等 treesit 通用设施）。
这条通路对上游 .scm 的兼容性有限（见下方注释），失败时静默退回「只建 parser」。"
  (when (treesit-ready-p lang t)
    (let ((wired
           (when font-lock
             (condition-case err
                 (progn
                   (treesit-generic-mode-setup lang)
                   (when treesit-font-lock-settings
                     (treesit-major-mode-setup)
                     t))
               (error
                (unless (memq lang my-treesit--query-failed)
                  (push lang my-treesit--query-failed)
                  (message "init-treesit: %s 的 highlights.scm 不可用（%s），退回只建 parser"
                           lang (error-message-string err)))
                nil)))))
      (unless wired
        (treesit-parser-create lang)))))

;; 第三方包提供的独立 major-mode：有 queries 就用 treesit 高亮
(add-hook 'zig-mode-hook     (lambda () (my-treesit-setup 'zig     t)))
(add-hook 'mojo-mode-hook    (lambda () (my-treesit-setup 'mojo    t)))
(add-hook 'haskell-mode-hook (lambda () (my-treesit-setup 'haskell t)))
(add-hook 'kotlin-mode-hook  (lambda () (my-treesit-setup 'kotlin  t)))

;; emacs-lisp-mode / ielm：只建 parser，不接管 font-lock。
;; elisp 自带的 font-lock 比 tree-sitter-elisp 的 highlights.scm 完整得多
;; （宏、docstring、backquote 等），让 treesit 接管是降级。
(add-hook 'emacs-lisp-mode-hook (lambda () (my-treesit-setup 'elisp)))
(add-hook 'ielm-mode-hook       (lambda () (my-treesit-setup 'elisp)))

;; web-mode 是多语言 mode，有自己的高亮引擎：只建 parser，不接管 font-lock。
(add-hook 'web-mode-hook
          (lambda ()
            (when-let* ((file-name (buffer-file-name))
                        (lang (pcase (file-name-extension file-name)
                                ("vue"  'vue)
                                ("html" 'html)
                                ("php"  'php))))
              (my-treesit-setup lang))))

;; ── 关于 defun 导航 advice（已于 Emacs 31.1 移除）─────────────────
;; 历史背景：Emacs 30 上 `treesit-major-mode-setup' 会把
;; `beginning-of-defun-function' / `end-of-defun-function' 设成
;; `treesit-beginning/end-of-defun' 并加 `<remap>' 键重映射，而后者经
;; `treesit-navigate-thing' → `treesit-thing-prev' → `treesit--thing-sibling'
;; 做整树搜索，大文件上 mark-defun (C-M-h) / M-a / M-e 可达 154ms~950ms。
;; 当时的对策是用 :around advice 换成基于 parent/prev-sibling/next-sibling
;; 的快速节点导航（`my-treesit--nav-*'，约 320 行）。
;;
;; Emacs 31.1 + tree-sitter 0.26 上重新实测（parse 预热后，单位 ms）：
;;   场景                                  原生     旧 advice
;;   C 858KB  end-of-defun@eob(无匹配)     15.66     19.73
;;   C 858KB  beginning-of-defun -1@eob     2.06      0.55
;;   C 858KB  mark-defun@mid                2.27      1.08
;;   JSON 867KB mark-defun@mid              0.94      1.21
;;   Lua 433KB  end-of-defun@eob            3.75      2.13
;; 原来的 154ms~950ms 复现不出来；两边都在个位数毫秒，互有胜负。
;; （3.4MB 的极端 C 文件上原生最坏 85ms，其中 54ms 是 `treesit-node-at'
;;   本身的开销，advice 同样躲不掉，实测 83.65ms。）
;;
;; 同时 advice 硬编码 nested 语义、忽略 `treesit-defun-tactic'，而 31.1 的
;; `c-ts-mode' 和 `yaml-ts-mode' 用的是 `top-level'，造成实际行为回归：
;;   yaml-ts-mode 点在深层 leaf 上 mark-defun
;;     原生   → 整个 top1: 块 (L1-L5)
;;     advice → 只有嵌套的 child: (L2-L4)
;;
;; 收益已经没有了、却带着一个行为回归和约 320 行（含早已不被调用的
;; syntax-ppss heuristic）需要维护，故整体移除，回归 31.1 原生实现。
;; 若日后又遇到 defun 导航卡顿，先确认：
;;   1. `treesit-defun-tactic' 是不是 `top-level'（整树爬升更贵）
;;   2. buffer 里有几个 parser——31.1 的 `treesit--thing-sibling' 会对
;;      `treesit-parsers-at' 返回的**每个** parser 各跑一遍全树扫描
;;   3. 是不是 `treesit-node-at' 在超大 buffer 上的固有开销（换 advice 无用）

(provide 'init-treesit)
