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
;; 问题根因：
;;   1. `treesit-major-mode-setup'（每个内置 ts-mode 都会调用）会把
;;      `beginning-of-defun-function' / `end-of-defun-function' 设为
;;      `treesit-beginning-of-defun' / `treesit-end-of-defun'，并添加
;;      `<remap> <beginning-of-defun>' / `<remap> <end-of-defun>' 键重映射。
;;      于是 mark-defun (C-M-h)、er/mark-defun (expand-region)、M-a/M-e
;;      全部走 treesit 导航。
;;   2. Emacs 30 的 treesit 导航（`treesit-navigate-thing' →
;;      `treesit-thing-prev' → `treesit--thing-sibling'）逐节点遍历语法树，
;;      在大文件上极慢（Windows 尤甚）。json-ts-mode 把每个 pair/object
;;      都当 defun，大 JSON 文件中一次 mark-defun 就要遍历上万个节点。
;;   3. 旧版修复挂在 `treesit-mode-hook' 上，但 Emacs 30 根本不存在
;;      `treesit-mode' 这个 minor mode，该 hook 从未运行，修复是死代码
;;      （且 ts-mode 不再设置 `treesit-language'，原判断条件也已失效）。
;;
;; 策略：用 :around advice 包裹 `treesit-beginning-of-defun' /
;; `treesit-end-of-defun'，全部走快速节点导航（`my-treesit--nav-*'，
;; parent/prev-sibling/next-sibling 链，单步 O(1) 的 C 调用）。
;; 不能走原生 `treesit-beginning-of-defun'：它通过
;; `treesit-navigate-thing' → `treesit--thing-sibling' 逐节点整树搜索，
;; 大文件（上万个顶层节点）上单次调用可达数百毫秒到数秒
;; （M-x 卡顿的 profiler 即此路径）。
;; 覆盖所有调用路径：buffer-local 变量、键重映射、直接调用。

;; ── 以下 syntax-ppss heuristic（历史实现）已从导航路径移除 ──────
;; 原因：对非大括号语言会误判（Lua 的 `local t = {...}' 把 table 的 `{'
;; 当 defun、C 的 `default:' 命中 def 正则）；导航路径统一走快速节点
;; 导航 `my-treesit--nav-*'。此处保留定义供参考与旧测试使用。

(defvar my-treesit--in-defun-nav nil
  "非 nil 时表示正处于快速 defun 导航中，advice 直接放行原函数。")

(defvar my-treesit--defun-climb-limit 24
  "快速 defun 导航的最大爬层数 / 候选括号数，超过则回退 treesit。")

(defvar my-treesit--defun-header-not-regexp
  "\\`\\(?:if\\|else\\|for\\|while\\|do\\|switch\\|case\\|default\\|try\\|catch\\|finally\\|return\\|throw\\|new\\|with\\)\\_>"
  "首个单词匹配此正则的 `{` 视为控制流语句而非 defun。")

(defun my-treesit--defun-header-start (brace-pos)
  "返回 BRACE-POS 处 `{` 所属声明的首个 token 位置。
声明头部 = 从 BRACE-POS 向前到最近的 `;'/`{'/`}'/`['/`]'（上限 800
字符）之间的文本；其中的注释与空白（如声明上方的文档注释）被跳过。
若头部不像 defun 声明（如 if/for 等），返回 nil。"
  (save-excursion
    (goto-char brace-pos)
    (skip-chars-backward "^;{}[]" (max (point-min) (- brace-pos 800)))
    ;; 跳过空白与注释，到达声明的第一个真实 token
    (forward-comment (point-max))
    (let ((start (point)))
      (when (and (< start brace-pos)
                 (looking-at "[@A-Za-z_$][A-Za-z0-9_$]*")
                 (not (string-match-p my-treesit--defun-header-not-regexp
                                      (match-string-no-properties 0))))
        start))))

(defun my-treesit--defun-brace-target (brace-pos)
  "若 BRACE-POS 处的 `{` 像一个 defun 体的开始，返回 defun 起点位置
（声明首 token；JSON 对象等初始化器风格则返回 BRACE-POS 本身）。
否则返回 nil。"
  (or (my-treesit--defun-header-start brace-pos)
      (and (save-excursion
             (goto-char brace-pos)
             (forward-comment most-negative-fixnum)
             ;; `"key": {`、`[ {` 等：JSON/对象字面量风格
             (memq (char-before) '(?: ?\[)))
           brace-pos)))

(defun my-treesit--climb-to-defun-brace ()
  "点在嵌套括号内时逐层向外爬，返回第一个像 defun 的 `{` 位置
（最迟在最外层接受）。失败返回 nil。"
  (let ((brace nil)
        (depth (car (syntax-ppss)))
        (guard my-treesit--defun-climb-limit))
    (while (and (> depth 0) (null brace) (> guard 0))
      (setq guard (1- guard))
      (if (ignore-errors (up-list -1) t)
          (progn
            (setq depth (car (syntax-ppss)))
            (when (and (eq (char-after) ?\{)
                       (or (= depth 0)
                           (my-treesit--defun-brace-target (point))))
              (setq brace (point))))
        (setq depth 0)))
    brace))

(defun my-treesit--scan-brace (backward depth)
  "向前/向后搜索嵌套深度为 DEPTH、且像 defun 的 `{`。
BACKWARD 非 nil 表示向后搜索（最多 `my-treesit--defun-climb-limit'
个候选）；向前搜索限制在 8 行以内，遇到深度 <= DEPTH 的 `;' 或 `}'
即停止。返回 (括号位置 . defun 起点位置)，未找到返回 nil。"
  (let ((tries my-treesit--defun-climb-limit)
        (limit (unless backward (line-end-position 9)))
        result)
    (save-excursion
      (if backward
          (while (and (null result) (> tries 0)
                      (re-search-backward "{" nil t))
            (setq tries (1- tries))
            (let ((ppss (syntax-ppss)))
              (when (and (= (car ppss) depth)
                         (not (nth 3 ppss)) (not (nth 4 ppss)))
                (let ((target (or (my-treesit--defun-brace-target (point))
                                  ;; 顶层括号即使无法识别头部也视为 defun
                                  (and (= depth 0) (point)))))
                  (when target
                    (setq result (cons (point) target)))))))
        (catch 'stop
          (while (and (null result) (re-search-forward "[{};]" limit t))
            (let* ((m (match-beginning 0))
                   ;; 注意：`(syntax-ppss POS)' 带参数时会把 point 移到 POS！
                   ;; 若直接调用，point 会被拉回匹配位置，re-search-forward
                   ;; 下一轮重复匹配同一处 → 死循环（Lua 等函数体内含括号
                   ;; 嵌套 `{` 的语言必现）。必须用 save-excursion 包裹。
                   (ppss (save-excursion (syntax-ppss m)))
                   (d (car ppss))
                   (ch (char-after m)))
              (cond
               ((or (nth 3 ppss) (nth 4 ppss)) nil) ; 字符串/注释内
               ((and (eq ch ?\{) (= d depth))
                (let ((target (my-treesit--defun-brace-target m)))
                  (if target
                      (setq result (cons m target))
                    (throw 'stop nil))))
               ((<= d depth) (throw 'stop nil))))))))
    result))

(defun my-treesit--defun-start-braces ()
  "大括号语言：移动点到包围/前一个 defun 的起点。成功返回 t。"
  (let ((depth (car (syntax-ppss))))
    (if (> depth 0)
        (when-let ((brace (my-treesit--climb-to-defun-brace)))
          (goto-char (or (my-treesit--defun-header-start brace) brace))
          t)
      (when-let ((hit (my-treesit--scan-brace t 0)))
        (goto-char (cdr hit))
        t))))

(defun my-treesit--defun-start-indent ()
  "缩进语言（Python/Ruby/Elixir）：向前搜索缩进更浅的
`def...'/`class' 行。成功返回 t。"
  (let ((orig-indent (save-excursion
                       (back-to-indentation)
                       (current-column)))
        (found nil))
    (when (> orig-indent 0)
      (while (and (not found)
                  (re-search-backward
                   "^[ \t]*\\(def\\sw*\\|class\\)\\_>" nil t))
        (let ((ind (save-excursion
                     (back-to-indentation)
                     (current-column))))
          (when (< ind orig-indent)
            (setq found t)))))
    found))

;; ── 快速节点导航（heuristic 不适用时的回退）─────────────────────
;; 原生 `treesit-beginning-of-defun' / `treesit-end-of-defun' 通过
;; `treesit-navigate-thing' → `treesit--thing-sibling' 导航：每次迭代
;; 都做整树搜索（`treesit-search-forward' + `treesit-node-top-level'
;; 爬升），大文件（上万个顶层节点）上单次调用可达数百毫秒。
;; 这里直接用节点 API（parent / prev-sibling / next-sibling，
;; 单步都是 O(1) 的 C 调用）实现相同语义：
;;   所在 defun → 前一个 / 后一个 defun。
;; defun 判断依据与 treesit 一致：`treesit-defun-type-regexp'
;; 或 `treesit-thing-settings' 的 `defun'。

(defun my-treesit--nav-defun-p (node)
  "NODE 是否为 defun 节点（依据同 `treesit-beginning-of-defun'）。"
  (and node (treesit-node-match-p node (or treesit-defun-type-regexp 'defun) t)))

(defun my-treesit--nav-top-level (node)
  "返回 NODE 的顶层祖先（root 的直接子节点）；NODE 为 nil 时返回 nil。"
  (when node
    (while (and (treesit-node-parent node)
                (treesit-node-parent (treesit-node-parent node)))
      (setq node (treesit-node-parent node)))
    node))

(defun my-treesit--nav-enclosing-defun (pos)
  "返回覆盖 POS 的最小 defun 节点（start <= POS < end），无则 nil。"
  (let ((node (treesit-node-at pos)))
    (and node
         (treesit-parent-until
          node
          (lambda (n)
            (and (my-treesit--nav-defun-p n)
                 (<= (treesit-node-start n) pos)
                 (< pos (treesit-node-end n))))
          t))))

(defun my-treesit--nav-prev-defun (pos)
  "返回 POS 之前最近的 defun 节点（其 end <= POS），无则 nil。"
  (let ((top (my-treesit--nav-top-level (treesit-node-at pos))))
    (while (and top
                (not (and (my-treesit--nav-defun-p top)
                          (<= (treesit-node-end top) pos))))
      (setq top (treesit-node-prev-sibling top t)))
    top))

(defun my-treesit--nav-next-defun (pos)
  "返回起点严格大于 POS 的最小 defun 节点，无则 nil。
逐层向上检查兄弟链（自底向上）：更深的 defun 起点必然更小，
首个命中的兄弟即全局最小起点。"
  (catch 'found
    (let ((cur (treesit-node-at pos)))
      (while cur
        ;; node-at 在节点间空白处可能直接返回下一个节点：先检查自身
        (when (and (my-treesit--nav-defun-p cur)
                   (> (treesit-node-start cur) pos))
          (throw 'found cur))
        (let ((sib (treesit-node-next-sibling cur)))
          (while (and sib (not (my-treesit--nav-defun-p sib)))
            (setq sib (treesit-node-next-sibling sib)))
          (when sib (throw 'found sib)))
        (setq cur (treesit-node-parent cur)))
      nil)))

(defun my-treesit--nav-bod-1 ()
  "移动点到所在 defun 的起点；不在任何 defun 内则到前一个 defun 起点。
成功返回 t。"
  (let* ((pos (max (1- (point)) (point-min)))
         (encl (my-treesit--nav-enclosing-defun pos)))
    (if encl
        (progn (goto-char (treesit-node-start encl)) t)
      (let ((prev (my-treesit--nav-prev-defun pos)))
        (when prev
          (goto-char (treesit-node-start prev))
          t)))))

(defun my-treesit--nav-bod-forward-1 ()
  "移动点到下一个 defun 起点（起点严格大于当前点）。成功返回 t。"
  (let ((next (my-treesit--nav-next-defun (point))))
    (when next
      (goto-char (treesit-node-start next))
      t)))

(defun my-treesit--nav-eod-1 ()
  "移动点到所在 defun 的结尾；不在任何 defun 内则到下一个 defun 结尾。
成功返回 t。"
  (let ((pos (point)))
    (if-let ((at (my-treesit--nav-enclosing-defun pos)))
        (progn (goto-char (treesit-node-end at)) t)
      (let ((next (my-treesit--nav-next-defun pos)))
        (when next
          (goto-char (treesit-node-end next))
          t)))))

(defun my-treesit--beginning-of-defun-1 ()
  "移动点到所在/前一个 defun 的起点（单步）。成功返回 t。
统一走快速节点导航：heuristic（`my-treesit--defun-start-braces' 等）
对非大括号语言会误判（如 Lua 的 `local t = {...}' 把 table 的 `{'
当成 defun、C 的 `default:' 命中 def 正则），已从路径中移除；
相关函数保留定义供参考与旧测试使用。"
  (my-treesit--nav-bod-1))

(defun my-treesit--beginning-of-defun-forward (n)
  "移动点到之后第 N 个 defun 的起点（快速节点导航）。"
  (let ((ok t))
    (while (and (> n 0) ok)
      (if (my-treesit--nav-bod-forward-1)
          (setq n (1- n))
        (setq ok nil)))
    ok))

(defun my-treesit-beginning-of-defun (&optional arg)
  "快速 defun 起点定位（快速节点导航）。"
  ;; 与 `treesit-beginning-of-defun' 原实现一致的 push-mark 行为
  (or (not (eq this-command 'treesit-beginning-of-defun))
      (eq last-command 'treesit-beginning-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (let ((forward-sexp-function nil)   ; 强制语法表扫描，不走 treesit
        (arg (or arg 1)))
    (cond
     ((< arg 0) (my-treesit--beginning-of-defun-forward (- arg)))
     (t
      (let ((ok t))
        (while (and (> arg 0) ok)
          (setq ok (my-treesit--beginning-of-defun-1))
          (setq arg (1- arg)))
        ok)))))

(defun my-treesit--end-of-defun-1 ()
  "移动点越过所在/后续 defun 的结尾。成功返回 t。"
  (my-treesit--nav-eod-1))

(defun my-treesit--end-of-defun-backward (n)
  "移动点之前第 N 个 defun 的结尾（快速节点导航）。"
  (let ((orig (point))
        (ok t))
    (while (and (> n 0) ok)
      (let* ((pos (max (1- (point)) (point-min)))
             (encl (my-treesit--nav-enclosing-defun pos))
             (node (or encl (my-treesit--nav-prev-defun pos))))
        (if (null node)
            (setq ok nil)
          (let ((end (treesit-node-end node)))
            (if (< end orig)
                ;; 该 defun 整体在 ORIG 之前：其结尾即目标
                (progn (goto-char end)
                       (setq orig end)
                       (setq n (1- n)))
              ;; 该 defun 包含原点：跳到其起点，下一轮找前一个
              (goto-char (treesit-node-start node)))))))
    (unless ok
      (goto-char orig))
    ok))

(defun my-treesit-end-of-defun (&optional arg _)
  "快速 defun 结尾定位（快速节点导航）。"
  ;; 与 `treesit-end-of-defun' 原实现一致的 push-mark 行为
  (or (not (eq this-command 'treesit-end-of-defun))
      (eq last-command 'treesit-end-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (let ((forward-sexp-function nil)   ; 强制语法表扫描，不走 treesit
        (arg (or arg 1)))
    (cond
     ((< arg 0) (my-treesit--end-of-defun-backward (- arg)))
     (t
      (let ((ok t))
        (while (and (> arg 0) ok)
          (setq ok (my-treesit--end-of-defun-1))
          (setq arg (1- arg)))
        ok)))))

;; 用 advice 安装到所有调用路径（buffer-local 变量、键重映射、直接调用）。
;; `my-treesit--in-defun-nav' 防止 advice 内的任何路径再次调用
;; treesit-* 原函数时重新进入 advice（递归防护）。
(advice-add 'treesit-beginning-of-defun :around
            (lambda (orig-fn &rest args)
              (if my-treesit--in-defun-nav
                  (apply orig-fn args)
                (let ((my-treesit--in-defun-nav t))
                  (apply #'my-treesit-beginning-of-defun args))))
            '((name . my-treesit-fast-beginning-of-defun)))

(advice-add 'treesit-end-of-defun :around
            (lambda (orig-fn &rest args)
              (if my-treesit--in-defun-nav
                  (apply orig-fn args)
                (let ((my-treesit--in-defun-nav t))
                  (apply #'my-treesit-end-of-defun args))))
            '((name . my-treesit-fast-end-of-defun)))

(provide 'init-treesit)
