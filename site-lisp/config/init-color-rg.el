;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'color-rg)
(require 'init-const)
(require 'lazy-load)

(setq color-rg-show-function-name-p nil)
(setq color-rg-search-no-ignore-file nil)  ; 尊重 .rgignore 文件
(setq color-rg-search-ignore-rules "-g \"!node_modules\" -g \"!dist\" -g \"!TAGS\" -g \"!tags\" -g \"!*~\"")

;; 跳过 VCS ignore 文件（.gitignore 等），但保留 .rgignore 和 .ignore
;; 适用于 SVN 管理的项目中 .gitignore 忽略了所有文件的情况
;; PCRE2 正则搜索支持（支持 lookahead/lookbehind）
;; GBK 编码搜索支持
(defvar color-rg--search-pcre nil
  "When non-nil, inject '-P' (PCRE2) into rg command.")

(defvar color-rg--search-encoding nil
  "When non-nil, inject '-E ENCODING' into rg command.")

(defun color-rg-build-command@inject-flags (orig-fn &rest args)
  "一次性注入 --no-ignore-vcs、PCRE2、encoding 参数。
合并原有的三层 advice 链，避免顺序依赖和正则匹配脆弱问题。"
  (let ((cmd (apply orig-fn args))
        (extra " --no-ignore-vcs"))
    (when color-rg--search-pcre
      (setq extra (concat extra " -P")))
    (when color-rg--search-encoding
      (setq extra (concat extra (format " -E %s" color-rg--search-encoding))))
    ;; 在 rg 命令后一次性注入所有额外参数
    (replace-regexp-in-string "\\`rg\\b" (concat "rg" extra) cmd)))

(advice-add #'color-rg-build-command :around #'color-rg-build-command@inject-flags)

(defcustom color-rg-project-root nil
  "If non-nil, overrides the project root directory location."
  :group 'color-rg
  :type 'string
  :safe #'stringp)

;; (progn (put 'color-rg-project-root 'safe-local-variable 'stringp))

(defun color-rg-project-root-dir@around(func &rest args)
    "Return special project root or `color-rg-project-root-dir'."
    (if (and color-rg-project-root (not (string-empty-p color-rg-project-root)))
        (file-name-as-directory color-rg-project-root)
      (apply func args)))

(advice-add #'color-rg-project-root-dir :around #'color-rg-project-root-dir@around)

(defun color-rg-open-file-and-stay-then-quit ()
  "Open current file and stay there then quit color-rg."
  (interactive)
  (let* ((match-file (color-rg-get-match-file))
         (match-line (color-rg-get-match-line))
         (match-column (color-rg-get-match-column)))
    (color-rg-quit)
    (save-excursion
      (let ((inhibit-message t))
        ;; open file
        (find-file match-file)
        ;; Jump to match point.
        (color-rg-move-to-point match-line match-column)))
    ))
(lazy-load-set-keys
 '(("m" . color-rg-open-file-and-stay-then-quit))
 color-rg-mode-map)

;; 修复windows分隔符不同导致原buffer在color-rg退出后被关闭
(when sys/windows-p
  (defun color-rg-get-match-buffer@override (filepath)
    (catch 'find-match
      (let ((project-root (color-rg-project-root-dir)))
        (setq filepath (file-relative-name filepath project-root))
        (dolist (buffer (buffer-list))
          (let ((bufferfile (buffer-file-name buffer)))
            (when bufferfile
              (setq bufferfile (file-relative-name bufferfile project-root))
              (when (string-equal bufferfile filepath)
                (throw 'find-match buffer)))))
        nil)))
  (advice-add #'color-rg-get-match-buffer :override #'color-rg-get-match-buffer@override)
  ;; (advice-remove #'color-rg-get-match-buffer #'color-rg-get-match-buffer@override)
)

;;;###autoload
(defun color-rg-search-input-pcre (&optional keyword directory)
  "Search with PCRE2 regexp (supports lookahead/lookbehind)."
  (interactive)
  (let ((color-rg--search-pcre t))
    (color-rg-search-input keyword directory)))

;;;###autoload
(defun color-rg-search-project-pcre ()
  "Search project with PCRE2 regexp."
  (interactive)
  (let ((color-rg--search-pcre t))
    (color-rg-search-project)))

;;;###autoload
(defun color-rg-search-input-gbk (&optional keyword directory)
  "Search GBK encoded files with input keyword."
  (interactive)
  (let ((color-rg--search-encoding "gbk"))
    (color-rg-search-input keyword directory)))

;;;###autoload
(defun color-rg-search-symbol-gbk ()
  "Search symbol at point in GBK encoded files."
  (interactive)
  (let ((color-rg--search-encoding "gbk"))
    (color-rg-search-symbol)))

;;;###autoload
(defun color-rg-search-project-gbk ()
  "Search GBK encoded files in project."
  (interactive)
  (let ((color-rg--search-encoding "gbk"))
    (color-rg-search-project)))

;;;###autoload
(defun color-rg-search-symbol-in-project-gbk ()
  "Search symbol at point in GBK encoded files in project."
  (interactive)
  (let ((color-rg--search-encoding "gbk"))
    (color-rg-search-symbol-in-project)))

;; 修复 color-rg-pointer-string: 当 color-rg-current-parse-state 返回 nil
;; (ignore-errors 吞掉了错误)，但 color-rg-in-string-p 通过 face 检测返回 t 时，
;; color-rg-string-start+end-points 中 (nth 8 nil) 为 nil，传给 goto-char 报错。
;; 由于原函数是字节编译的，cl-letf 替换子函数后编译代码仍对 nil 做 (1+ (car nil))，
;; 故用 :override 接管整个函数，在字符串提取失败时 fallback 到 thing-at-point。
(defun color-rg-pointer-string@override ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let* ((current-string
            (if (color-rg-in-string-p)
                (condition-case nil
                    (let ((string-node-bound (color-rg-get-string-node-bound)))
                      (if string-node-bound
                          (buffer-substring-no-properties
                           (car string-node-bound)
                           (cdr string-node-bound))
                        (buffer-substring-no-properties
                         (1+ (car (color-rg-string-start+end-points)))
                         (cdr (color-rg-string-start+end-points)))))
                  (error ""))
              ""))
           (current-symbol
            (if (or (string-empty-p current-string)
                    (and current-string (string-match-p "[[:space:]]" current-string)))
                (thing-at-point 'symbol)
              current-string)))
      (cond ((and current-symbol (string-prefix-p "." current-symbol))
             (string-remove-prefix "." current-symbol))
            ((and current-symbol (string-prefix-p "#" current-symbol))
             (string-remove-prefix "#" current-symbol))
            (t current-symbol)))))

(advice-add #'color-rg-pointer-string :override #'color-rg-pointer-string@override)

(provide 'init-color-rg)
