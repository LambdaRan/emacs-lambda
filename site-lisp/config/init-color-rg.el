;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'color-rg)
(require 'init-const)
(require 'lazy-load)

(setq color-rg-show-function-name-p nil)
(setq color-rg-search-ignore-file nil)
(setq color-rg-search-ignore-rules "-g \"!node_modules\" -g \"!dist\" -g \"!TAGS\"")
;; (when (eq system-type 'windows-nt)
;;   (setq color-rg-command-prefix "powershell"))

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
      (setq project-root (color-rg-project-root-dir))
      (setq filepath (file-relative-name filepath project-root))
      (dolist (buffer (buffer-list))
        (setq bufferfile (buffer-file-name buffer))
        (when bufferfile
          (setq bufferfile (file-relative-name bufferfile project-root))
          (when (string-equal bufferfile filepath)
            (throw 'find-match buffer))
          )
        )
      nil))
  (advice-add #'color-rg-get-match-buffer :override #'color-rg-get-match-buffer@override)
  ;; (advice-remove #'color-rg-get-match-buffer #'color-rg-get-match-buffer@override)
)

;; GBK 编码搜索支持
(defvar color-rg--search-encoding nil
  "When non-nil, inject '-E ENCODING' into rg command.")

(defun color-rg-build-command@inject-encoding (orig-fn &rest args)
  "Inject encoding flag into rg command when `color-rg--search-encoding' is set."
  (let ((cmd (apply orig-fn args)))
    (if color-rg--search-encoding
        (replace-regexp-in-string
         "\\`rg " (format "rg -E %s " color-rg--search-encoding) cmd)
      cmd)))

(advice-add #'color-rg-build-command :around #'color-rg-build-command@inject-encoding)

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

(provide 'init-color-rg)
