;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'init-const)
(require 'fastctags)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
(setq fastctags-stop-auto-update-tags t)
;; Use ripgrep instead of git grep for fallback searching
(setq fastctags-use-git-grep-p nil)
(setq fastctags-use-ripgrep-force t)
(setq fastctags-grep-program "rg")

;; 将该变量标记为始终安全（接受任何值）
(put 'fastctags-extra-tags-files 'safe-local-variable #'always)

;; Completion at point: fastctags + cape-dabbrev 合并
;; 使用 cape-capf-super 将 fastctags 和 dabbrev 的结果合并（两者补全起点一致，可合并）。
;; cape-line 不放入 super：其补全起点为行首(pos-bol)，与词/符号类不同，
;; cape-capf-super 无法合并不同起点的 capf（会被静默丢弃），故改为按需按键触发（见 init-corfu）。
;; 此 hook 必须在 fastctags 加载后注册，确保 fastctags-completion-at-point 可用。
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super #'cape-dabbrev
                                               #'fastctags-completion-at-point)
                              #'cape-file
                              #'cape-keyword))))

;; consult-imenu 供 ran-fastctags-imenu 使用（consult 已由 init-vertico 加载）。
;; fastctags 已在文件顶部 require，无需 with-eval-after-load 包裹；semantic 从未启用，
;; 不再加载 semantic/fw、也不保留 (semantic-active-p) 死分支。
(require 'consult-imenu)

(defun ran-fastctags-imenu ()
  "List all imenu tag with consult-imenu or imenu."
  (interactive)
  (if (null (ignore-errors (imenu--make-index-alist)))
      (call-interactively 'imenu)
    (call-interactively 'consult-imenu)))

(defun fastctags-nav-find-tag-at-point-in-specific-directory ()
  "Find tag using tagname at point, selecting from specific tags files.
Use `pop-tag-mark' to jump back."
  (interactive)
  (let* ((tagname (fastctags-tagname-at-point)))
    (cond
     (tagname
      (fastctags-nav-find-tag-in-specific-directory tagname buffer-file-name))
     (t
      (message "No tag at point")))))

(defun fastctags-nav-find-tag-in-specific-directory (tagname current-file)
  "Select a tags file from `fastctags-extra-tags-files' and search TAGNAME."
  (when (and fastctags-extra-tags-files tagname)
    (let* ((tagfiles (ff-list-replace-env-vars fastctags-extra-tags-files))
           (curtagfile (fastctags-locate-tags-file))
           (all-files (if curtagfile (cons curtagfile tagfiles) tagfiles))
           (selected (completing-read "Select tag file then search: "
                                      all-files nil t)))
      (when selected
        (let ((default-directory (file-name-directory (expand-file-name selected))))
          (fastctags-nav-find-tag-api tagname nil current-file))))))

(provide 'init-fastctags)
