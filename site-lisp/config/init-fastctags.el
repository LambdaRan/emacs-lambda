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

;; Completion at point
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions #'fastctags-completion-at-point nil t)))

(defun ran-fastctags-imenu()
  "List all imenu tag with counsel-semantic-or-imenu or imenu."
  (interactive)
  (require 'semantic/fw)
  (if (and (not (semantic-active-p))
           (seq-empty-p (counsel--imenu-candidates)))
      (call-interactively 'imenu)
    (call-interactively 'counsel-semantic-or-imenu)))


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
