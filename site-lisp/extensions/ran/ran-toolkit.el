

(defun ran-get-full-path ()
  "copy buffer file full path to kill ring"
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "ffname : %s" buffer-file-name))
    (message "empty buffer file name")))

;; https://emacs.stackexchange.com/questions/39105/insert-file-path-via-counsel
(defun ran-counsel-insert-file-path ()
  "Insert file path."
  (interactive)
  (unless (featurep 'counsel) (require 'counsel))
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action
            (lambda (x)
              (insert x))))

;; (magit-toplevel) 函数得到 仓库根目录
;; https://emacs.stackexchange.com/questions/9323/get-git-repo-root-directory-preferably-with-magit
;;
(defun ran-git-refs-for ()
  "git push origin HEAD:refs/for/[branch] "
  (interactive)
  (require 'magit)
  (let ((git-refs-command (format "%s push origin HEAD:refs/for/" magit-git-executable))
        (branch (magit-read-other-branch-or-commit "Checkout" "origin/master")))
    (setq git-refs-command (concat git-refs-command branch))
    (when (y-or-n-p (concat "Next run : " git-refs-command))
      (magit-git-command git-refs-command))))

;; https://github.com/manateelazycat/smart-align
;; 增加选择区域
(defun smart-align ()
  (interactive)
  (with-demoted-errors
      "Something wrong when align."

    ;; (let ((align-start
    ;;        (save-excursion
    ;;          (backward-up-list)
    ;;          (point)
    ;;          ))
    ;;       (align-end
    ;;        (save-excursion
    ;;          (up-list)
    ;;          (point))))
    ;;   (align-regexp align-start align-end "\\(\\s-*\\)\\(=\\|:\\)" 1 1))
    (let ((align-start) (align-end))
      (if (use-region-p)
        (setq align-start (region-beginning)
              align-end (region-end))
        (setq align-start (save-excursion
                            (backward-up-list)
                            (point))
              align-end (save-excursion
                          (up-list)
                          (point))))
      (align-regexp align-start align-end "\\(\\s-*\\)\\(=\\|:\\)" 1 1))))

(provide 'ran-toolkit)

;;; basic-toolkit.el ends here
