
;; 使用浏览器打开光标处url
;; browse-url-at-point
;; (defun ran-open-url-with-browser ()
;;   (interactive)
;;   (thing-copy-url nil)
;;   (browse-url (car kill-ring-yank-pointer)))

;; 预览markdown
(defun ran-markdown-to-html ()
  (interactive)
  (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
  (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

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


(defun ran-comment-line-next-line ()
  "Comment line and move to the next line."
  (interactive)
  (comment-line 1))

(defun ran-comment-line-prev-line ()
  "Comment line and move to previous line. "
  (interactive)
  (comment-line -1))

;; {{ From lazycat-toolkit
;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/extensions/lazycat/lazycat-toolkit.el
(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun insert-changelog-date ()
  "Insert changelog date, like yyyy/mm/dd."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(defun switch-to-scratch ()
  "Select buffer *scratch* in the current window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun switch-to-messages ()
  "Select buffer *message* in the current window."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun ielm-toggle ()
  "Toggle ielm buffer."
  (interactive)
  (require 'ielm)
  (let ((ielm-buffer-name "*ielm*"))
    (if (get-buffer ielm-buffer-name)
        (if (string-equal ielm-buffer-name (buffer-name))
            (bury-buffer)
          (switch-to-buffer ielm-buffer-name))
      (ielm))))
;; }}

;; {{
;; https://github.com/manateelazycat/duplicate-line/blob/master/duplicate-line.el
(defun comment-or-uncomment-region+ ()
  "This function is to comment or uncomment a line or a region."
  (interactive)
  (let (beg end)
    (if mark-active
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (setq beg (line-beginning-position))
      (setq end (line-end-position)))
    (save-excursion
      (comment-or-uncomment-region beg end))))
;; }}


;; {{
;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/extensions/lazycat/window-extension.el
(defun delete-buffer-window (buffer-name)
  "Delete the window of special buffer.
Argument BUFFER-NAME the buffer name that will delete."
  (interactive)
  (if (bufferp (get-buffer buffer-name))
      (delete-window (get-buffer-window (get-buffer buffer-name)))
    (message "Buffer %s is not exist." buffer-name)))

(defun delete-buffer-and-window (buffer-name)
  "Delete buffer and window that special.
Argument BUFFER-NAME the buffer name that will delete."
  (interactive)
  (if (bufferp (get-buffer buffer-name))
      (progn
        (delete-buffer-window buffer-name)
        (kill-buffer (get-buffer buffer-name)))
    (message "Buffer %s is not exist." buffer-name)))

(defun delete-current-buffer-and-window ()
  "Delete current buffer and window."
  (interactive)
  (delete-buffer-and-window (buffer-name)))

(defun delete-current-buffer-window ()
  "Delete the window of current buffer."
  (interactive)
  (delete-buffer-window (current-buffer)))

;;}}

;; {{
(defun xah-show-in-desktop()
  "Show current file in desktop.
 (Mac Finder, Windows Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-11-20 2021-01-18"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) default-directory)))
    (cond
      ((string-equal system-type "windows-nt")
       (shell-command (format "PowerShell -Command Start-Process Explorer -FilePath %s" (shell-quote-argument default-directory)))
       ;; (shell-command (concat "start explorer /e,/select,\"" (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\""))
       ;; todo. need to make window highlight the file
       )
      ((string-equal system-type "darwin")
       (if (eq major-mode 'dired-mode)
           (let (($files (dired-get-marked-files )))
             (if (eq (length $files) 0)
                 (shell-command (concat "open " (shell-quote-argument (expand-file-name default-directory ))))
               (shell-command (concat "open -R " (shell-quote-argument (car (dired-get-marked-files )))))))
         (shell-command
          (concat "open -R " (shell-quote-argument $path)))))

      ((string-equal system-type "gnu/linux")
       (let ((process-connection-type nil)
             (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                  "/usr/bin/gvfs-open"
                                "/usr/bin/xdg-open")))
         (start-process "" nil openFileProgram (shell-quote-argument $path)))
       ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
       ))))
;; }}
(provide 'ran-toolkit)

;;; basic-toolkit.el ends here
