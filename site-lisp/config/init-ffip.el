;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-const)
(require 'find-file-in-project)
(require 'ivy)

(when sys/mac-cocoa-p
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; config
;; use fd instread find
(setq ffip-use-rust-fd t)
;; "Don 't show search results from '.*ignore' files."

(setq ffip-prefer-ido-mode nil)

;; (setq ffip-project-root "~/projs/PROJECT_DIR")
;; ffip-project-root-function

;; find-file-in-project
;; ffip-lisp-find-file-in-project
;; ffip-show-diff
;; ffip-split-window-horizontally
;; ffip-split-window-vertically
;; ffip-insert-file

;; M-o 显示如下命令
;; https://oremacs.com/swiper/

;; 参考：http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html
;; 目录，文件的话就直接用默认程序打开
(defun show-in-desktop (dir)
  "Show DIR in desktop."
  (let (cmd)
    (cond
      ((string-equal system-type "windows-nt")
       (setq cmd (format "PowerShell -Command Start-Process Explorer -FilePath %s" (shell-quote-argument dir))))
      ((string-equal system-type "darwin")
       (setq cmd (concat "open " (shell-quote-argument dir))))
      ((string-equal system-type "gnu/linux")
       (let ((program (if (file-exists-p "/usr/bin/gvfs-open") "/usr/bin/gvfs-open" "/usr/bin/xdg-open")))
         (setq cmd (format "%s %s" program (shell-quote-argument dir)))))
      (t (error "Not support system!!!")))
    (when cmd
      (let ((process-connection-type nil))
        (start-process-shell-command "" nil cmd)))
    ))

;; 文件或目录
(defun open-in-vscode (path)
  "Open file or dir in vscode."
  (let (cmd)
    (cond
      ((string-equal system-type "windows-nt")
       (setq cmd (format "Code \"%s\"" path)))    
      ((string-equal system-type "darwin")
       (setq cmd (format "open -a Visual\\ Studio\\ Code.app \"%s\"" path)))
      ((string-equal system-type "gnu/linux")
       (setq cmd (format "code \"%s\"" path)))
      (t (error "Not support system!!!")))
    (message "VSCode Cmd: %s" cmd)
    (when cmd
      (let ((process-connection-type nil))
        (start-process-shell-command "" nil cmd)))
    ))

;; 只能是目录
(defun open-in-terminal (dir)
  "Open th Direction DIR in a new terminal."
  (let (cmd)
    (cond
      ((string-equal system-type "windows-nt")
       (setq cmd (concat "wt -w 0 nt --tabColor #f59218 -d " (shell-quote-argument (expand-file-name dir)))))
      ((string-equal system-type "darwin")
       (setq cmd (concat "open -a terminal " (shell-quote-argument (expand-file-name dir)))))
      ((string-equal system-type "gnu/linux")
       (setq cmd (concat "x-terminal-emulator --working-directory=" (shell-quote-argument dir))))
      (t (error "Not support System!!")))
    (message "Terminal Cmd: %s" cmd)
    (when cmd
      (let ((process-connection-type nil))
        (start-process-shell-command "" nil cmd)))
    ))

;; https://tortoisesvn.net/docs/release/TortoiseSVN_en/tsvn-automation.html
;; 只文件
(defun svn-blame-in-windows (file)
  "Show TortoiseProc exe blame window for FILE. Only Support Windows!!!"
  (if (string-equal system-type "windows-nt")
      (let ((process-connection-type nil)
            (cmd (concat "TortoiseProc.exe /command:blame /path:" (shell-quote-argument file))))
        (message "Svn Blame Cmd: %s" cmd)
        (start-process-shell-command "" nil cmd))
    (error "Only Support Windows!!!")))

;; 文件或目录
(defun svn-log-in-windows (path)
  "Show TortoiseProc exe log window for PATH. Only Support Windows!!!"
  (if (string-equal system-type "windows-nt")
      (let ((process-connection-type nil)
            (cmd (concat "TortoiseProc.exe /command:log /path:" (shell-quote-argument path))))
        (message "Svn Log Cmd: %s" cmd)
        (start-process-shell-command "" nil cmd))
    (error "Only Support Windows!!!")))

(defun ffip-search-do-action (find-directory-p action)
  "Find file with ffip then do ACTION."
  (let ((keyword (ffip-read-keyword))
        cands lnum)
    (when (and keyword (stringp keyword) (string-match "^\\(.*\\):\\([0-9]+\\):?$" keyword))
      (setq lnum (string-to-number (match-string 2 keyword)))
      (setq keyword (match-string 1 keyword)))
    (setq cands (ffip-project-search keyword find-directory-p))
    (if (> (length cands) 0)
        (ffip-completing-read (ffip-hint) cands action)
      (message "Nothing found!"))))

(defun ffip-ivy-read (find-directory-p action caller)
  "Find file with ffip then use 'ivy-read' dispatch to CALLER do ACTION."
  (let ((keyword (ffip-read-keyword))
        cands lnum)
    (when (and keyword (stringp keyword) (string-match "^\\(.*\\):\\([0-9]+\\):?$" keyword))
      (setq lnum (string-to-number (match-string 2 keyword)))
      (setq keyword (match-string 1 keyword)))
    (setq cands (ffip-project-search keyword find-directory-p))
    (if (> (length cands) 0)
          (progn
            (ivy-read (ffip-hint)
                      cands
                      :matcher #'counsel--find-file-matcher
                      :action action
                      :history 'file-name-history
                      :keymap counsel-find-file-map
                      :caller caller))
      (message "Nothing found!"))))

;;;;;;;;;;;;; Cmd 
(defun ran-vscode ()
  "Use vscode."
  (interactive)
  (if (null current-prefix-arg)
      (let ((path (buffer-file-name)))
        (when (and (not path) (eq major-mode 'dired-mode))
          (let ((marks (dired-get-marked-files)))
            (if (> (length marks) 0)
                (setq path (car marks))
              (setq path (dired-current-directory)))
            ))
        (if path
            (open-in-vscode path)            
          (ffip-search-do-action nil #'open-in-vscode)))
    (ffip-search-do-action nil #'open-in-vscode)))

;; 有问题 todo
(defun ran-terminal ()
  "Use terminal."
  (interactive)
  (if (null current-prefix-arg)
      (let* ((path (buffer-file-name))
            (dir (when path (file-name-directory path))))
        (when (and (not dir) (eq major-mode 'dired-mode))
          (setq dir (dired-current-directory)))
        (if dir
            (open-in-terminal dir)
          (ffip-search-do-action t #'open-in-terminal)))
    (ffip-search-do-action t #'open-in-terminal)))

(defun ran-desktop ()
  "Show Desktop."
  (interactive)
  (if (null current-prefix-arg)
      (let* ((path (buffer-file-name))
            (dir (when path (file-name-directory path))))
        (when (and (not dir) (eq major-mode 'dired-mode))
          (setq dir (dired-current-directory)))
        (if dir
            (show-in-desktop dir)
          (ffip-search-do-action t #'show-in-desktop)))
    (ffip-search-do-action t #'show-in-desktop)))


;; 只文件
(defun ran-svn-blame-in-windows ()
  "Svn blame."
  (interactive)
  (if (null current-prefix-arg)
      (let ((path (buffer-file-name)))
        (when (and (not path) (eq major-mode 'dired-mode))
          (let ((marks (dired-get-marked-files)))
            (when (> (length marks) 0)
                (setq path (car marks)))))
        (if path
            (svn-blame-in-windows path)            
          (ffip-search-do-action nil #'svn-blame-in-windows)))
    (ffip-search-do-action nil #'svn-blame-in-windows)))

;; 文件和目录
(defun ran-svn-log-in-windows ()
  "Svn log."
  (interactive)
  (if (null current-prefix-arg)
      (let ((path (buffer-file-name)))
        (when (and (not path) (eq major-mode 'dired-mode))
          (let ((marks (dired-get-marked-files)))
            (if (> (length marks) 0)
                (setq path (car marks))
              (setq path (dired-current-directory)))
            ))
        (if path
            (svn-log-in-windows path)
          (ffip-search-do-action nil #'svn-log-in-windows)))
    (ffip-search-do-action (integerp current-prefix-arg) #'svn-log-in-windows)))

(provide 'init-ffip)
