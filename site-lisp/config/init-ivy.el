
(require 'ivy)
(require 'counsel)
(require 'lazy-load)
(require 'zlua)

(when sys/mac-cocoa-p
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(ivy-mode t)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)
;; Show #/total when scrolling buffers
(setq ivy-count-format "%d/%d ")
;; work around ivy issue.
;; @see https://github.com/abo-abo/swiper/issues/828
(setq ivy-display-style 'fancy)

;; (setq counsel-find-file-at-point t)
(eval-after-load 'counsel
  '(progn
    ;; automatically pick up cygwin cli tools for counsel
    (cond
      ((executable-find "rg")
       ;; ripgrep says that "-n" is enabled actually not,
       ;; so we manually add it
       (setq counsel-grep-base-command
             (concat (executable-find "rg")
                     " -n -M 512 --no-heading --color never -i \"%s\" %s")))
      ('t (message "Not find rg")))
    ))

(defun counsel-dired-jump@override (orig &optional initial-input initial-directory)
  "Jump to a directory (see `dired-jump') below the current directory.
List all sub-directories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
if nil, is used as the project root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (counsel-read-directory-name "From directory: "))))
  ;; Ĭ���ڹ���������Ŀ¼
  (unless initial-directory
    (let ((project (project-current)))
      (when project
          (setq initial-directory (expand-file-name (cdr project))))))

  (counsel-require-program find-program)
  (let ((default-directory (or initial-directory default-directory)))
    (ivy-read "Find directory: "
              (cdr
               (counsel--find-return-list counsel-dired-jump-args))
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action (lambda (d) (dired-jump nil (expand-file-name d)))
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-dired-jump)))

(advice-add 'counsel-dired-jump :override #'counsel-dired-jump@override)

;; ����zlua�ű�·��
(setq zlua-path "~/ransysconf/zlua/z.lua")

;;; ### Unset key ###
(lazy-load-unset-keys                   ; ȫ�ְ���ж��
 '("C-x C-f" "M-x" "C-x b" "M-y"))
(lazy-load-set-keys
 '(
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ("C-c c s" . swiper-isearch)
   ("C-c c z" . zlua-jump-to-directory)

   ("M-y" . counsel-yank-pop)

   ("C-c M-l" . counsel-locate)
   ("C-c M-d" . counsel-dired-jump)
   ("C-c M-e" . counsel-find-file-extern)
   ;; ("C-c M-i" . counsel-semantic-or-imenu)
   ))

;; Integration with `magit'
(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'init-ivy)