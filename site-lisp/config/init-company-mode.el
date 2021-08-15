;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-company-mode.el --- Company-mode configuration

;;; Require

;;; Code:
;; (add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (require company)
;;               (global-company-mode)))

(add-hook 'prog-mode-hook
          #'(lambda ()
              (require 'company)
              (global-company-mode)))

(defun company-tabnine-sort-by-detail (candidates)
  "Sort tabnine response by detail value"
  (sort candidates
        (lambda (c1 c2)
          (let ((d1 (get-text-property 0 'detail c1))
                (d2 (get-text-property 0 'detail c2)))
            ;; 默认忽略type
            (or d1 (setq d1 "1%"))
            (or d2 (setq d2 "1%"))
            (>= (string-to-number d1) (string-to-number d2))))))

(defun company-sort-by-tabnine-and-ctags (candidates)
  "sort company backends response"
  (when (or (functionp company-backend)
            (not (and (listp company-backend)
                      (memq 'company-tabnine company-backend))))
    candidates)
  (let (candidates-tabnine candidates-ctags candidates-yas)
    (setq candidates-max-length (min (length candidates) 20))
    (dolist (candidate candidates)
      (setq backend-property (get-text-property 0 'company-backend candidate))
      (cond
        ((eq backend-property 'company-ctags)
         (push candidate candidates-ctags))
        ((eq backend-property 'company-yasnippet)
         (push candidate candidates-yas))
        (t (push candidate candidates-tabnine))))
    ;; (setq candidates-tabnine (nreverse candidates-tabnine))
    (setq candidates-tabnine (company-tabnine-sort-by-detail candidates-tabnine))
    (setq candidates-ctags (nreverse candidates-ctags))
    (setq candidates-yas (nreverse candidates-yas))
    (setq candidates (nconc (seq-take candidates-tabnine 5)
                            (seq-take candidates-ctags 3)
                            (seq-take candidates-yas 2)))
    (setq candidates-other (nconc (seq-drop candidates-tabnine 5)
                                  (seq-drop candidates-ctags 3)
                                  (seq-drop candidates-yas 2)))
    (let ((len (length candidates)))
      (when (< len candidates-max-length)
        (setq candidates (nconc candidates (seq-take candidates-other (- candidates-max-length len))))))

    candidates))

(with-eval-after-load 'company
  (require 'lazy-load)
  (require 'company-yasnippet)
  (require 'company-dabbrev)
  (require 'company-files)
  (require 'company-tng)

  ;; Config for company mode.
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)
  ;; 补全的最小前缀长度
  (setq company-minimum-prefix-length 2) ; pop up a completion menu by tapping a character
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)   ; do not display numbers on the left
  (setq company-require-match nil) ; allow input string that do not match candidate words

  ;; `compay-dabbrev` search same major mode buffers.
  (setq company-dabbrev-other-buffers t)
  ;; (setq company-dabbrev-minimum-length 4)
  ;; (setq company-format-margin-function nil)
  ;; Don't downcase the returned candidates.
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t       
        ;; make previous/next selection in the popup cycles
        company-selection-wrap-around t
        ;; @see https://github.com/company-mode/company-mode/issues/146
        company-tooltip-align-annotations t)

  ;; (add-to-list 'company-transformers 'company-sort-by-backend-importance)
  ;; (add-to-list 'company-transformers 'company-sort-by-tabnine-and-ctags t)
  ;; Remove duplicate candidate.
  (add-to-list 'company-transformers #'delete-dups)
  ;; NOT to load company-mode for certain major modes.
  ;; Ironic that I suggested this feature but I totally forgot it
  ;; until two years later.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
        '(not
          shell-mode eshell-mode comint-mode erc-mode gud-mode rcirc-mode text-mode
          minibuffer-inactive-mode))

  ;; Customize company backends.
  ;; (setq company-backends (delete backend company-backends)))
  (setq company-backends '(company-capf
                           (company-dabbrev-code company-keywords)                           
                           company-dabbrev
                           company-files                           
                           ;; other backends
                           ))

  ;; Add `company-elisp' backend for elisp.
  (defun setup-company-elisp ()
    (require 'company-elisp)
    (unless (catch 'found
              (dolist (b company-backends)
                (cond
                 ((equal b 'company-elisp)
                  (throw 'found t))
                 ((and (listp b) (member 'company-elisp b))
                  (throw 'found t))
                 (t nil))))
      (add-to-list 'company-backends 'company-elisp)))
  (add-hook 'emacs-lisp-mode-hook 'setup-company-elisp)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  ;; Key settings.
  (lazy-load-unset-keys
   '("TAB")
   company-mode-map)         ;unset default keys

  (lazy-load-unset-keys
   '("M-p" "M-n" "M-s" "C-m" "C-n" "C-p" "C-h")
   company-active-map)

  (lazy-load-set-keys
   '(
     ("TAB" . company-complete-selection)
     ;; ("M-h" . company-complete-selection)
     ("C-h" . company-complete-common)
     ;; ("M-H" . company-complete-common)
     ("M-w" . company-show-location)
     ("M-s" . company-search-candidates)
     ("M-S" . company-filter-candidates)
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous)
     ("M-i" . yas-expand)
     ("RET" . company-complete-selection)
     )
   company-active-map)
  )

(with-eval-after-load 'company-yasnippet
  ;; Add yasnippet support for all company backends.
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (add-hook 'prog-mode-hook
            #'(lambda ()
                (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
                )))

(provide 'init-company-mode)

;;; init-company-mode.el ends here
