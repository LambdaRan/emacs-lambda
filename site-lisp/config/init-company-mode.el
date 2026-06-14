;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-company-mode.el --- Company-mode configuration

;;; Require

;;; Code:

(require 'company)
(global-company-mode 1)
(require 'lazy-load)
(require 'company-dabbrev)
(require 'company-files)
(require 'company-tng)

;; Config for company mode.
;; Trigger completion immediately.
(setq company-idle-delay 0.2)
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

;; NOT to load company-mode for certain major modes.
;; Ironic that I suggested this feature but I totally forgot it
;; until two years later.
;; https://github.com/company-mode/company-mode/issues/29
(setq company-global-modes
      '(not
        shell-mode eshell-mode comint-mode erc-mode gud-mode rcirc-mode text-mode
        minibuffer-inactive-mode))

;; Customize company backends.
(setq company-backends '((company-capf company-dabbrev)
                         company-keywords
                         company-files
                         ;; other backends
                         ))

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

(provide 'init-company-mode)

;;; init-company-mode.el ends here
