;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-awesome-tab.el --- Configuration for awesome-tab.el

(require 'awesome-tab)
(require 'all-the-icons)
(require 'init-const)

(awesome-tab-mode t)

(cond
  (sys/windows-p
   (setq awesome-tab-height 100)
   (setq awesome-tab-icon-height 0.7)
   (setq awesome-tab-active-bar-height 22))
  (sys/mac-p
   (setq awesome-tab-height 145)
   ;; (setq awesome-tab-icon-v-adjust 0)
   (setq awesome-tab-icon-height 0.9)
   ;; (setq awesome-tab-label-fixed-length 14)
   (setq awesome-tab-active-bar-height 24))
  (sys/linux-p
   (setq awesome-tab-height 100)
   (setq awesome-tab-icon-height 0.7)
   (setq awesome-tab-active-bar-height 22)   
   )
  (t (message "Not support OS.")))

(provide 'init-awesome-tab)
