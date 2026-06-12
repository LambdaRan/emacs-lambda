;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-const)

(tool-bar-mode -1)                      ;禁用工具栏
(menu-bar-mode -1)                      ;禁用菜单栏
(scroll-bar-mode -1)                    ;禁用滚动条

(when sys/mac-cocoa-p
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil)
  ;; 默认先最大化。
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (run-at-time "1sec" nil #'(lambda () (toggle-frame-fullscreen)))
)

(provide 'init-startup)
