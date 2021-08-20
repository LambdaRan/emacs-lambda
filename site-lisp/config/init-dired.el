;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-dired.el --- Dired configuration

;; Filename: init-dired.el
;; Description: Dired configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:38:03
;; Version: 0.2
;; Last-Updated: 2017-10-16 23:14:09
;;           By: Andy Stewart
;; URL:
;; Keywords: dired
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Dired configuration
;;

;;; Installation:
;;
;; Put init-dired.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-dired)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'dired)
(require 'dired-x)

;;; Code:

(setq dired-recursive-copies t)         ;可以递归的进行拷贝
(setq dired-recursive-deletes t)        ;可以递归的删除目录
(setq dired-recursive-deletes 'always)  ;删除东西时不提示
(setq dired-recursive-copies 'always)   ;拷贝东西时不提示
(setq dired-listing-switches "-aluh")   ;传给 ls 的参数
(setq dired-details-hidden-string "")   ;设置隐藏dired里面详细信息的字符串
(setq directory-free-space-args "-Pkh") ;目录空间选项
(setq dired-omit-size-limit nil)        ;dired忽略的上限
(setq dired-dwim-target t)              ;Dired试着猜处默认的目标目录
(setq my-dired-omit-status t)           ;设置默认忽略文件
(setq my-dired-omit-regexp "^\\.?#\\|^\\..*") ;设置忽略文件的匹配正则表达式
(setq dired-omit-verbose nil)                 ; 不显示message
(setq my-dired-omit-extensions '(".cache" ".BIN" ".Msi")) ;设置忽略文件的扩展名列表
(add-hook 'dired-after-readin-hook #'(lambda ()
                                       (progn
                                         (require 'dired-extension)
                                         (dired-sort-method)))) ;先显示目录, 然后显示文件
(add-hook 'dired-mode-hook
          #'(lambda ()
              (require 'dired-extension)
              (dired-omit-method)                 ;隐藏文件的方法
              ))

;; 拷贝文件时猜测目的地
(setq dired-dwim-target t)
;; 隐藏详细信息，按左括号键'('切换状态
(add-hook 'dired-after-readin-hook 'dired-hide-details-mode)

;; ls does not support --dired; see ‘dired-use-ls-dired’ for more details.
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq dired-guess-shell-alist-user      ;设置文件默认打开的模式
      '(
        ;; 压缩包
        (list "\\.rar$" "unrar e -ad")
        (list "\\.tar.bz2$" "tar jxvf")
        (list "\\.gz$" "gzip -d")
        (list "\\.tar.gz$" "tar zxvf")
        ;; 其他
        (list "\\.exe$" "wine")))

;;; ### Dired ###
;;; --- 文件浏览器
(lazy-load-set-keys
 '(
   ("h" . dired-next-subdir)            ;下一个子目录
   ("l" . dired-prev-subdir)            ;上一个子目录
   ("n" . dired-next-dirline)           ;下一个目录
   ("p" . dired-prev-dirline)           ;上一个目录
   ("P" . dired-do-kill-lines)          ;删除标记的行
   ("I" . image-dired)                  ;打开浏览模式
   ("w" . dired-x-find-file)            ;查找文件
   ("z" . dired-goto-file)              ;跳到某个文件
   ("J" . awesome-tab-backward-tab)
   ("K" . awesome-tab-forward-tab)
   ("," . dired-diff)                        ;比较文件
   ("c" . kill-this-buffer)                  ;关闭当前标签
   ("W" . wdired-change-to-wdired-mode)      ;切换到dired编辑模式
   )
 dired-mode-map)

(lazy-load-local-keys
 '(
   ("W" . wdired-change-to-wdired-mode))
 dired-mode-map
 "wdired")

;; search file name only when focus is over file
(setq dired-isearch-filenames 'dwim)
(lazy-load-set-keys
 '(
   ("C-s" . dired-isearch-filenames)
   ("ESC C-s" . dired-isearch-filenames-regexp)
   )
 dired-mode-map)

(lazy-load-local-keys
 '(
   ("M-o" . dired-toggle-omit)          ;切换忽略状态
   ("?" . dired-get-size)               ;得到文件的大小
   ("[" . dired-rename-with-copy)       ;重命名函数
   ("'" . dired-up-directory-single)    ;返回上一级目录
   ("4" . dired-serial-rename)          ;批量重命名
   ("7" . dired-move-to-last-file)      ;移动到最后一个文件
   ("8" . dired-move-to-first-file)     ;移动到第一个文件
   ("k" . dired-previous-file-line)     ;上一行
   ("j" . dired-next-file-line)         ;下一行
   ("e" . dired-touch-now)              ;Touch命令
   ("f" . dired-find-file+)             ;打开当前文件或目录
   )
 dired-mode-map
 "dired-extension")

(defun dired-open-file ()
  "Dired find file function.
Open file use another tool"
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)))

(lazy-load-local-keys
 '(("F" . dired-open-file))              ;批量打开文件
 dired-mode-map)

(provide 'init-dired)

;;; init-dired.el ends here
