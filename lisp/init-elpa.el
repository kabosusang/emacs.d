;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'cl-lib)

;;; Standard package repositories
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; 清空默认源，使用清华 tuna 镜像
(setq package-archives nil)
(add-to-list 'package-archives '("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") t)
(add-to-list 'package-archives '("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)  ; org-mode 官方源

;; 可选：添加 melpa-stable（稳定版）
;; (add-to-list 'package-archives '("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/") t)

;;; Fire up package.el
;;(package-initialize)

(provide 'init-elpa)
;;; init-elpa.el ends here
