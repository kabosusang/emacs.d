;;; init-acm-terminal.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:
;;;     Common programming settings shared across all languages.
;;; Code:
(when (not (display-graphic-p))
  (add-hook 'after-init-hook
            (lambda ()
              (add-to-list 'load-path (expand-file-name "vendor/popon" user-emacs-directory))
              (require 'popon)
              
              (add-to-list 'load-path (expand-file-name "vendor/acm-terminal" user-emacs-directory))
              (require 'acm-terminal)
              )))


(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))



(provide 'init-acm-terminal.el)
