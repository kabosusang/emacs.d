;;; disable-lsp-bridge.el --- Disable lsp-bridge temporarily

;;; Code:

;; Disable lsp-bridge completely
(setq global-lsp-bridge-mode nil)
(unless (featurep 'lsp-bridge)
  (message "lsp-bridge not loaded"))

;; Revert to original lsp-mode setup for now
;; Enable lsp-mode for all files
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c-mode c++-mode python-mode rust-mode) . lsp-deferred)
  :config
  (setq lsp-completion-provider :none))

(provide 'disable-lsp-bridge)