;;; cleanup-lsp.el --- Clean up conflicting LSP configurations

;;; Code:

;; Function to safely disable all LSP packages
(defun my/disable-all-lsp ()
  "Safely disable all LSP servers"
  (interactive)

  ;; Disable lsp-mode and its derivatives
  (when (featurep 'lsp-mode)
    (lsp-mode -1))
  (when (featurep 'lsp-pyright)
    (lsp-pyright-mode -1))
  (when (featurep 'lsp-pylsp)
    (lsp-pylsp-mode -1))
  (when (featurep 'lsp-bridge)
    (lsp-bridge-mode -1))

  ;; Report status
  (message "All LSP servers disabled")
  (message "lsp-mode: %s" (featurep 'lsp-mode))
  (message "lsp-bridge: %s" (featurep 'lsp-bridge)))

;; Function to enable only lsp-bridge
(defun my/enable-only-lsp-bridge ()
  "Enable only lsp-bridge, disable others"
  (interactive)

  ;; First disable all
  (my/disable-all-lsp)

  ;; Then enable lsp-bridge
  (require 'lsp-bridge)
  (global-lsp-bridge-mode)
  (message "lsp-bridge enabled: %s" (global-lsp-bridge-mode)))

;; Test function
(defun my/test-lsp-status ()
  "Show current LSP status"
  (interactive)
  (message "=== LSP Status ===")
  (message "lsp-mode feature: %s" (featurep 'lsp-mode))
  (message "lsp-bridge feature: %s" (featurep 'lsp-bridge))
  (message "lsp-pyright feature: %s" (featurep 'lsp-pyright))
  (message "lsp-mode active: %s" lsp-mode)
  (message "lsp-bridge mode: %s" (lsp-bridge-mode))
  (message "Company mode: %s" company-mode)
  (message "=================="))

(provide 'cleanup-lsp)