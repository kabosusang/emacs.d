;;; simple-test.el --- Simple test for lsp-bridge

;;; Code:

(message "Loading simple-test.el...")

;; Check if lsp-bridge is available
(unless (featurep 'lsp-bridge)
  (message "lsp-bridge not loaded yet, trying to load...")
  (add-to-list 'load-path (expand-file-name "lsp-bridge" user-emacs-directory))
  (require 'lsp-bridge)
  (message "lsp-bridge loaded: %s" (featurep 'lsp-bridge)))

;; Test function
(defun test-python-lsp ()
  (interactive)
  (message "Testing Python LSP...")
  (if (featurep 'lsp-bridge)
      (message "lsp-bridge is loaded")
    (message "ERROR: lsp-bridge is NOT loaded"))

  (when (featurep 'lsp-bridge)
    (message "Available commands:")
    (message "  lsp-bridge-mode: %s" (fboundp 'lsp-bridge-mode))
    (message "  lsp-bridge-find-def: %s" (fboundp 'lsp-bridge-find-def))
    (message "  lsp-bridge-status: %s" (fboundp 'lsp-bridge-status))))

(provide 'simple-test)