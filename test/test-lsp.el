;;; test-lsp.el --- Test configuration for lsp-bridge

;;; Commentary:
;;; This file helps test lsp-bridge functionality.

;;; Code:

;; Set up minimal configuration
(setq default-directory "/home/kabosu")

;; Add lsp-bridge to load path
(add-to-list 'load-path (expand-file-name "lsp-bridge" user-emacs-directory))

(require 'yasnippet)

;; Load lsp-bridge
(message "Loading lsp-bridge...")
(require 'lsp-bridge)
(message "lsp-bridge loaded: %s" (featurep 'lsp-bridge))

;; Define test function
(defun test-python-lsp ()
  "Test lsp-bridge with Python"
  (interactive)
  (message "=== Testing Python LSP ===")
  (message "1. lsp-bridge feature loaded: %s" (featurep 'lsp-bridge))
  (message "2. lsp-bridge-mode function exists: %s" (fboundp 'lsp-bridge-mode))
  (message "3. Python mode major mode: %s" (if (derived-mode-p 'python-mode) "Python" "Not Python"))
  (message "4. Buffer file: %s" (buffer-file-name))

  ;; Try to enable lsp-bridge
  (if (lsp-bridge-mode)
      (message "lsp-bridge is already active")
    (progn
      (lsp-bridge-mode)
      (message "Enabled lsp-bridge, status: %s" (lsp-bridge-mode))))

  (message "=== End Test ==="))

(provide 'test-lsp)