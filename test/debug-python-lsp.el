;;; debug-python-lsp.el --- Debug Python LSP issues
;;; Commentary:
;;; This file helps debug why lsp-bridge isn't working for Python

;;; Code:

;; Force lsp-bridge for Python mode
(add-hook 'python-mode-hook
          (lambda ()
            (message "Python mode hook started...")

            ;; Disable any existing LSP clients
            (when (featurep 'lsp-mode)
              (message "Disabling lsp-mode...")
              (lsp-mode -1))
            (when (featurep 'lsp-pyright)
              (message "Disabling lsp-pyright...")
              (lsp-pyright-mode -1))

            ;; Check if we're in a remote directory
            (if (file-remote-p default-directory)
                (message "Remote directory, skipping lsp-bridge")
              (progn
                (message "Enabling lsp-bridge...")
                (lsp-bridge-mode)
                (message "lsp-bridge-mode status: %s" (lsp-bridge-mode-p)))))

;; Add after advice to track lsp-bridge initialization
(defadvice lsp-bridge-mode (after lsp-bridge-mode-advice activate)
  (message "lsp-bridge-mode called for %s, result: %s" major-mode lsp-bridge-mode))

;; Show status when opening Python files
(add-hook 'python-mode-hook
          (lambda ()
            (message "Python buffer loaded: %s" (buffer-file-name))
            (message "Current major mode: %s" major-mode)
            (message "lsp-bridge-mode is %s" (if (lsp-bridge-mode-p) "enabled" "disabled"))
            (when (featurep 'lsp-bridge)
              (message "lsp-bridge feature is loaded"))
            (message "Available LSP clients: %s" (lsp-bridge-servers))))

;; Test command: M-x test-lsp-bridge
(defun test-lsp-bridge ()
  "Test lsp-bridge functionality"
  (interactive)
  (message "Testing lsp-bridge...")
  (message "Current buffer: %s" (buffer-file-name))
  (message "Major mode: %s" major-mode)
  (message "lsp-bridge-mode: %s" (lsp-bridge-mode-p))
  (message "Features loaded: %s" (featurep 'lsp-bridge))

  ;; Try to manually enable lsp-bridge
  (if (lsp-bridge-mode-p)
      (message "lsp-bridge is already active")
    (progn
      (lsp-bridge-mode)
      (message "Manually enabled lsp-bridge, status: %s" (lsp-bridge-mode-p)))))

(provide 'debug-python-lsp)