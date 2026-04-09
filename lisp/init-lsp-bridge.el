;;; init-lsp-bridge.el --- LSP Bridge configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; This file configures lsp-bridge as a replacement for lsp-mode.
;;; lsp-bridge uses its own completion framework (acm), NOT company-mode.
;;; It communicates with LSP servers via a Python subprocess for better performance.

;;; Code:

;; Add lsp-bridge to load path (installed via git clone in .emacs.d)
(add-to-list 'load-path (expand-file-name "lsp-bridge" user-emacs-directory))

;; Require dependencies before lsp-bridge
(require 'yasnippet)

;; Load lsp-bridge only if Python deps are available
(condition-case err
    (progn
      (require 'lsp-bridge)
      (message "lsp-bridge loaded successfully"))
  (error
   (message "Failed to load lsp-bridge: %s" (error-message-string err))
   (message "Python dependencies are missing. Please install them first.")
   ;; Disable lsp-bridge completely for now
   (setq global-lsp-bridge-mode nil)
   (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))))

;; ===========================================
;; Basic Settings
;; ===========================================

;; Enable signature help (function parameter hints)
(setq lsp-bridge-enable-signature-help t)

;; Enable diagnostics (error/warning underlines)
(setq lsp-bridge-enable-diagnostics t)

;; Enable search words across workspace
(setq lsp-bridge-enable-search-words t)

;; Disable auto-format on save (can be slow on large files)
(setq lsp-bridge-enable-auto-format-code nil)

;; Disable debug logging
(setq lsp-bridge-enable-debug nil)

;; Enable inlay hints (e.g., Rust type hints)
;; (setq lsp-bridge-enable-inlay-hint t)

;; ===========================================
;; Language Server Settings
;; ===========================================

;; C/C++: use clangd (default)
(setq lsp-bridge-c-lsp-server "clangd")

;; Python: use basedpyright (default, faster than pyright/pylsp)
(setq lsp-bridge-python-lsp-server "basedpyright")

;; Ensure lsp-bridge finds Python correctly
;; Try to find python3, fallback to python
(if (executable-find "python3")
    (setq lsp-bridge-python-command "python3")
  (setq lsp-bridge-python-command "python"))

;; Debug: show lsp-bridge status in Python mode
(add-hook 'python-mode-hook
          (lambda ()
            (message "Python mode hook: lsp-bridge-mode = %s" (lsp-bridge-mode))))

;; Before loading lsp-bridge, ensure no other LSP is active
(defun my/disable-other-lsp ()
  "Disable other LSP servers before enabling lsp-bridge"
  (when (featurep 'lsp-mode)
    (lsp-mode -1))
  (when (featurep 'lsp-pyright)
    (lsp-pyright-mode -1)))

;; Add this hook to disable other LSP when switching buffers
(add-hook 'find-file-hook 'my/disable-other-lsp)

;; Rust: rust-analyzer is the default, no need to set

;; ===========================================
;; ACM (completion UI) Settings
;; ===========================================

;; Enable auto-import on completion
(setq acm-backend-lsp-enable-auto-import t)

;; Enable yasnippet in completion
(setq acm-enable-yas t)

;; Enable doc display in completion popup
(setq acm-enable-doc t)

;; Enable icons in completion
(setq acm-enable-icon t)

;; ===========================================
;; Disable company-mode in lsp-bridge buffers
;; ===========================================
;; lsp-bridge uses its own acm completion framework.
;; company-mode must be disabled to avoid conflicts.

(add-hook 'lsp-bridge-mode-hook
          (lambda ()
            (company-mode -1)))

;; ===========================================
;; Key Bindings (preserve original lsp-mode bindings)
;; ===========================================

(define-key lsp-bridge-mode-map (kbd "<f12>") #'lsp-bridge-find-def)
(define-key lsp-bridge-mode-map (kbd "M-<left>") #'lsp-bridge-find-def-return)
(define-key lsp-bridge-mode-map (kbd "M-?") #'lsp-bridge-find-references)
(define-key lsp-bridge-mode-map (kbd "C-c C-d") #'lsp-bridge-popup-documentation)
(define-key lsp-bridge-mode-map (kbd "C-c l s") #'lsp-bridge-workspace-list-symbols)
(define-key lsp-bridge-mode-map (kbd "C-c l r") #'lsp-bridge-rename)
(define-key lsp-bridge-mode-map (kbd "C-c l a") #'lsp-bridge-code-action)
(define-key lsp-bridge-mode-map (kbd "C-c l f") #'lsp-bridge-code-format)
(define-key lsp-bridge-mode-map (kbd "C-c l i") #'lsp-bridge-find-impl)
(define-key lsp-bridge-mode-map (kbd "C-c l t") #'lsp-bridge-find-type-def)

;; ===========================================
;; Enable lsp-bridge for specific modes
;; ===========================================

;; Remove the default lsp-mode hook override in init-programming-python.el
;; to avoid conflicts with lsp-bridge
(defun enable-lsp-if-not-remote ()
  "Enable lsp-bridge if not in remote directory."
  (unless (file-remote-p default-directory)
    (lsp-bridge-mode)))

;; Temporarily disable lsp-bridge to prevent errors
;; (global-lsp-bridge-mode)  ; Commented out

;; Manually enable for specific modes only when needed
(dolist (hook '(c-mode-hook c++-mode-hook python-mode-hook rust-mode-hook))
  (add-hook hook 'enable-lsp-if-not-remote))

;; Override the python-mode hook specifically to use lsp-bridge instead of lsp-pyright
(add-hook 'python-mode-hook
          (lambda ()
            ;; First, disable any lsp-mode related functions
            (when (featurep 'lsp-mode)
              (lsp-mode -1))
            (when (featurep 'lsp-pyright)
              (lsp-pyright-mode -1))
            ;; Then enable lsp-bridge if not remote
            (unless (file-remote-p default-directory)
              (lsp-bridge-mode))))

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
