;;; init-lsp-bridge.el --- LSP Bridge configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; This file configures lsp-bridge as a replacement for lsp-mode.
;;; lsp-bridge uses its own completion framework (acm), NOT company-mode.
;;; It communicates with LSP servers via a Python subprocess for better performance.

;;; Code:

;; Add lsp-bridge to load path (installed via git clone in .emacs.d)
(add-to-list 'load-path (expand-file-name "vendor/lsp-bridge" user-emacs-directory))

;; 使用 use-package 声明 yasnippet 依赖
(use-package yasnippet
  :ensure t
  :defer t)

;; Load lsp-bridge only if Python deps are available
(condition-case err
    (progn
      (require 'lsp-bridge)
      (message "lsp-bridge loaded successfully"))
  (error
   (message "Failed to load lsp-bridge: %s" (error-message-string err))
   (message "Python dependencies are missing. Please install them first.")
   ;; Disable lsp-bridge completely for now
   (setq global-lsp-bridge-mode nil)))

;; ===========================================
;; All lsp-bridge configuration in with-eval-after-load
;; ===========================================
(with-eval-after-load 'lsp-bridge
  
  ;; Basic Settings
  (setq lsp-bridge-enable-signature-help t)
  (setq lsp-bridge-enable-diagnostics t)
  (setq lsp-bridge-enable-search-words t)
  (setq lsp-bridge-enable-auto-format-code nil)
  (setq lsp-bridge-enable-debug nil)
  (setq lsp-bridge-enable-inlay-hint t)
  (setq lsp-bridge-inlay-hint-delay 1.5) 

  ;; Language Server Settings
  (setq lsp-bridge-c-lsp-server "clangd")
  (setq lsp-bridge-python-lsp-server "basedpyright")

  ;; Python path
  (if (executable-find "python3")
      (setq lsp-bridge-python-command "python3")
    (setq lsp-bridge-python-command "python"))

  ;; ACM (completion UI) Settings
  (setq acm-backend-lsp-enable-auto-import t)
  (setq acm-enable-yas t)
  (setq acm-enable-doc t)
  (setq acm-enable-icon t)

  ;; Disable company-mode in lsp-bridge buffers
  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (company-mode -1)))

  ;; Key Bindings
  (define-key lsp-bridge-mode-map (kbd "<f12>") #'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "M-<left>") #'lsp-bridge-find-def-return)
  (define-key lsp-bridge-mode-map (kbd "M-?") #'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "C-c C-d") #'lsp-bridge-popup-documentation)
  (define-key lsp-bridge-mode-map (kbd "C-c l s") #'lsp-bridge-workspace-list-symbols)
  (define-key lsp-bridge-mode-map (kbd "C-c l r") #'lsp-bridge-rename)
  (define-key lsp-bridge-mode-map (kbd "C-c l a") #'lsp-bridge-code-action)
  (define-key lsp-bridge-mode-map (kbd "C-c l f") #'lsp-bridge-code-format)
  (define-key lsp-bridge-mode-map (kbd "C-c l i") #'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-c l t") #'lsp-bridge-find-type-def))

;; ===========================================
;; Functions and hooks (can be defined before lsp-bridge loads)
;; ===========================================

;; Before loading lsp-bridge, ensure no other LSP is active
(defun my/disable-other-lsp ()
  "Disable other LSP servers before enabling lsp-bridge"
  (when (featurep 'lsp-mode)
    (lsp-mode -1))
  (when (featurep 'lsp-pyright)
    (lsp-pyright-mode -1)))

;; Add this hook to disable other LSP when switching buffers
(add-hook 'find-file-hook 'my/disable-other-lsp)

;; Debug: show lsp-bridge status in Python mode
(add-hook 'python-mode-hook
          (lambda ()
            (message "Python mode hook: lsp-bridge-mode = %s" (lsp-bridge-mode))))

;; Enable lsp-bridge for specific modes
(defun enable-lsp-if-not-remote ()
  "Enable lsp-bridge if not in remote directory."
  (unless (file-remote-p default-directory)
    (lsp-bridge-mode)))

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

;; ===========================================
;; Mouse hover support via help-echo overlay
;; ===========================================
(defvar my/lsp-bridge-hover-overlay nil
  "Overlay for displaying hover info as help-echo on mouse hover.")
(defvar my/lsp-bridge-hover-timer nil
  "Idle timer for updating hover help-echo.")
(defvar my/lsp-bridge-hover--intercept nil
  "Non-nil when we should intercept the hover callback result.")
(defvar my/lsp-bridge-hover--target-pos nil
  "Buffer position where the hover overlay should be placed.")
(defvar my/lsp-bridge-hover--last-pos nil
  "Last position where hover was requested.")

(defun my/lsp-bridge-hover--intercept-callback (orig-fun value)
  "Intercept hover callback to set help-echo instead of showing buffer."
  (if my/lsp-bridge-hover--intercept
      (progn
        (setq my/lsp-bridge-hover--intercept nil)
        (when my/lsp-bridge-hover--target-pos
          (save-excursion
            (goto-char my/lsp-bridge-hover--target-pos)
            (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                        (ov (or my/lsp-bridge-hover-overlay
                                (make-overlay (car bounds) (cdr bounds)))))
              (move-overlay ov (car bounds) (cdr bounds))
              (overlay-put ov 'help-echo
                           (replace-regexp-in-string
                            "```[^`]*```" ""
                            (replace-regexp-in-string "\n\n+" "\n" value)))
              (setq my/lsp-bridge-hover-overlay ov)))
          (setq my/lsp-bridge-hover--target-pos nil)))
    (funcall orig-fun value)))

(advice-add 'lsp-bridge-show-documentation--callback
            :around #'my/lsp-bridge-hover--intercept-callback)

(defun my/lsp-bridge-hover--update ()
  "Update help-echo overlay at current point via LSP hover."
  (when (and lsp-bridge-mode
             (not (minibufferp))
             (lsp-bridge-has-lsp-server-p)
             (symbol-at-point)
             (not (equal (point) my/lsp-bridge-hover--last-pos)))
    (let ((pos (point)))
      (setq my/lsp-bridge-hover--last-pos pos
            my/lsp-bridge-hover--intercept t
            my/lsp-bridge-hover--target-pos pos)
      (lsp-bridge-call-file-api
       "hover"
       (lsp-bridge--point-position pos)
       (lsp-bridge--point-position pos)
       "show"))))

(defun my/lsp-bridge-hover--cleanup ()
  "Remove hover overlay when cursor moves."
  (setq my/lsp-bridge-hover--last-pos nil)
  (when my/lsp-bridge-hover-overlay
    (delete-overlay my/lsp-bridge-hover-overlay)
    (setq my/lsp-bridge-hover-overlay nil)))

(defun my/lsp-bridge-hover-enable ()
  "Enable mouse hover support via help-echo."
  (interactive)
  (setq my/lsp-bridge-hover-timer
        (run-with-idle-timer 0.5 t #'my/lsp-bridge-hover--update))
  (add-hook 'post-command-hook #'my/lsp-bridge-hover--cleanup nil t)
  (message "Mouse hover (help-echo) enabled"))

(defun my/lsp-bridge-hover-disable ()
  "Disable mouse hover support."
  (interactive)
  (when my/lsp-bridge-hover-timer
    (cancel-timer my/lsp-bridge-hover-timer)
    (setq my/lsp-bridge-hover-timer nil))
  (remove-hook 'post-command-hook #'my/lsp-bridge-hover--cleanup t)
  (my/lsp-bridge-hover--cleanup)
  (message "Mouse hover disabled"))

;; Enable hover in C/C++ modes
(add-hook 'c-mode-hook #'my/lsp-bridge-hover-enable)
(add-hook 'c++-mode-hook #'my/lsp-bridge-hover-enable)
;; Also enable for rust and python
(add-hook 'rust-mode-hook #'my/lsp-bridge-hover-enable)
(add-hook 'python-mode-hook #'my/lsp-bridge-hover-enable)

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
