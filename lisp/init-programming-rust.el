;;; init-programming-rust.el --- Rust Configuration -*- lexical-binding: t -*-
;;; Code:

(use-package rust-mode
  :ensure t
  :functions dap-register-debug-template
  :hook
  ((rust-mode . my/rust-mode-setup))
  :bind
  (:map rust-mode-map
        ("C-c f" . rust-format-buffer)
        ("C-c C-f" . rust-format-buffer))
  :config
  (defun my/rust-mode-setup ()
    "Setup for rust-mode"
    (message "Rust mode setup"))
  
  ;; Debug 模板
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb-vscode"
                                     :request "launch"
                                     :name "rust-lldb::Run"
                                     :target nil
                                     :cwd nil)))

(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))

;; 禁用 Rust 模式的 flycheck
(with-eval-after-load 'flycheck
  (add-hook 'rust-mode-hook (lambda () (flycheck-mode -1))))

(provide 'init-programming-rust)
;;; init-programming-rust.el ends here
