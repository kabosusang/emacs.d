;;; init-programming.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:
;;;     Common programming settings shared across all languages.
;;; Code:

;; ========== lsp-bridge 通用配置 ==========

;; 自定义 lsp-bridge 项目根目录检测
(defun my/lsp-bridge-get-project-path (file-path)
  "自定义项目根目录检测"
  (let* ((default-directory (file-name-directory file-path))
         (project-root
          (cond
           ((string-match-p "\\.rs\\'" file-path)
            (or (locate-dominating-file default-directory "Cargo.toml")
                (locate-dominating-file default-directory ".git")))
           ((string-match-p "\\.py\\'" file-path)
            (or (locate-dominating-file default-directory "pyproject.toml")
                (locate-dominating-file default-directory "setup.py")
                (locate-dominating-file default-directory ".git")))
           ((string-match-p "\\.\\(c\\|cpp\\|h\\|hpp\\|ixx\\|cppm\\)\\'" file-path)
            (or (locate-dominating-file default-directory "compile_commands.json")
                (locate-dominating-file default-directory "CMakeLists.txt")
                (locate-dominating-file default-directory ".git")))
           (t (locate-dominating-file default-directory ".git")))))
    (expand-file-name (or project-root default-directory))))

(setq lsp-bridge-get-project-path-by-filepath #'my/lsp-bridge-get-project-path)

;; lsp-bridge 优化配置
(setq lsp-bridge-completion-delay 0.2)
(setq lsp-bridge-completion-min-interval 0.15)
(setq lsp-bridge-enable-auto-completion t)
(setq lsp-bridge-enable-completion-popup t)
(setq lsp-bridge-enable-semantic-completion t)
(setq lsp-bridge-enable-snippet t)
(setq lsp-bridge-completion-max-items 50)
(setq lsp-bridge-enable-diagnostics 'defer)
(setq lsp-bridge-diagnostics-delay 0.5)
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-enable-diagnostic t)



;; ========== 通用编程辅助功能 ==========

;; Print ANSI colors in compilation mode buffer
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun shell-other-window ()
  "Open shell in other window."
  (interactive)
  (other-window 1)
  (shell))

;; ========== 代码折叠 ==========
(add-hook 'prog-mode-hook #'hs-minor-mode)

(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-c z t") #'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-c z c") #'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c z o") #'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c z C-c") #'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c z C-o") #'hs-show-all)
  (define-key hs-minor-mode-map (kbd "<f2>") #'hs-toggle-hiding))

(setq hs-isearch-open t)
(setq hs-allow-nesting t)
(setq hs-hide-comments-when-hiding-all nil)

;; ========== 加载语言特定配置 ==========
(require 'init-programming-cpp)
(require 'init-programming-rust)
(require 'init-programming-python)
(require 'init-programming-scheme)


;;(require 'init-dape)

(provide 'init-programming)
;;; init-programming.el ends here
