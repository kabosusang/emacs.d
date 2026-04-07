;;; init-programming-python.el --- Python Config
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Python 配置 (使用 uv + lsp-pyright)
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook
  (python-mode . (lambda ()
                   ;; 设置 Python 解释器路径（使用 uv 管理的虚拟环境）
                   (setq-local python-indent-offset 4)
                   (setq-local python-indent-guess-indent-offset nil)
                   
                   ;; 尝试自动检测并激活 uv 虚拟环境
                   (when (and (executable-find "uv")
                              (or (file-exists-p "pyproject.toml")
                                  (file-exists-p "requirements.txt")
                                  (file-exists-p ".venv")))
                     (my/setup-uv-virtualenv))
                   
                   ;; 可选：禁用自动格式化（如果不需要，可以注释掉）
                   ;; (setq-local electric-indent-local-mode -1)
                   ))
  :config
  ;; 启用 Python 语法高亮的额外特性
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4))

;; 使用 pyvenv 管理虚拟环境（可选，如果你需要切换环境）
(use-package pyvenv
  :ensure t
  :config
  ;; 设置虚拟环境目录（uv 默认使用 .venv）
  (setq pyvenv-default-virtualenv-name ".venv")
  (setq pyvenv-workon-home nil)  ; 使用项目本地 .venv
  :hook
  (python-mode . pyvenv-mode))

;; LSP 服务器配置 - 使用基于 pyright 的 lsp-pyright（更现代）
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred)))
  :init
  (setq lsp-pyright-langserver-command "pyright")
  :custom
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-typecheck-mode "strict")  ; 可选: "off", "standard", "strict"
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-diagnostic-mode "workspace"))

;; 替代方案：如果你更喜欢 python-lsp-server (pylsp)，可以使用这个配置
;; 但 lsp-pyright 通常更快、更准确
;; (use-package lsp-pylsp
;;   :ensure t
;;   :hook (python-mode . (lambda () (require 'lsp-pylsp) (lsp-deferred))))

;; Python 辅助工具集成
(use-package poetry  ; 如果你使用 poetry（uv 可以兼容 poetry 项目）
  :ensure t
  :defer t
  :config
  (setq poetry-tracking-strategy 'switch-buffer)
  (setq poetry-after-switch-project-functions
        '((lambda () (pyvenv-workon poetry-venv-name)))))

;; 更好的 Python 代码导航和重构
(use-package pyimport
  :ensure t
  :defer t
  :bind (:map python-mode-map
              ("C-c i" . pyimport-insert-missing)
              ("C-c r" . pyimport-remove-unused)))

;; 辅助函数：设置 uv 虚拟环境
(defun my/setup-uv-virtualenv ()
  "自动检测并激活 uv 管理的虚拟环境"
  (interactive)
  (let* ((project-root (or (locate-dominating-file default-directory ".git")
                           (locate-dominating-file default-directory "pyproject.toml")
                           (locate-dominating-file default-directory "requirements.txt")))
         (venv-path (when project-root
                      (expand-file-name ".venv" project-root))))
    (when (and venv-path (file-exists-p venv-path))
      ;; 设置 pyvenv 使用的虚拟环境
      (pyvenv-activate venv-path)
      
      ;; 设置 LSP 使用的 Python 路径
      (let ((python-path (expand-file-name "bin/python" venv-path)))
        (when (file-exists-p python-path)
          (setq-local lsp-python-venv-path venv-path)
          (setq-local lsp-python-executable-cmd python-path)
          (setq-local python-shell-interpreter python-path)))
      
      (message "已激活 uv 虚拟环境: %s" venv-path))))

;; 快捷命令：使用 uv 运行当前脚本
(defun my/python-run-uv ()
  "使用 uv run 执行当前 Python 文件"
  (interactive)
  (let* ((current-file (buffer-file-name))
         (project-root (locate-dominating-file default-directory "pyproject.toml")))
    (if (and project-root (executable-find "uv"))
        (let ((default-directory project-root))  ; 关键：直接设置编译的默认目录
          (compile (format "uv run python %s" 
                          (shell-quote-argument (file-relative-name current-file project-root)))))
      (message "未找到 pyproject.toml 或 uv 命令"))))


;; 绑定快捷键
(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "C-c C-c") 'my/python-run-uv)
  (define-key python-mode-map (kbd "C-c v") 'pyvenv-activate)
  (define-key python-mode-map (kbd "C-c V") 'pyvenv-deactivate))

;; ;; 调试配置（如果你使用 dap-mode）
;; (use-package dap-python
;;   :ensure t
;;   :after dap-mode
;;   :config
;;   ;; 设置 dap-python 使用的 Python 解释器
;;   (setq dap-python-executable 
;;         (lambda () 
;;           (or (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env "bin/python")
;;               "python3"))))

;; 项目根目录检测增强
(defun my/python-project-root ()
  "检测 Python 项目根目录"
  (or (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory "setup.py")
      (locate-dominating-file default-directory "requirements.txt")
      (locate-dominating-file default-directory ".venv")
      (locate-dominating-file default-directory ".git")
      default-directory))

;; 添加到 lsp 根目录检测
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local lsp-locate-project-root-function #'my/python-project-root)))

(provide 'init-programming-python)
