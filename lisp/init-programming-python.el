;;; init-programming-python.el --- Python Config
;;; Commentary:
;;; Code:

;; Python 配置 (使用 uv + lsp-bridge)
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook
  (python-mode . (lambda ()
                   (setq-local python-indent-offset 4)
                   (setq-local python-indent-guess-indent-offset nil)
                   (when (and (executable-find "uv")
                              (or (file-exists-p "pyproject.toml")
                                  (file-exists-p "requirements.txt")
                                  (file-exists-p ".venv")))
                     (my/setup-uv-virtualenv))))
  :config
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4))

(defun my/setup-uv-virtualenv ()
  "自动检测并激活 uv 管理的虚拟环境"
  (interactive)
  ;; 优先找 pyproject.toml（和 uv 逻辑一致）
  (let* ((project-root (or (locate-dominating-file default-directory "pyproject.toml")
                           (locate-dominating-file default-directory "requirements.txt")
                           (locate-dominating-file default-directory ".git")))
         (venv-path (when project-root
                      (expand-file-name ".venv" project-root))))
    (when (and venv-path (file-exists-p venv-path))
      (let ((python-path (expand-file-name "bin/python" venv-path)))
        (when (file-exists-p python-path)
          ;; 设置 Python 解释器
          (setq-local python-shell-interpreter python-path)
          
          ;; 关键：设置 pyright 使用的 Python
          (setq-local lsp-bridge-python-command python-path)
          
          ;; 设置环境变量
          (setenv "VIRTUAL_ENV" venv-path)
          (setenv "PATH" (concat (expand-file-name "bin" venv-path) ":" (getenv "PATH")))
          (setq-local exec-path (cons (expand-file-name "bin" venv-path) exec-path))
          
          ;; 创建 pyrightconfig.json（保底）
          (let ((config-file (expand-file-name "pyrightconfig.json" project-root)))
            (unless (file-exists-p config-file)
              (with-temp-file config-file
                (insert "{\n  \"venvPath\": \".\",\n  \"venv\": \".venv\",\n  \"pythonVersion\": \"3.12\"\n}"))))
          
          ;; 重启 lsp-bridge
          (when (fboundp 'lsp-bridge-restart-process)
            (lsp-bridge-restart-process))
          
          (message "对 已激活虚拟环境: %s" venv-path))))))


;; 快捷命令：使用 uv 运行当前脚本
(defun my/python-run-uv ()
  "使用 uv run 执行当前 Python 文件"
  (interactive)
  (let* ((current-file (buffer-file-name))
         (project-root (locate-dominating-file default-directory "pyproject.toml")))
    (if (and project-root (executable-find "uv"))
        (let ((default-directory project-root))
          (compile (format "uv run python %s" 
                          (shell-quote-argument (file-relative-name current-file project-root)))))
      (message "未找到 pyproject.toml 或 uv 命令"))))

;; 绑定快捷键
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-c") 'my/python-run-uv))

;; 配置 lsp-bridge
(setq lsp-bridge-python-lsp-server 'pyright)

;; 可选：pyright 优化配置
(setq lsp-bridge-get-completion-trigger-characters
      '((python-mode . ("." "("))))


(provide 'init-programming-python)
