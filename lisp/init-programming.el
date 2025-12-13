;;; init-programming.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:
;;;     lsp-mode and dap-mode should be installed and loaded first.
;;; Code:


;; 3. lsp-mode配置
(use-package lsp-mode
  :ensure t
  :custom
  ;; clangd配置
  (lsp-clients-clangd-executable "clangd")
  (lsp-clients-clangd-args '("--background-index"
                             "--clang-tidy"
                             "--completion-style=detailed"
                             "--header-insertion=iwyu"
                             "--header-insertion-decorators"
                             "--cross-file-rename"
                             "--completion-parse=auto"))
  ;; 性能优化
  (lsp-idle-delay 0.5)
  (lsp-log-io nil) ; 设置为t可以调试，但会影响性能
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c-mode c++-mode) . lsp-deferred))

;; 4. C/C++配置 - 禁用自动格式化
(use-package c++-mode
  :hook
  ((c-mode c++-mode) . (lambda ()
			 ;; 启用modern C++ highlighting
                         (c-toggle-auto-newline 1)
                         (c-toggle-hungry-state 1)
                         ;; ========== 关键修复 ==========
                         ;; 禁用所有自动缩进和格式化
                         (setq-local c-electric-flag nil)      ; 禁用自动缩进
                         (setq-local c-auto-newline nil)       ; 禁用自动换行
                         (setq-local c-electric-flag nil)
                         (setq-local c-electric-brace nil)
                         (setq-local c-electric-colon nil)
                         (setq-local c-electric-lt-gt nil)
                         (setq-local c-electric-paren nil)
                         (setq-local c-electric-slash nil)
                         (setq-local c-electric-star nil)
                         
                         ;; 禁用所有electric模式
                         (electric-indent-local-mode -1)
                         (electric-pair-local-mode -1)
                         (electric-layout-local-mode -1)
                         
                         ;; 使用tab缩进
                         (setq-local indent-line-function 'insert-tab)
                         
                         ;; 禁用company的自动触发
                         ;(setq-local company-auto-commit nil)
                         ;(setq-local company-auto-complete nil)
                         ;(setq-local company-idle-delay nil)
                         
                         ;; 禁用yasnippet的自动触发
                         ;(setq-local yas-minor-mode nil)
                         
                         ;; 强制禁用electric-pair（全局）
                         (setq electric-pair-inhibit-predicate 
                               (lambda (c) (or (eq major-mode 'c++-mode)
                                               (eq major-mode 'c-mode))))))
  :bind
  ("C-c o" . ff-find-other-file)
  ("C-c f" . clang-format-buffer)
  :config
  ;; 设置clang-format样式
  (setq clang-format-style "file")
  
  ;; 全局禁用自动格式化
  (setq c-tab-always-indent nil)
  
  ;; 全局禁用electric-pair
  (setq-default electric-pair-mode nil))

;; 5. clang-format集成 - 只用于手动格式化
(use-package clang-format
  :ensure t
  :config
  ;; 全局快捷键 - 手动格式化
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  (global-set-key (kbd "C-c f") 'clang-format-region))

;; 6. CMake支持
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :hook
  (cmake-mode . (lambda ()
                  (setq-local company-backends
                              '(company-cmake company-files))))
  :config
  ;; 添加vcpkg支持
  (defun my/cmake-vcpkg-setup ()
    "自动检测并添加vcpkg toolchain文件"
    (let ((vcpkg-paths '("~/vcpkg/scripts/buildsystems/vcpkg.cmake"
                         "~/vcpkg/scripts/buildsystems/vcpkg.cmake"
                         "/usr/local/share/vcpkg/scripts/buildsystems/vcpkg.cmake")))
      (cl-loop for path in vcpkg-paths
               when (file-exists-p (expand-file-name path))
               do (setq-local cmake-args
                              (list (concat "-DCMAKE_TOOLCHAIN_FILE="
                                            (expand-file-name path))))
               and return t)))
  
  (add-hook 'cmake-mode-hook #'my/cmake-vcpkg-setup))

;; 7. compile-commands.json支持
(defun my/setup-compile-commands ()
  "为clangd设置compile_commands.json"
  (when (or (file-exists-p "compile_commands.json")
            (file-exists-p "build/compile_commands.json"))
    (make-local-variable 'lsp-clients-clangd-args)
    (setq lsp-clients-clangd-args
          (append lsp-clients-clangd-args
                  '("--compile-commands-dir=."
                    "--background-index"
                    "--clang-tidy"
                    "--all-scopes-completion")))))

(add-hook 'lsp-before-initialize-hook #'my/setup-compile-commands)

;; 8. 项目根目录检测
(defun my/c-cpp-project-root ()
  "检测C/C++项目根目录"
  (or (locate-dominating-file default-directory "compile_commands.json")
      (locate-dominating-file default-directory "CMakeLists.txt")
      (locate-dominating-file default-directory ".git")
      default-directory))

;; 设置lsp的root目录
(setq lsp-auto-guess-root t)

;; 9. Debug配置
(use-package dap-cpptools
  :after dap-mode)

(when *is-a-mac*
  (use-package dap-lldb
    :after dap-mode
    :custom
    (dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
    (dap-lldb-debugged-program-function
     (lambda () (read-file-name "Select file to debug: ")))))

;; 10. Python配置
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("~/miniconda3/bin/python3" . python-mode)
  :config
  ;; for debug
  (require 'dap-python))

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))
  (pyvenv-mode t)
  :hook
  (python-mode . (lambda () (pyvenv-workon ".."))))

(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

;; 11. Rust配置
(use-package rust-mode
  :ensure t
  :functions dap-register-debug-template
  :hook
  (rust-mode . lsp-deferred)
  :config
  ;; debug
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb-vscode"
                                     :request "launch"
                                     :name "rust-lldb::Run"
                                     :target nil
                                     :cwd nil)))

;; 12. Cargo配置
(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))

;; 13. 辅助功能

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

;; 14. 修复函数 - 如果还有问题可以调用
(defun my/disable-all-auto-formatting ()
  "禁用所有自动格式化功能"
  (interactive)
  (electric-indent-local-mode -1)
  (electric-pair-local-mode -1)
  (electric-layout-local-mode -1)
  (setq-local c-electric-flag nil)
  (setq-local c-auto-newline nil)
  (setq-local indent-line-function 'insert-tab)
  (message "已禁用所有自动格式化"))

;; 绑定修复函数到快捷键
(global-set-key (kbd "C-c F") 'my/disable-all-auto-formatting)

(provide 'init-programming)
;;; init-programming.el ends here
