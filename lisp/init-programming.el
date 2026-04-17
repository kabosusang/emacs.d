;;; init-programming.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:
;;;     lsp-mode and dap-mode should be installed and loaded first.
;;; Code:


;; 3. lsp-mode配置 - 已禁用，使用 lsp-bridge 替代
;; (use-package lsp-mode
;;   :ensure t
;;   :custom
;;   ;; clangd配置
;;   (lsp-clients-clangd-executable "clangd")
;;   (lsp-clients-clangd-args '("--background-index"
;;                              "--clang-tidy"
;;                              "--completion-style=detailed"
;;                              "--header-insertion=iwyu"
;;                              "--header-insertion-decorators"
;;                              "--cross-file-rename"
;;                              "--completion-parse=auto"))
;;   ;; 性能优化
;;   (lsp-idle-delay 0.5)
;;   (lsp-log-io nil) ; 设置为t可以调试，但会影响性能
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   ((c-mode c++-mode) . lsp-deferred))

;; 4. C/C++配置 - 禁用自动格式化
(use-package c++-mode
  :hook
  ((c-mode c++-mode) . (lambda ()
			 ;; 启用modern C++ highlighting
                         (c-toggle-auto-newline -1)
                         (c-toggle-hungry-state -1)
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
;; 禁用 dap-python 的默认配置
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; 禁用 dap-python 依赖，因为可能干扰 lsp-bridge
  ;; (require 'dap-python)
  )

(use-package pyvenv
  :ensure t
  :config
  ;;(setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))
  (pyvenv-mode t)
  :hook
  (python-mode . (lambda () (pyvenv-workon ".."))))

;; 禁用 lsp-pyright，使用 lsp-bridge 替代
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook
;;   (python-mode . (lambda ()
;;                    (require 'lsp-pyright)
;;                    (lsp-deferred))))

;; 11. Rust配置
;; 禁用 lsp-deferred，使用 lsp-bridge 替代
(use-package rust-mode
  :ensure t
  :functions dap-register-debug-template
  ;; :hook 已禁用，改为手动启用
  :hook
  ((rust-mode . my/rust-mode-setup))
  :bind
  (:map rust-mode-map
        ("C-c f" . rust-format-buffer)
        ("C-c C-f" . rust-format-buffer))
  :config

;; Rust mode setup function
(defun my/rust-mode-setup ()
  "Setup for rust-mode"
  ;; 其他 rust-mode 设置可以在这里添加
  (message "Rust mode setup"))
  ;; 启用LSP的保存时格式化
  (add-hook 'lsp-mode-hook
            (lambda ()
              (when (derived-mode-p 'rust-mode)
                ;; 启用保存时格式化
                (setq lsp-enable-on-type-formatting nil)  ; 禁用输入时格式化
                (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

  
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
;; 在 rust-mode 中禁用 flycheck
(add-hook 'rust-mode-hook (lambda () (flycheck-mode -1)))


;; ========== 自定义 lsp-bridge 项目根目录检测 ==========
(defun my/lsp-bridge-get-project-path (file-path)
  "自定义项目根目录检测，优先使用 Cargo.toml"
  (let ((default-directory (file-name-directory file-path)))
    (expand-file-name
     (cond
      ((string-match-p "\\.rs\\'" file-path)
       (or (locate-dominating-file default-directory "Cargo.toml")
           (locate-dominating-file default-directory ".git")))
      ((string-match-p "\\.py\\'" file-path)
       (or (locate-dominating-file default-directory "pyproject.toml")
           (locate-dominating-file default-directory "setup.py")
           (locate-dominating-file default-directory ".git")))
      ((string-match-p "\\.\\(c\\|cpp\\|h\\|hpp\\)\\'" file-path)
       (or (locate-dominating-file default-directory "compile_commands.json")
           (locate-dominating-file default-directory "CMakeLists.txt")
           (locate-dominating-file default-directory ".git")))
      (t (locate-dominating-file default-directory ".git"))))))

(setq lsp-bridge-get-project-path-by-filepath #'my/lsp-bridge-get-project-path)


;; lsp-bridge 优化配置 - 清理版本

;; 调试（测试完记得关闭）
;;(setq lsp-bridge-enable-log t)
;;(setq lsp-bridge-enable-debug t)

;; 补全延迟和频率控制
(setq lsp-bridge-completion-delay 0.2)           ; 输入后等待 0.2 秒
(setq lsp-bridge-completion-min-interval 0.15)   ; 最小间隔 150ms

;; 补全功能开关
(setq lsp-bridge-enable-auto-completion t)       ; 启用自动补全
(setq lsp-bridge-enable-completion-popup t)      ; 启用弹窗
(setq lsp-bridge-enable-semantic-completion t)   ; 启用语义补全
(setq lsp-bridge-enable-snippet t)               ; 启用代码片段

;; 性能优化 - 限制补全数量
(setq lsp-bridge-completion-max-items 50)        ; 最多 50 个补全项

;; 诊断优化 - 延迟处理
(setq lsp-bridge-enable-diagnostics 'defer)      ; 延迟诊断（字符串符号）
(setq lsp-bridge-diagnostics-delay 0.5)          ; 延迟 0.5 秒

;; 禁用不必要的功能（提升速度）
(setq lsp-bridge-enable-hover-diagnostic t)    ; 禁用悬停诊断
;;(setq lsp-bridge-enable-search-words nil)        ; 禁用单词搜索
(setq lsp-bridge-enable-diagnostic t)


;; ============================================================
;; 代码折叠配置 —— 使用 Emacs 内置的 hs-minor-mode
;; ============================================================

;; 在所有编程模式中自动启用代码折叠
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; 配置快捷键（使用 C-c z 前缀，与你的 C-c f 不冲突）
(with-eval-after-load 'hideshow
  ;; 定义快捷键
  (define-key hs-minor-mode-map (kbd "C-c z t") #'hs-toggle-hiding)    ; 切换折叠
  (define-key hs-minor-mode-map (kbd "C-c z c") #'hs-hide-block)       ; 折叠当前块
  (define-key hs-minor-mode-map (kbd "C-c z o") #'hs-show-block)       ; 展开当前块
  (define-key hs-minor-mode-map (kbd "C-c z C-c") #'hs-hide-all)       ; 折叠所有块
  (define-key hs-minor-mode-map (kbd "C-c z C-o") #'hs-show-all)       ; 展开所有块
  
  ;; 单键快速切换
  (define-key hs-minor-mode-map (kbd "<f2>") #'hs-toggle-hiding))

;; 优化折叠体验
(setq hs-isearch-open t)        ; 搜索时自动展开折叠区域
(setq hs-allow-nesting t)       ; 允许嵌套折叠
(setq hs-hide-comments-when-hiding-all nil)  ; 折叠所有时也折叠注释

;; 可选：显示折叠指示器
(setq hs-set-up-overlay
      (defun my-display-code-line-counts (ov)
        (when (eq 'code (overlay-get ov 'hs))
          (overlay-put ov 'display
                       (format "... (%d lines) ..."
                               (count-lines (overlay-start ov)
                                            (overlay-end ov)))))))


(provide 'init-programming)
;;; init-programming.el ends here
