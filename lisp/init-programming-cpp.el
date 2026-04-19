;;; init-programming-cpp.el --- C/C++ Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; C/C++ 配置 - 禁用自动格式化
(use-package c++-mode
  :hook
  ((c-mode c++-mode) . (lambda ()
                         (c-toggle-auto-newline -1)
                         (c-toggle-hungry-state -1)
                         ;; 禁用所有自动缩进和格式化
                         (setq-local c-electric-flag nil)
                         (setq-local c-auto-newline nil)
                         (setq-local c-electric-brace nil)
                         (setq-local c-electric-colon nil)
                         (setq-local c-electric-lt-gt nil)
                         (setq-local c-electric-paren nil)
                         (setq-local c-electric-slash nil)
                         (setq-local c-electric-star nil)
                         
                         ;; 禁用所有 electric 模式
                         (electric-indent-local-mode -1)
                         (electric-pair-local-mode -1)
                         (electric-layout-local-mode -1)
                         
                         ;; 使用 tab 缩进
                         (setq-local indent-line-function 'insert-tab)
                         
                         ;; 强制禁用 electric-pair
                         (setq electric-pair-inhibit-predicate 
                               (lambda (c) (or (eq major-mode 'c++-mode)
                                               (eq major-mode 'c-mode))))))
  :bind
  (:map c-mode-base-map
        ("C-c o" . ff-find-other-file)
        ("C-c f" . clang-format-buffer))
  :config
  (setq clang-format-style "file")
  (setq c-tab-always-indent nil)
  (setq-default electric-pair-mode nil))

;; clang-format 集成
(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  (global-set-key (kbd "C-c f") 'clang-format-region))

;; CMake 支持
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :hook
  (cmake-mode . (lambda ()
                  (setq-local company-backends
                              '(company-cmake company-files))))
  :config
  (defun my/cmake-vcpkg-setup ()
    "自动检测并添加 vcpkg toolchain 文件"
    (let ((vcpkg-paths '("~/vcpkg/scripts/buildsystems/vcpkg.cmake"
                         "/usr/local/share/vcpkg/scripts/buildsystems/vcpkg.cmake")))
      (cl-loop for path in vcpkg-paths
               when (file-exists-p (expand-file-name path))
               do (setq-local cmake-args
                              (list (concat "-DCMAKE_TOOLCHAIN_FILE="
                                            (expand-file-name path))))
               and return t)))
  (add-hook 'cmake-mode-hook #'my/cmake-vcpkg-setup))

;; compile_commands.json 支持（给 lsp-mode 用的，lsp-bridge 不需要这个）
;; (defun my/setup-compile-commands () ...)

;; C/C++ 项目根目录检测
(defun my/c-cpp-project-root ()
  "检测 C/C++ 项目根目录"
  (or (locate-dominating-file default-directory "compile_commands.json")
      (locate-dominating-file default-directory "CMakeLists.txt")
      (locate-dominating-file default-directory ".git")
      default-directory))

;; Debug 配置
(use-package dap-cpptools
  :after dap-mode)

(when *is-a-mac*
  (use-package dap-lldb
    :after dap-mode
    :custom
    (dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
    (dap-lldb-debugged-program-function
     (lambda () (read-file-name "Select file to debug: ")))))

;; 禁用 C/C++ 模式的 flycheck
(with-eval-after-load 'flycheck
  (add-hook 'c-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'c++-mode-hook (lambda () (flycheck-mode -1))))

;;CMake LSP
(setq lsp-bridge-cmake-lsp-server "cmake-language-server")







(provide 'init-programming-cpp)
;;; init-programming-cpp.el ends here
