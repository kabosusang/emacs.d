;;; init-programming-cpp.el --- C/C++ Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ========== C/C++ 编辑配置 ==========
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

;; ========== clang-format 集成 ==========
(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  (global-set-key (kbd "C-c f") 'clang-format-region))

;; ========== CMake 支持 ==========
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

;; ========== 项目根目录检测 ==========
(defun my/c-cpp-project-root ()
  "检测 C/C++ 项目根目录"
  (or (locate-dominating-file default-directory "compile_commands.json")
      (locate-dominating-file default-directory "CMakeLists.txt")
      (locate-dominating-file default-directory ".git")
      default-directory))

;; ========== Debug 配置 ==========
(use-package dap-cpptools
  :after dap-mode)

(when *is-a-mac*
  (use-package dap-lldb
    :after dap-mode
    :custom
    (dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
    (dap-lldb-debugged-program-function
     (lambda () (read-file-name "Select file to debug: ")))))

;; ========== 禁用 flycheck ==========
(with-eval-after-load 'flycheck
  (add-hook 'c-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'c++-mode-hook (lambda () (flycheck-mode -1))))

;; ========== CMake LSP ==========
(setq lsp-bridge-cmake-lsp-server "cmake-language-server")

;; ========== C++ 项目构建与运行 ==========

(defun my/cpp-build ()
  "在项目根目录的 build 文件夹中编译"
  (interactive)
  (let* ((root (my/c-cpp-project-root))
         (default-directory root))
    (unless (file-exists-p "build")
      (make-directory "build"))
    (if (file-exists-p "build/CMakeCache.txt")
        (compile "cmake --build build --parallel 8")
      (progn
        (message "首次配置 CMake，请稍候...")
        (async-shell-command "cmake -B build -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                             "*cmake-configure*")
        (message "CMake 配置已启动，完成后按 F5 编译")))))

(defun my/cpp-find-executable (root)
  "在 build 目录下查找可执行文件"
  (let* ((build-dir (expand-file-name "build" root))
         (files (when (file-exists-p build-dir)
                  (directory-files build-dir t "^[^.]"))))
    (catch 'found
      (dolist (file files)
        (when (and (file-executable-p file)
                   (not (file-directory-p file))
                   (not (string-match-p "\\.\\(o\\|a\\|so\\|dylib\\|cmake\\|ninja\\)$" file)))
          (throw 'found file)))
      nil)))

(defun my/cpp-run ()
  "智能查找并运行 build 目录下的可执行文件"
  (interactive)
  (let* ((root (my/c-cpp-project-root))
         (executable (my/cpp-find-executable root)))
    (if executable
        (compile (format "cd %s && %s"
                         root
                         (shell-quote-argument executable)))
      (message "在 %s/build/ 下未找到可执行文件，请先编译 (F5)" root))))

(defun my/cpp-build-and-run ()
  "编译并运行当前项目（同步等待编译完成）"
  (interactive)
  (let* ((root (my/c-cpp-project-root))
         (default-directory root))
    (unless (file-exists-p "build")
      (make-directory "build"))
    (unless (file-exists-p "build/CMakeCache.txt")
      (shell-command "cmake -B build -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))
    (message "正在编译...")
    (let ((result (shell-command "cmake --build build --parallel 8")))
      (if (= result 0)
          (my/cpp-run)
        (message "编译失败！")))))

;; ========== 清理函数 ==========

(defun my/cpp-clean-light ()
  "轻量清理：只删除编译产物，保留 CMake 缓存"
  (interactive)
  (let* ((root (my/c-cpp-project-root))
         (default-directory root))
    (if (file-exists-p "build")
        (progn
          (message "清理编译产物...")
          (shell-command "cmake --build build --target clean")
          (message "清理完成"))
      (message "build 目录不存在"))))

(defun my/cpp-clean-deep ()
  "深度清理：删除整个 build 目录"
  (interactive)
  (let* ((root (my/c-cpp-project-root))
         (build-dir (expand-file-name "build" root)))
    (when (file-exists-p build-dir)
      (if (yes-or-no-p (format "确定要删除 %s 吗？" build-dir))
          (progn
            (delete-directory build-dir t)
            (message "已删除 %s" build-dir))
        (message "已取消清理")))))

(defun my/cpp-clean-all ()
  "完全清理：删除 build 目录和 compile_commands.json 软链接"
  (interactive)
  (let* ((root (my/c-cpp-project-root))
         (build-dir (expand-file-name "build" root))
         (cdb-link (expand-file-name "compile_commands.json" root)))
    (when (file-exists-p build-dir)
      (if (yes-or-no-p (format "确定要删除 %s 吗？" build-dir))
          (delete-directory build-dir t)
        (message "跳过 build 清理")))
    (when (and cdb-link (file-symlink-p cdb-link))
      (delete-file cdb-link)
      (message "已删除软链接"))))

(defun my/cpp-clean-and-rebuild ()
  "深度清理后重新构建"
  (interactive)
  (my/cpp-clean-deep)
  (sit-for 0.5)
  (my/cpp-build))

;; ========== 单文件快速运行（无 CMake） ==========

(defun my/cpp-run-single-file ()
  "编译并运行当前单个 .cpp 文件"
  (interactive)
  (let* ((source (buffer-file-name))
         (target (file-name-sans-extension source)))
    (compile (format "clang++ -std=c++20 -g -Wall %s -o %s && %s"
                     (shell-quote-argument source)
                     (shell-quote-argument target)
                     (shell-quote-argument target)))))

;; ========== 快捷键绑定 ==========
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'my/cpp-build)
            (local-set-key (kbd "<f6>") 'my/cpp-run)
            (local-set-key (kbd "<f7>") 'my/cpp-build-and-run)
            (local-set-key (kbd "<f8>") 'my/cpp-clean-light)
            (local-set-key (kbd "<f9>") 'my/cpp-clean-deep)
            (local-set-key (kbd "<f10>") 'my/cpp-clean-and-rebuild)
            (local-set-key (kbd "<f12>") 'my/cpp-run-single-file)))

(provide 'init-programming-cpp)
;;; init-programming-cpp.el ends here
