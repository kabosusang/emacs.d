;;; init-programming-cpp.el --- C/C++ Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

 	
(with-eval-after-load 'treemacs
  ;; 给 .cppm 文件设置图标（TUI 终端也能用）
  (treemacs-define-custom-icon " " "cppm")
  
  ;; 给 .ixx 文件设置图标
  (treemacs-define-custom-icon " " "ixx"))

;; ========== C/C++ 编辑配置 ==========
(use-package c++-mode
  :hook
  ((c-mode c++-mode) . (lambda ()
                         ;; 启用基础配对
                         (electric-pair-local-mode 1)
                         ;; 启用回车缩进
                         (electric-indent-local-mode 1)

                         ;; 缩进设置（使用空格缩进）
                         (setq-local indent-tabs-mode nil)
                         (setq-local tab-width 4)
                         (setq-local c-basic-offset 4)

                         ;; 启用 cc-mode 智能功能
                         (setq-local c-electric-flag t)
                         (c-toggle-electric-state 1)
                         (setq-local c-auto-newline nil)

                         ;; TAB 键智能缩进
                         (setq-local c-tab-always-indent t)

						 ;; ── 显示空白字符（只显示尾部空格和 TAB） ──
                         ;; (setq-local show-trailing-whitespace t)

                         ;; 智能 backspace：退到可缩进位置
                         ;; (local-set-key (kbd "<backspace>")
                         ;;               (lambda ()
                         ;;                 (interactive)
                         ;;                 (if (and (not (bolp))
                         ;;                          (save-excursion
                         ;;                            (skip-chars-backward " \t")
                         ;;                            (bolp)))
                         ;;                     (back-to-indentation)
                         ;;                   (delete-backward-char 1))))
						 
                         ;; 这些字符输入时自动格式化
                         (setq-local electric-indent-chars '(?\n ?\} ?\: ?\#))))
  :bind
  (:map c-mode-base-map
        ("C-c o" . ff-find-other-file)
        ("C-c f" . clang-format-buffer))
  :config
  (setq clang-format-style "file"))

;; ========== clang-format 集成 ==========
(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  (global-set-key (kbd "C-c f") 'clang-format-buffer)    ;; 格式化buffer
  (global-set-key (kbd "C-c M-f") 'clang-format-region)) ;; 区域格式化

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
  (defun cpp/cmake-vcpkg-setup ()
    "自动检测并添加 vcpkg toolchain 文件"
    (let ((vcpkg-paths '("~/vcpkg/scripts/buildsystems/vcpkg.cmake"
                         "/usr/local/share/vcpkg/scripts/buildsystems/vcpkg.cmake")))
      (cl-loop for path in vcpkg-paths
               when (file-exists-p (expand-file-name path))
               do (setq-local cmake-args
                              (list (concat "-DCMAKE_TOOLCHAIN_FILE="
                                            (expand-file-name path))))
               and return t)))
  (add-hook 'cmake-mode-hook #'cpp/cmake-vcpkg-setup))

;; ========== 项目根目录检测 ==========
(defun cpp/c-cpp-project-root ()
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

(defun cpp/cpp-build ()
  "在项目根目录的 build 文件夹中编译"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root))
    (unless (file-exists-p "build")
      (make-directory "build"))
    (if (file-exists-p "build/CMakeCache.txt")
        (compile "cmake --build build --parallel 8")
      (progn
        (message "首次配置 CMake，请稍候...")
        (async-shell-command "cmake -B build -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                     "*cmake-configure*")
        (message "CMake 配置已启动，完成后按 F5 编译")))))

(defun cpp/cpp-find-executable (root)
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

(defun cpp/cpp-run ()
  "智能查找并运行 build 目录下的可执行文件"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (executable (cpp/cpp-find-executable root)))
    (if executable
        (compile (format "cd %s && %s"
                         root
                         (shell-quote-argument executable)))
      (message "在 %s/build/ 下未找到可执行文件，请先编译 (F5)" root))))

(defun cpp/cpp-build-and-run ()
  "编译并运行当前项目（同步等待编译完成）"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root))
    (unless (file-exists-p "build")
      (make-directory "build"))
    (unless (file-exists-p "build/CMakeCache.txt")
       (shell-command "cmake -B build -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))
    (message "正在编译...")
    (let ((result (shell-command "cmake --build build --parallel 8")))
      (if (= result 0)
          (cpp/cpp-run)
        (message "编译失败！")))))

;; ========== 清理函数 ==========
(defun cpp/cpp-reconfigure ()
  "强制重新运行 CMake 配置，更新 compile_commands.json"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root))
    (message "重新配置 CMake...")
    (async-shell-command "cmake -B build -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                         "*cmake-configure*")))

(defun cpp/cpp-clean-light ()
  "轻量清理：只删除编译产物，保留 CMake 缓存"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root))
    (if (file-exists-p "build")
        (progn
          (message "清理编译产物...")
          (shell-command "cmake --build build --target clean")
          (message "清理完成"))
      (message "build 目录不存在"))))

(defun cpp/cpp-clean-deep ()
  "深度清理：删除整个 build 目录"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (build-dir (expand-file-name "build" root)))
    (when (file-exists-p build-dir)
      (if (yes-or-no-p (format "确定要删除 %s 吗？" build-dir))
          (progn
            (delete-directory build-dir t)
            (message "已删除 %s" build-dir))
        (message "已取消清理")))))

(defun cpp/cpp-clean-all ()
  "完全清理：删除 build 目录和 compile_commands.json 软链接"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (build-dir (expand-file-name "build" root))
         (cdb-link (expand-file-name "compile_commands.json" root)))
    (when (file-exists-p build-dir)
      (if (yes-or-no-p (format "确定要删除 %s 吗？" build-dir))
          (delete-directory build-dir t)
        (message "跳过 build 清理")))
    (when (and cdb-link (file-symlink-p cdb-link))
      (delete-file cdb-link)
      (message "已删除软链接"))))

(defun cpp/cpp-clean-and-rebuild ()
  "深度清理后重新构建"
  (interactive)
  (cpp/cpp-clean-deep)
  (sit-for 0.5)
  (cpp/cpp-build))

;; ========== 单文件快速运行（无 CMake） ==========

(defun cpp/cpp-run-single-file ()
  "编译并运行当前单个 .cpp 文件"
  (interactive)
  (let* ((source (buffer-file-name))
         (target (file-name-sans-extension source)))
    (compile (format "clang++ -std=c++23 -g -Wall %s -o %s && %s"
                     (shell-quote-argument source)
                     (shell-quote-argument target)
                     (shell-quote-argument target)))))

;; ========== 快捷键绑定 ==========
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'cpp/cpp-build)
            (local-set-key (kbd "<f6>") 'cpp/cpp-run)
            (local-set-key (kbd "<f7>") 'cpp/cpp-build-and-run)
            (local-set-key (kbd "<f8>") 'cpp/cpp-clean-light)
            (local-set-key (kbd "<f9>") 'cpp/cpp-clean-deep)
            (local-set-key (kbd "<f10>") 'cpp/cpp-clean-and-rebuild)
            (local-set-key (kbd "<f12>") 'cpp/cpp-run-single-file)))


;; ========== Vcpkg 集成 ==========

(defun cpp/cpp-vcpkg-toolchain-path ()
  "返回 vcpkg toolchain cmake 文件的路径"
  (let ((vcpkg-root (or (getenv "VCPKG_ROOT")
                        (expand-file-name "~/.local/share/vcpkg"))))
    (when (and vcpkg-root (file-exists-p vcpkg-root))
      (let ((toolchain (expand-file-name "scripts/buildsystems/vcpkg.cmake" vcpkg-root)))
        (when (file-exists-p toolchain)
          toolchain)))))

(defun cpp/cpp-build-vcpkg ()
  "使用 vcpkg toolchain 配置并编译项目（异步）"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root)
         (toolchain (cpp/cpp-vcpkg-toolchain-path)))
    (unless toolchain
/      (error "vcpkg toolchain 未找到，请检查 VCPKG_ROOT 环境变量"))
    (unless (file-exists-p "build")
      (make-directory "build"))
    (if (file-exists-p "build/CMakeCache.txt")
        ;; 已配置过，直接异步编译
        (compile "cmake --build build --parallel 8")
      ;; 首次配置 + 编译，全异步
      (progn
        (message "首次配置 CMake (vcpkg)，正在后台进行...")
        (compile
         (format "cmake -B build -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_TOOLCHAIN_FILE=%s && cmake --build build --parallel 8"
                 toolchain))))))

(defun cpp/cpp-build-and-run-vcpkg ()
  "使用 vcpkg toolchain 异步编译，完成后自动运行"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root)
         (toolchain (cpp/cpp-vcpkg-toolchain-path)))
    (unless toolchain
      (error "vcpkg toolchain 未找到"))
    (unless (file-exists-p "build")
      (make-directory "build"))
    (if (file-exists-p "build/CMakeCache.txt")
        ;; 已配置：编译，并在编译成功后运行
        (compile
         (format "cmake --build build --parallel 8 && %s"
                 (or (cpp/cpp-find-executable root) "echo 'No executable found'")))
      ;; 首次配置 + 编译 + 运行
      (compile
       (format "cmake -B build -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_TOOLCHAIN_FILE=%s && cmake --build build --parallel 8 && %s"
               toolchain
               (or (cpp/cpp-find-executable root) "echo 'No executable found'"))))))

(defun cpp/cpp-reconfigure-vcpkg ()
  "使用 vcpkg toolchain 重新配置 CMake（异步）"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root)
         (toolchain (cpp/cpp-vcpkg-toolchain-path)))
    (unless toolchain
      (error "vcpkg toolchain 未找到"))
    (message "重新配置 CMake (vcpkg)...")
    (compile
     (format "cmake -B build -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_TOOLCHAIN_FILE=%s"
             toolchain))))


(provide 'init-programming-cpp)
;;; init-programming-cpp.el ends here
