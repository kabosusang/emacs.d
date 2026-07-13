;;; init-programming-cpp.el --- C/C++ Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 放在 treemacs 加载之前
(if (display-graphic-p)
     (with-eval-after-load 'treemacs
       (treemacs-define-custom-image-icon
         "~/.emacs.d/themes/icons/cpp-module.png"
         "cppm"
         "ixx"))
   (setq treemacs-custom-icon-config
         '(("cppm" . "M<")
           ("ixx" . "M<"))))

(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

;;(setenv "SDL_VIDEO_DRIVER" "wayland")

;; ========== C/C++ 编辑配置 ==========
(use-package c++-mode
  :hook
  ((c-mode c++-mode) . (lambda ()
                         (electric-pair-local-mode 1)
                         (electric-indent-local-mode -1)
                         (setq-local indent-tabs-mode nil)
                         (setq-local tab-width 4)
                         (setq-local c-basic-offset 4)
                         (setq-local c-tab-always-indent t)
                         ;; 关闭 cc-mode 内建的所有电缩进键
                         (setq-local c-electric-flag nil)
                                                  ;; 让 lsp-bridge 在保存时格式化
                         ;; (add-hook 'before-save-hook #'lsp-bridge-code-format nil t)
						 ))
  :bind
  (("C-c f" . lsp-bridge-code-format)  ;; 改用 lsp-bridge 的格式化
   ("C-c o" . ff-find-other-file)))

;; ========== clang-format 集成 ==========
(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  (global-set-key (kbd "C-c f") 'clang-format-buffer)
  (global-set-key (kbd "C-c M-f") 'clang-format-region))

;; ========== CMake 支持 ==========
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :hook
  (cmake-mode . (lambda ()
                  (setq-local company-backends
                              '(company-cmake company-files)))))

;; ========== 项目根目录检测 ==========
(defun cpp/c-cpp-project-root ()
  "检测 C/C++ 项目根目录（最顶层的 CMakeLists.txt 所在目录）"
  (or (locate-dominating-file default-directory "compile_commands.json")
      (cpp/cpp-find-top-cmake-dir default-directory)
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun cpp/cpp-find-top-cmake-dir (dir)
  "从 dir 往上找最顶层的 CMakeLists.txt"
  (let* ((current (locate-dominating-file dir "CMakeLists.txt"))
         (parent (when current
                   (locate-dominating-file (expand-file-name ".." current) "CMakeLists.txt"))))
    (if parent
        (cpp/cpp-find-top-cmake-dir (expand-file-name ".." current))
      current)))

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

;; ========== 工具函数 ==========

(defun cpp/cpp-detect-vcpkg-toolchain (root)
  "检测项目是否使用 vcpkg。返回 toolchain 路径或 nil"
  (let* ((vcpkg-root (or (getenv "VCPKG_ROOT")
                         (expand-file-name "~/.local/share/vcpkg")))
         (toolchain (expand-file-name "scripts/buildsystems/vcpkg.cmake" vcpkg-root)))
    ;; 同时满足：toolchain 文件存在，且项目根目录有 vcpkg.json
    (when (and (file-exists-p toolchain)
               (file-exists-p (expand-file-name "vcpkg.json" root)))
      toolchain)))

(defun cpp/cpp-cmake-configure-cmd (root)
  "返回 CMake 配置命令字符串，自动检测是否使用 vcpkg"
  (let ((toolchain (cpp/cpp-detect-vcpkg-toolchain root)))
    (format "cmake -B build -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s"
            (if toolchain
                (concat "-DCMAKE_TOOLCHAIN_FILE=" toolchain)
              ""))))

(defun cpp/cpp-collect-executables (root)
  "递归收集 build 目录下所有可能的可执行文件"
  (let* ((build-dir (expand-file-name "build" root))
         (candidates nil))
    (when (file-exists-p build-dir)
      (cl-labels ((walk (dir)
                    (dolist (file (directory-files dir t "^[^.]"))
                      (cond
                       ((file-directory-p file)
                        (unless (or (string-match-p "/CMakeFiles$" file)
                                    (string-match-p "/\\.cache" file))
                          (walk file)))
                       ((and (file-executable-p file)
                             (not (file-directory-p file))
                             (not (string-match-p
                                   "\\.\\(o\\|a\\|so\\|dylib\\|cmake\\|ninja\\|h\\|hpp\\|cpp\\|c\\|json\\|txt\\|in\\|py\\|sh\\)$"
                                   file)))
                        (push file candidates))))))
        (walk build-dir)))
    (sort candidates #'string<)))

(defun cpp/cpp-find-executable (root)
  "收集所有可执行文件；只有一个直接返回，多个弹出选择"
  (let ((candidates (cpp/cpp-collect-executables root)))
    (cond
     ((null candidates) nil)
     ((= (length candidates) 1) (car candidates))
     (t
      (let* ((names (mapcar (lambda (f)
                              (file-relative-name f (expand-file-name "build" root)))
                            candidates))
             (chosen (completing-read "选择可执行文件: " names nil t)))
        (expand-file-name chosen (expand-file-name "build" root)))))))

(defun cpp/cpp-run-executable (executable)
  "运行指定的可执行文件，cwd 切到项目根（解决 Slang dlopen 相对路径问题）"
  (let* ((root (cpp/c-cpp-project-root))
         (vcpkg-lib (expand-file-name "vcpkg_installed/x64-linux/lib" root))
         (default-directory root))   ; ← 关键：cwd 切到项目根
    (compile
     (format "bash -c 'exec env LD_LIBRARY_PATH=%s:\"${LD_LIBRARY_PATH:-}\" %s'"
             (shell-quote-argument vcpkg-lib)
             (shell-quote-argument executable)))))




;; ========== 构建命令 ==========

(defun cpp/cpp-build ()
  "编译项目（自动检测 vcpkg/普通 CMake）"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root))
    (unless (file-exists-p "build")
      (make-directory "build"))
    (if (file-exists-p "build/CMakeCache.txt")
        (compile "cmake --build build --parallel 8")
      (progn
        (message "首次配置 CMake，请稍候...")
        (compile (cpp/cpp-cmake-configure-cmd root))
        (message "配置完成后请再次按 F5 编译")))))

(defun cpp/cpp-run ()
  "智能查找并运行 build 目录下的可执行文件"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (exe (cpp/cpp-find-executable root)))
    (if exe
        (cpp/cpp-run-executable exe)
      (user-error "在 %s/build/ 下未找到可执行文件，请先编译 (F5)" root))))

(defun cpp/cpp-build-and-run ()
  "编译并运行（强制使用项目根目录）"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root))          ;; ← 这行确保永远在根目录
    (unless (file-exists-p "build")
      (make-directory "build"))
    (unless (file-exists-p "build/CMakeCache.txt")
      (message "首次配置 CMake...")
      (let ((ret (shell-command (cpp/cpp-cmake-configure-cmd root))))
        (unless (= ret 0)
          (user-error "CMake 配置失败"))))
    (message "正在编译...")
    (compile "cmake --build build --parallel 8")))

;; ========== 清理函数 ==========
(defun cpp/cpp-reconfigure ()
  "重新运行 CMake 配置"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (default-directory root))
    (message "重新配置 CMake...")
    (compile (cpp/cpp-cmake-configure-cmd root))))

(defun cpp/cpp-clean-light ()
  "轻量清理：只删除编译产物"
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

(defun cpp/cpp-clean-and-rebuild ()
  "深度清理后重新构建"
  (interactive)
  (cpp/cpp-clean-deep)
  (sit-for 0.5)
  (cpp/cpp-build))

;; ========== 单文件快速运行 ==========

(defun cpp/cpp-run-single-file ()
  "编译并运行当前单个 .cpp 文件"
  (interactive)
  (let* ((source (buffer-file-name))
         (target (file-name-sans-extension source)))
    (compile (format "clang++ -std=c++23 -g -Wall %s -o %s && %s"
                     (shell-quote-argument source)
                     (shell-quote-argument target)
                     (shell-quote-argument target)))))

;; ========== GDB 调试 ==========

(defun cpp/gdb-debug ()
  "启动 GDB 调试当前项目"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (build-dir (expand-file-name "build" root))
         (default-dir (if (file-directory-p build-dir) build-dir root))
         (program (read-file-name "Program to debug: " default-dir nil t)))
    (gdb (format "gdb -i=mi %s" program))))

;; ========== 快捷键绑定 ==========
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'cpp/cpp-build)
            (local-set-key (kbd "<f6>") 'cpp/cpp-run)
            (local-set-key (kbd "<f7>") 'cpp/cpp-build-and-run)
            (local-set-key (kbd "<f8>") 'cpp/cpp-clean-light)
            (local-set-key (kbd "<f9>") 'cpp/cpp-clean-deep)
            (local-set-key (kbd "<f10>") 'cpp/cpp-clean-and-rebuild)
            (local-set-key (kbd "<f11>") 'cpp/cpp-reconfigure)
            (local-set-key (kbd "<f12>") 'cpp/cpp-run-single-file)
            (local-set-key (kbd "C-c d g") 'cpp/gdb-debug)))




(defun cpp/insert-section-separator ()
  "插入一个带有标题的装饰性分隔符（紧贴代码，无多余空行）"
  (interactive)
  (let* ((title (read-string "Section title: "))
         (desc (read-string "Description (optional): "))
         (width 80)
         (line-char ?-)
         (line (make-string width line-char)))
    (insert (format "// %s\n" line))
    (insert (format "// %s\n" title))
    (when (not (string-empty-p desc))
      (insert (format "// %s\n" desc)))
    (insert (format "// %s" line))  ;; 注意：这里没有 \n
    (end-of-line)))                 ;; 光标移到行尾




;; 快捷键绑定
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c i") 'cpp/insert-section-separator)))



(provide 'init-programming-cpp)
;;; init-programming-cpp.el ends here
