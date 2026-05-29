;;; init-dape.el --- Debug configuration -*- lexical-binding: t; -*-
;;; Code:

(use-package dape
  :ensure t
  :config
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-info-variable-format '("" . ": %s")))

(defun dape-debug-project ()
  "调试当前项目（使用 gdb）"
  (interactive)
  (let* ((root (cpp/c-cpp-project-root))
         (candidates (cpp/cpp-collect-executables root)))
    (if candidates
        (let* ((names (mapcar (lambda (f)
                                (file-relative-name f (expand-file-name "build" root)))
                              candidates))
               (chosen (if (= (length names) 1)
                           (car names)
                         (completing-read "选择: " names nil t)))
               (exe (expand-file-name chosen (expand-file-name "build" root))))
          (dape (list :type "gdb"
                      :request "launch"
                      :program exe
                      :args []
                      :cwd root
                      :stopOnEntry t)))
      (user-error "未找到可执行文件，请先编译"))))

(global-set-key (kbd "C-c d d") #'dape-debug-project)
(global-set-key (kbd "C-c d b") #'dape-breakpoint-toggle)
(global-set-key (kbd "C-c d q") #'dape-quit)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "<f6>") #'dape-debug-project)
            (local-set-key (kbd "C-c d d") #'dape-debug-project)
            (local-set-key (kbd "C-c d b") #'dape-breakpoint-toggle)))

(provide 'init-dape)
;;; init-dape.el ends here
