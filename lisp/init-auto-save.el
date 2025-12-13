;;; ========================================
;; Auto-save and Backup Configuration
;; ========================================

;; 确保必要的目录存在
(defun my/ensure-emacs-dirs ()
  "Ensure all necessary Emacs directories exist."
  (interactive)
  (let ((dirs '("~/.emacs.d/auto-save/"
                "~/.emacs.d/auto-save-list/"
                "~/.emacs.d/backups/"
                "~/.emacs.d/saves/"
                "~/.cache/emacs/auto-save/"
                "~/.cache/emacs/backups/")))
    (dolist (dir dirs)
      (let ((full-dir (expand-file-name dir)))
        (unless (file-exists-p full-dir)
          (make-directory full-dir t)
          (message "Created directory: %s" full-dir))))))

;; 启动时执行
(my/ensure-emacs-dirs)

;; 自动保存配置
(setq auto-save-default t)                     ; 启用自动保存
;(setq auto-save-visited-mode t)                ; 即使文件未修改也自动保存


;; 设置自动保存文件位置（二选一）

;; 选项A：使用 .emacs.d/auto-save/（保持一致性）
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))

;; 选项B：使用 .cache/emacs/（推荐，更整洁）
;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name "~/.cache/emacs/auto-save/") t)))

;; 备份文件配置
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "~/.emacs.d/backups/"))))
(setq backup-by-copying t)                     ; 备份时复制，不移动
(setq delete-old-versions t)                   ; 删除旧版本
(setq kept-new-versions 6)                     ; 保留新版本数量
(setq kept-old-versions 2)                     ; 保留旧版本数量
(setq version-control t)                       ; 使用版本号

;; 避免在特定模式下创建备份
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (string-match-p "\\`/tmp/" name))
             (not (string-match-p "\\`/dev/shm/" name)))))

;; 自动保存间隔（按输入字符数）
(setq auto-save-interval 200)                  ; 每输入200字符保存一次

;; 自动保存间隔（按时间）
;(setq auto-save-timeout 30)                    ; 30秒无操作后保存

;; 显示自动保存状态
(defun my/auto-save-notification ()
  "Show notification when auto-saving."
  (when (and auto-save-default
             (buffer-modified-p)
             (buffer-file-name))
    (message "Auto-saving %s..." (buffer-name))))

(add-hook 'auto-save-hook 'my/auto-save-notification)

;; 自动保存文件命名格式
(setq auto-save-file-name-format
      (concat (file-name-directory (expand-file-name "~/.emacs.d/auto-save/"))
              "#%f#"))

;; 恢复自动保存的文件
(use-package autorevert
  :config
  (global-auto-revert-mode t)
  (setq auto-revert-verbose nil))              ; 安静地自动恢复

;; 快捷键：手动保存所有buffer
;(global-set-key (kbd "C-x s") 'save-some-buffers)

;; 快捷键：恢复自动保存的文件
;(global-set-key (kbd "C-c r") 'recover-file)

;; 调试函数：检查自动保存状态
(defun my/check-auto-save-status ()
  "Check auto-save status and directories."
  (interactive)
  (message "Auto-save directory: %s" (expand-file-name "~/.emacs.d/auto-save/"))
  (message "Directory exists: %s" (file-exists-p "~/.emacs.d/auto-save/"))
  (message "Auto-save enabled: %s" auto-save-default)
  (message "Current buffer auto-save file: %s" (auto-save-file-name)))

(global-set-key (kbd "C-c a s") 'my/check-auto-save-status)

;; 清理旧的自动保存文件
(defun my/cleanup-auto-save-files ()
  "Clean up old auto-save files."
  (interactive)
  (let* ((auto-save-dir (expand-file-name "~/.emacs.d/auto-save/"))
         (files (directory-files auto-save-dir t "^#")))
    (dolist (file files)
      (let ((days-ago (time-to-days (time-since (file-attribute-modification-time
                                                 (file-attributes file))))))
        (when (> days-ago 7)  ; 删除7天前的自动保存文件
          (delete-file file)
          (message "Deleted old auto-save: %s" (file-name-nondirectory file))))))
  (message "Auto-save cleanup complete!"))

;; 每周清理一次（可选）
(run-at-time "0:00" (* 7 24 3600) 'my/cleanup-auto-save-files)


(provide 'init-auto-save)
