;;; ========================================
;; VSCODE STYLE Configuration
;; ========================================
;; ===== 简单可靠的智能注释函数 =====
(defun my-quick-comment ()
  "快速注释：无选区时注释当前行，有选区时注释选区。"
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-beginning-position 2))))

;; ===== 绑定到不会冲突的标准快捷键 C-x C-; =====
(global-set-key (kbd "C-c ;") 'my-quick-comment)

;; 将 Ctrl+Shift+l 绑定到标记所有匹配
(global-set-key (kbd "C-S-l") 'mc/mark-all-dwim)


(use-package move-text
  :ensure t
  :config
  ;; 启用默认快捷键 M-up / M-down（原生支持选区和单行移动）
  (move-text-default-bindings)
  
  ;; 可选：移动后自动缩进
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))



(provide 'init-vscode-style)

