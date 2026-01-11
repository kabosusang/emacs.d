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
(provide 'init-vscode-style)
