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


;; 自定义移动行函数
(defun my-move-line-up ()
  "向上移动当前行（Shift+Alt+Up）"
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun my-move-line-down ()
  "向下移动当前行（Shift+Alt+Down）"
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

;; 绑定快捷键
(global-set-key (kbd "<M-up>") 'my-move-line-up)
(global-set-key (kbd "<M-down>") 'my-move-line-down)




(provide 'init-vscode-style)

