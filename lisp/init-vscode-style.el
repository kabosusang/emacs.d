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
(global-set-key (kbd "C-c ;") 'vsc/quick-comment)

;; 将 Ctrl+Shift+l 绑定到标记所有匹配
(global-set-key (kbd "C-S-l") 'mc/mark-all-dwim)


;; ===== move-text ==========================
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


;; ===== 复制行到上方/下方（类似 VSCode Alt+Shift+↑/↓）=====
(defun vsc/duplicate-line-or-region-up ()
  "复制当前行到上一行，光标留在新复制的那一行。"
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (goto-char (region-beginning))
        (insert text "\n"))
    (let* ((col (current-column))
           (text (buffer-substring (line-beginning-position) (line-end-position))))
      (beginning-of-line)
      (insert text "\n")
      ;; 向上移动一行到新复制的行
      (previous-line 1)
      (move-to-column col))))

(defun vsc/duplicate-line-or-region-down ()
  "复制当前行或选区到下一行，保持光标位置。"
  (interactive)
  (if (use-region-p)
      ;; 有选区：复制选区到选区下方
      (let ((text (buffer-substring (region-beginning) (region-end)))
            (beg (region-beginning)))
        (goto-char (region-end))
        (insert "\n" text)
        ;; 光标保持在原选区开始位置
        (goto-char beg)
        (setq deactivate-mark nil))
    ;; 无选区：复制当前行到下一行
    (let* ((col (current-column))
           (text (buffer-substring (line-beginning-position) (line-end-position))))
      ;; 到行尾，插入新行和内容
      (end-of-line)
      (newline)
      (insert text)
      ;; 光标保持在新插入的行（原位置向下），并恢复到原来的列位置
      (move-to-column col))))

;; 绑定到 M-S-up / M-S-down
(global-set-key (kbd "M-S-<up>") 'vsc/duplicate-line-or-region-up)
(global-set-key (kbd "M-S-<down>") 'vsc/duplicate-line-or-region-down)


(provide 'init-vscode-style)

