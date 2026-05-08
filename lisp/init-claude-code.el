;;; init-claude-code.el --- Claude Code (Manual Install)
;;; Commentary:
;;; Code:

;; 手动克隆一次（在终端执行）：
;; git clone https://github.com/stevemolitor/claude-code.el.git ~/.emacs.d/claude-code

;; inheritenv 已手动下载到 ~/.emacs.d/inheritenv/
(add-to-list 'load-path "~/.emacs.d/vendor/inheritenv")
(require 'inheritenv)

(add-to-list 'load-path "~/.emacs.d/vendor/claude-code")
(require 'claude-code)

;; 确保 eat 已安装
;; (unless (package-installed-p 'eat)
;;   (package-refresh-contents)
;;   (package-install 'eat))

(setq claude-code-program "~/.bun/bin/ccb")
(setq claude-code-terminal-backend 'vterm)  ; vterm 比 eat 更稳定快速

;; 快捷键
(global-set-key (kbd "C-c c") 'claude-code)


(provide 'init-claude-code)
;;; init-claude-code.el ends here
