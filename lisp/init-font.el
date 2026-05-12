;;; init-benchmarking.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (set-frame-font "ComicMono-22" nil t)
 ;; 设置主字体为某个 Nerd Font 的 Mono 版本
(set-frame-font "CaskaydiaMono Nerd Font-22" nil t)

; (set-frame-font "FiraCode Nerd Font Mono" nil t)
;; 追加符号字体，确保图标字符有字体可用
; (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)



;;Icons
(use-package treemacs-nerd-icons
  :ensure t            ; 确保包管理器自动安装此包
  :after treemacs      ; 确保在 treemacs 加载后再加载此包
  :config
  (treemacs-load-theme "nerd-icons")) ; 加载 nerd-icons 主题


;;字体大小修改
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-reset)


(provide 'init-font)
;;; init-benchmarking.el ends here




