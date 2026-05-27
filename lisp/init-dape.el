;;; init-dape.el --- Simple Dape config -*- lexical-binding: t; -*-
;;; Code:

(use-package dape
  :ensure t
  :config
  (setq dape-buffer-window-arrangement 'gud))

;; 最简单的调试函数：直接问你程序路径
(defun dape-simple-start ()
  "启动调试，直接询问程序路径"
  (interactive)
  (let ((program (read-file-name "Program to debug: ")))
    (dape (list :request "launch"
                :program program
                :args []
                :cwd default-directory
                :stopOnEntry t))))

;; 快捷键
(global-set-key (kbd "C-c d s") #'dape-simple-start)
(global-set-key (kbd "C-c d b") #'dape-breakpoint-toggle)
(global-set-key (kbd "C-c d q") #'dape-disconnect)

(provide 'init-dape)
;;; init-dape.el ends here
