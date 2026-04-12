;;;;;;;;;;;;;
;; Scheme with Guile
;;;;;;;;;;;;;
(require 'cmuscheme)
;; 使用Emacs内置的comint模式，不需要geiser
(setq scheme-program-name "guile")



;; 启动Scheme进程
(defun run-scheme-guile ()
  "启动Guile REPL"
  (interactive)
  (run-scheme scheme-program-name))

;; 获取当前Scheme进程
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme-guile)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

;; 原生分屏函数
(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
;    (split-window-horizontally (floor (* 0.65 (window-width))))
	 (split-window-vertically (floor (* 0.68 (window-height))))
	(other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   (t
    ;; 切换到下一个窗口并显示scheme buffer
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

;; 发送代码函数
(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

;; 新增：加载整个文件
(defun scheme-load-file-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-load-file (buffer-file-name)))


;; 主hook配置
(add-hook 'scheme-mode-hook
  (lambda ()
    ;; paredit需要先安装：M-x package-install paredit
    (when (require 'paredit nil t)
      (paredit-mode 1))
    
    ;; 快捷键
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
	(define-key scheme-mode-map (kbd "<f6>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f7>") 'scheme-load-file-split-window)
	
    ;; 可：启动REPL的快捷键
    (define-key scheme-mode-map (kbd "C-c C-z") 'run-scheme-guile)))

;; 文件关联
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))

;; *********************
(provide 'init-programming-scheme)

