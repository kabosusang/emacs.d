;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-obsolete-function-alias 'after-load 'with-eval-after-load "")

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; Like diminish, but for major modes
(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

;; (defun sanityinc/major-mode-lighter (mode name)
;;   (add-hook (derived-mode-hook-name mode)
;;             (apply-partially 'sanityinc/set-major-mode-name name)))

(defun sanityinc/major-mode-lighter (mode name)             
  (add-hook (intern (concat (symbol-name mode) "-hook"))                   
            (apply-partially 'sanityinc/set-major-mode-name name)))

;; String utilities missing from core emacs

(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;; Delete the current file

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; Browse current HTML file

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-whole-line ()
  "Copy the whole line."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
  (message "1 line copied"))

(defun file-name-only ()
  "Get the current buffer file name without directory."
  (file-name-nondirectory (buffer-name)))

(defun file-name-only-noext ()
  "Get the currennt buffer file name without directory and extension."
  (file-name-sans-extension (file-name-only)))

;; Faster move cursor
;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))


;; pretty paste and copy
(unless (display-graphic-p)
  (defun pbpaste ()
	"Paste data from pasteboard."
	(interactive)
	(shell-command-on-region
	 (point)
	 (if mark-active (mark) (point))
	 "pbpaste" nil t))

  (defun pbcopy ()
	"Copy region to pasteboard."
	(interactive)
	(print (mark))
	(when mark-active
	  (shell-command-on-region
	   (point) (mark) "pbcopy")
	  (kill-buffer "*Shell Command Output*"))))

(defun toggle-window-split ()
  "Switch windows-spliting between horizontally and vertically."
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
n
		  (if this-win-2nd (other-window 1))))))

(when *is-a-mac*
  (defun pv/osx-get-keychain-password (account-name)
	"Gets ACCOUNT-NAME keychain password from OS X Keychain."
	(let ((cmd (concat "security 2>&1 >/dev/null find-generic-password -ga '" account-name "'")))
	  (let ((passwd (shell-command-to-string cmd)))
		(when (string-match (rx "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\"") passwd)
		  (match-string 1 passwd))))))


;; 将自动保存文件集中至 ~/.emacs.d/auto-save/
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save/" t)))


;; 删除所有buffer
(defun mybuffer/kill-other-buffers ()
  "Kill all other buffers, leaving only the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; 删除所有buffer和历史buffer
(defun mybuffer/kill-other-buffers-and-history ()
  "Kill all other buffers and clear switch history, leaving only the current one."
  (interactive)
  ;; 1. 杀死其他所有缓冲区
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  ;; 2. 根据模式清除历史记录（可多选）
  ;; 清除内置历史记录
  (setq buffer-name-history '())
  (setq file-name-history '())
  ;; 如果是ido模式，也清除其虚拟缓冲区
  (when (boundp 'ido-virtual-buffers)
    (setq ido-virtual-buffers '()))
  ;; 清除recentf列表（如果启用）
  (when (boundp 'recentf-list)
    (setq recentf-list '()))
  (message "All other buffers killed and history cleared."))

(setq-default cursor-type '(bar . 5))

;; 关闭备份文件（以 ~ 结尾的文件）
(setq make-backup-files nil)

;; 关闭自动保存文件（带 @ 和时间戳的文件）
(setq auto-save-default nil)

;; 关闭锁文件（以 .# 开头的文件）
(setq create-lockfiles nil)

;;打开最近文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;;高亮一行
(global-hl-line-mode 1)

;; 关闭菜单栏、工具栏、滚动条
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;防止鼠标误操作
;;启用 disable-mouse-mode 以忽略所有鼠标/触控板事件
;; (define-minor-mode disable-mouse-mode
;;   "A minor-mode that disables all mouse keybinds."
;;   :global t
;;   :lighter " 🐭"
;;   :keymap (make-sparse-keymap))

;; (dolist (type '(mouse down-mouse drag-mouse
;;                       double-mouse triple-mouse))
;;   (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
;;     (dotimes (n 7)
;;       (let ((k (format "%s%s-%s" prefix type n)))
;;         (define-key disable-mouse-mode-map
;;           (vector (intern k)) #'ignore)))))
;; (disable-mouse-mode 1)

;; 打开配置文件
(defun open-init-file()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "<f4>") 'open-init-file)

;;让鼠标滚动更好用
(setq mouse-wheel-scroll-amount ' (1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)



;; 加载 xclip 包
(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))




(provide 'init-utils)
;;; init-utils.el ends here





