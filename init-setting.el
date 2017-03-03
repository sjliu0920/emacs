;;; lsj_function.el --- own function 
;;; Commentary:
;;; Code:

(defun split-window-right-ignore (&optional size)
  (interactive)
  (split-window-right (or size (/ (window-total-width) 2))))

(define-key ctl-x-map "3" 'split-window-right-ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote mailclient-send-it))
 '(sr-speedbar-right-side nil)
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#ffffff"))
       ;; foreground color (yellow)

(set-cursor-color "white")
(set-mouse-color "black")
(set-foreground-color "white")
(set-background-color "black")

;;(set-default-font "Liberation Mono-10")
(set-frame-font "Liberation Mono-10")
;;(set-default-font "Monospace-10")
;;(add-to-list 'default-frame-alist '(font . "Liberation Mono-10"))
;;(add-to-list 'default-frame-alist '(font . "Monospace-10"))
(add-to-list 'load-path "~/.emacs.d/plugins/color-theme")
(require 'color-theme)
(color-theme-initialize)
;;(color-theme-vim-colors)
;;(color-theme-classic)
;;(color-theme-gray30)
(set-background-color "#333333")
(set-background-color "gray15")


(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)
(setq visible-bell t)
(global-linum-mode t)
(setq inhibit-startup-message t)
(setq column-number-mode t) 
(setq mouse-yank-at-point t)
(setq kill-ring-max 200)
(setq default-fill-column 60)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-stop-list ())
(setq sentence-end "\\([¡££¡£¿]\\|¡­¡­\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq enable-recursive-minibuffers t)
(setq scroll-margin 3
       scroll-conservatively 10000)
(setq default--mode 'text-mode)
(show-paren-mode t)
	(setq show-paren-style 'parentheses)

(mouse-avoidance-mode 'animate)
(mouse-avoidance-mode 'nil)
(setq frame-title-format "emacs@%b")
(auto-image-file-mode)
(global-font-lock-mode t)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

(add-to-list 'load-path "~/.emacs.d/plugins")

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete/popup-el")
(require 'popup)
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete-clang")
(require 'auto-complete-clang)  
(setq ac-clang-auto-save t)  
(setq ac-auto-start t)
(setq ac-quick-help-delay 0.1) 
(ac-set-trigger-key "TAB") 
(define-key ac-mode-map  [(control tab)] 'auto-complete)  

;; delete below two path
;; /usr/include
;; /usr/include/boost

(defun my-ac-basic-config ()  
  (setq ac-clang-flags
		(mapcar(lambda (item)(concat "-I" item))  
               (split-string  
                "/usr/include/c++/5
			/usr/include/x86_64-linux-gnu/c++/5
			/usr/include/c++/5/backward
			/usr/lib/gcc/x86_64-linux-gnu/5/include
			/usr/local/include
			/usr/local/include/glog
			/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed
			/usr/include/x86_64-linux-gnu
            /usr/include/asm-generic
			/home/double/parallel_world/engine/include
			/home/double/parallel_world/gjson/include
			/home/double/parallel_world/db/include
            /home/double/parallel_world/gopw/src
			")))

  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)  
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)  
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)  
  (add-hook 'css-mode-hook 'ac-css-mode-setup)  
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)  
  (global-auto-complete-mode t))

(defun my-ac-cc-mode-setup ()  
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))  
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)  
 ;;ac-source-gtags  
(my-ac-basic-config)

(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			  (ggtags-mode 1))))


;; ===============
(require 'ido)
(ido-mode t)

(require 'sr-speedbar)
(setq speedbar-use-images nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq sr-speedbar-right-side nil)
;;(sr-speedbar-open)

;;===========helm like everything in windows=======
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-async")
(add-to-list 'load-path "~/.emacs.d/plugins/helm")
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;;=====================
(load "lsj_function")

;;==============
(load "google-c-style")
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)



(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register. 
Use ska-jump-to-register to jump back to the stored 
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))
(global-set-key [(control ?\.)] 'ska-point-to-register)
(global-set-key [(control ?\,)] 'ska-jump-to-register)

(autoload 'table-insert "table" "WYGIWYS table editor")

(add-to-list 'load-path "~/.emacs.d/plugins/xcscope")
(require 'xcscope)

(require 'rect-mark)

(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)


(defun sb-expand-curren-file ()
  "Expand current file in speedbar buffer"  
  (interactive)                                                                                                                               
  (setq current-file (buffer-file-name))
  (sr-speedbar-refresh)
  (switch-to-buffer-other-frame "*SPEEDBAR*")
  (speedbar-find-selected-file current-file)
  (speedbar-expand-line))

;;=================================
;;(require 'ibuffer)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(autoload 'ibuffer "ibuffer" "Buffer List" t)
;;(add-to-list 'ibuffer-never-show-regexps "^\\*")

;;(require 'easymenu)
;;(require 'recentf)
;;(setq recentf-max-saved-items 100)
;;(recentf-mode 1)
;;(defun recentf-open-files-compl ()
;;  (interactive)
;;  (let* ((all-files recentf-list)
;;	 (tocpl (mapcar (function 
;;			 (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
;;	 (prompt (append '("File name: ") tocpl))
;;	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
;;  (find-file (cdr (assoc-ignore-representation fname tocpl)))))

;;(global-set-key [(control x)(control r)] 'recentf-open-files-compl)

(load "recentf-buffer")
(require 'recentf-buffer)               
(global-set-key (kbd "C-x C-r") 'recentf-open-files-in-simply-buffer)

;;=====git======
;;(add-to-list 'load-path "~/.emacs.d/plugins/git-emacs")
;;(require 'git-emacs)

(add-to-list 'load-path "~/.emacs.d/plugins/magit/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/with-editor")
(add-to-list 'load-path "~/.emacs.d/elpa/dash-2.12.0")
(require 'magit)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/plugins/magit/Documentation/"))

(load "~/.emacs.d/plugins/magit/magit-autoloads")

;; ace-jump-mode
(autoload
   'ace-jump-mode
   "ace-jump-mode"
   "Emacs quick move minor mode"
   t)
;; you can select the key you prefer to
(define-key global-map [(control return)]  'ace-jump-mode)

;; find-file-in-project

;; Yes, I want my copies in the same dir as the original.
;;(require 'flymake)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)
;;(flymake-mode-off)
;;(setq flymake-run-in-place t)

;; Nope, I want my copies in the system temp dir.
;;(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
;;(setq temporary-file-directory "~/.emacs.d/tmp/")
;; I want to see at most the first 4 errors for a line.
;;(setq flymake-number-of-errors-to-display 4)

;; I want to see all errors for the line.
;;(setq flymake-number-of-errors-to-display nil)

;;(autoload 'flymake-find-file-hook "flymake" "" t)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)
;;(setq flymake-gui-warnings-enabled t)
;;(setq flymake-log-level 0)
;;(setq flymake-no-changes-timeout 0.5)

(add-to-list 'load-path "/home/double/.emacs.d/plugins/yasnippet-master")
(require 'yasnippet)

;;(add-to-list 'yas-snippet-dirs "/home/double/.emacs.d/plugins/yasnippet-snippets-master")

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
;;       "~/.emacs.d/snippets"           ;; foo-mode and bar-mode snippet collection
        "/home/double/.emacs.d/plugins/yasnippet-snippets-master" ;; the yasmate collection
;;        "/path/to/yasnippet/snippets"         ;; the default collection
        ))

(yas-global-mode 1)

;;====window-number=============
(require 'window-number)
(global-set-key (kbd "C-x o") 'window-number-switch)
;;(global-set-key (kbd "C-x o") 'other-window)
;;==============

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defvar unscroll-point nil
  "Text position for next call to 'unscroll'.")

(defvar unscroll-window-start nil
  "Window start for next call to unscroll.")

(defvar unscroll-hscroll nil
  "Hscroll for next call to 'unscroll'.")

(defadvice scroll-up (before remember-for-unscroll
                             activate compile)
  "Remember where we started from, for unscroll."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
                             activate compile)
  "Remember where we started from, for unscroll."
  (unscroll-maybe-remember))

(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn
        (setq unscroll-point (point))
        (setq unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(defun unscroll ()
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

;;============
(defun insert-current-time ()
  "Insert the current time"
  (interactive "*")
  (insert (current-time-string)))

;;=============
(add-to-list 'load-path "~/.emacs.d/plugins/paredit")
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(add-to-list 'load-path "~/.emacs.d/plugins/highlight-symbol")
(require 'highlight-symbol)

(global-set-key (kbd "C-c C-.") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c C-n") 'highlight-symbol-next)
(global-set-key (kbd "C-c C-p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c C-q") 'highlight-symbol-query-replace)

;;========================major mode ==================
;; lua-mode

(add-to-list 'load-path "~/.emacs.d/plugins/lua-mode")
(require 'lua-mode)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; oracle pc file
(add-to-list 'auto-mode-alist '("\\.pc\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.q\\'" . quip-mode))
(require 'quip)
;;=======================================================

;;elisp high-light

(setq org-src-fontify-natively t)

(set-background-color "#323232")

(add-to-list 'load-path "~/.emacs.d/plugins/diredful")
(require 'diredful)
(diredful-mode 1)

;;=============================================================
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-which-key")
(require 'which-key)
(setq which-key-idle-delay 0.5)
(setq which-key-show-prefix 'left)
(setq which-key-show-remaining-keys t)
(which-key-mode)

;;========================================
(require 'undo-tree)
(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)
(defalias 'undo 'undo-tree-undo)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "M--") 'redo)

;;========================
(desktop-save-mode 1)

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;;=============xah-comment===========
(require 'xah-comment)
(global-set-key (kbd "M-;") 'xc-comment-smart)

;;=============logview-mode====================
;; (add-to-list 'load-path "/home/double/.emacs.d/plugins/logview")
;; (add-to-list 'load-path "/home/double/.emacs.d/plugins/datetime")
;; (require 'logview)

;;================flycheck=============
(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;===============lookup============
(add-to-list 'load-path "~/.emacs.d/plugins/lookup-word-on-internet")
(require 'xah-lookup)
(require 'eww)
(setq xah-lookup-browser-function 'eww)

;;;;

