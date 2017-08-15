;;; init-setting.el --- own function 
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

(set-cursor-color "white")
(set-mouse-color "black")
(set-foreground-color "white")

(set-frame-font "Liberation Mono-10")
(add-to-list 'load-path "~/.emacs.d/plugins/color-theme")
(require 'color-theme)
(color-theme-initialize)
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
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
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

(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete-c-headers")
(add-to-list 'ac-sources 'ac-source-c-headers)

(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

;; delete below two path
;; /usr/include/boost

(defun my-ac-basic-config-pw ()  
  (setq ac-clang-flags
		(mapcar(lambda (item)(concat "-I" item))  
               (split-string  
			"/home/double/pw/common
            /home/double/pw/ProtoFiles/src
            /home/double/pw/pw_switch/src
            /home/double/pw/pw_switch/include
            /home/double/pw/pw_robot/src
            /home/double/pw/pw_robot/include
            /home/double/pw/pw_space/src
            /home/double/pw/pw_space/include
            /home/double/pw/pw_weather/src
            /home/double/pw/pw_weather/include
            /home/double/pw/pw_emergency/src
            /home/double/pw/pw_emergency/include
            /home/double/pw/pw_timer/include
            /home/double/pw/pw_timer/timer/src
            /usr/include/c++/5
            /usr/include
            /usr/include/lua5.0
            /use/include/linux
			/usr/include/c++/5/backward
			/usr/local/include
            /usr/include/x86_64-linux-gnu
            /usr/include/x86_64-linux-gnu/c++/5
            /usr/include/c++/5/tr1
            /usr/lib/gcc/x86_64-linux-gnu/5/include-fixed
            /usr/include/x86_64-linux-gnu
            /usr/include/asm-generic
            /usr/lib/gcc/x86_64-linux-gnu/5/include
			/usr/local/include/glog")))

  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)  
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)  
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)  
  (add-hook 'css-mode-hook 'ac-css-mode-setup)  
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)

;;====compile======
(global-set-key (kbd "M-g M-p") 'previous-error)
(global-set-key (kbd "M-g M-n") 'next-error)

;;=====
(defun my-ac-cc-mode-setup ()  
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))  
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)  
 ;;ac-source-gtags  
(my-ac-basic-config-pw)

(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			  (ggtags-mode 1))))

(global-set-key (kbd "M-<f1>") 'gtags-find-file)
(global-set-key (kbd "M-<f2>") 'gtags-find-tag)
(global-set-key (kbd "M-<f3>") 'gtags-find-rtag)
(global-set-key (kbd "M-<f4>") 'gtags-find-symbol)
(global-set-key (kbd "M-<f5>") 'gtags-find-with-grep)

(global-set-key (kbd "M-0") 'ggtags-prev-mark)

;; ===============
(require 'ido)
(ido-mode t)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(setq ido-enable-flex-matching t)

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
(load "lsj-function")

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

;;(add-to-list 'load-path "~/.emacs.d/plugins/xcscope")
;;(require 'xcscope)

(require 'rect-mark)

(defun sb-expand-curren-file ()
  "Expand current file in speedbar buffer"  
  (interactive)                                                                                                                               
  (setq current-file (buffer-file-name))
  (sr-speedbar-refresh)
  (switch-to-buffer-other-frame "*SPEEDBAR*")
  (speedbar-find-selected-file current-file)
  (speedbar-expand-line))

;;=================================
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
(global-set-key (kbd "M-o") 'window-number-switch)
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

;;======================goto-windows======================
(defun lsj-goto-windows-1 ()
  "Do something"
  (interactive)
  (window-number-select 1))

(defun lsj-goto-windows-2 ()
  "Do something"
  (interactive)
  (window-number-select 2))

(defun lsj-goto-windows-3 ()
  "Do something"
  (interactive)
  (window-number-select 3))

(defun lsj-goto-windows-4 ()
  "Do something"
  (interactive)
  (window-number-select 4))

(global-set-key (kbd "M-1") 'lsj-goto-windows-1)
(global-set-key (kbd "M-2") 'lsj-goto-windows-2)
(global-set-key (kbd "M-3") 'lsj-goto-windows-3)
(global-set-key (kbd "M-4") 'lsj-goto-windows-4)
;;=======================delete-windows-buffer=============
(defun lsj-delete-windows-1 ()
  "Do something"
  (interactive)
  (let ((window (nth 0 (window-number-list))))
    (delete-window window)))

(defun kill-window-buffer (&optional num)
  "Do something"
  (interactive)
  (let ((buffer (buffer-name (window-buffer (nth (1- num) (window-number-list))))))
    (kill-buffer buffer)))

(defun kill-window-buffer-1 ()
  "Do something"
  (interactive)
  (kill-window-buffer 1))

(defun kill-window-buffer-2 ()
  "Do something"
  (interactive)
  (kill-window-buffer 2))

(defun kill-window-buffer-3 ()
  "Do something"
  (interactive)
  (kill-window-buffer 3))

(defun kill-window-buffer-4 ()
  "Do something"
  (interactive)
  (kill-window-buffer 4))

(global-set-key (kbd "C-M-1") 'kill-window-buffer-1)
(global-set-key (kbd "C-M-2") 'kill-window-buffer-2)
(global-set-key (kbd "C-M-3") 'kill-window-buffer-3)
(global-set-key (kbd "C-M-4") 'kill-window-buffer-4)
;;========================Major Mode ==================
;; Lua-mode
(add-to-list 'load-path "~/.emacs.d/plugins/lua-mode")
(require 'lua-mode)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; oracle pc file
(add-to-list 'auto-mode-alist '("\\.pc\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.q\\'" . quip-mode))
(require 'quip)
;;====================lsj-log-mode===================================
(add-to-list 'load-path "~/.emacs.d/plugins/")
(require 'lsj-log-mode)

(add-to-list 'auto-mode-alist '("\\.INFO\\'" . lsj-log-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . c-mode))

;;elisp high-light

(setq org-src-fontify-natively t)


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

;; (hl-line-mode)
;;;;


;;===============org=============
;;(setq org-image-actual-width t)
;;(setq org-image-actual-width '(100))
;;(setq org-html-inline-images t)

;;========template====

(require 'template)
(template-initialize)
(dolist (cmd '(ido-select-text ido-magic-forward-char
                               ido-exit-minibuffer))
  (add-to-list 'template-find-file-commands cmd))


;;=============abbrev=======
;; (load "~/.emacs.d/plugins/lsj-abbrev.el")

(defcustom xah-shell-abbrev-alist nil "alist of xah's shell abbrevs")

(setq xah-shell-abbrev-alist
      '(
        ("cdpw" . "cd ~/pw")
        ("netstat" . "netstat -nap | grep 8009")
        ("find-sed-delete" . "find . -name '*.cpp' | xargs grep 'std::LOG' -l | xargs sed -i \"s/std::LOG/LOG/g\"")
        ("find-cpp-h" . "find . -name '*.h' -o '*.cpp'")
        ("query-replace-regexp" . "(query-replace-regexp \" = [0-9]\\\\{1,2\\\\}\" \"\")")
        ("apt-get install" . "sudo apt-get install ")
        ("git-status" . "git status")
        ("git-stash" . "git stash")
        ("git-commit" . "git commit -m 'modify'")
        ("git-reset" . "git reset")
        ("git-check" . "git checkout")
        ("git-push" . "git push origin master")
        ("git-pull" . "git pull ")
        ("git-clone pw" . "git@172.16.0.2:liushuangjian/parallel_world.git")
        ))

(defun xah-shell-commands (*cmd-abbrev)
  "insert shell command from a list of abbrevs."
  (interactive
   (list
    (ido-completing-read "shell abbrevs:"
                         (mapcar (lambda (x) (car x)) xah-shell-abbrev-alist) "PREDICATE" "REQUIRE-MATCH")))
  (progn
    (insert (cdr (assoc *cmd-abbrev xah-shell-abbrev-alist)))))

(global-set-key (kbd "C-0") 'xah-shell-commands)

;;============set evn===========
(setenv "PATH"
  (concat
   "C:/cygwin/bin" ";"
   (getenv "PATH")
  ))

(setenv "ORACLE_HOME" "/opt/oracle/11.2/client64")
(setenv "LD_LIBRARY_PATH" "/home/double/parallel_world/lib:/lib:/lib:/lib:/opt/oracle/11.2/client64/lib")

(setenv "LC_CTYPE" "zh_CN.UTF-8")

;;==============括号间跳转===========
(global-set-key (kbd "C->") 'forward-sexp)
(global-set-key (kbd "C-<") 'backward-sexp)
;;;;

;;================xah-fly-keys========
;;(add-to-list 'load-path "~/.emacs.d/plugins/xah-fly-keys/")
;;(require 'xah-fly-keys)

;;(xah-fly-keys-set-layout "qwerty") ; required if you use qwerty ;;_
;;(xah-fly-set-layout "dvorak") ; by default, it's dvorak

;;(xah-fly-keys 0)

;;===================
(require 'multiple-cursors)

(global-set-key (kbd "C-c C-m") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-all-symbols-like-this-in-defun)
;;(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;;=============
(defun xah-run-current-file ()
  (interactive)
  (let (
        ($suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("hs" . "runhaskell")
           ("js" . "node") ; node.js
           ("ts" . "tsc --alwaysStrict --lib DOM,ES2015,DOM.Iterable,ScriptHost --target ES5") ; TypeScript
           ("sh" . "bash")
           ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        $fname
        $fSuffix
        $prog-name
        $cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fSuffix (file-name-extension $fname))
    (setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
    (setq $cmd-str (concat $prog-name " \""   $fname "\""))
    (cond
     ((string-equal $fSuffix "el") (load $fname))
     ((string-equal $fSuffix "go")
      (when (fboundp 'gofmt)
        (gofmt)
        (shell-command $cmd-str "*xah-run-current-file output*" )))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command $cmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory $fname))))))
     (t (if $prog-name
            (progn
              (message "Running…")
              (shell-command $cmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(global-set-key (kbd "<f8>") 'xah-run-current-file)

(require 'find-file-in-project)

(setq ffip-project-root "~/pw/")
