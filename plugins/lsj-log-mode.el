;;; lsj-log-mode.el --- set color for glog

;;; Commentary:

;;; Code:

(defvar mymath-highlights)

(defface lsj-log-warning-face
  '(
    (t
     (
      :foreground "yellow"
     )
     )
    )
  "Default face used in the tab bar."
  :group 'lsj-log)

(defface lsj-log-error-face
  '(
    (t
     (
      :foreground "red"
     )
     )
    )
  "Default face used in the tab bar."
  :group 'lsj-log)

(setq mymath-highlights
      '(("\\(^E[0-9]+ .*\\)" . 'lsj-log-error-face)
        ("\\(^W[0-9]+ .*\\)" . 'lsj-log-warning-face)))

(defun goto-next-line (&optional arg)
  "Do something ARG."
  (interactive "P")
  (end-of-line)
  (re-search-forward arg)
  (beginning-of-line))

(defun goto-back-line (&optional arg)
  "Do something ARG."
  (interactive "P")
  (beginning-of-line)
  (re-search-backward arg)
  (beginning-of-line))

(defvar lsj-log-error-reg nil "Keymap for `lsj-log-mode'.")
(defvar lsj-log-warning-reg nil "Keymap for `lsj-log-mode'.")

(setq lsj-log-error-reg "\\(^E[0-9]+\\)")
(setq lsj-log-warning-reg "\\(^W[0-9]+\\)")

(defun goto-next-line-error()
  (interactive)
  (goto-next-line lsj-log-error-reg))

(defun goto-next-line-warning()
  (interactive)
  (goto-next-line lsj-log-warning-reg))

(defun goto-back-line-error()
  (interactive)
  (goto-back-line lsj-log-error-reg))

(defun goto-back-line-warning()
  (interactive)
  (goto-back-line lsj-log-warning-reg))

(defun get-unit (&optional arg)
  "Do Something"
  (interactive)
  (let (-reg -unit)
    (setq -reg " \\([0-9]\\{3,5\\}\\) \\([a-zA-Z0-9_.]+\\):\\([0-9]+\\)] ")
    (beginning-of-line)
    (re-search-forward -reg)
    (cond
     ((equal arg "thread")
      (setq -unit (match-string 1)))
     ((equal arg "src")
      (setq -unit (match-string 2)))
     ((equal arg "line")
      (setq -unit (match-string 3))))
    (message "%s" -unit)))

(defun get-proj-path (&optional arg)
  "Get project full path"
  (interactive)
  (let (-path)
    (cond
     ((equal arg "inc")
      (setq -path (concat "/home/double/pw/" (file-name-sans-extension (buffer-name)) "/include/")))
     ((equal arg "src")
      (setq -path (concat "/home/double/pw/" (file-name-sans-extension (buffer-name)) "/server/"))))
    (message "%s" -path)))

;;(get-proj-path "src")

(defun open-current-line ()
  "Do something."
  (interactive)
  (let ((-file (get-unit "src"))
        (-num (get-unit "line"))
        (-path-inc (get-proj-path "inc"))
        (-path-src (get-proj-path "src"))
        files
        check-file)
    (setq files (list "/home/double/pw/common/"
                  "/home/double/pw/ProtoFiles/src/"
                  (symbol-value '-path-inc)
                  (symbol-value '-path-src)))

    (catch 'break
      (while (> (length files) 0)
        (setq check-file (concat (pop files) -file))
        (if (file-exists-p check-file)
            (progn
              (find-file-other-window check-file)
              (throw 'break nil)))))

    (read-only-mode)
    (goto-char 1)
    (forward-line (1- (string-to-number -num)))
    (recenter)))

(defun lsj-refresh-log-file () 
  "Do something."
  (interactive)  
  (revert-buffer t (not (buffer-modified-p)) t))

(defun lsj-filter-lines(&optional arg)
  (interactive)
  (let (-reg)
    (if (equal arg nil)
        (setq -reg (read-string "Enter regexp:"))
      (setq -reg (get-unit arg)))
    (list-matching-lines -reg)))

(defun lsj-filter-thread()
  (interactive)
  (lsj-filter-lines "thread"))

(defun lsj-filter-src()
  (interactive)
  (lsj-filter-lines "src"))

(defun lsj-close-log-filter()
  (interactive)
  (kill-buffer "*Occur*"))

(defvar lsj-log-mode-map nil "Keymap for `lsj-log-mode'.")

(if lsj-log-mode-map
    nil
  (setq lsj-log-mode-map (make-sparse-keymap))
  (define-key lsj-log-mode-map (kbd "e") 'goto-next-line-error)
  (define-key lsj-log-mode-map (kbd "w") 'goto-next-line-warning)
  (define-key lsj-log-mode-map (kbd "E") 'goto-back-line-error)
  (define-key lsj-log-mode-map (kbd "W") 'goto-back-line-warning)
  (define-key lsj-log-mode-map (kbd "o") 'open-current-line)
  (define-key lsj-log-mode-map (kbd "n") 'next-line)
  (define-key lsj-log-mode-map (kbd "p") 'previous-line)
  (define-key lsj-log-mode-map (kbd "g") 'lsj-refresh-log-file)
  (define-key lsj-log-mode-map (kbd "t") 'lsj-filter-thread)
  (define-key lsj-log-mode-map (kbd "s") 'lsj-filter-src)
  (define-key lsj-log-mode-map (kbd "f") 'lsj-filter-lines)
  (define-key lsj-log-mode-map (kbd "q") 'lsj-close-log-filter))

(defvar lsj-log-mode-hook nil
  "*List of functions to call when entering lsj log mode.")

(add-hook 'lsj-log-mode-hook
          (lambda ()
            (hl-line-mode t)
            (read-only-mode)))

(define-derived-mode lsj-log-mode fundamental-mode "glog"
  "major mode for editing glog."
  (setq font-lock-defaults '(mymath-highlights))
  (use-local-map lsj-log-mode-map)
  (run-hooks 'lsj-log-mode-hook))

(provide 'lsj-log-mode)

;;; lsj-log-mode.el ends here

