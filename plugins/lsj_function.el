(define-prefix-command 'lsj-map)
(global-set-key (kbd "C-c") 'lsj-map)

(defun lsj-line-to-top-of-window ()
  "Move the line point to top of window"
  (interactive)
  (recenter 0)
  )

(global-set-key (kbd "C-c t t") 'lsj-line-to-top-of-window)

(defun lsj-new-line()
  "Add a new line below"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  )
(global-set-key (kbd "C-c n l") 'lsj-new-line)

(defun lsj-new-shift-line()
  "Add a new line above"
  (interactive)
;;  (previous-line)
  (forward-line -1)
  (move-end-of-line nil)
  (newline-and-indent)
  )
(global-set-key (kbd "C-c n L") 'lsj-new-shift-line)

(defun lsj-copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line"
  (interactive "P")
  (let ((beg (line-beginning-position)) 
        (end (line-end-position arg)))
    (copy-region-as-kill beg end))
  )
(global-set-key (kbd "C-c c l") 'lsj-copy-line)

(defun lsj-copy-word (&optional arg)
  "Copy words at point"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point))) 
        (end (progn (forward-word arg) (point))))
    (copy-region-as-kill beg end))
  )
(global-set-key (kbd "C-c c w") 'lsj-copy-word)

(defun lsj-delete-word (&optional arg)
  "delete words at point"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[^ \n\"']" 1) (backward-word 1)) (point))) 
    (end (progn (forward-word arg) (point))))
    (kill-region beg end))
  )
(global-set-key (kbd "C-c d w") 'lsj-delete-word)

(defun lsj-copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point))) 
        (end (progn (forward-paragraph arg) (point))))
    (copy-region-as-kill beg end))
  )
(global-set-key (kbd "C-c c p") 'lsj-copy-paragraph)

(defun lsj-delete-line-anywhere ()
  "Delete whole line from anywhere of one line"
  (interactive)
  (move-beginning-of-line nil)
  (kill-whole-line)
  )
(global-set-key (kbd "C-c d l") 'lsj-delete-line-anywhere)

(defun lsj-delete-to-begin()
  (interactive)
  (let ((beg (line-beginning-position)) )
    (kill-region beg (point))
    )
  )
(global-set-key (kbd "C-c d b") 'lsj-delete-to-begin)

(defun lsj-join-two-line()
  "Add a new line below"
  (interactive)
  (move-end-of-line nil)
  (kill-line)
  )
(global-set-key (kbd "C-c j") 'lsj-join-two-line)

(defun lsj-go-to-char (n char)
  "Move forward to Nth occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(define-key global-map (kbd "C-c f") 'lsj-go-to-char)

(defun lsj-recursive-count-words (region-end)
  (if (and (< (point) region-end)
           (re-search-forward "\\w+\\W*" region-end t) )
      (1+ (recursive-count-words region-end))
    0
    )
  )

(defun lsj-count-words-region (beginning end)
  "Print numbers of words in the region"
  (interactive "r")
  (message "Counting words in region...")
  (save-excursion
   (goto-char beginning)
   (let ((count (recursive-count-words end)))
     (cond ( (zerop count)
             (message "no words"))
           ( (= 1 count)
             (message "1 words") )
           ( t
             (message "%d words" count) )
           )
     )
   )
  )

(global-set-key (kbd "C-c r w") 'lsj-count-words-region )

(defun lsj-auto-add-mutt-comm-string()
  "Insert the common string in mutte"
  (interactive)
  (save-excursion
;;    (end-of-buffer)
    (goto-char (point-max))
    (if (and (find (aref (buffer-name) 0) "m") (find (aref (buffer-name) 1) "u") 
             (find (aref (buffer-name) 2) "t") (find (aref (buffer-name) 3) "t") )
        (insert "\n\n-------\n   Best regards")
      )
    )
  )

;;(auto-add-mutt-comm-string)

(global-set-key (kbd "C-c m") 'auto-add-mutt-comm-string )

;;(add-hook 'text-mode-hook 'auto-add-comm-string)

(defun lsj-refresh-file ()  
  (interactive)  
  (revert-buffer t (not (buffer-modified-p)) t))  

(global-set-key [(control f5)] 'lsj-refresh-file) 


(defun lsj-scroll-up ()
  (interactive)
  (scroll-up-line 1)
  )

(defun lsj-comment-line ()
  (interactive)
  (let (begin end)
    (beginning-of-line)
    (skip-chars-forward "^ ")
    (setq begin (point))
    (end-of-line)
    (setq end (point))
    (comment-region begin end)
    )
  )


(defun lsj-select-fun()
  (interactive)
  (let (p1 p2)
    (setq p1 (progn (beginning-of-defun) (point)))
    (setq p2 (progn (end-of-defun) (point)))
    (goto-char p2)
    (push-mark p1)
    (setq mark-active t)
    )
  )

(global-set-key (kbd "C-c s f") 'lsj-select-fun )

(defun lsj-select-line()
  (interactive)
  (let (p1 p2)
    (setq p1 (progn (beginning-of-line) (point)))
    (setq p2 (progn (end-of-line) (point)))
    (goto-char p2)
    (push-mark p1)
    (setq mark-active t)
    )
  )

(global-set-key (kbd "C-c s l") 'lsj-select-line)

;;===============open file at cursor from log=========
(defun lsj-open-file-at-cursor-from-log()
  "Open file under cursor or select region"
  (interactive)
  (let ((-path (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2 lsj-regexp)
                   (setq lsj-regexp "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p0 (point))
                   (skip-chars-backward lsj-regexp)
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward lsj-regexp)
                   (setq p2 (point))
                   (buffer-substring-no-properties p1 p2)
                   ))))
    (progn
      (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" -path)
          (progn
            (let (-dir
                  (-fpath (match-string 1 -path))
                  (-num (match-string 2 -path)))
              (if (string-match "DB.cpp" -fpath)
                  (setq -dir "/home/double/parallel_world/db/src/")
                (setq -dir "/home/double/parallel_world/gopw/src/"))
              (if (file-exists-p (concat -dir -fpath))
                  (progn
                    (find-file (concat -dir -fpath))
                    (goto-char 1)
                    (forward-line (1- (string-to-number -num)))
                    (recenter)
                    (message "[%s, %s, %s]" -dir -fpath -num)
                    ))))))))

;;=====================open file at cursor from src===============
(defun lsj-open-file-at-cursor-from-src()
  "Open file under cursor from src"
  (interactive)
  (let ((-path (let (p0 p1 p2 lsj-regexp-src)
                 (setq lsj-regexp-src "^\"<>")
                 (setq p0 (point))
                 (skip-chars-backward lsj-regexp-src)
                 (setq p1 (point))
                 (goto-char p0)
                 (skip-chars-forward lsj-regexp-src)
                 (setq p2 (point))
                 (buffer-substring-no-properties p1 p2))))
    (progn
      (let ((-dir (file-name-directory (buffer-file-name))))
        (if (file-exists-p (concat -dir -path))
            (find-file (concat -dir -path)))))))

(global-set-key [f7] 'lsj-open-file-at-cursor-from-src)

;;=========== switch between .h and .cpp =================
(defun switch-source-file ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (string-match "\\.cpp" file-name)
        (find-file (replace-regexp-in-string "\\.cpp" "\.h" file-name)))
    (if (string-match "\\.h" file-name)
        (find-file (replace-regexp-in-string "\\.h" "\.cpp" file-name)))))

(global-set-key [f6] 'switch-source-file)

;;=====================
(defun lsj-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path."
  (interactive "P")
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (buffer-file-name)
               (buffer-file-name)
             (user-error "Current buffer is not associated with a file.")))))
    (kill-new
     (if *dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory -fpath))
           (file-name-directory -fpath))
       (progn
         (message "File path copied: 「%s」" -fpath)
         -fpath )))))

(defun lsj-goto-line-indent()
  (interactive)
  (let ((line (read-number "Goto line: ")))
    (goto-char 0)
    (forward-line (1- line)))
  (c-indent-line-or-region))

(global-set-key (kbd "M-g M-g") 'lsj-goto-line-indent)

;; ======================highlight================
(global-set-key (kbd "C-c l i") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c l n") 'highlight-symbol-next)
(global-set-key (kbd "C-c l p") 'highlight-symbol-prev)

;;;;lsj_function.el ends here
