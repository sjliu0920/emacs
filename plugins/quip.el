(require 'derived)

(defvar quip-mode-map4 nil
  "Keymap for Quip major mode.")

(defun count-quips ()
  "Count the quips in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (count-matches "^%%$"))))


(define-derived-mode quip-mode text-mode "Quips"
  "Major mode for editing Quip files.
Special commands:
\\{quip-mode-map}"
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'page-delimiter)
  (setq paragraph-start "%%\\[[ \t\n\^L]")
  (setq paragraph-separate "%%$\\ [ \t\^L]*$")
  (setq page-delimiter "^%%$")
  (use-local-map quip-mode-map4)
  )

(defalias 'backward-quip 'backward-page)
(defalias 'forward-quip 'forward-page)
(defalias 'narrow-to-quip 'narrow-to-page)
(defalias 'what-quip 'what-page)

(if quip-mode-map4
    nil
  (setq quip-mode-map4 (copy-keymap text-mode-map))
  (define-key quip-mode-map4 "\C-x[" 'backward-quip)
  (define-key quip-mode-map4 "\C-x]" 'forward-quip)
  (define-key quip-mode-map4 "\C-xnq" 'narrow-to-quip)
  (define-key quip-mode-map4 "\C-cw" 'what-quip))

(provide 'quip)
