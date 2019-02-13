;; functions for dealing with non-word-wrapped text files by rebinding common
;; navigation commands.

(defun unwrap-forward-line (pfx)
  (interactive "p")
  (forward-char (* pfx (window-width))))

(defun unwrap-backward-line (pfx)
  (interactive "p")
  (backward-char (* pfx (window-width))))

