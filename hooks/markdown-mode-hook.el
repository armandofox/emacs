(setq markdown-mode-hook
      '(lambda ()
         (define-key markdown-mode-map "\C-cd" 'markdown-insert-details)
         ))

(defun markdown-insert-details ()
  (interactive)
  (insert "\n<details>\n<summary>\n\n</summary>\n<p><blockquote>\n\n</blockquote></p>\n</details>")
  (previous-line 5)
  (beginning-of-line))

(defun markdown-convert-latex ()
  (interactive)
  (save-excursion
    (narrow-to-region (point) (mark))
    (change-many '(("``\\([^']+\\)''"  "\"\\1\"")
                   ("\\\\C{\\([^}]+\\)}" "`\\1`")
                   ("\\\\T{\\([^}]+\\)}" "`\\1`")
                   ("\\\\B{\\([^}]+\\)}" "**\\1**")
                   ("\\\\index{[^}]+}%?" "")
                   ("\\\\emph{\\([^}]+\\)}" "_\\1_")
                   ("\\\\\\([_$#]\\)" "\\1")
                   ))
    (widen)))

                   
