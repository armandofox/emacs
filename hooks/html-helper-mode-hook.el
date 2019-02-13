;;
;; for html-helper-mode
;;

(defun html-helper-yank-as-href (arg)
  "Yank kill ring contents inside an <A HREF= ...></A> construction."
  (interactive "p")
  (insert "<A HREF=\"")
  (yank arg)
  (insert "\"></A>")
  (backward-char 4))

(defun html-helper-insert-mailto ()
  "Insert mailto: link with someone's email address."
  (interactive)
  (let ((e (read-string "Address: ")))
    (insert (format "&lt;<I><A HREF=\"mailto:%s\">%s</A></I>&gt;" e e))))

(setq  html-helper-mode-hook
       '(lambda ()
          (html-helper-add-cookie '(entity "\C-c\"" "&quot;"
                                           "html-doublequote" ("&quot;")))
          (define-key html-helper-mode-map
            "\C-c\C-y" 'html-helper-yank-as-href)
          (define-key html-helper-mode-map
            "\C-cm" 'html-helper-insert-mailto)
          (define-key html-helper-mode-map
            "\C-m" 'reindent-then-newline-and-indent)
          (define-key html-helper-mode-map
            "\C-c-" 'tempo-template-html-subscript)
          (define-key html-helper-mode-map
            "\C-c+" 'tempo-template-html-superscript)
          (define-key html-helper-mode-map
            "\C-c_" 'tempo-template-html-horizontal-rule)
          (define-key html-helper-mode-map "\C-c\C-y" 'yank-as-href)
          (auto-fill-mode 0)
          ))

(setq html-helper-load-hook
      '(lambda ()
         (setq html-helper-build-new-buffer t)
         ))

(defun convert-to-table ()
  "Convert region to table.  Row start/row end are \\[ \\], column start/end
  are \\( \\)."
  (interactive)
  (save-restriction
    (save-excursion
      (narrow-to-region (point) (mark))
      (dolist (rpl '(("\\[" . "<TR>") ("\\]" . "</TR>")
                     ("\\(" . "<TD>") ("\\)" . "</TD>")))
        (beginning-of-buffer)
        (while (search-forward (car rpl) nil t)
          (replace-match (cdr rpl) nil t))))))
