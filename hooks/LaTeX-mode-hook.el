;;;
;;; LaTeX
;;;

(setq TeX-default-mode 'LaTeX-mode)

(add-hook 'latex-mode-hook
          (lambda ()
            (define-key tex-mode-map "\C-c;" 'comment-region)
            (define-key tex-mode-map "\M-q" 'wrap-at-sentences)))

(defun wrap-at-sentences ()
  "Fills the current paragraph, but starts each sentence on a new line."
  (interactive)
  (save-mark-and-excursion
    ;; Select the entire paragraph.
    (let ((start-of-paragraph (region-beginning))
          (paragraph-start "\\|\\s-*$\\|\\s-*\\\\")
          (end-of-paragraph (region-end))
          (fill-prefix
           (save-mark-and-excursion (beginning-of-line) (re-search-forward "\\s-*") (match-string 0))))
      ;; Move to the start of the paragraph.
      (mark-paragraph)
      (goto-char start-of-paragraph)
      ;; Wrap lines with 'hard' newlines (i.e., real line breaks).
      (let ((use-hard-newlines 't)
            (sentence-end "\\([%.?!…‽][]\"'”’)}]*\\($\\|[  ]$\\|	\\|[  ][  ]\\)\\|[。．？！]+\\)[  	\n]*"))
        ;; Loop over each sentence in the paragraph.
        (while (< (point) end-of-paragraph)
          ;; Determine the region spanned by the sentence.
          (setq start-of-sentence (point))
          (forward-sentence)
          ;; Wrap the sentence with hard newlines.
          (fill-region start-of-sentence (point))
          ;; Delete the whitespace following the period, if any.
          (while (char-equal (char-syntax (preceding-char)) ?\s)
            (delete-char -1))
          ;; Insert a newline before the next sentence.
          (insert "\n")))
      (goto-char start-of-paragraph)
      (while (re-search-forward "^\s-*" end-of-paragraph t)
        (replace-match ""  nil nil))
      (goto-char start-of-paragraph)
      (while (re-search-forward "\n\n" end-of-paragraph t)
        (replace-match "\n"  nil nil)))))


