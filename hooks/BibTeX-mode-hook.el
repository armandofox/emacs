(defun BibTeX-copy-citation-to-kill-ring ()
  "Copy current citation key, surrounded by \\cite{...}, to kill ring."
  (interactive)
  (save-excursion
    (when (re-search-backward "^\\s-*@\\sw+{\\s-*\\(\\sw+\\)\\s-*}" nil t)
      (copy-region-as-kill (match-beginning 1) (match-end 1)))))
  

(defun BibTeX-paste-authors-changing-commas-to-ands ()
  "Paste author list into a BibTeX entry, changing commas to the string ' and '."
  (interactive)
  (save-restriction
    (yank)
    (narrow-to-region (mark) (point))
    (beginning-of-buffer)
    (while (re-search-forward "," nil t)
      (replace-match " AND " nil nil))))

(defun BibTeX-keep-matching (re match)
  (interactive "*sEntries matching:
XKeep (t) or delete (nil):")
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (while (bibtex-skip-to-valid-entry)
        (save-restriction
          (bibtex-narrow-to-entry)
          (let ((fnd (search-forward-regexp re (point-max) t)))
            (unless (or (and match fnd) (not (or match found)))
              (bibtex-kill-entry)
              (while (re-search-forward "\\n\\n+" nil t)
                (replace-match "\\n\\n" nil nil)))))))))
            
      
(setq bibtex-mode-hook
      '(lambda ()
         (define-key bibtex-mode-map "\C-\M-y"
           'BibTeX-paste-authors-changing-commas-to-ands)
         ))
