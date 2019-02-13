;; (require 'cl)
;; (setq ruby-mode-hook
;;       '(lambda ()
;;          (local-set-key "\C-m" 'reindent-then-newline-and-indent)
;;          (auto-fill-mode 0)
;;          (define-key ruby-mode-map "\C-c;" 'comment-region)
;;          ))

(require 'cl)

(add-hook 'ruby-mode-hook
      '(lambda ()
         (load-library "ruby-electric")
         (setq ruby-electric-expand-delimiters-list '())
         (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)
         (define-key ruby-mode-map "\C-c;" 'comment-region)
         (make-local-variable 'font-lock-defaults)
         (make-local-variable 'font-lock-keywords)
         (make-local-variable 'font-lock-syntax-table)
         (make-local-variable 'font-lock-syntactic-keywords)
         (setq font-lock-defaults
               '((ruby-font-lock-keywords)
                 nil nil))
         (setq font-lock-keywords ruby-font-lock-keywords)
         (setq font-lock-syntax-table ruby-font-lock-syntax-table)
         (setq font-lock-syntactic-keywords ruby-font-lock-syntactic-keywords)
         (setq ruby-deep-indent-paren nil)
         ;; override ruby-indent-command to provide an indent hook to do
         ;; various electric things before indenting the line
         (define-key ruby-mode-map "\t"
           '(lambda ()
              (interactive)
              (ruby-custom-indent-hook)
              (ruby-indent-line)))
))

(defun ruby-custom-indent-hook ()
  (save-excursion
    (cond ((looking-back "do\\(\\s-*|[^|]+|\\s-*\\)?")
           (end-of-line)
           (open-line 1)
           (forward-line 1)
           (insert "end")
           (ruby-indent-line t)
           nil))))

(defun hamlize-buffer ()
  (interactive)
  "Convert some elements of RHTML to HAML equivalents"
  (save-excursion
    (goto-char (point-min))
    (let ((tag-open "<\\(\\sw+\\)")
          (tag-attr "\\(\\sw+\\)\\s-*=\\s-*['\"]?\\(\\S-*\\)['\"]"))
      (replace-regexp (concat tag-open "\\(/\\)?>")
                      (concat "%" (match-string 1) (match-string 2)))
      (replace-regexp (concat tag-open "\\s-+" tag-attr "\\s-*>")
                      (concat "%" (match-string 1) "{:"
                              (match-string 2) " => :" (match-string 3) "}"))
      (replace-regexp (concat tag-open "\\s-+" tag-attr "\\s-+" tag-attr "\\s-*>")
                      (concat "%" (match-string 1) "{:"
                              (match-string 2) " => :" (match-string 3) ", :"
                              (match-string 4) " => :" (match-string 5) "}"))
      (replace-regexp ("<%=" "="))
      (replace-regexp ("%>" ""))
      (replace-regexp ("<%" "-"))
      (replace-regexp (">%" "")))))



