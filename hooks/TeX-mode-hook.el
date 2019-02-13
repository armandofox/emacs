;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Customizations for TeX/LaTeX mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tex-default-mode 'latex-mode
      TeX-default-mode 'latex-mode
      tex-command "latex")

(defun tex-insert-environment (env &optional asterisk-p)
  (interactive "sEnvironment name: \nP")
  (save-excursion
    (save-restriction
      ;; (narrow-to-region (region-beginning) (region-end))
      ;; (goto-char (point-min))
      (insert "\\begin{" env (if asterisk-p "*" "") "}\n\n")
      ;; (goto-char (point-max))
      (insert "\n\\end{" env "}")))
  (next-line 2))

(defun tex-begin-block (blk)
  (interactive "sBlock name: ")
  (insert "\\" blk "{}\n")
  (previous-line 1)
  (end-of-line)
  (backward-char 1))

(defun tex-begin-cite (blk)
  (interactive "sCitation environment: ")
  (insert "~\\" blk "{}")
  (backward-char 1))

(defun forward-subsection ()
  "Move point to beginning of next (sub)section."
  (interactive)
  (re-search-forward "\\\(sub\\)*section.*$"))

(defun backward-subsection ()
  "Move point to beginning of previous (sub)section."
  (interactive)
  (re-search-backward "\\\(sub\\)*section.*$"))

(defun indent-paragraph (n)
  (interactive "n")
  (save-excursion
    (mark-paragraph)
    (indent-rigidly (point) (mark) (if (= n 0) 2 n))))

(add-hook 'tex-mode-hook 
          (lambda ()
         (setq comment-start "% "
               comment-start-skip "%\\s-+"
               comment-end ""
               comment-indent-function '(lambda () 0))
         (auto-fill-mode 1)
         (define-key tex-mode-map "\C-c;" 'comment-region)
         (define-key tex-mode-map "\C-c\C-b" 'my-tex-buffer)))

(add-hook 'latex-mode-hook
          (lambda ()
            (when (featurep 'hilit19)
              (hilit-add-pattern "^\\s-*\\chapter" "$" 'blue-bold))
            (define-key tex-mode-map "\M-/" '(lambda () (interactive) (insert "\\slash{}")))
            (define-key tex-mode-map "\C-\M-n" 'forward-subsection)
            (define-key tex-mode-map "\C-\M-p" 'backward-subsection)
            (define-key tex-mode-map "\C-c{" 'tex-insert-environment)
            (define-key tex-mode-map "\C-c\C-c"
              '(lambda () (interactive) (tex-begin-cite "cite")))
            (define-key tex-mode-map "\C-c\C-r"
              '(lambda () (interactive) (tex-begin-cite "ref")))
            (define-key tex-mode-map "\C-cc" 
              '(lambda () (interactive) (tex-begin-block "chapter")))
            (define-key tex-mode-map "\C-cc" 
              '(lambda () (interactive) (tex-begin-block "chapter")))
            (define-key tex-mode-map "\C-cs" 
              '(lambda () (interactive) (tex-begin-block "section")))
            (define-key tex-mode-map "\C-\M-i" 
              '(lambda () (interactive) (tex-begin-block "textit")))
            (define-key tex-mode-map "\C-\M-e" 
              '(lambda () (interactive) (tex-begin-block "textem")))
            (define-key tex-mode-map "\C-\M-b" 
              '(lambda () (interactive) (tex-begin-block "textbf")))
            (define-key tex-mode-map "\C-\M-r" 
              '(lambda () (interactive) (tex-begin-block "textrm")))
            (define-key tex-mode-map "\C-cs" 
              '(lambda () (interactive) (tex-begin-block "section")))
            (define-key tex-mode-map "\C-cu" 
              '(lambda () (interactive) (tex-begin-block "subsection")))
            (define-key tex-mode-map "\C-cb" 
              '(lambda () (interactive) (tex-insert-environment "sidebar")))
            (define-key tex-mode-map "\C-ci" 
              '(lambda () (interactive) (tex-insert-environment "itemize")))
            (define-key tex-mode-map "\C-cn" 
              '(lambda () (interactive) (tex-insert-environment "enumerate")))
            (define-key tex-mode-map "\C-cq" 
              '(lambda () (interactive) (tex-insert-environment "quotation")))
            (define-key tex-mode-map "\C-ce" 
              '(lambda () (interactive) (tex-insert-environment "elaboration")))
            (define-key tex-mode-map "\C-ck" 
              '(lambda () (interactive) (tex-insert-environment "checkyourself")))
            (define-key tex-mode-map "\C-ca" 
              '(lambda () (interactive) (tex-insert-environment "answer")))
            (define-key tex-mode-map "\C-cE" 
              '(lambda () (interactive) (tex-insert-environment "eqnarray")))
            (define-key tex-mode-map "\C-ct" 
              '(lambda () (interactive) (tex-insert-environment "tabular")))
            (define-key tex-mode-map "\C-cf" 
              '(lambda () (interactive) (tex-insert-environment "figure")))
            ;; (define-key tex-mode-map "\C-cv" 
            ;;   '(lambda () (interactive) (tex-insert-environment "verbatim")))
            ;; (define-key tex-mode-map "\C-cl" 
            ;;   '(lambda () (interactive) (tex-insert-environment "lstlisting")))
            ;; (define-key tex-mode-map "\C-\M-l" 
            ;;   '(lambda () (interactive) (tex-begin-block "lstinputlisting")))
            (define-key tex-mode-map "\C-cp" 
              '(lambda () (interactive) (tex-insert-environment "pitfall")))
            (define-key tex-mode-map "\C-cy" 
              '(lambda () (interactive) (tex-insert-environment "fallacy")))
            (define-key tex-mode-map "\C-cx" 
              '(lambda () (interactive) (tex-insert-environment "exercise")))
            (define-key tex-mode-map "\C-\\"
              '(lambda () (interactive) (insert "\\/")))
            (define-key tex-mode-map "\C-cr" 
              '(lambda () (interactive) (tex-insert-environment "verse")))
            (define-key tex-mode-map "\t" 'tab-to-tab-stop) 
            (make-local-variable 'outline-regexp)
            ;;(setq outline-regexp "(sub)*section")
            ))

(setq bibtex-mode-hook
      '(lambda ()
         (define-key bibtex-mode-map "\t" 'bibtex-next-field)
         (setq comment-start "^% "
               comment-start-skip "^%\\s-*"
               comment-end ""
               comment-column 0)
         ))
