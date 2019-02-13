;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  C mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'c-comment-box "/75*75*/")
(fset 'c-winged-comment  "	/*	 **/  ")
(fset 'c-comment-this-line   "\C-a/*\C-e */\C-a\C-n")
(fset 'c-basic-offset 2)

(defun toggle-winged-comment (pfx)
  "Enter or exit winged comment and set fill prefix accordingly."
  (interactive "P")
  (if (save-excursion (beginning-of-line)
                      (looking-at "[ 	]*/?\\*[ 	]*"))
      ;; inside a comment
      (if pfx
          (set-winged-comment-mode t)
        ;; else cancel comment mode and leave comment
        (when (search-forward "*/" nil t)
          (beginning-of-line)
          (set-winged-comment-mode nil)
          (forward-char)
          (insert ?\012)))
    ;; not inside a comment: start a new one
    (progn (execute-kbd-macro "	/*	 * */  .")
           (setq fill-prefix
                 (save-excursion (beginning-of-line)
                                 (let ((p (point)))
                                   (end-of-line)
                                   (buffer-substring p (point)))))
           (auto-fill-mode 1))))

(defun set-winged-comment-mode (arg)
  (if arg
      (save-excursion
        (beginning-of-line)
        (let ((p (point)) str)
          (when (looking-at "[ 	]*/?\\*[ 	]*")
            (setq fill-prefix
                  (buffer-substring (match-beginning 0) (match-end 0)))
            (auto-fill-mode 1))))
    (progn
      (setq fill-prefix nil)
      (auto-fill-mode 0))))

(defun c-fill-function ()
  "Reindent current function."
  (interactive)
  (save-excursion
    (mark-c-function)
    (indent-region (point) (mark) nil)))

(defun cancel-fill-prefix ()
  "Cancel existing fill prefix."
  (interactive)
  (setq fill-prefix nil)
  (message "Fill-prefix cancelled"))

(setq c-adaptive-fill-function
      '(lambda ()
         (save-excursion
           (beginning-of-line)
           (cond ((looking-at "\\s-*\\*+\\s-*")
                  (buffer-substring (match-beginning 0) (match-end 0)))
                 ((looking-at "\\(\\s-*\\)/\\(\\*+\\s-*\\)")
                  (concat (buffer-substring (match-beginning 1) (match-end 1))
                          " "
                          (buffer-substring (match-beginning 2) (match-end 2))))
                 (t nil)))))

(setq c-mode-hook
      '(lambda ()
         (auto-fill-mode 0)
         (setq indent-tabs-mode nil
               c-indent-level 4
               truncate-partial-width-windows t
               truncate-lines nil)
         (make-local-variable 'adaptive-fill-mode)
         (setq adaptive-fill-mode 1)
         (make-local-variable 'adaptive-fill-function)
         (setq adaptive-fill-function c-adaptive-fill-function)
         (setq fill-column 79)
         (define-key c-mode-map "\C-x;" 'c-winged-comment)
         (make-local-variable 'adaptive-fill-regexp)
         (setq adaptive-fill-regexp '())
         (define-key c-mode-map "\C-m" 'newline-and-indent)
         (define-key c-mode-map "\C-x*" 'c-comment-box)
         ;;(define-key c-mode-map "\C-x\C- " 'cancel-fill-prefix)
         (define-key c-mode-map "\C-x;" 'c-winged-comment)
         (define-key c-mode-map "\C-c;" 'c-comment-this-line)
         (define-key c-mode-map "\C-c<" 'beginning-of-defun)
         (define-key c-mode-map "\C-c>" 'end-of-defun)
         (define-key c-mode-map "\C-c(" 'blink-paren)
         (define-key c-mode-map "\M-q" 'c-fill-function)))
