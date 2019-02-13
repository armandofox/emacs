;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Hooks for Lisp and Elisp modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lisp-indent-defun ()
  "Reindent current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (point) (mark) nil)))

(defun lisp-set-indentation ()
  (setq indent-tabs-mode nil)
  (setq tab-width 8)
  (put 'if 'lisp-indent-hook nil)
  (put 'let 'lisp-indent-hook 1)
  (put 'do 'lisp-indent-hook 1)
  (put 'dolist 'lisp-indent-hook 1)
  (put 'dotimes 'lisp-indent-hook 1))

(defun lisp-customize (keymap)
  (define-key keymap "\C-m" 'newline-and-indent)
  (define-key keymap "\C-ce" 'eval-defun)
  (define-key keymap "\C-c\C-e" 'eval-current-buffer)
  (define-key keymap "\M-q" 'lisp-indent-defun)
  (lisp-set-indentation)
  (setq fill-column 79)
  (auto-fill-mode 1))

(setq lisp-mode-hook
      '(lambda () (lisp-customize lisp-mode-map)))
                 
(setq emacs-lisp-mode-hook
      '(lambda () (lisp-customize emacs-lisp-mode-map)))
