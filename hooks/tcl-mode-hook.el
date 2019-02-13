;;; tcl mode customization

(setq tcl-mode-hook
      '(lambda ()
         (setq comment-start ";#")
         (define-key tcl-mode-map "\C-m" 'newline-and-indent)
         (auto-fill-mode 1)
         ))
