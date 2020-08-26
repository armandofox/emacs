(setq js2-mode-hook
      '(lambda ()
         (column-number-mode 1)
         (setq js-indent-level 2
               js2-auto-indent-p t
               js2-basic-offset 2
               js2-bounce-indent-p nil
               js2-cleanup-whitespace t
               js2-enter-indents-newline t
               js2-indent-on-enter-key t
               )
         (define-key js-mode-map "\C-c;" 'comment-region)
         ))
(setq js-mode-hook js2-mode-hook)
