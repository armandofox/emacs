(setq js2-mode-hook
      '(lambda ()
         (column-number-mode 1)
         (setq js-indent-level 2)
         (define-key js-mode-map "\C-c;" 'comment-region)
         ))
(setq js-mode-hook js2-mode-hook)
