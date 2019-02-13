(setq feature-mode-hook
      '(lambda ()
         (electric-indent-mode -1)
         (local-set-key "\C-m" 'newline-and-indent)))
