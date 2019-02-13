;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Text mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq rc-file-mode-hook
      '(lambda ()
         (auto-fill-mode 0)
         (setq comment-start "# "
               comment-start-skip "^#\\s-*"
               )
         (define-key text-mode-map "\C-c;" 'comment-region)))

(setq longlines-mode-hook
      '(lambda ()
         (setq longlines-show-hard-newlines t)
))
(setq text-mode-hook
      '(lambda ()
         (define-key text-mode-map "\M-\t" 'set-tab-here)
         (setq indent-tabs-mode nil)
         (setq comment-indent-hook nil)
         (setq comment-start nil)
         (auto-fill-mode 1)
         (when (and (stringp (buffer-file-name))
                    (string-match "rc$" (buffer-file-name)))
           (run-hooks 'rc-file-mode-hook))
         ))

(setq makefile-mode-hook
      '(lambda ()
         (setq indent-tabs-mode t)
         (local-set-key "$" 'self-insert-command)
         (auto-fill-mode 0)
         ))

