;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff for shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq shell-mode-hook
      '(lambda ()
         (define-key shell-mode-map [up] 'comint-previous-input)
         (define-key shell-mode-map [up] 'comint-next-input)
         ))

         
