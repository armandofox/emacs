;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Autoload some functions when the corresponding m-X command is invoked.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'tar-mode "tar-mode" "Major mode for viewing a tar file." t)
;;;(autoload 'js2-mode "js2-mode" "Major mode for JavaScript." t)
(autoload 'coffee-mode "coffee-mode" "Major mode for CoffeeScript" t)
(autoload 'haml-mode "haml-mode")
(autoload 'feature-mode "feature-mode")

(defun rhtml-mode ()
  "Turn on rhtml-minor-mode in two-mode-mode"
  (interactive)
  (require 'two-mode-mode)
  (require 'rhtml-minor-mode)
  (two-mode-mode)
  (rhtml-minor-mode)
  (auto-fill-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Specify auto-moding for nonstandard file extensions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append auto-mode-alist
             (list '("\\.[ly]$" . c-mode) ;for lex/yacc input
                   '("\\.js$" . js-mode)
                   '("\\.coffee$" . coffee-mode)
                   '("\\.feature$" . feature-mode)
                   '("\\.rxml" . sgml-mode)
                   '("\\.haml$" . haml-mode)
                   '("\\.p[lm]$" . perl-mode)
                   '("\\.tgz" . tar-mode)
                   '("\\.tar.gz" . tar-mode)
                   '("\\.html$". html-mode)
                   '("\\.rhtml$". rhtml-mode)
                   '("\\.html\\.erb$" . rhtml-mode)
                   '("\\.ruby$" . ruby-mode)
                   '("\\.rb$" . ruby-mode)
                   '("\\.rake$" . ruby-mode)
                   '("\\(Gem\\|Rake\\|Cap\\)file$" . ruby-mode)
                   )
             ))

