;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
(add-hook 'nxml-mode-hook
	  #'(lambda ()
	      (make-local-variable 'indent-tabs-mode)
	      (setq indent-tabs-mode nil)
	      (add-to-list 'rng-schema-locating-files
			   "~/.emacs.d/nxml-schemas/schemas.xml")))
