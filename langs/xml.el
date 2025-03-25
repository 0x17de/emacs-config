(use-package nxml-mode
  :ensure nil
  :defer t
  :mode (("\\.xsd\\'" . nxml-mode)
         ("\\.xslt\\'" . nxml-mode))
  :hook (nxml-mode . (lambda ()
	               (make-local-variable 'indent-tabs-mode)
	               (setq indent-tabs-mode nil)
	               (add-to-list 'rng-schema-locating-files
			            "~/.emacs.d/nxml-schemas/schemas.xml"))))
