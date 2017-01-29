(defun defguard (guard)
  "Inserts guard header for C++"
  (interactive "sGuard name: ")
  (save-excursion
	(beginning-of-buffer)
	(insert (concat "#ifndef " (upcase guard) "_H\n#define " (upcase guard) "_H\n\n"))
	(end-of-buffer)
	(insert "\n#endif")
	(jump-to-register)))
