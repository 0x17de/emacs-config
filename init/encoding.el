;; -*- lexical-binding: t; -*-
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))
