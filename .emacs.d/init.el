;;; init.el --- Main entry for Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs configuration.
;;; Code:

(load (expand-file-name "custom.el" user-emacs-directory))
(load (expand-file-name "keybindings.el" user-emacs-directory))

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(eval-when-compile (require 'use-package))

(provide 'init)
;;; init.el ends here
