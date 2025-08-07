;;; init.el --- Main entry for Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs configuration.
;;; Code:

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless package--initialized
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'org)
(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)

(require 'use-package)
(setq use-package-always-ensure t)
(require 'treemacs)

(setq load-prefer-newer t)
(setq read-process-output-max (* 1024 1024))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'keybindings)

(load "~/.emacs.d/lisp/config.el")
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(use-package sublime-themes
  :config
  (load-theme 'spolsky t)
  (set-face-attribute 'default nil :background "black" :foreground "white")
  (set-face-background 'vertical-border "gray20"))

(setq custom-file (expand-file-name "faces.el" user-emacs-directory))
;;; init.el ends here
