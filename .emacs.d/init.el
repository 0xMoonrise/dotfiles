;;; init.el --- Main entry for Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs configuration.
;;; Code:
(require 'package)

(load (expand-file-name "custom.el" user-emacs-directory))
(load (expand-file-name "keybindings.el" user-emacs-directory))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company flycheck go-mode lsp-mode lsp-treemacs lsp-ui python-mode
             sublime-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
