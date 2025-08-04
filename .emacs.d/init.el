;; init.el --- Main entry for Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs configuration.
;;; Code:
(require 'package)

(load (expand-file-name "custom.el" user-emacs-directory))
(load (expand-file-name "keybindings.el" user-emacs-directory))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

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
   '(cape consult corfu corfu-popupinfo corfu-terminal flycheck
          go-mode lsp-treemacs lsp-ui marginalia orderless python-mode
          pyvenv seq sublime-themes vertico)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package sublime-themes
  :ensure t
  :config
  (load-theme 'spolsky t)
  (set-face-attribute 'default nil :background "black" :foreground "white")
  (set-face-background 'vertical-border "gray20"))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
