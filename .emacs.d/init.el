;;; :init.el --- Main entry for Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs configuration entry point.
;;; Code:

;; --------------------------------------------------
;; Package system bootstrap
;; --------------------------------------------------

(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      load-prefer-newer t
      read-process-output-max (* 1024 1024))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "faces.el" user-emacs-directory))

(require 'config)
(require 'keybindings)

;;; init.el ends here
