;;; :init.el --- Main entry for Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs configuration entry point.
;;; Code:

;; --------------------------------------------------
;; Package system bootstrap
;; --------------------------------------------------

(setq load-prefer-newer t
      read-process-output-max (* 1024 1024))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         (or (bound-and-true-p straight-base-dir)
                             user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-demand nil)   

(require 'keybindings)
(setq custom-file (expand-file-name "faces.el" user-emacs-directory))
(require 'config)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
