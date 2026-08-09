;;; init.el --- Main entry for Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs configuration entry point.
;;; Code:

;; --------------------------------------------------
;; 1. Basic Settings
;; --------------------------------------------------

(setq load-prefer-newer t
      read-process-output-max (* 1024 1024))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; --------------------------------------------------
;; 2. Package Manager (Straight.el)
;; --------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
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

;; --------------------------------------------------
;; 3. Custom File
;; --------------------------------------------------

(setq custom-file (expand-file-name "faces.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; --------------------------------------------------
;; 4. Editor Behavior
;; --------------------------------------------------

;; Text formatting
(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 2)

;; Scrolling and navigation
(setq scroll-step 1
      scroll-conservatively 10000
      sentence-end-double-space nil)

;; File handling
(setq create-lockfiles nil
      vc-follow-symlinks t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist `(("." . "/tmp/emacs-backups")))

;; Auto-save directory
(let ((auto-save-dir "/tmp/emacs-autosaves/"))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;; --------------------------------------------------
;; 5. UI & Visual Settings
;; --------------------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)

;; --------------------------------------------------
;; 6. Minor Modes
;; --------------------------------------------------

(set-default 'tab-always-indent 'complete)
(electric-pair-mode 1)
(electric-indent-mode -1)
(savehist-mode 1)
(repeat-mode 1)

;; --------------------------------------------------
;; 7. Hooks
;; --------------------------------------------------

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; --------------------------------------------------
;; 8. Load Configuration Modules
;; --------------------------------------------------

(require 'keybindings)
(require 'config)

;; --------------------------------------------------
;; 9. Misc Settings
;; --------------------------------------------------

;; Enable 'dired-find-alternate-file' without confirmation
(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here
