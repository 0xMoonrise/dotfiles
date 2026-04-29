;;; config.el --- Core customization and packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; UI, setup helpers, package, modes, company, LSP, themes, etc.
;;; Code:

;; --------------------------------------------------
;; Core editor defaults
;; --------------------------------------------------
(eval-when-compile
  (setq byte-compile-warnings '(not free-vars unresolved)))

(require 'utils)
(require 'ansi-color)

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 2)

(setq scroll-step 1
      scroll-conservatively 10000
      sentence-end-double-space nil
      create-lockfiles nil
      vc-follow-symlinks t
      eldoc-print-after-edit nil)

(set-default 'tab-always-indent 'complete)
(electric-pair-mode 1)
(electric-indent-mode 0)
(global-display-line-numbers-mode 1)
(savehist-mode 1)
(repeat-mode 1)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; --------------------------------------------------
;; Files, backups, history
;; --------------------------------------------------
(setq backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist `(("." . "/tmp/emacs-backups")))

(let ((auto-save-dir "/tmp/emacs-autosaves/"))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

(use-package recentf
  :straight nil
  :init (recentf-mode 1))

;; --------------------------------------------------
;; Theme / Appearance
;; --------------------------------------------------
(use-package sublime-themes
  :config
  (load-theme 'spolsky t)
  (set-face-background 'vertical-border "gray20")
  (set-face-attribute 'default nil
                      :background "black"
                      :foreground "white")
  (unless (display-graphic-p)
    (set-terminal-parameter nil 'background-mode 'dark)))

;; --------------------------------------------------
;; Org mode
;; --------------------------------------------------
(require 'org-tempo)

(use-package org
  :straight nil
  :custom
  (org-archive-location "./Archive/done.org::* Archived")
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-hide-block-startup t)
  (org-ellipsis " [...]")
  (org-ellipsis-with-spaces t)
  (org-ellipsis-after-blank-lines 1)
  (org-startup-indented t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-align-all-tables t)
  (org-support-shift-select t)
  :config
  (set-face-underline 'org-ellipsis nil))

;; --------------------------------------------------
;; Completion (Vertico / Consult / Corfu / Cape)
;; --------------------------------------------------
(use-package vertico
  :config
  (setq vertico-count 10)
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :config
  (setq consult-buffer-filter
        (append consult-buffer-filter
                '("\\*.*\\*"))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (setq tab-always-indent 'complete)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-position 'point)
  :hook ((eldoc-mode . corfu-mode))
  :bind
  (:map corfu-map
        ("TAB"     . corfu-next)
        ([tab]     . corfu-next)
        ("S-TAB"   . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET"     . corfu-insert)))

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (funcall #'corfu-terminal-mode +1)))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t))

(use-package yasnippet
  :init
  (yas-global-mode 1))

;; --------------------------------------------------
;; Diagnostics
;; --------------------------------------------------
(use-package flycheck
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode nil)
  (flycheck-checkers '(go-golangci-lint))
  :config
  (add-to-list 'flycheck-disabled-checkers 'go-build)
  (add-to-list 'flycheck-disabled-checkers 'go-vet)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-to-list 'flycheck-disabled-checkers 'python-pyright)
  (add-to-list 'flycheck-disabled-checkers 'org-lint))

;; --------------------------------------------------
;; Eglot (Go, Python)
;; --------------------------------------------------
(use-package eglot
  :straight nil
  :hook ((python-mode . eglot-ensure)
         (go-mode     . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout 120)
  (eglot-sync-connect nil)
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local eldoc-documentation-functions
                        (list #'eglot-hover-eldoc-function))
            (flymake-mode 1)
            (setq-local flymake-no-changes-timeout 0.5)
            (add-hook 'after-save-hook #'flymake-start nil t)))

(advice-add 'eglot-hover-eldoc-function :around
            (lambda (orig &rest args)
              (when (eglot-current-server)
                (apply orig args))))

(use-package consult-eglot
  :after (consult eglot))

;; --------------------------------------------------
;; Languages
;; --------------------------------------------------
(use-package go-mode
  :hook (go-mode . (lambda ()
                     (setq-local tab-width 2
                                 indent-tabs-mode t)
                     (add-hook 'before-save-hook #'gofmt-before-save nil t))))

(use-package python
  :straight nil
  :hook (python-mode . (lambda ()
                         (setq indent-tabs-mode nil
                               tab-width 4
                               python-indent-offset 4))))

(use-package yaml-mode)

;; --------------------------------------------------
;; UI / buffers / tools
;; --------------------------------------------------
(use-package magit
  :demand t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
        magit-bury-buffer-function    'magit-restore-window-configuration)
  :bind (:map magit-status-mode-map
              ("C-c d" . my/magit-copy-diff)))

(use-package ido
  :straight nil
  :config
  (ido-mode 'buffers)
  (setq ido-enable-flex-matching t))

(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("C-w" . dired-up-directory)))

(use-package verb
  :config
  (add-to-list 'org-src-lang-modes '("http" . verb)))

(use-package arduino-mode
  :straight t
  :mode ("\\.ino\\'" . arduino-mode))

(use-package drag-stuff)
(use-package easy-kill)

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)))

(setq compilation-scroll-output t)

;; --------------------------------------------------
;; Custom
;; --------------------------------------------------
(load-file "~/.emacs.d/lisp/custom/nmap-mode.el")
(load-file "~/.emacs.d/lisp/custom/command-mode.el")

(provide 'config)

;;; config.el ends here
