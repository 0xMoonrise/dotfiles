;;; config.el --- Core customization and packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; UI, setup helpers, package, modes, company, LSP, themes, etc.
;;; Code:

(eval-when-compile
  (setq byte-compile-warnings '(not free-vars unresolved)))

(require 'utils)
(require 'ansi-color)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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
;; LSP con Eglot
;; --------------------------------------------------
(use-package eglot
  :straight nil
  :hook ((go-mode python-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout 120)
  (eglot-sync-connect nil)
  (eglot-verbose nil)
  (eglot-ignored-server-capabilities '(:signatureHelpProvider))
  :config
  (add-to-list 'eglot-server-programs
               '(go-mode . ("gopls" "-v")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

;; --------------------------------------------------
;; Go
;; --------------------------------------------------
(use-package go-mode
  :hook (go-mode . (lambda ()
                     (setq-local tab-width 2
                                 indent-tabs-mode t)
                     (add-hook 'before-save-hook
                               #'eglot-format-buffer nil t))))

;; --------------------------------------------------
;; Python
;; --------------------------------------------------
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

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000
        vterm-buffer-name "terminal"))

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . (lambda ()
                     (setq ibuffer-filter-groups
                           (ibuffer-project-generate-filter-groups))
                     (setq ibuffer-filter-groups
                           (seq-remove
                            (lambda (group)
                              (string-match-p "^Directory:" (car group)))
                            ibuffer-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative)))))

(use-package magit
  :demand t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
        magit-bury-buffer-function    'magit-restore-window-configuration)
)

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
