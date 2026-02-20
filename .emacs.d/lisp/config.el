;;; config.el --- Core customization and packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; UI, setup helpers, package, modes, company, LSP, themes, etc.
;;; Code:

;; --------------------------------------------------
;; Core editor defaults
;; --------------------------------------------------

(require 'utils)

(setq-default
 fill-column 80
 indent-tabs-mode nil
 tab-width 2)

(electric-indent-mode t)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)

(setq scroll-step 1
      scroll-conservatively 10000)

(use-package exec-path-from-shell
  :ensure t
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

(savehist-mode 1)
(repeat-mode 1)

(use-package recentf
  :init (recentf-mode 1))

;; --------------------------------------------------
;; Theme / Appearance
;; --------------------------------------------------

(use-package sublime-themes
  :config
  (load-theme 'spolsky t)

  (set-face-attribute 'default nil
                      :background "black"
                      :foreground "white")

  (set-face-background 'vertical-border "gray20"))

;; --------------------------------------------------
;; Org mode
;; --------------------------------------------------

(use-package org
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
  :config
  (set-face-underline 'org-ellipsis nil)
  (let ((map org-mode-map))
    (define-key map (kbd "C-c a") 'org-insert-item)
    (define-key map (kbd "C-c s") 'org-insert-heading)
    (define-key map (kbd "C-c d") 'insert-org-date-with-brackets)
    (define-key map (kbd "C-c w") 'org-meta-return)
    (define-key map (kbd "C-l")   'org-insert-link)
    (define-key map (kbd "C-c RET") 'org-insert-entry)
		(define-key map (kbd "C-x RET") 'org-insert-task-with-id)
    (define-key map (kbd "C-j") 'completion-at-point)
    (define-key map (kbd "C-c f") 'org-mark-done-with-date)
    (define-key map (kbd "C-c c") 'org-archive-subtree)
    (define-key map (kbd "C-c 1") (lambda () (interactive) (org-surround "*")))
    (define-key map (kbd "C-c 2") (lambda () (interactive) (org-surround "_")))
    (define-key map (kbd "C-c 3") (lambda () (interactive) (org-surround "/")))))


;; --------------------------------------------------
;; Completion (Corfu / Cape)
;; --------------------------------------------------

(declare-function corfu-next "corfu")
(declare-function corfu-previous "corfu")

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-position 'point)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . corfu-insert))
  :hook (emacs-lisp-mode . corfu-mode))

(setq tab-always-indent 'complete)

(when (not (display-graphic-p))
  (use-package corfu-terminal
    :after corfu
    :config
    (corfu-terminal-mode +1)))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;; --------------------------------------------------
;; Diagnostics
;; --------------------------------------------------

(use-package flycheck
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-go-build-install-deps t)
  (flycheck-check-syntax-automatically '(save))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-error-list-minimum-level 'info)
  (flycheck-indication-mode nil))

;; (use-package flycheck-eglot
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))

;; --------------------------------------------------
;; LSP
;; --------------------------------------------------
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (js-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp))
  :commands lsp
  :init
  (setq gc-cons-threshold (* 200 1024 1024))
  :custom
  (lsp-completion-provider :none)
  (lsp-session-file "~/.emacs.d/.lsp-session-v1")
  (lsp-diagnostics-provider :flycheck)
  (lsp-diagnostic-clean-after-change t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-render-documentation nil)
  (lsp-signature-auto-activate nil)
  (lsp-eldoc-enable-signature-help nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-delay 0.1)
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-update-mode 'line))

;; (use-package treemacs)

;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)

;; --------------------------------------------------
;; Languages
;; --------------------------------------------------

(use-package go-mode
  :ensure t
  :hook ((before-save . gofmt-before-save))
  :config
  (setq tab-width 2))

(use-package python
  :ensure nil
  :hook (python-mode . (lambda ()
                         (local-set-key (kbd "TAB") #'indent-for-tab-command)
                         (setq indent-tabs-mode nil)
                         (setq tab-width 4)
                         (setq python-indent-offset 4))))

;; --------------------------------------------------
;; UI / buffers / tools
;; --------------------------------------------------

(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
        magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package ido
  :ensure nil
  :config
  (ido-mode 'buffers)
  (setq ido-enable-flex-matching t))

(use-package verb
  :ensure t
  :config
  (add-to-list 'org-src-lang-modes '("http" . verb)))

(use-package vertico
  :ensure t
  :config
  (setq vertico-count 10)
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)))

(setq compilation-scroll-output t)

(use-package yaml-mode)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-w" . dired-up-directory)))

(use-package drag-stuff
  :ensure t)

(use-package easy-kill
  :ensure t)
  
(provide 'config)

;;; config.el ends here
