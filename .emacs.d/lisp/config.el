;;; config.el --- Core customization and packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; UI, setup helpers, package, modes, company, LSP, themes, etc.
;;; Code:

;; --------------------------------------------------
;; Core editor defaults
;; --------------------------------------------------

(require 'utils)

(setq-default electric-indent-mode t
              fill-column 80
              indent-tabs-mode nil
              tab-width 2)

(electric-pair-mode 1)
(global-display-line-numbers-mode 1)

(setq scroll-step 1
      scroll-conservatively 10000
      indent-line-function #'indent-relative)

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
  :hook (org-mode . (lambda ()
                      (setq indent-tabs-mode nil)))
  :custom
  (org-archive-location "./Archive/done.org::* Archived")
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-hide-block-startup t)
  (org-ellipsis " [...]")
  (org-ellipsis-with-spaces t)
  (org-ellipsis-after-blank-lines 1)
  :config
  (set-face-underline 'org-ellipsis nil)
  (let ((map org-mode-map))
    (define-key map (kbd "C-c a") 'org-insert-item)
    (define-key map (kbd "C-c s") 'org-insert-heading)
    (define-key map (kbd "C-c d") 'insert-org-date-with-brackets)
    (define-key map (kbd "C-c w") 'org-meta-return)
    (define-key map (kbd "C-l")   'org-insert-link)
    (define-key map (kbd "C-c t") 'org-insert-task-with-id)
    (define-key map (kbd "C-c f") 'org-mark-done-with-date)
    (define-key map (kbd "C-c c") 'org-archive-subtree)
    (define-key map (kbd "C-c 1") (lambda () (interactive) (org-surround "*")))
    (define-key map (kbd "C-c 2") (lambda () (interactive) (org-surround "_")))
    (define-key map (kbd "C-c 3") (lambda () (interactive) (org-surround "/")))))


;; --------------------------------------------------
;; Spell checking
;; --------------------------------------------------

(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "en")
  (ispell-extra-args '("--sug-mode=ultra")))

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
  :config
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map (kbd "<tab>") #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") #'corfu-previous))

(when (not (display-graphic-p))
  (use-package corfu-terminal
    :after corfu
    :config
    (corfu-terminal-mode +1)))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; --------------------------------------------------
;; Diagnostics
;; --------------------------------------------------

(use-package flycheck
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

;; --------------------------------------------------
;; LSP
;; --------------------------------------------------

(use-package lsp-mode
  :hook ((go-mode python-mode sql-mode) . lsp-deferred)
  :commands lsp
  :init
  (setq gc-cons-threshold (* 200 1024 1024))
  :custom
  (lsp-completion-provider :none)
  (lsp-session-file "~/.emacs.d/.lsp-session-v1")
  (lsp-diagnostics-provider :flycheck)
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
  (lsp-ui-doc-enable t))

(use-package treemacs)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; --------------------------------------------------
;; Languages
;; --------------------------------------------------

(use-package go-mode
  :hook (go-mode . display-line-numbers-mode)
  :config
  (define-key go-mode-map (kbd "C-c C-f") #'gofmt))

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

(use-package magit)

(use-package ibuffer
  :commands ibuffer
  :bind (:map ibuffer-mode-map
              ("C-x RET" . ibuffer-visit-buffer-other-window)
              ("p" . find-file)))

(use-package vertico
  :init (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult)

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)))

(setq compilation-scroll-output t)

(use-package yasnippet
  :commands yas-global-mode
  :init
  (yas-global-mode 1))

(provide 'config)

;;; config.el ends here
