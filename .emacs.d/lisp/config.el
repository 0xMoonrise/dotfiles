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
(setq-default indent-tabs-mode nil)

(setq scroll-step 1
      scroll-conservatively 10000
      indent-line-function #'indent-relative)

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
  :commands (cape-ispell)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

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
  (flycheck-indication-mode nil))

;; (use-package flycheck-eglot
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))

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

;; taken from https://olddeuteronomy.github.io/post/emacs-ibuffer-config/
(use-package ibuffer :ensure nil
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-title-face 'font-lock-doc-face)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("C++" (or
                   (mode . c++-mode)
                   (mode . c++-ts-mode)
                   (mode . c-mode)
                   (mode . c-ts-mode)
                   (mode . c-or-c++-ts-mode)))
           ("Python" (or
                      (mode . python-ts-mode)
                      (mode . c-mode)
                      (mode . python-mode)))
           ("Go" (or
                  (mode . go-mode)))
           ("Build" (or
                     (mode . make-mode)
                     (mode . makefile-gmake-mode)
                     (name . "^Makefile$")
                     (mode . change-log-mode)))
           ("Scripts" (or
                       (mode . shell-script-mode)
                       (mode . shell-mode)
                       (mode . sh-mode)
                       (mode . lua-mode)
                       (mode . bat-mode)))
           ("Config" (or
                      (mode . conf-mode)
                      (mode . conf-toml-mode)
                      (mode . toml-ts-mode)
                      (mode . conf-windows-mode)
                      (name . "^\\.clangd$")
                      (name . "^\\.gitignore$")
                      (name . "^Doxyfile$")
                      (name . "^config\\.toml$")
                      (mode . yaml-mode)))
           ("Web" (or
                   (mode . mhtml-mode)
                   (mode . html-mode)
                   (mode . web-mode)
                   (mode . nxml-mode)))
           ("CSS" (or
                   (mode . css-mode)
                   (mode . sass-mode)))
           ("JS" (or
                  (mode . js-mode)
                  (mode . rjsx-mode)))
           ("Markup" (or
                   (mode . markdown-mode)
                   (mode . adoc-mode)))
           ("Org" (mode . org-mode))
           ("LaTeX" (name . "\.tex$"))
           ("Magit" (or
                     (mode . magit-blame-mode)
                     (mode . magit-cherry-mode)
                     (mode . magit-diff-mode)
                     (mode . magit-log-mode)
                     (mode . magit-process-mode)
                     (mode . magit-status-mode)))
           ("Apps" (or
                    (mode . elfeed-search-mode)
                    (mode . elfeed-show-mode)))
           ("Fundamental" (or
                           (mode . fundamental-mode)
                           (mode . text-mode)))
           ("Emacs" (or
                     (mode . emacs-lisp-mode)
                     (name . "^\\*Help\\*$")
                     (name . "^\\*Custom.*")
                     (name . "^\\*Org Agenda\\*$")
                     (name . "^\\*info\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Backtrace\\*$")
                     (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")))
)

(use-package verb
  :ensure t
  :config
  (add-to-list 'org-src-lang-modes '("http" . verb)))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult)

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
