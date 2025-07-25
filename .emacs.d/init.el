;;; init.el --- Clean custom Emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; My Emacs configuration, focused on control, explicit actions, LSP, and minimal distractions.
;;; Code:

;; ----------------------------------------
;; Package System Setup
;; ----------------------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

;; ----------------------------------------
;; Basic UI Settings
;; ----------------------------------------
(global-display-line-numbers-mode t)
(electric-pair-mode 1)

;; ----------------------------------------
;; Backup and Auto-save Configuration
;; ----------------------------------------
(let ((auto-save-dir "/tmp/emacs-autosaves/"))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))
(setq backup-directory-alist `(("." . "/tmp/emacs-backups")))
(setq backup-by-copying t)
(setq vc-make-backup-files t)

;; ----------------------------------------
;; Editor Behavior
;; ----------------------------------------
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; ----------------------------------------
;; Helper Functions
;; ----------------------------------------
(defun reload-init-file ()
  "Reload this init.el."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun insertar-tab ()
  "Insert a literal tab."
  (interactive)
  (insert "\t"))

;; ----------------------------------------
;; Keybindings
;; ----------------------------------------

(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "M-d") 'xref-find-definitions)
(global-set-key (kbd "M-f") 'eglot-format-buffer)
(global-set-key (kbd "M-k") 'eldoc)
(global-set-key (kbd "M-r") 'xref-find-references)

(global-set-key (kbd "C-x a") (lambda () (interactive) (ibuffer nil "*Ibuffer*" '((not (name . ".*\\*.*"))))))
(global-set-key (kbd "C-x e") 'other-window)
(global-set-key (kbd "C-x r") 'xref-go-back)
(global-set-key (kbd "C-x f") 'lsp-find-implementation)
(global-set-key (kbd "C-x s") 'lsp-find-references)
(global-set-key (kbd "C-x c") (lambda () (interactive) (switch-to-buffer (other-buffer))))

(global-set-key (kbd "C-c TAB") 'insertar-tab)
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "C-c f") 'lsp-ui-find-workspace-symbol)

(global-set-key (kbd "C-<down>") 'forward-paragraph)
(global-set-key (kbd "C-<up>") 'backward-paragraph)
(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-n") 'isearch-repeat-forward)
(global-set-key (kbd "C-o") 'save-buffer)
(global-set-key (kbd "C-p") 'find-file)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-r") 'lsp-find-definition)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c RET") 'completion-at-point)

;; ----------------------------------------
;; Flycheck
;; ----------------------------------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; ----------------------------------------
;; Company Mode - Manual Completion Only
;; ----------------------------------------

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :bind (:map company-active-map
  ;;            ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection))
  :config
;;  (define-key company-mode-map (kbd "TAB") 'company-indent-or-complete-common)
  (define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common))

;; ----------------------------------------
;; LSP Configuration
;; ----------------------------------------
(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (python-mode . lsp-deferred))
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-session-file "~/.emacs.d/.lsp-session-v1")
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-render-documentation nil)
  :config
  (define-key lsp-mode-map (kbd "C-x RET") 'lsp-describe-thing-at-point))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-delay 0.1)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-enable nil))

(use-package lsp-treemacs
  :ensure t
  :after treemacs
  :commands lsp-treemacs-errors-list)

;; ----------------------------------------
;; Language Modes
;; ----------------------------------------

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . display-line-numbers-mode))

(use-package python-mode
  :ensure t)

;; ----------------------------------------
;; Org Mode
;; ----------------------------------------

;; ----------------------------------------
;; Custom Set Variables (generated by Custom)
;; ----------------------------------------
(custom-set-variables
 '(custom-safe-themes
   '("2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7"
     default))
 '(org-agenda-files '("~/Documents/file.org"))
 '(package-selected-packages
   '(company flycheck go-mode lsp-treemacs lsp-ui python-mode sublime-themes)))

;; ----------------------------------------
;; Themes
;; ----------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package sublime-themes
  :ensure t
  :config
  (load-theme 'spolsky t)
  (set-face-background 'vertical-border "gray20")
  (set-face-attribute 'default nil :background "black" :foreground "white"))

(provide 'init)
(put 'dired-find-alternate-file 'disabled nil)
;;; init.el ends here
