;;; custom.el --- Core customization and packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; UI, package setup, helpers, modes, company, LSP, themes, etc.
;;; Code:

;; UI settings
(global-display-line-numbers-mode t)
(electric-pair-mode 1)
(setq scroll-conservatively 101
      scroll-margin 3
      scroll-step 1
      next-screen-context-lines 5
      scroll-preserve-screen-position t)

;; Backup & autosave
(let ((auto-save-dir "/tmp/emacs-autosaves/"))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))
(setq backup-directory-alist `(("." . "/tmp/emacs-backups")))
(setq backup-by-copying t)
(setq vc-make-backup-files t)

;; Editor behavior
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Helper functions
(defun reload-init-file ()
  "Reload the main init.el file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun insert-literal-tab ()
  "Insert a literal tab character."
  (interactive)
  (insert "\t"))

(defun my-insert-pair (pair)
  "Insert PAIR (a string of two chars) around region or at point."
  (interactive "sPair (e.g. {}, (), []): ")
  (let ((open (substring pair 0 1))
        (close (substring pair 1 2)))
    (if (region-active-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (insert close)
            (goto-char beg)
            (insert open)))
      (insert open close)
      (backward-char 1))))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Company
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :bind (:map company-active-map
              ("TAB" . company-complete-selection))
  :config
  (define-key company-mode-map (kbd "TAB") 'company-indent-or-complete-common))

;; LSP
(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (python-mode . lsp-deferred))
  :commands lsp
  :custom
  (lsp-completion-provider :none)
  (lsp-session-file "~/.emacs.d/.lsp-session-v1")
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-render-documentation nil)
  (lsp-signature-auto-activate nil)
  (lsp-eldoc-enable-signature-help nil))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-x RET")
    (lambda ()
      (interactive)
      (when (fboundp 'lsp-describe-thing-at-point)
        (lsp-describe-thing-at-point)
        (let ((help-window (get-buffer-window "*lsp-help*")))
          (when help-window
            (select-window help-window)))))))

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

;; Language modes
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . display-line-numbers-mode))

(use-package python-mode
  :ensure t)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package sublime-themes
  :ensure t
  :config
  (load-theme 'spolsky t)
  (set-face-background 'vertical-border "gray20")
  (set-face-attribute 'default nil :background "black" :foreground "white"))

;; Misc
(put 'dired-find-alternate-file 'disabled nil)

(provide 'custom)
;;; custom.el ends here
