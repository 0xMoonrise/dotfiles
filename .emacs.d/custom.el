;;; custom.el --- Core customization and packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; UI, setup helpers, package, modes, company, LSP, themes, etc.
;;; Code:

;; UI settings
(add-hook 'org-mode-hook 'org-indent-mode)
(global-display-line-numbers-mode t)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq backup-by-copying t)
(setq vc-make-backup-files t)


(require 'ispell)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Backup & autosave
(let ((auto-save-dir "/tmp/emacs-autosaves/"))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))
(setq backup-directory-alist `(("." . "/tmp/emacs-backups")))


;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  :config
  (corfu-popupinfo-mode 1)
  (let ((map corfu-map))
    (define-key map (kbd "TAB") 'corfu-next)
    (define-key map (kbd "S-TAB") 'corfu-previous)))

(when (not (display-graphic-p))
  (use-package corfu-terminal
    :ensure t
    :after corfu
    :config
    (corfu-terminal-mode +1)))

(setq tab-always-indent 'complete)
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
  (lsp-eldoc-enable-signature-help nil)
  (semantic-highlighting t))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-x RET")
              (lambda ()
                (interactive)
                (when (fboundp 'lsp-describe-thing-at-point)
                  (lsp-describe-thing-at-point)
                  (let ((help-window (get-buffer-window "*lsp-help*")))
                    (when help-window
                      (select-window help-window)))))))

(setq corfu-quit-at-boundary nil
      corfu-quit-no-match nil
      corfu-on-exact-match nil
      corfu-preselect 'prompt)

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
  (lsp-ui-doc-enable t))


(use-package lsp-treemacs
  :ensure t
  :after treemacs
  :commands lsp-treemacs-errors-list)

;; Language modes
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . display-line-numbers-mode)
  :config
  (define-key go-mode-map (kbd "C-c l") 'gofmt))

(use-package python-mode
  :ensure t)


(use-package magit
  :ensure t)

(use-package seq
  :ensure t)
;; Misc
(put 'dired-find-alternate-file 'disabled nil)

(use-package ibuffer
  :commands ibuffer
  :bind (:map ibuffer-mode-map
              ("C-x RET" . ibuffer-visit-buffer-other-window)
              ("p" . find-file)))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult
  :ensure t)


(provide 'custom)
;;; custom.el ends here
  
