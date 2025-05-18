;; -------------------------------
;; Package Initialization
;; -------------------------------
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; -------------------------------
;; Keybindings
;; -------------------------------

;; Basic keybindings
(global-set-key (kbd "M-.") 'find-file)
(global-set-key (kbd "M-/") 'comment-line)

(global-set-key (kbd "M-d") 'xref-find-definitions)
(global-set-key (kbd "M-f") 'eglot-format-buffer)
(global-set-key (kbd "M-k") 'eldoc)
(global-set-key (kbd "M-r") 'xref-find-references)

(global-set-key (kbd "C-<down>") 'forward-paragraph)
(global-set-key (kbd "C-<up>") 'backward-paragraph)
(global-set-key (kbd "C-S-<down>") 'mark-paragraph)
(global-set-key (kbd "C-S-<up>") 'mark-paragraph)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)


(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-n") 'isearch-repeat-forward)
(global-set-key (kbd "C-o") 'save-buffer)
(global-set-key (kbd "C-p") 'find-file)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)

;; LSP navigation keybindings
(global-set-key (kbd "C-r") #'lsp-find-definition)     ;; Go to definition
(global-set-key (kbd "C-l") #'xref-pop-marker-stack)   ;; Go back to previous location

;; Completion keybindings
(global-set-key (kbd "C-<return>") 'completion-at-point)
(global-set-key (kbd "C-SPC") 'completion-at-point)

;; LSP documentation
(global-set-key (kbd "C-c d") #'lsp-describe-thing-at-point)

;; -------------------------------
;; General Editing Settings
;; -------------------------------

(setq lsp-headerline-breadcrumb-enable nil)

;; Use flycheck as diagnostics provider for LSP
(setq lsp-diagnostics-provider :flycheck)

;; Set tab width to 4 spaces
(setq-default tab-width 4)

;; Use spaces instead of tab characters
(setq-default indent-tabs-mode nil)

;; Enable automatic pairing of brackets and quotes
(electric-pair-mode 1)

;; Compatibility: define comment-line if not available (older Emacs versions)
(unless (fboundp 'comment-line)
  (defun comment-line ()
    "Comment or uncomment current line."
    (interactive)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; -------------------------------
;; Company (Auto-completion)
;; -------------------------------

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay nil)                 ;; Manual completion trigger only
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
        ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection)
        ("<escape>" . company-abort)))

(with-eval-after-load 'company
  ;; Use TAB to indent or complete
  (define-key company-mode-map (kbd "TAB") 'company-indent-or-complete-common)
  (define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common))

(defun my-python-tab ()
  "Smart indent for Python: indent region or line with spaces."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-for-tab-command)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;; Usa espacios
            (setq tab-width 4)
            (setq python-indent-offset 4)
            (local-set-key (kbd "TAB") 'my-python-tab)))
;; -------------------------------
;; LSP Mode and UI Configuration
;; -------------------------------

(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp))                   ;; Enable LSP in Go files
  :commands lsp
  :custom
  (lsp-auto-guess-root t))                  ;; Auto-detect project root

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable t)                     ;; Enable inline documentation popup
  (lsp-ui-sideline-show-hover nil)           ;; Disable hover doc in sideline to reduce clutter
  (lsp-ui-sideline-show-diagnostics t)      ;; Show diagnostics in sideline
  (lsp-ui-sideline-delay 0.5))               ;; Quick sideline updates

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)        ;; View LSP errors in treemacs

;; -------------------------------
;; Flycheck (Syntax Checking)
;; -------------------------------

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))               ;; Enable flycheck globally

;; -------------------------------
;; Programming Language Modes
;; -------------------------------

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . display-line-numbers-mode)) ;; Show line numbers in Go files

;; -------------------------------
;; UI Settings
;; -------------------------------

;; Enable line numbers globally
(global-display-line-numbers-mode t)

;; -------------------------------
;; Backup and Autosave Files Settings
;; -------------------------------

(let ((auto-save-dir "/tmp/emacs-autosaves/"))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

(setq backup-directory-alist
      `(("." . "/tmp/emacs-backups")))
(setq backup-by-copying t)
(setq vc-make-backup-files t)

;; -------------------------------
;; Custom Variables (from customize)
;; -------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(dap-mode go-dlv lsp-python-ms flycheck lsp-ui lsp-treemacs go-mode company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
;(use-package lsp-python-ms
;  :ensure t
;  :init (setq lsp-python-ms-auto-install-server t)
;  :hook (python-mode . (lambda ()
;                          (require 'lsp-python-ms)
;                          (lsp))))  ; or lsp-deferred

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(setq lsp-signature-auto-activate nil)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-bright t))
