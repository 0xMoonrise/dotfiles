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
;; Themes
;; ----------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

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
(defun open-lsp-log-buffer ()
  "Open LSP log buffer in a pop-up window."
  (interactive)
  (let ((buf (get-buffer "*lsp-log*")))
    (if buf (display-buffer buf '(display-buffer-pop-up-window))
      (message "No LSP log buffer available"))))

(defun insertar-tab ()
  (interactive)
  (insert "\t"))

(defun cerrar-xref-buffer ()
  (interactive)
  (let ((xref-buf (get-buffer "*xref*")))
    (when xref-buf
      (let ((xref-win (get-buffer-window xref-buf 'visible)))
        (when xref-win (delete-window xref-win)))
      (kill-buffer xref-buf)
      (message "Buffer *xref* closed."))))

(defun my/ibuffer-no-system-buffers ()
  "Open ibuffer filtering out unwanted system buffers and hide header."
  (interactive)
  (let ((buf (get-buffer-create "*Ibuffer*")))
    (with-current-buffer buf
      (setq-local header-line-format nil))
    (ibuffer nil buf
             '((not (or
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*lsp-log\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*gopls.*\\*$")
                     (name . "^\\*Async-native.*\\*$")))))))
                     
;; ----------------------------------------
;; Keybindings
;; ----------------------------------------
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
(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-n") 'isearch-repeat-forward)
(global-set-key (kbd "C-o") 'save-buffer)
(global-set-key (kbd "C-p") 'find-file)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-r") #'lsp-find-definition)
(global-set-key (kbd "C-x r") #'xref-pop-marker-stack)
(global-set-key (kbd "C-<return>") 'completion-at-point)
(global-set-key (kbd "C-SPC") 'completion-at-point)
(global-set-key (kbd "C-c TAB") 'insertar-tab)
(global-set-key (kbd "C-c x") #'cerrar-xref-buffer)
(global-set-key (kbd "C-x a") #'my/ibuffer-no-system-buffers)

;; ----------------------------------------
;; Compatibility Functions / Declarations
;; ----------------------------------------
(unless (fboundp 'comment-line)
  (defun comment-line ()
    "Comment or uncomment current line."
    (interactive)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(declare-function eglot-format-buffer "eglot")
(declare-function lsp-find-definition "lsp-mode")
(declare-function xref-find-references "xref")
(declare-function xref-pop-marker-stack "xref")

;; ----------------------------------------
;; Flycheck
;; ----------------------------------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; ----------------------------------------
;; Company Mode
;; ----------------------------------------
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :bind (:map company-active-map
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection)))

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "TAB") 'company-indent-or-complete-common)
  (define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common))

;; ----------------------------------------
;; LSP Configuration
;; ----------------------------------------
(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp)
         (python-mode . lsp))
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable nil))

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
  :commands lsp-treemacs-errors-list)

;; ----------------------------------------
;; Language Modes
;; ----------------------------------------
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . display-line-numbers-mode))

;; Add python-mode here if needed.

;; ----------------------------------------
;; Yaml Mode Color Patch (for catppuccin theme users)
;; ----------------------------------------
(add-hook 'yaml-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-variable-name-face
                                     (list :foreground (catppuccin-get-color 'blue)))))

;; ----------------------------------------
;; Custom Set Variables (generated by Custom)
;; ----------------------------------------
(custom-set-variables
 '(package-selected-packages nil))

(custom-set-faces)

(provide 'init)
