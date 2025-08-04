;;; keybindings.el --- All keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Custom global keybindings.
;;; Code:

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

(global-set-key (kbd "C-c TAB") 'insert-literal-tab)
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "C-c f") 'lsp-ui-find-workspace-symbol)
(global-set-key (kbd "C-c e") 'select-to-end-of-line)

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
(global-set-key (kbd "C-]") 'open-config-file)

(global-set-key (kbd "C-c 1") (lambda () (interactive) (my-insert-pair "()")))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (my-insert-pair "{}")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (my-insert-pair "[]")))

(eval-when-compile
  (require 'org))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a") 'org-insert-item)
  (define-key org-mode-map (kbd "C-c s") 'org-insert-heading)
  (define-key org-mode-map (kbd "C-c d") 'org-insert-timestamp)
  (define-key org-mode-map (kbd "C-c w") 'org-meta-return))

(add-hook 'org-mode-hook 'org-indent-mode)

(defun my/python-tab-complete-or-indent ()
  "Try completion, fall back to indent in real Python buffers."
  (interactive)
  (let ((completion-fn (run-hook-with-args-until-success 'completion-at-point-functions)))
    (if (and completion-fn (thing-at-point 'symbol))
        (completion-at-point)
      (indent-for-tab-command))))

(add-hook 'python-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode) ;; solo si es python-mode real
              (local-set-key (kbd "TAB") #'my/python-tab-complete-or-indent)
              (local-set-key (kbd "<tab>") #'my/python-tab-complete-or-indent))))

(defun my-insert-pair (pair)
  "Insert PAIR (a string of two chars) around region or at point."
  (interactive)
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

(defun reload-init-file ()
  "Reload the main init.el file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun select-to-end-of-line ()
  "Select from point to end of line."
  (interactive)
  (set-mark (point))
  (end-of-line))

(defun open-config-file ()
  "Open a buffer with one of the Emacs config files."
  (interactive)
  (let* ((config-files '(("init.el" . "~/.emacs.d/init.el")
                         ("custom.el" . "~/.emacs.d/custom.el")
                         ("keybindings.el" . "~/.emacs.d/keybindings.el")))
         (completion-extra-properties '(:annotation-function config-annot-fn))
         (choice (completing-read "Choose config file: " config-files))
         (file-path (expand-file-name (cdr (assoc choice config-files)))))
    (unless (file-exists-p file-path)
      (with-temp-buffer (write-file file-path)))
    (find-file file-path)))

(defun config-annot-fn (candidate)
  "Return aligned annotation for config file CANDIDATE."
  (let ((annotations '(("init.el" . "Main init file")
                       ("custom.el" . "User customizations")
                       ("keybindings.el" . "Custom keybindings")))
        (max-width 15))
    (let* ((desc (cdr (assoc candidate annotations)))
           (padding (make-string (- max-width (length candidate)) ?\s)))
      (concat padding desc))))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . c++-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)))
               ("LaTeX"
                (mode . latex-mode))
               ("Directories"
                (mode . dired-mode))
               ))))
(provide 'keybindings)
;;; keybindings.el ends here
