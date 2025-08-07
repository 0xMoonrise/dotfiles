;;; keybindings.el --- All keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Custom global keybindings.
;;; Code:

(require 'myfuncs)

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

(define-key org-mode-map (kbd "C-c a") 'org-insert-item)
(define-key org-mode-map (kbd "C-c s") 'org-insert-heading)
(define-key org-mode-map (kbd "C-c d") 'insert-org-date-with-brackets)
(define-key org-mode-map (kbd "C-c w") 'org-meta-return)
(define-key org-mode-map (kbd "C-l")   'org-insert-link)
(define-key org-mode-map (kbd "C-c t") 'org-insert-task-with-id)
(define-key org-mode-map (kbd "C-c f") 'org-mark-done-with-date)
(define-key org-mode-map (kbd "C-c c") 'org-archive-subtree)

(define-key org-mode-map (kbd "C-c 1") (lambda () (interactive) (org-surround "*")))
(define-key org-mode-map (kbd "C-c 2") (lambda () (interactive) (org-surround "_")))
(define-key org-mode-map (kbd "C-c 3") (lambda () (interactive) (org-surround "/")))

(provide 'keybindings)
;;; keybindings.el ends here
