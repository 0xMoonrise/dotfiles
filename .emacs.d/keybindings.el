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
;; (global-set-key (kbd "C-c RET") 'completion-at-point)

(global-set-key (kbd "C-c 1") (lambda () (interactive) (my-insert-pair "()")))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (my-insert-pair "{}")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (my-insert-pair "[]")))

(provide 'keybindings)
;;; keybindings.el ends here
