;;; keybindings.el --- All keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Custom global keybindings.
;;; Code:

(require 'utils)

(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "M-d") 'xref-find-definitions)
(global-set-key (kbd "M-f") 'eglot-format-buffer)
(global-set-key (kbd "M-k") 'eldoc)
(global-set-key (kbd "M-r") 'xref-find-references)
(global-set-key (kbd "M-l") 'duplicate-dwim)


(global-set-key (kbd "C-x a") (lambda () (interactive) (ibuffer nil "*Ibuffer*" '((not (name . ".*\\*.*"))))))
(global-set-key (kbd "C-x e") 'other-window)
(global-set-key (kbd "C-x r") 'xref-go-back)
(global-set-key (kbd "C-x f") 'lsp-find-implementation)
(global-set-key (kbd "C-x s") 'lsp-find-references)
(global-set-key (kbd "C-x l") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-x q") 'delete-window)

;; (global-set-key (kbd "C-c c") 'completion-at-point)
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "C-c f") 'lsp-ui-find-workspace-symbol)
(global-set-key (kbd "C-c e") 'compile)
(global-set-key (kbd "C-c <right>") 'indent-rigidly-2-right)
(global-set-key (kbd "C-c <left>") 'indent-rigidly-2-left)
(global-set-key (kbd "C-c x") 'compile)
(global-set-key (kbd "C-c q") 'keyboard-escape-quit)


(global-set-key (kbd "C-c <left>")  #'windmove-left)
(global-set-key (kbd "C-c <right>") #'windmove-right)
(global-set-key (kbd "C-c <up>")    #'windmove-up)
(global-set-key (kbd "C-c <down>")  #'windmove-down)

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
(global-set-key (kbd "C-j") 'completion-at-point)

(global-set-key (kbd "C-c 1") (lambda () (interactive) (my-insert-pair "()")))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (my-insert-pair "{}")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (my-insert-pair "[]")))

(global-set-key (kbd "M-<up>")   #'drag-stuff-up)
(global-set-key (kbd "M-<down>") #'drag-stuff-down)

(provide 'keybindings)
;;; keybindings.el ends here
