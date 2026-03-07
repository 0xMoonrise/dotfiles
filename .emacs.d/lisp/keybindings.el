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
(global-set-key (kbd "C-x f") 'eglot-find-implementation)
(global-set-key (kbd "C-x s") 'xref-find-references)
(global-set-key (kbd "C-x l") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-x q") 'delete-window)
(global-set-key (kbd "C-x c") 'my/org-src-block-copy-osc52)

(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-c a") 'consult-buffer)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "M-g g") 'consult-goto-line)

(global-set-key (kbd "C-c c") 'my/copy-region-to-clipboard-osc52)
(global-set-key (kbd "C-c g") 'my/dlv-breakpoint)
(global-set-key (kbd "C-c P") 'flymake-show-project-diagnostics)
(global-set-key (kbd "C-c r") 'reload-init-file)
(global-set-key (kbd "C-c f") 'consult-eglot-symbols)
(global-set-key (kbd "C-c p") 'consult-flymake)
(global-set-key (kbd "C-c e") 'compile)
(global-set-key (kbd "C-c x") 'compile)
(global-set-key (kbd "C-c q") 'keyboard-escape-quit)
(global-set-key (kbd "C-c ]") 'my/log-entry)
(global-set-key (kbd "C-c [") 'my/open-daily-log)
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
(global-set-key (kbd "C-r") 'xref-find-definitions)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-]") 'open-config-file)

(global-set-key (kbd "C-c 1") (lambda () (interactive) (my-insert-pair "()")))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (my-insert-pair "{}")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (my-insert-pair "[]")))

(global-set-key (kbd "M-<up>")   'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)

;; ── Org-mode keybindings ──────────────────────────────────────────────────────
(with-eval-after-load 'org
  (let ((map org-mode-map))
    (define-key map (kbd "C-c i") 'org-insert-item)
    (define-key map (kbd "C-c s") 'org-insert-heading)
    (define-key map (kbd "C-c d") 'insert-org-date-with-brackets)
    (define-key map (kbd "C-c w") 'org-meta-return)
    (define-key map (kbd "C-l")   'org-insert-link)
    (define-key map (kbd "C-c RET") 'org-insert-entry)
    (define-key map (kbd "C-x RET") 'org-insert-task-with-id)
    (define-key map (kbd "C-j") 'completion-at-point)
    (define-key map (kbd "C-c f") 'org-mark-done-with-date)
    (define-key map (kbd "C-c 1") (lambda () (interactive) (org-surround "*")))
    (define-key map (kbd "C-c 2") (lambda () (interactive) (org-surround "_")))
    (define-key map (kbd "C-c 3") (lambda () (interactive) (org-surround "/")))))

(provide 'keybindings)
;;; keybindings.el ends here
