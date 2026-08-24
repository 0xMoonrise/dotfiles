;;; keybindings.el --- All keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Custom global keybindings.
;;; Code:
(require 'utils)
(require 'cl-lib)

(defmacro my/bind-keys (keymap &rest bindings)
  "Bind BINDINGS (KEY . COMMAND pairs) in KEYMAP.
KEYMAP can be a keymap symbol (e.g. `org-mode-map') or `global-map'.
When KEYMAP is a mode-specific map (ending in -map), automatically
wraps in `with-eval-after-load' for the corresponding feature."
  (declare (indent 1))
  (let* ((map-sym (if (eq keymap 'global-map) 'global-map keymap))
         (bind-forms
          (cl-loop for (key . cmd) in bindings
                   collect `(define-key ,map-sym (kbd ,key)
                              ,(if (symbolp cmd) `#',cmd cmd)))))
    (if (or (eq keymap 'global-map)
            (not (string-match "-\\(?:mode-\\)?map\\'" (symbol-name keymap))))
        `(progn ,@bind-forms)
      (let ((feature-name
             (intern (replace-regexp-in-string
                      "-\\(?:mode-\\)?map\\'" ""
                      (symbol-name keymap)))))
        `(with-eval-after-load ',feature-name
           ,@bind-forms)))))

(global-unset-key (kbd "C-d"))

(my/bind-keys global-map
  ("M-/" . comment-line)
  ("M-d" . xref-find-definitions)
  ("M-f" . eglot-format-buffer)
  ("M-k" . eldoc)
  ("M-r" . xref-find-references)
  ("M-l" . duplicate-dwim)
  ("M-y" . consult-yank-pop)

  ("M-<up>"   . drag-stuff-up)
  ("M-<down>" . drag-stuff-down)

  ("C-s" . consult-line)
  ("C-l" . my/jump-toggle)
  ("C-t" . vterm)
  ("C-a" . move-beginning-of-line)
  ("C-e" . move-end-of-line)
  ("C-k" . kill-whole-line)
  ("C-n" . isearch-repeat-forward)
  ("C-o" . save-buffer)
  ("C-p" . my/open-file)
  ("C-q" . save-buffers-kill-terminal)
  ("C-r" . xref-find-definitions)
  ("C-w" . backward-kill-word)
  ("C-]" . open-config-file)

  ("C-<down>" . forward-paragraph)
  ("C-<up>" . backward-paragraph)

  ("C-x a" . (lambda () (interactive) (ibuffer nil "*Ibuffer*" '((not (name . ".*\\*.*"))))))
  ("C-x e" . other-window)
  ("C-x r" . xref-go-back)
  ("C-x f" . eglot-find-implementation)
  ("C-x s" . xref-find-references)
  ("C-x l" . (lambda () (interactive) (switch-to-buffer (other-buffer))))
  ("C-x q" . delete-window)
  ("C-x c" . my/org-src-block-copy-osc52)

  ("C-c a" . consult-buffer)
  ("C-c d" . consult-goto-line)
  ("C-c l" . my/jump-to-line)
  ("C-c c" . my/copy-region-to-clipboard-osc52)
  ("C-c v" . yank-pop)
  ("C-c g" . my/dlv-breakpoint)
  ("C-c P" . flymake-show-project-diagnostics)
  ("C-c r" . reload-init-file)
  ("C-c f" . consult-eglot-symbols)
  ("C-c p" . consult-flymake)
  ("C-c e" . flymake-show-project-diagnostics)
  ("C-c x" . compile)
  ("C-c q" . keyboard-escape-quit)
  ("C-c ]" . my/log-entry)
  ("C-c [" . my/open-daily-log)
  ("C-c d" . eldoc-doc-buffer)
  ("C-c k" . 'kill-current-buffer)

  ("C-c 1" . (lambda () (interactive) (my-insert-pair "()")))
  ("C-c 2" . (lambda () (interactive) (my-insert-pair "{}")))
  ("C-c 3" . (lambda () (interactive) (my-insert-pair "[]")))

  ("C-c <left>"  . windmove-left)
  ("C-c <right>" . windmove-right)
  ("C-c <up>"    . windmove-up)
  ("C-c <down>"  . windmove-down))


(my/bind-keys org-mode-map
  ("C-c i"   . org-insert-item)
  ("C-c s"   . org-insert-heading)
  ("C-c d"   . insert-org-date-with-brackets)
  ("C-c w"   . org-meta-return)
  ("C-l"     . org-insert-link)
  ("C-c RET" . org-insert-entry)
  ("C-x RET" . org-insert-task-with-id)
  ("C-j"     . completion-at-point)
  ("C-c f"   . org-mark-done-with-date)
  ("C-c 1"   . (lambda () (interactive) (org-surround "*")))
  ("C-c 2"   . (lambda () (interactive) (org-surround "_")))
  ("C-c 3"   . (lambda () (interactive) (org-surround "/"))))

(my/bind-keys magit-status-mode-map
  ("C-c d" . my/magit-copy-diff))

(provide 'keybindings)
;;; keybindings.el ends here
