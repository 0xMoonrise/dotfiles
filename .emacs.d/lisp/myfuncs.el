;; myfuncs.el --- Functions elisp emacs utils -*- lexical-binding: t; -*-
;;; Commentary:
;;; Custom Emacs functions utils & varity.
;;; Code:

(require 'org)

(defun insert-org-date-with-brackets ()
  "Insert a date in the format [YYYY-MM-DD DDD] with calendar selection."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Select date: "))
         (clean-date (replace-regexp-in-string "[<>]" "" date))
         (formatted (format "[%s]" clean-date)))
    (insert formatted)))

(defun org-surround (char)
  "Wraps the selected region or word in CHAR."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (beg (car bounds))
         (end (cdr bounds)))
    (when bounds
      (save-excursion
        (goto-char end)
        (insert char)
        (goto-char beg)
        (insert char)))))

(defun org-mark-done-with-date ()
  "Mark the current Org entry as DONE."
  (interactive)
  (org-todo "DONE")
  (org-set-property "DONE" (format-time-string "[%Y-%m-%d %a]")))

(defun my/python-tab-complete-or-indent ()
  "Try completion, fall back to indent in real Python buffers."
  (interactive)
  (let ((completion-fn (run-hook-with-args-until-success 'completion-at-point-functions)))
    (if (and completion-fn (thing-at-point 'symbol))
        (completion-at-point)
      (indent-for-tab-command))))

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
                         ("config.el" . "~/.emacs.d/lisp/config.el")
                         ("keybindings.el" . "~/.emacs.d/lisp/keybindings.el")
                         ("myfuncs.el" . "~/.emacs.d/lisp/myfuncs.el")))
         (completion-extra-properties '(:annotation-function config-annot-fn))
         (choice (completing-read "Choose config file: " config-files))
         (file-path (expand-file-name (cdr (assoc choice config-files)))))
    (unless (file-exists-p file-path)
      (with-temp-buffer (write-file file-path)))
    (find-file file-path)))

(defun config-annot-fn (candidate)
  "Return aligned annotation for config file CANDIDATE."
  (let ((annotations '(("init.el" . "Main init file")
                       ("config.el" . "User customizations")
                       ("keybindings.el" . "Custom keybindings")
                       ("myfuncs.el" . "Utility functions")))
        (max-width 15))
    (let* ((desc (cdr (assoc candidate annotations)))
           (padding (make-string (- max-width (length candidate)) ?\s)))
      (concat padding desc))))

(defun org-insert-task-with-id()
  "Create a new org note."
  (interactive)
  (require 'org-id)
  (let ((task-title (read-string "Task name: "))
        (task-id (org-id-new))
        (created-date (format-time-string "[%Y-%m-%d %a]")))
    (insert (format "
** TODO %s
:PROPERTIES:
:ID:       %s
:CREATED:  %s
:END:
" task-title task-id created-date))))


(provide 'myfuncs)
;;; myfuncs.el ends here
