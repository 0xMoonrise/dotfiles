;; utils.el --- Functions elisp emacs utils -*- lexical-binding: t; -*-
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
  "Reload init.el and all files in lisp/ ."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (dolist (file (directory-files
                 (expand-file-name "lisp" user-emacs-directory)
                 t "\\.el$"))
    (load-file file)))


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
                         ("utils.el" . "~/.emacs.d/lisp/utils.el")))
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
                       ("utils.el" . "Utility code")))
        (max-width 15))
    (let* ((desc (cdr (assoc candidate annotations)))
           (padding (make-string (- max-width (length candidate)) ?\s)))
      (concat padding desc))))

(defun org-insert-task-with-id()
  "Create a new TODO entry."
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

(defun org-insert-entry ()
  "Create a new org note file."
  (interactive)
  (require 'org-id)
  (let* ((name (read-string "Name: "))
        (id (org-id-new))
        (created-date (format-time-string "[%Y-%m-%d %a]"))
        (file-path (format "./Notes/%s.org" name)))
    (org-insert-link name file-path (concat created-date " " name))
    (write-region
     (format ":PROPERTIES:\n:NAME:     %s\n:ID:       %s\n:CREATED:  %s\n:END:"
             name id created-date)
     				 nil
     				 file-path
     				 nil)))

(defun my/open-daily-log ()
  "Open today's log file, creating it if it doesn't exist."
  (interactive)
  (let* ((date-str (format-time-string "%Y-%m"))
         (file-path (expand-file-name (format "~/journal/Logs/log-%s.org" date-str))))
    (find-file file-path)
    (when (= (buffer-size) 0)
      (insert (format "* %s\n"
                      (format-time-string "%A, %d %B %Y")))
      (save-buffer))))

(defun my/log-entry ()
  "Insert a timestamped log entry, creating a day heading if needed."
  (interactive)
  (find-file (expand-file-name
              (format "~/journal/Logs/log-%s.org"
                      (format-time-string "%Y-%m"))))
  (let ((today (format-time-string "* %A, %d %B %Y")))
    (goto-char (point-max))
    (unless (save-excursion
              (re-search-backward (regexp-quote today) nil t))
      (insert (format "\n%s\n" today)))
    (goto-char (point-max))
    (insert (format "\n** %s " (format-time-string "%H:%M"))))
  (save-buffer))

(defun my/magit-copy-diff ()
  "Copy the full git diff (staged + unstaged) to clipboard via OSC 52."
  (interactive)
  (let* ((root (locate-dominating-file default-directory ".git"))
         (_ (unless root (user-error "Not inside a git repository")))
         (default-directory root)
         (staged (shell-command-to-string "git diff --cached"))
         (unstaged (shell-command-to-string "git diff"))
         (diff (cond
                ((and (string-empty-p staged) (string-empty-p unstaged))
                 (user-error "No changes found in the repository"))
                ((string-empty-p staged) unstaged)
                ((string-empty-p unstaged) staged)
                (t (concat "=== STAGED ===\n" staged
                           "\n=== UNSTAGED ===\n" unstaged)))))
    (kill-new diff)
    (send-string-to-terminal
     (concat "\033]52;c;"
             (base64-encode-string (encode-coding-string diff 'utf-8) t)
             "\a"))
    (message "Diff copied to clipboard (%d chars)" (length diff))))

(provide 'utils)

;;; utils.el ends here
