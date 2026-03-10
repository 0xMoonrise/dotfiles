;;; nmap-mode.el --- Syntax highlighting for command highlight -*- lexical-binding: t -*-

;; Author: 362026
;; Version: 0.1
;; Keywords: command, syntax, tools

;;; Commentary:
;; Major mode for command syntax highlight.

;;; Code:

(require 'sh-script)
(require 'org-src)

(defvar command-mode-font-lock-keywords
  '(("^[[:space:]]*\\([a-zA-Z][a-zA-Z0-9_-]*\\)" 1 font-lock-builtin-face)
    ("-[a-zA-Z0-9]+" . font-lock-type-face)
    ("--[a-zA-Z0-9_-]+" . font-lock-keyword-face)
    ("'[^']*'" . font-lock-string-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" . font-lock-constant-face)
    ("[|><&]+" . font-lock-warning-face)))

(define-derived-mode command-mode sh-mode "Command"
  "Major mode for highlighting shell/CLI commands in Org blocks."
  (setq font-lock-defaults '(command-mode-font-lock-keywords))
  (font-lock-mode 1))

(add-to-list 'org-src-lang-modes '("command" . command))

(provide 'command-mode)
;;; command-mode.el ends here
