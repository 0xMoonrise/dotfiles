;;; command-mode.el --- Syntax highlighting for shell commands in Org src blocks

(require 'sh-script) ; heredamos de sh-mode

(defvar command-mode-font-lock-keywords
  '(
    ;; Comando: primera palabra de cada línea
    ("^[[:space:]]*\\([a-zA-Z][a-zA-Z0-9_-]*\\)" 1 font-lock-builtin-face)
    ;; Flags cortas: -T4 -A -p
    ("-[a-zA-Z0-9]+" . font-lock-type-face)
    ;; Flags largas: --output --verbose
    ("--[a-zA-Z0-9_-]+" . font-lock-keyword-face)
    ;; Strings
    ("'[^']*'" . font-lock-string-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ;; IPs
    ("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" . font-lock-constant-face)
    ;; Pipes y redirecciones
    ("[|><&]+" . font-lock-warning-face)))

(define-derived-mode command-mode sh-mode "Command"
  "Major mode para resaltar comandos de shell/CLI en bloques Org."
  (setq font-lock-defaults '(command-mode-font-lock-keywords))
  (font-lock-mode 1))

(add-to-list 'org-src-lang-modes '("command" . command))

(provide 'command-mode)
;;; command-mode.el ends here
