;;; nmap-mode.el --- Syntax highlighting for nmap output -*- lexical-binding: t -*-

;; Author: 362026
;; Version: 0.1
;; Keywords: nmap, syntax, tools

;;; Commentary:
;; Major mode for nmap scan output with syntax highlighting.
;; Also registers "nmap" as a valid language in Org-mode src blocks.

;;; Code:

(defgroup nmap-mode nil
  "Major mode for nmap output."
  :group 'tools)

;; ─── Faces ────────────────────────────────────────────────────────────────────

(defface nmap-open-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for open ports.")

(defface nmap-closed-face
  '((t :foreground "#ff5555"))
  "Face for closed ports.")

(defface nmap-filtered-face
  '((t :foreground "#ffb86c" :weight bold))
  "Face for filtered ports.")

(defface nmap-port-number-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for port numbers (e.g. 80/tcp).")

(defface nmap-service-face
  '((t :foreground "#bd93f9"))
  "Face for service names (http, ssh, ftp...).")

(defface nmap-ip-face
  '((t :foreground "#f1fa8c" :weight bold))
  "Face for IP addresses.")

(defface nmap-hostname-face
  '((t :foreground "#f1fa8c"))
  "Face for hostnames.")

(defface nmap-header-face
  '((t :foreground "#6272a4" :slant italic))
  "Face for nmap header/meta lines.")

(defface nmap-mac-face
  '((t :foreground "#ff79c6"))
  "Face for MAC addresses.")

(defface nmap-timing-face
  '((t :foreground "#6272a4"))
  "Face for timing and stats lines.")

(defface nmap-os-face
  '((t :foreground "#50fa7b" :slant italic))
  "Face for OS detection results.")

(defface nmap-script-face
  '((t :foreground "#8be9fd" :slant italic))
  "Face for NSE script output lines.")

(defface nmap-url-face
  '((t :foreground "#f8f8f2" :underline t))
  "Face for URLs in nmap output.")

;; ─── Font Lock Keywords ────────────────────────────────────────────────────────

(defconst nmap-font-lock-keywords
  `(
    ;; Header lines
    ("^Starting Nmap.*$"        . 'nmap-header-face)
    ("^Nmap scan report for.*$" . 'nmap-header-face)
    ("^Nmap done.*$"            . 'nmap-timing-face)
    ("^Host is up.*$"           . 'nmap-timing-face)
    ("^# Nmap.*$"               . 'nmap-header-face)

    ;; IP addresses
    ("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+"
     . 'nmap-ip-face)

    ;; MAC addresses (XX:XX:XX:XX:XX:XX)
    ("[0-9A-Fa-f][0-9A-Fa-f]:[0-9A-Fa-f][0-9A-Fa-f]:[0-9A-Fa-f][0-9A-Fa-f]:[0-9A-Fa-f][0-9A-Fa-f]:[0-9A-Fa-f][0-9A-Fa-f]:[0-9A-Fa-f][0-9A-Fa-f]"
     . 'nmap-mac-face)

    ;; Port/protocol e.g. 80/tcp 443/udp
    ("[0-9]+/\\(tcp\\|udp\\|sctp\\)"
     . 'nmap-port-number-face)

    ;; Port states
    ("\\bopen\\b"     . 'nmap-open-face)
    ("\\bclosed\\b"   . 'nmap-closed-face)
    ("\\bfiltered\\b" . 'nmap-filtered-face)

    ;; Service names
    ("\\b\\(http\\|https\\|ssl/https\\|ssh\\|ftp\\|smtp\\|dns\\|rdp\\|smb\\|mysql\\|postgres\\|mongodb\\|redis\\|telnet\\|ldap\\|snmp\\|vnc\\|imap\\|pop3\\|nfs\\|msrpc\\|netbios\\|domain\\|gopher\\|auth\\)\\b"
     . 'nmap-service-face)

    ;; OS detection
    ("OS details:.*$"  . 'nmap-os-face)
    ("Running:.*$"     . 'nmap-os-face)
    ("OS CPE:.*$"      . 'nmap-os-face)

    ;; NSE script output (lines starting with |)
    ("^|.*$"           . 'nmap-script-face)

    ("https?://[^ \t\n]+" 0 'nmap-url-face t)

    ;; Latency / timing info
    ("latency"                  . 'nmap-timing-face)
    ("^Service detection.*$"    . 'nmap-timing-face)
    ("^Read data files.*$"      . 'nmap-timing-face)
    )
  "Font lock keywords for `nmap-mode'.")

;; ─── Major Mode ───────────────────────────────────────────────────────────────

;;;###autoload
(define-derived-mode nmap-mode text-mode "Nmap"
  "Major mode for viewing nmap scan output with syntax highlighting."
  (setq font-lock-defaults '(nmap-font-lock-keywords t))
  (font-lock-mode 1))

;; ─── File association ─────────────────────────────────────────────────────────

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nmap\\'"  . nmap-mode))
(add-to-list 'auto-mode-alist '("\\.gnmap\\'" . nmap-mode))

;; ─── Org-mode integration ─────────────────────────────────────────────────────

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("nmap" . nmap)))

(provide 'nmap-mode)
;;; nmap-mode.el ends here
