;; Display the architecture using:
;;   gcc -march=native -Q --help=target | grep march
;;
;; The above command asks the compiler to resolve native for your current CPU
;; and display the resulting target. For example, if the output shows
;; -march=skylake, you know that skylake is the identifier you should pass to
;; -mtune and -march.
;; E15 -> tigerlake
;; Rasp -> cortex-a76
;; Architecture configuration
;; Options: "x86_64", "arm", or nil for auto-detection
;; Set to your current architecture or leave as nil for automatic detection
;;; Native compilation optimizations - Auto-detected architecture

(setq my-cpu-architecture-type nil)

(defun my-detect-architecture ()
  "Detect the system architecture based on system-configuration."
  (cond
   ((string-match-p "aarch64\\|arm" system-configuration) 'arm)
   ((string-match-p "x86_64\\|amd64" system-configuration) 'x86_64)
   (t 'unknown)))

(defvar my-effective-architecture
  (or my-cpu-architecture-type
      (my-detect-architecture))
  "Effective architecture used for native compilation.
Set 'my-cpu-architecture-type' to override auto-detection.")

(defvar my-cpu-microarchitecture
  (cond
   ((eq my-effective-architecture 'arm)
    "cortex-a76+crc+crypto")
   ((eq my-effective-architecture 'x86_64)
    "tigerlake")
   (t
    "native"))
  "CPU microarchitecture for compiler optimizations.")

(setq native-comp-compiler-options
      `("-O2"
        ,(cond
          ((eq my-effective-architecture 'arm)
           (format "-mcpu=%s" my-cpu-microarchitecture))
          ((eq my-effective-architecture 'x86_64)
           (format "-march=%s" my-cpu-microarchitecture))
          (t
           (format "-mtune=%s" my-cpu-microarchitecture)))
        "-g0"
        "-fno-omit-frame-pointer"
        "-fno-finite-math-only"))

(setq native-comp-driver-options
      '("-Wl,-z,pack-relative-relocs"
        "-Wl,-O2"
        "-Wl,--as-needed"))

(message "Native compilation optimized for: %s"
         (cond
          ((eq my-effective-architecture 'arm) "ARM (Raspberry Pi/Apple Silicon)")
          ((eq my-effective-architecture 'x86_64) "x86_64 (Intel/AMD)")
          (t "Unknown/Generic - using 'native' fallback")))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16000000
                  gc-cons-percentage 0.1)
            (garbage-collect)
            (message "GC configured for interactive session")))

(setq native-comp-async-report-warnings-errors nil
      native-comp-async-query-on-exit nil
      native-comp-async-jobs-number (max 1 (/ (num-processors) 2))
      native-comp-verbose 0)

(setq json-serializer 'json-serialize
      json-parser 'json-parse-buffer)

(setq sqlite-version-check t)
