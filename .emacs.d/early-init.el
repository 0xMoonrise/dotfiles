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
(setq my-cpu-architecture-type 'x86_64)  ;; Change to 'x86_64 or nil for auto-detection

;; Automatic architecture detection function
(defun my-detect-architecture ()
  "Detect the system architecture based on system-configuration."
  (cond
   ((string-match-p "aarch64\\|arm" system-configuration) 'arm)
   ((string-match-p "x86_64\\|amd64" system-configuration) 'x86_64)
   (t 'unknown)))

;; Determine the effective architecture to use
;; Uses manual setting if provided, otherwise falls back to auto-detection
(defvar my-effective-architecture
  (or my-cpu-architecture-type
      (my-detect-architecture))
  "Effective architecture used for native compilation.")

;; Define microarchitecture targets based on the detected/specified architecture
(defvar my-cpu-microarchitecture
  (cond
   ((eq my-effective-architecture 'arm)
    "cortex-a76+crc+crypto")     ;; ARM/Raspberry Pi specific optimizations
   ((eq my-effective-architecture 'x86_64)
    "tigerlake")                 ;; Intel Tiger Lake (11th gen) and compatible
   (t
    "native"))                   ;; Generic fallback - let GCC detect best options
  "CPU microarchitecture for compiler optimizations.")

;; Set the CPU architecture variable used in compiler options
(setq my-cpu-architecture my-cpu-microarchitecture)

;; Compiler options for native compilation
;; These flags are passed directly to GCC when compiling elisp to native code
(setq native-comp-compiler-options
      `("-O2"                     ;; Standard optimization level
        ,(cond
          ((eq my-effective-architecture 'arm)
           (format "-mcpu=%s" my-cpu-architecture))     ;; ARM uses -mcpu flag
          ((eq my-effective-architecture 'x86_64)
           (format "-march=%s" my-cpu-architecture))    ;; x86_64 uses -march flag
          (t
           (format "-mtune=%s" my-cpu-architecture)))   ;; Fallback uses -mtune
        "-g0"                     ;; No debug info - reduces .eln file size
        "-fno-omit-frame-pointer" ;; Better debugging/profiling support
        "-fno-finite-math-only")) ;; Conservative floating-point math (safer)

;; Linker options for the native compilation driver
;; These flags optimize the generated shared objects (.eln files)
(setq native-comp-driver-options
      '("-Wl,-z,pack-relative-relocs"  ;; Compress relocation tables (smaller files, faster loading)
        "-Wl,-O2"                      ;; Standard linker optimizations (string merging, etc.)
        "-Wl,--as-needed"))            ;; Only link against libraries actually used


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
