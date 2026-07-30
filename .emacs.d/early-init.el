(defun my--native-comp-flags ()
  "Return native-comp compiler flags optimized for the current CPU."
  (let ((base '("-O2" "-g0"
                "-fno-omit-frame-pointer"
                "-fno-finite-math-only")))
    (cond ((string-match-p "x86_64" system-configuration)
           (append base '("-march=native")))
          ((string-match-p "aarch64\\|^arm" system-configuration)
           (append base '("-mcpu=native")))
          (t
           (append base '("-march=native" "-mtune=native"))))))

(setq native-comp-compiler-options (my--native-comp-flags))

(setq native-comp-driver-options
      '("-Wl,-z,pack-relative-relocs"
        "-Wl,-O2"
        "-Wl,--as-needed"))
