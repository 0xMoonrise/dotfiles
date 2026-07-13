;; Display the architecture using:
;;   gcc -march=native -Q --help=target | grep march
;;
;; The above command asks the compiler to resolve native for your current CPU
;; and display the resulting target. For example, if the output shows
;; -march=skylake, you know that skylake is the identifier you should pass to
;; -mtune and -march.
;; E15 -> tigerlake

(setq my-native-comp-target-flags
      (if (string-prefix-p "aarch64" system-configuration)
          '("-mcpu=cortex-a76+crc+crypto")
        (list (format "-mtune=%s" my-cpu-architecture)
              (format "-march=%s" my-cpu-architecture))))

(setq native-comp-compiler-options
      (append '("-O2")
              my-native-comp-target-flags
              '("-g0"
                "-fno-omit-frame-pointer"
                "-fno-finite-math-only")))
