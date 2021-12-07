;;; Compilation

(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name (convert-standard-filename "var/eln-cache/")
                            user-emacs-directory)))
