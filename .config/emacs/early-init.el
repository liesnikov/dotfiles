;;; -*- lexical-binding: t; -*-

;;; Compilation

(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name (convert-standard-filename "eln-cache/")
                            (expand-file-name "~/.cache/emacs/"))))

;;; Startup Performance Tweaks
;; 1. Defer garbage collection and file handlers during startup
(setq gc-cons-threshold most-positive-fixnum)
(defvar liesnikov--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024)) ; 20 MB for normal runtime
            (setq file-name-handler-alist liesnikov--file-name-handler-alist)))

;; 2. Prevent UI flash by disabling UI elements *before* the first frame draws
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(setq initial-frame-alist '(;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
