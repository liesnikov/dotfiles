;;; package -- summary
;; M-x occur ';;;' to look for headers

(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(when (< emacs-major-version 27) (package-initialize))

;;; Custom:
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;;; Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package use-package
  :custom
  ;; disable :hook suffix to use abnormal hooks with the same syntax
  (use-package-hook-name-suffix nil)
  :config
  ;; The :ensure-system-package keyword allows
  ;; to ensure system binaries exist alongside package declarations.
  (use-package use-package-ensure-system-package
    :ensure t))

;; ensure all packages -- installs them
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; not a system package, but we have to change paths before anything else kicks in
(use-package no-littering
  :ensure t
  :init
  (setq no-littering-etc-directory
          (expand-file-name "etc/" "~/.config/emacs")
        no-littering-var-directory
          (expand-file-name "~/.cache/emacs/")

        auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        auto-save-list-file-prefix
          (no-littering-expand-var-file-name "auto-save-list/.saves-")
        backup-directory-alist
          `(("." . ,(no-littering-expand-var-file-name "backup")))
        eshell-directory-name
          (no-littering-expand-var-file-name "eshell")
        transient-history-file
          (no-littering-expand-var-file-name "transient/history.el")))

;;;# Built-in packages, for neatness

(use-package auto-complete
  :ensure nil
  :custom
  (ac-comphist-file "~/.cache/emacs/ac-comphist.dat"))

(use-package desktop
  :ensure nil
  :custom
  (desktop-base-lock-name "lock")
  (desktop-path '("~/.cache/emacs/desktop"))
  (desktop-auto-save-timeout 30)
  (desktop-load-locked-desktop t)
;;(desktop-save 0)
  :hook
  (after-make-frame-functions . (lambda (frame)
                                  (with-selected-frame frame
                                    (unless desktop-save-mode
                                      (desktop-save-mode 1)
                                      (desktop-read))))))

(use-package dired
  :ensure nil
  :custom
  (dired-async-mode t)
  (dired-listing-switches "-al")
  :bind
  (:map dired-mode-map
        ("C-c o"   . dired-open-file)
        ("C-c C-o" . dired-open-file))
  :config
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file))))

(use-package eshell
  :ensure nil
  :custom
  (password-cache-expiry 300)
  (eshell-load-hook (lambda nil (setenv "PAGER" "")))
  (eshell-prefer-lisp-functions t)
  (eshell-prefer-lisp-variables t)
  (eshell-prompt-function (lambda ()
                            (concat (file-name-base (eshell/pwd))
                                    " ⊢")))
  (eshell-prompt-regexp "[^/]+ ⊢")
  :config
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package display-line-numbers
  :ensure nil
  :init
  (defcustom display-line-numbers-exempt-modes '()
   "Major modes on which to disable the linum mode, exempts them from global requirement"
   :group 'display-line-numbers
   :type 'list
   :version "green")
  (defun display-line-numbers--turn-on ()
    "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
    (if (and
         (not (member major-mode display-line-numbers-exempt-modes))
         (not (minibufferp)))
        (display-line-numbers-mode)))
  :custom
;; "Major modes on which to disable the linum mode, exempts them from global requirement"
  (display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode))
  :hook (prog-mode-hook . display-line-numbers-mode))

(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t))

(use-package ibuffer
  :ensure nil
  ;; commented out for potential performance gains?
  ;; :custom
  ;; (ibuffer-saved-filter-groups nil)
  ;; (ibuffer-saved-filters nil)
  :bind (;; map C-x C-b to ibuffer instead of default `list-buffers`
         ("C-x C-b" . ibuffer)))

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file "~/.cache/emacs/recentf"))

(use-package windmove
  :ensure nil
  :init
  ;; Windmove is a library built into GnuEmacs starting with version 21.
  ;; It lets you move point from window to window using Shift and the arrow keys.
  ;; https://www.emacswiki.org/emacs/WindMove
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))
  :custom
  (window-resize-pixelwise 't)
  (frame-resize-pixelwise 't)
  :bind (;; new bindings to change widnow sizes
         ;; similar bindings to windmove (see below),
         ;; which has S-<arrow> as moving binding
         ("S-C-<left>" . shrink-window-horizontally)
         ("S-C-<right>".  enlarge-window-horizontally)
         ("S-C-<down>" . shrink-window)
         ("S-C-<up>" . enlarge-window)))

(use-package nxml-mode
  :ensure nil
  :config
  (defun xml-pretty-print ()
    (interactive)
    sgml-pretty-print)
  (use-package noxml-fold
    :hook
    (nxml-mode-hook . (lambda () (noxml-fold-mode 1)))))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-column 100))

; rectangular editining with C-<return>
; borrowed from https://karthinks.com/software/more-batteries-included-with-emacs/
(use-package cua-base
  :config
  (cua-mode 't))

; contex-menu mode and functions
; borrowed from http://amodernist.com/texts/emacs-mouse.html
(use-package mouse
  :custom
  (context-menu-mode 't)
  (context-menu-functions '(context-menu-ffap
                            occur-context-menu
                            context-menu-region
                            context-menu-undo
                            dictionary-context-menu)))



;; end of built-in packages

;;;# Installed packages

;;;## Package managment

;; quelpa is a tool to compile and install Emacs Lisp packages locally from local or remote source code.
(use-package quelpa
  :config
  ;; quelpa handler for use-package
  (use-package quelpa-use-package
    :requires use-package))

;;;## Visual things
(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  ; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ; if nil, bold is universally disabled
  (doom-themes-enable-italic t)
  :config
  ;; (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; highlight all whitespaces
(use-package unicode-whitespace
  :custom
  (global-whitespace-mode 1)
  (global-whitespace-newline-mode 1)
  (whitespace-global-modes '(not agda2-mode))
  :config
  (unicode-whitespace-setup 'subdued-faces))

(use-package emojify
  ;; :wave:
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode github))
  :hook
  (after-init-hook . global-emojify-mode))

;;;## General goodies

(use-package auto-complete
  :config
  (ac-config-default)
  ;; doesn't really work, enables auto-fill-mode globally)
  (global-auto-complete-mode t)
  (use-package auto-complete-auctex)
  (use-package ac-math))

;; provide a popup when you press a button with all bindings that follow
(use-package which-key
  :config
  (which-key-mode))

;; provide colours in occur mode
(use-package color-moccur
  :bind (("M-s O" . moccur)))

;; turn frame around, somehow not available by default
(use-package transpose-frame)

;; because elpa keys are expiring sometimes
(use-package gnu-elpa-keyring-update)

;; projectile organizes buffers in projects
(use-package projectile
  :bind (:map projectile-mode-map
         ("C-x p". projectile-command-map))

  :custom
  (projectile-enable-caching t)
  (projectile-mode t nil (projectile))
  (projectile-file-exists-local-cache-expire (* 5 60))
  (projectile-cache-file "~/.cache/emacs/projectile/cache")

  :config

  (use-package ibuffer-projectile
    :config
    :hook
    (ibuffer-hook . ;; ibuffer-projectile automatic sorting
      (lambda ()
        (ibuffer-projectile-set-filter-groups)
        (unless (eq ibuffer-sorting-mode 'alphabetic)
          (ibuffer-do-sort-by-alphabetic))))))


;; search package instead of grep
(use-package rg
  :ensure-system-package (rg . ripgrep))

;; ivy is an autocompletion framework
(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer))
  :custom
  (ivy-mode t)
  (ivy-read-action-format-function 'ivy-read-action-format-columns) ;; use columns in ivy
  (enable-recursive-minibuffers t) ;; technically not an ivy variable, but useful to have for some reasons atm unknown to me
  (ivy-use-virtual-buffers t) ;; add recent files and/or bookmarks to ‘ivy-switch-buffer’.
  :config
  (use-package ivy-hydra)
  (use-package ivy-rich
    :config
    (ivy-rich-mode t)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)))

(use-package swiper
  :bind (("C-s" . swiper-isearch)))

(use-package counsel
  :custom
  (counsel-mode t)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))


;; view pdfs in emacs
(use-package pdf-tools
  :config
  (pdf-tools-install) ;; enable pdftools instead of docview

  :hook
  ; Disable linum (line numbers) when entering pdf-tools mode.
  ; from https://stackoverflow.com/a/6839968
  (pdf-view-mode-hook . (lambda ()
                          (add-hook 'after-change-major-mode-hook
                                    (lambda () (linum-mode 0))
                                    :append :local))))

;; Flymake
(use-package flymake
  :custom
  (flymake-run-in-place nil)
  :config
  (use-package flymake-haskell-multi
    :requires flymake))

;; increment-decrement numbers as in vim
(use-package evil-numbers
  :bind (("C-c +" . 'evil-numbers/inc-at-pt)
         ("C-c -" . 'evil-numbers/dec-at-pt)
         ("C-c C-+" . 'evil-numbers/inc-at-pt-incremental)
         ("C-c C--" . 'evil-numbers/dec-at-pt-incremental)))

;; move around efficiently
(use-package avy
  :bind (("C-;" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line))
  :config
  (avy-setup-default))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

;;;## Writing & reading

;; word processor and markup
(use-package wc-mode)

;; loading auctex directly doesn't work for some reason
;; https://github.com/jwiegley/use-package/issues/379
(use-package tex-mode
  :after auctex
  :custom
  (reftex-plug-into-AUCTeX t)
  :hook
  (TeX-mode-hook . (lambda () (flyspell-mode t)))
  (LaTeX-mode-hook . LaTeX-math-mode)
  (LaTeX-mode-hook .
              (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "\\(" "\\)"))))
  ;; Turn on RefTeX with AUCTeX LaTeX mode
  (LaTeX-mode-hook . turn-on-reftex)
  ;; with Emacs latex mode
  (latex-mode-hook . turn-on-reftex)
  :config
  (use-package company-auctex
    :requires company
    :config
    (company-auctex-init)))

;; package for writing mode, introduces margins
;; for search purposes: org-mode
(use-package olivetti
  :custom
  (olivetti-body-width 90)
  :config
  (defun olivetti ()
    (interactive)
    (call-interactively #'olivetti-mode )))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package markdown-mode
  :config
  ;; from https://gist.github.com/kleinschmidt/5ab0d3c423a7ee013a2c01b3919b009a
  ;; define markdown citation formats
  (defvar markdown-cite-format)
  (setq markdown-cite-format
        '(
          (?\C-m . "[@%l]")
          (?p . "[@%l]")
          (?t . "@%l")
          )
        )

  ;; wrap reftex-citation with local variables for markdown format
  (defun markdown-reftex-citation ()
    (interactive)
    (let ((reftex-cite-format markdown-cite-format)
          (reftex-cite-key-separator "; @"))
      (reftex-citation)))
  :hook
  ; bind modified reftex-citation to C-c[, without enabling reftex-mode
  ; https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
  (markdown-mode-hook . (lambda ()
                          (define-key markdown-mode-map "\C-c["
                            'markdown-reftex-citation))))

(use-package org
  :bind (("C-c l" . org-store-link))
  :custom
  (org-agenda-files nil)
  (org-cycle-include-plain-lists (quote integrate))
  (org-export-backends (quote (ascii beamer html icalendar latex md odt)))
  (org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
  (org-modules
   (quote
    (ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus
     ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m)))
  :config
  (use-package org-download)
  (use-package org-present)
  (use-package org-modern
    :hook
    ((org-mode-hook . org-modern-mode)
     (org-agenda-finalize-hook . org-modern-agenda)))
  :hook
  ; activate org-indent-mode on org-indent
  (org-mode-hook . org-indent-mode))

;;;## Programming


;;;### misc
(use-package magit
  :custom
  ;; highlight word-differences in diffs
  (magit-diff-refine-hunk (quote all))
  :bind (;; magit open default window binding
         ("C-x g" .  magit-status)))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

;; Language server protocol
(use-package eglot)

;;;### ocaml
(use-package tuareg
  ;; activate tuareg (ocaml) mode in ml4 files
  ;; (syntax extensions for coq)
  :mode "\\.ml4\\'")

(use-package merlin
  :requires tuareg
  :hook
   ;; from https://github.com/ocaml/merlin/wiki/emacs-from-scratch
  (tuareg-mode-hook . merlin-mode)
  )

;;;### haskell
(use-package haskell-mode
  :hook
  (haskell-mode-hook . interactive-haskell-mode))

;;;### nix
(use-package nix-mode
  ;; activate nix-mode in .nix files
  :mode "\\.nix\\'")

;;### proof assistants
(use-package idris-mode)
(use-package idris2-mode
  :quelpa (idris2-mode :fetcher github :repo "idris-community/idris2-mode"))

(use-package proof-general
  :custom
    (coq-auto-adapt-printing-width t)
    (coq-compile-auto-save (quote ask-coq))
    (coq-compile-before-require nil)
    (coq-compile-parallel-in-background t)
    (coq-maths-menu-enable nil)
    (coq-unicode-tokens-enable nil)
    (proof-autosend-enable nil)
    (proof-three-window-enable nil)
    (proof-toolbar-enable t))

(use-package company-coq
  :requires company
  :hook
  ;; company-coq is an addon on top of proofgeneral,
  ;; enable it as we enter coq mode
  (coq-mode-hook . company-coq-mode))

;;; Commentary:

;; NB:

; to stop wrapping lines: toggle-truncate-lines

; occur mode (M-s o)

; interactive regex building (M-x) bound to re-builder

; text-mode menu bar (M-`) bound to tmm-menubar

; artist-mode to draw ASCII diagrams: artist-mode

; to open two different views on the same buffer: make-indirect buffer 

;;; Bindings:

;;; Code:

(defun kill-filename ()
  ;; copy current buffer's file path to kill-ring
  (interactive)
  (kill-new buffer-file-name))

;;(add-to-list
;; due to a weird bug, both tokens from PG and company-coq are used
;; which results in "token undefined" errors when using PG ones
;; 'coq-mode-hook (unicode-tokens-use-shortcuts nil))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation BUFFER if succeeded without warnings (check STRING).
Source: https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close"
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string))
       ; (not
       ;  (with-current-buffer buffer
       ;    (goto-char 1)
       ;    (search-forward "warning" nil t))))
      (run-with-timer 0.5 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (delete-window (get-buffer-window buf)))
                      buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Automatically switch themes

(defun set-theme (&optional name)
  "Detect xfce4 system theme (or NAME) and switch Emacs theme accordingly."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "System theme: ")
                       nil)))
  (let* ((command "xfconf-query -c xsettings -p /Net/ThemeName")
         (newtheme (or (if name (concat name "\n"))
                       (shell-command-to-string command)))
         (expected-value "Arc\n")
         (dark-theme 'doom-one)
         (light-theme 'doom-one-light))
    (if
        (string= newtheme
                 expected-value)
        (progn (load-theme light-theme t)
               (disable-theme dark-theme))
        (progn (load-theme dark-theme t)
               (disable-theme light-theme)))))

(defun detect-and-switch-theme (servname setpath themename)
  (if
      (string= setpath
               "/Net/ThemeName")
      (set-theme (car themename))))

(dbus-register-signal
 :session
 nil ;; service name, nil is a wildcard
 "/org/xfce/Xfconf" ;; path
 "org.xfce.Xfconf" ;; interface
 "PropertyChanged" ;; message
 #'detect-and-switch-theme)

(set-theme)

;; Screenshot to svg
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring.
Source: https://old.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/"
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;; "Compile on save" in Emacs
;; from https://rtime.ciirc.cvut.cz/~sojka/blog/compile-on-save/
(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

;; I use this from commandline
(defun shutdown ()
  (interactive)
  (progn
    (desktop-save "~/.cache/emacs/desktop")
    (save-buffers-kill-emacs)))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))


(defun eshell/trueclear ()
  "True clear for eshell, instead of default scroll."
  (interactive)
   (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))


;; agda2 mode, gets appended by `agda-mode setup`
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.config/emacs/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init.el)
;;; init.el ends here
