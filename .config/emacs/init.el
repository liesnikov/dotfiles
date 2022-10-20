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
  ; disable :hook suffix to use abnormal hooks with the same syntax
  (use-package-hook-name-suffix nil))
(use-package use-package-ensure-system-package
  ; The :ensure-system-package keyword allows
  ; to ensure system binaries exist alongside package declarations.
  :requires use-package
  :ensure t)

;; ensure all packages -- installs them
; (require 'use-package-ensure)
; (setq use-package-always-ensure t)

(use-package no-littering
  ; not a system package, but we have to change paths before anything else kicks in
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
        create-lockfiles ; get rid of .# files, which are annoying
          nil
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
; (desktop-save 0)
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
  :bind (:map dired-mode-map
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
    "turn on line numbers but excempting certain majore modes
     defined in `display-line-numbers-exempt-modes'"
    (if (and
         (not (member major-mode display-line-numbers-exempt-modes))
         (not (minibufferp)))
        (display-line-numbers-mode)))
  :custom
; "Major modes on which to disable the linum mode, exempts them from global requirement"
  (display-line-numbers-exempt-modes
   '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode))
  :hook (prog-mode-hook . display-line-numbers-mode))

(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t))

(use-package ibuffer
  :ensure nil
  ; commented out for potential performance gains?
  ; :custom
  ; (ibuffer-saved-filter-groups nil)
  ; (ibuffer-saved-filters nil)
  :bind (; map C-x C-b to ibuffer instead of default `list-buffers`
         ("C-x C-b" . ibuffer)))

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file "~/.cache/emacs/recentf"))

(use-package windmove
  :ensure nil
  :init
  ; Windmove is a library built into GnuEmacs starting with version 21.
  ; It lets you move point from window to window using Shift and the arrow keys.
  ; https://www.emacswiki.org/emacs/WindMove
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))
  :custom
  (window-resize-pixelwise 't)
  (frame-resize-pixelwise 't)
  :bind (; new bindings to change widnow sizes
         ; similar bindings to windmove (see below),
         ; which has S-<arrow> as moving binding
         ("S-C-<left>" . shrink-window-horizontally)
         ("S-C-<right>".  enlarge-window-horizontally)
         ("S-C-<down>" . shrink-window)
         ("S-C-<up>" . enlarge-window)))

(use-package nxml-mode
  :ensure nil
  :config
  (defun xml-pretty-print ()
    (interactive)
    sgml-pretty-print))
(use-package noxml-fold
  :requires nxml-node
  :hook
  (nxml-mode-hook . (lambda () (noxml-fold-mode 1))))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator 't)
  (global-display-fill-column-indicator-mode 't)
  (display-fill-column-indicator-column 90))

; (use-package cua-base
;   ; rectangular editining with C-<return>
;   ; borrowed from https://karthinks.com/software/more-batteries-included-with-emacs/
;   ; disabled because it messes with the usual rectangular editing
;   ; and doesn't allow to delete rectangles efficiently
;   :config
;   (cua-mode 't))

(use-package mouse
  ; contex-menu mode and functions
  ; borrowed from http://amodernist.com/texts/emacs-mouse.html
  :ensure nil
  :custom
  (context-menu-mode 't)
  (context-menu-functions '(context-menu-ffap
                            occur-context-menu
                            context-menu-region
                            context-menu-undo
                            dictionary-context-menu)))

(use-package calendar
  :ensure nil
  :custom
  ; The day of the week on which a week in the calendar begins.
  ; 1 means Monday
  (calendar-week-start-day 1))

(use-package simple
  :ensure nil
  :custom
  ; Indentation can insert tabs if this is non-nil.
  (indent-tabs-mode nil)
  ; Don't wrap lines by default
  (toggle-truncate-lines 1))

(use-package menu-bar
  :ensure nil
  :custom
  ; disable menu bar
  (menu-bar-mode nil))

(use-package scroll-bar
  :ensure nil
  :custom
  ; disable scroll bar
  (scroll-bar-mode nil))

(use-package mule
  :ensure nil
  :custom
  ; Specify coding system for keyboard input.
  (keyboard-coding-system 'utf-8-unix))

(use-package select
  :ensure nil
  :custom
  ; Coding system for communicating with other programs.
  (selection-coding-system 'utf-8))

(use-package paren
  :ensure nil
  :custom
  ; Toggle visualization of matching parens
  (show-paren-mode t))

(use-package pixel-scroll
  :ensure nil
  :custom
  ; When enabled, this minor mode allows to scroll the display
  ; precisely, according to the turning of the mouse wheel.
  (pixel-scroll-precision-mode 't))

(use-package cus-start
  ; catch-all package for all the things that don't have their own package
  :ensure nil
  :custom
  ; personal info
  (user-full-name "Bohdan Liesnikov")
  (user-mail-address "bohdan@liesnikov")

  ; don't show startup emacs screen
  (inhibit-startup-screen t)
  ; If the value is nil and ‘inhibit-startup-screen’ is nil, show the startup screen.
  ; If t, open the ‘*scratch*’ buffer.
  (initial-buffer-choice t)
  ; text present in the scratch buffer by default
  (initial-scratch-message nil)

  ; If the value is greater than 100, redisplay will never recenter point,
  ; but will always scroll just enough text to bring point into view,
  ; even if you move far away.
  (scroll-conservatively 10000)

  ; The number of lines to try scrolling a window by when point moves out.
  (scroll-step 1)

  ; pressing tab always indents
  (tab-always-indent t)
  ; display tab width
  (tab-width 2)

  ; disable tool-bar
  (tool-bar-mode nil)

  ; make mode-line line indicator be line-number:colon-number
  (mode-line-position (list "%l:%c"))

  ; fonts
  :custom-face
  (default ((t (:height 130
                :width condensed
                :foundry "ADBO"
                :family "Source Code Pro")))))

;; end of built-in packages

;;;# Installed packages

;;;## Package managment

(use-package quelpa
  ; quelpa is a tool to compile and install Emacs Lisp packages
  ; locally from local or remote source code.
  )
(use-package quelpa-use-package
  ; quelpa handler for use-package
  :requires (quelpa use-package))

;;;## Visual things

(use-package doom-themes
  ; color theme
  :custom
  ;; Global settings (defaults)
  ; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ; if nil, bold is universally disabled
  (doom-themes-enable-italic t)
  :config
  ; (load-theme 'doom-one t)
  ; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package minions
  ; hide minor modes under a drop-down menu
  :config (minions-mode 1))

(use-package moody
  ; modeline as tabs
  :custom
  (x-underline-at-descent-line t)
  (moody-mode-line-height 45)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package unicode-whitespace
  ; highlight all whitespaces
  :custom
  (global-whitespace-newline-mode 1)
  ; in agda it's simply annoying, but for magit it's causing errors
  ; see https://github.com/magit/magit/issues/4766 and
  ; https://emacs.stackexchange.com/questions/38771/magit-status-does-not-open-when-using-global-whitespace-mode-1/38778#38778
  (whitespace-global-modes '(not agda2-mode magit-mode))
  :config
  (unicode-whitespace-setup 'subdued-faces))

(use-package emojify
  ; enable emoji rendering where they are typeset as text :nonexistent: or :wave:
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode github))
  :hook
  (after-init-hook . global-emojify-mode))

;;;## General goodies

(use-package auto-complete
  :config
  (ac-config-default)
  ; doesn't really work, enables auto-fill-mode globally)
  (global-auto-complete-mode t))

(use-package auto-complete-auctex
  :requires auto-complete)
(use-package ac-math
  :requires auto-complete)

(use-package which-key
  ; provide a popup when you press a button with all bindings that follow
  :custom
  (which-key-min-display-lines 10)
  :config
  (which-key-mode))

(use-package color-moccur
  ; provide colours in occur mode
  :bind (("M-s O" . moccur)))

(use-package transpose-frame
  ; turn frame around, somehow not available by default
  )

(use-package gnu-elpa-keyring-update
  ; because elpa keys are expiring sometimes
  )

(use-package projectile
  ; projectile organizes buffers in projects
  :bind (:map projectile-mode-map
         ("C-x p". projectile-command-map))

  :custom
  (projectile-enable-caching t)
  (projectile-mode t nil (projectile))
  (projectile-file-exists-local-cache-expire (* 5 60))
  (projectile-cache-file "~/.cache/emacs/projectile/cache"))

(use-package ibuffer-projectile
  ; integrate projectile with ibuffer
  :requires projectile
  :hook
  (ibuffer-hook . ; ibuffer-projectile automatic sorting
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package rg
  ; search package instead of grep
  :ensure-system-package (rg . ripgrep))

(use-package ivy
  ; ivy is an autocompletion framework
  :bind (("C-x b" . ivy-switch-buffer))
  :custom
  ; enable ivy
  (ivy-mode t)
  ; use columns in ivy
  (ivy-read-action-format-function 'ivy-read-action-format-columns)
  ; technically not an ivy variable, but useful to have for some reasons atm unknown to me
  (enable-recursive-minibuffers t)
  ; add recent files and/or bookmarks to ‘ivy-switch-buffer’.
  (ivy-use-virtual-buffers t))
(use-package ivy-hydra
  :requires ivy)
(use-package ivy-rich
  :requires ivy
  :config
  (ivy-rich-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package counsel
  :custom
  (counsel-mode t)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c r g" . counsel-rg)))

(use-package pdf-tools
  ; view pdfs in emacs
  :config
  (pdf-tools-install) ; enable pdftools instead of docview

  :hook
  ; Disable linum (line numbers) when entering pdf-tools mode.
  ; from https://stackoverflow.com/a/6839968
  (pdf-view-mode-hook . (lambda ()
                          (add-hook 'after-change-major-mode-hook
                                    (lambda () (linum-mode 0))
                                    :append :local))))

(use-package flymake
  ; Flymake is a minor Emacs mode performing on-the-fly syntax checks.
  :custom
  (flymake-run-in-place nil))
(use-package flymake-haskell-multi
  ; Syntax-check haskell-mode using both ghc and hlint
  :requires (flymake haskell-mode)
  :hook
  (haskell-mode-hook . flymake-haskell-multi-load))

(use-package evil-numbers
  ; increment-decrement numbers as in vim
  :bind (("C-c +" . 'evil-numbers/inc-at-pt)
         ("C-c -" . 'evil-numbers/dec-at-pt)
         ("C-c C-+" . 'evil-numbers/inc-at-pt-incremental)
         ("C-c C--" . 'evil-numbers/dec-at-pt-incremental)))

(use-package avy
  ; move around efficiently
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

(use-package envrc
  :ensure-system-package direnv
  ; package for direnv, usefull when working with nix
  :config
  (envrc-global-mode)
  (defun envrc-reload-or-clear ()
    (interactive)
    (envrc--clear (current-buffer))
    (condition-case nil
        (envrc--with-required-current-env env-dir
          (when (string= (envrc--find-env-dir) env-dir)
            (envrc--update)
            (message "Refreshed %s in env %s" (buffer-name) env-dir)))
      (user-error
       (message "Unloaded env for %s" (buffer-name)))))
  :hook
  (eshell-directory-change-hook . envrc-reload-or-clear))


;;;## Writing & reading

;; word processor and markup

(use-package wc-mode
  ; count words
  )

(use-package tex-mode
  ; loading auctex directly doesn't work for some reason
  ; https://github.com/jwiegley/use-package/issues/379
  :after auctex
  :custom
  (reftex-plug-into-AUCTeX t)
  :hook
  (TeX-mode-hook . (lambda () (flyspell-mode t)))
  (LaTeX-mode-hook . LaTeX-math-mode)
  (LaTeX-mode-hook .
              (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "\\(" "\\)"))))
  ; Turn on RefTeX with AUCTeX LaTeX mode
  (LaTeX-mode-hook . turn-on-reftex)
  ; with Emacs latex mode
  (latex-mode-hook . turn-on-reftex))
(use-package company-auctex
  :requires (company tex-mode)
  :config
  (company-auctex-init))

(use-package olivetti
  ; package for writing mode, introduces margins
  ; for search purposes: org-mode
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
  ; from https://gist.github.com/kleinschmidt/5ab0d3c423a7ee013a2c01b3919b009a
  ; define markdown citation formats
  (defvar markdown-cite-format)
  (setq markdown-cite-format
        '(
          (?\C-m . "[@%l]")
          (?p . "[@%l]")
          (?t . "@%l")
          )
        )

  ; wrap reftex-citation with local variables for markdown format
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
  :defines org-date-today
  :bind (:map org-mode-map
         ; and to insert current date use org-time-stamp bound to C-c .
         ("C-c l" . org-store-link))
  :custom
  (org-agenda-files nil)
  (org-cycle-include-plain-lists (quote integrate))
  (org-export-backends (quote (ascii beamer html icalendar latex md odt)))
  (org-format-latex-options
   (quote
    (:foreground default :background default
     :scale 1.2 :html-foreground "Black"
     :html-background "Transparent" :html-scale 1.0
     :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
  (org-modules (quote
    (ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus
     ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m)))
  :hook
  ; activate org-indent-mode on org-indent
  (org-mode-hook . org-indent-mode))

(use-package org-download
  :requires org)
(use-package org-present
  :requires org)
(use-package org-modern
  :requires org
  :hook
  ((org-mode-hook . org-modern-mode)
   (org-agenda-finalize-hook . org-modern-agenda)))

;;;## Programming


;;;### misc
(use-package magit
  ; emacs git interface
  :custom
  ; highlight word-differences in diffs
  (magit-diff-refine-hunk (quote all))
  :bind (; magit open default window binding
         ("C-x g" .  magit-status)))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

;; Language server protocol
(use-package eglot)

;;;### rust
(use-package rustic
  :requires eglot
  :custom
  (rustic-lsp-client 'eglot))


;;;### ocaml
(use-package tuareg
  ; activate tuareg (ocaml) mode in ml4 files
  ; (syntax extensions for coq)
  :mode "\\.ml4\\'")

(use-package merlin
  :requires tuareg
  :hook
   ; from https://github.com/ocaml/merlin/wiki/emacs-from-scratch
  (tuareg-mode-hook . merlin-mode)
  )

;;;### haskell
(use-package haskell-mode
  :custom
  (haskell-process-type 'auto)
  (haskell-compiler-type 'auto)
  :hook
  (haskell-mode-hook . interactive-haskell-mode))

;;;### nix
(use-package nix-mode
  ; activate nix-mode in .nix files
  :mode "\\.nix\\'")

;;### proof assistants
(use-package idris-mode)
(use-package idris2-mode
  :quelpa (idris2-mode :fetcher github :repo "idris-community/idris2-mode"))

;;;#### coq
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
  ; company-coq is an addon on top of proofgeneral,
  ; enable it as we enter coq mode
  (coq-mode-hook . company-coq-mode))

;;;#### agda
;; agda2 mode, gets appended by `agda-mode setup`
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))


;;;#Commentary:

;; NB:

; to stop wrapping lines: toggle-truncate-lines

; occur mode (M-s o)

; interactive regex building (M-x) bound to re-builder

; text-mode menu bar (M-`) bound to tmm-menubar

; artist-mode to draw ASCII diagrams: artist-mode

; to open two different views on the same buffer: make-indirect-buffer

;;;#Bindings:

;;;#Code:

(defun kill-filename ()
  ; copy current buffer's file path to kill-ring
  (interactive)
  (kill-new buffer-file-name))

;(add-to-list
; due to a weird bug, both tokens from PG and company-coq are used
; which results in "token undefined" errors when using PG ones
; 'coq-mode-hook (unicode-tokens-use-shortcuts nil))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation BUFFER if succeeded without warnings (check STRING).
Source: https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close"
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (goto-char 1)
          (search-forward "warning" nil t))))
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
        (progn (disable-theme dark-theme)
               (load-theme light-theme t))
        (progn (disable-theme light-theme)
               (load-theme dark-theme t)))))

(defun detect-and-switch-theme (servname setpath themename)
  (if
      (string= setpath
               "/Net/ThemeName")
      (set-theme (car themename))))

(dbus-register-signal
 :session
 nil ; service name, nil is a wildcard
 "/org/xfce/Xfconf" ; path
 "org.xfce.Xfconf" ; interface
 "PropertyChanged" ; message
 #'detect-and-switch-theme)

; actually set the correct theme when loading
(set-theme)

; Screenshot to svg
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

; "Compile on save" in Emacs
; from https://rtime.ciirc.cvut.cz/~sojka/blog/compile-on-save/
(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

; I use this from commandline
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

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.config/emacs/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init.el)
;;; init.el ends here
