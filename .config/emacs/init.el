;;; package -- summary
;; M-x occur ';;;' to look for headers

(require 'package)

;;; Setup package archives

(unless (assoc-default "jcs-elpa" package-archives)
  (add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/packages/") t))
(when (< emacs-major-version 27) (package-initialize))

;;; Custom:
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;;; Use-package
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))
(eval-when-compile
  (require 'use-package))
(use-package use-package
  :demand t
  :custom
  ;; disable :hook suffix to use abnormal hooks with the same syntax
  (use-package-hook-name-suffix nil))

;; load no-littering as the very first package
(use-package no-littering
  :ensure t
  :demand t
  ;; not a system package, but we have to change paths before anything else kicks in
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
        (no-littering-expand-var-file-name "transient/history.el"))
  ;; don't include litter directories in recentf
  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  :config
  (no-littering-theme-backups))

(use-package use-package-ensure-system-package
  ;; The :ensure-system-package keyword allows
  ;; to ensure system binaries exist alongside package declarations.
  :requires use-package
  :demand t)

;; ensure all packages -- installs them
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install '(vc-use-package :url "https://github.com/slotThe/vc-use-package")))
(require 'vc-use-package)

;;;# Built-in packages, for neatness

(use-package desktop
  :ensure nil
  :custom
  (desktop-base-lock-name "lock")
  (desktop-path '("~/.cache/emacs/desktop"))
  (desktop-auto-save-timeout 30)
  (desktop-load-locked-desktop t)
  (desktop-restore-forces-onscreen nil) ; from https://stackoverflow.com/questions/18612742/emacs-desktop-save-mode-error#comment47963002_26546872
;; (desktop-save 0)
  :hook
  (after-make-frame-functions . (lambda (frame)
                                  (with-selected-frame frame
                                    (unless desktop-save-mode
                                      (desktop-save-mode 1)
                                      (desktop-read)))))
  :functions liesnikov/shutdown
  :config
  ;; use this from M-x
  (defun liesnikov/shutdown ()
    "Shut down and save desktop file."
    (interactive)
    (progn
      (desktop-save "~/.cache/emacs/desktop")
      (save-buffers-kill-emacs)))
  )

(use-package dired
  :ensure nil
  :custom
  (dired-async-mode t)
  (dired-listing-switches "-al")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  ;; for some reason this errors out on master-81d7827
  (dired-make-directory-clickable nil)
  :bind (:map dired-mode-map
         ("C-c o"   . dired-open-file)
         ("C-c C-o" . dired-open-file))
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
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
  :functions eshell/trueclear
  :config
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (defun eshell/trueclear ()
    "True clear for eshell, instead of default scroll."
    (interactive)
    (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer))))

(use-package display-line-numbers
  :ensure nil
  :init
  (defcustom display-line-numbers-exempt-modes '()
    "Major modes on which to disable the display-line-numbers mode,
     exempts them from global requirement"
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
;; "Major modes on which to disable the display-line-numbers mode, exempts them from global requirement"
  (display-line-numbers-exempt-modes
   '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode))
  :hook (prog-mode-hook . display-line-numbers-mode))

(use-package gdb-mi
  :ensure nil
  :defer t
  :custom
  (gdb-many-windows t))

(use-package project
  :ensure nil
  :functions project-try-magit
  :bind (:map project-prefix-map
         ("m" . project-try-magit))
  :custom
  (project-buffers-viewer        'project-list-buffers-ibuffer)
  (project-vc-extra-root-markers '(".projectile"))
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-dir "Find directory")
     (project-try-magit "Magit")
     (project-eshell "Eshell")))
  :init
  (defun project-try-magit ()
    (interactive)
    (require 'magit)
    (condition-case err
        (magit-project-status)
      (error (progn
               (message "%s" (error-message-string err)
               (project-find-dir)))))))

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
  ;; ivy makes searching through long lists easy, bump these up some from
  ;; the defaults.
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 25)
  (recentf-save-file "~/.cache/emacs/recentf")
  :config
  (add-to-list 'recentf-exclude "-autoloads.el"))

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
  :bind (:map windmove-mode-map
         ;; new bindings to change widnow sizes
         ;; similar bindings to windmove (see below),
         ;; which has S-<arrow> as moving binding
         (("C-S-<left>" . shrink-window-horizontally)
          ("C-S-<right>".  enlarge-window-horizontally)
          ("C-S-<down>" . shrink-window)
          ("C-S-<up>" . enlarge-window))))

(use-package nxml-mode
  :ensure nil
  :config
  (defun xml-pretty-print ()
    (interactive)
    sgml-pretty-print))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator 't)
  (global-display-fill-column-indicator-mode 't)
  (display-fill-column-indicator-column 90))

;; (use-package cua-base
;;   :ensure nil
;;   ;; rectangular editining with C-<return>
;;   ;; borrowed from https://karthinks.com/software/more-batteries-included-with-emacs/
;;   ;; disabled because it messes with the usual rectangular editing
;;   ;; and doesn't allow to delete rectangles efficiently
;;   :config
;;   (cua-mode 't))

(use-package mouse
  ;; contex-menu mode and functions
  ;; borrowed from http://amodernist.com/texts/emacs-mouse.html
  :ensure nil
  :custom
  (context-menu-mode 't))
  ;;(context-menu-functions '(context-menu-ffap
  ;;                          occur-context-menu
  ;;                          context-menu-region
  ;;                          context-menu-undo
  ;;                          dictionary-context-menu)))

(use-package calendar
  :ensure nil
  :defer t
  :custom
  ;; The day of the week on which a week in the calendar begins.
  ;; 1 means Monday
  (calendar-week-start-day 1))

(use-package simple
  :ensure nil
  :custom
  ;; Indentation can insert tabs if this is non-nil.
  (indent-tabs-mode nil)
  ;; Don't wrap lines by default
  (toggle-truncate-lines 1)
  (kill-ring-max 500)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  :bind
  ("C-z" . undo)
  ;; The -dwim versions of these three commands are new in Emacs 26 and
  ;; better than their non-dwim counterparts, so override those default
  ;; bindings:
  ("M-l" . downcase-dwim)
  ("M-c" . capitalize-dwim)
  ("M-u" . upcase-dwim)
  (:map global-map
        ("C-g" . liesnikov/keyboard-quit-dwim))
  :functions liesnikov/keyboard-quit-dwim
  :config
  ;; C-o runs `open-line', which I never use and is annoying to hit by accident
  (unbind-key "C-o")
  ;; define the new function for quitting
  (defun liesnikov/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.
The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'.
From https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/"
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit)))))

(use-package prog-mode
  :ensure nil
  :config
  ;; Prettify-symbols-mode will replace some symbols (like "lambda") with
  ;; their prettier cousins (like λ), but smartly as it's configured by
  ;; major modes themselves.
  (global-prettify-symbols-mode))

(use-package compile
  :ensure nil
  :functions liesnikov/compile-on-save-start
  :defines liesnikov/compile-on-save-mode
  :hook
  (compilation-finish-functions . compile-bury-buffer-if-successful)
  :config
  (defun liesnikov/compile-bury-buffer-if-successful (buffer string)
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
  ;; "Compile on save" in Emacs
  ;; from https://rtime.ciirc.cvut.cz/~sojka/blog/compile-on-save/
  (defun liesnikov/compile-on-save-start ()
    "Start compilation on the current buffer if there is no ongoing compilation."
    (let ((buffer (compilation-find-buffer)))
      (unless (get-buffer-process buffer)
        (recompile))))
  (define-minor-mode liesnikov/compile-on-save-mode
    "Minor mode to automatically call `recompile' when the current buffer is saved.
When there is ongoing compilation, nothing happens."
    :lighter " CoS"
    (if liesnikov/compile-on-save-mode
        (progn  (make-local-variable 'after-save-hook)
                (add-hook 'after-save-hook 'liesnikov/compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook))))

(use-package menu-bar
  :ensure nil
  :custom
  ;; disable menu bar
  (menu-bar-mode nil))

(use-package scroll-bar
  :ensure nil
  :custom
  ;; disable scroll bar
  (scroll-bar-mode nil))

(use-package mule
  :ensure nil
  :custom
  ;; Specify coding system for keyboard input.
  (keyboard-coding-system 'utf-8-unix))

(use-package select
  :ensure nil
  :custom
  ;; Coding system for communicating with other programs.
  (selection-coding-system 'utf-8))

(use-package paren
  :ensure nil
  :custom
  ;; Toggle visualization of matching parens
  (show-paren-mode t))

(use-package whitespace
  :ensure nil
  ;; highlight all whitespaces
  :custom
  ;; global-whitespace-mode is known to break org-mode export sometimes
  (global-whitespace-mode t)
  (whitespace-style '(face
                      trailing
                      space-mark spaces
                      tab-mark tabs
                      empty
                      ;;indentation::space
                      ;;indentation::tab
                      ;;newline newline-mark
                      space-after-tab::tab space-after-tab::space
                      space-before-tab::tab space-before-tab::space))
  ;; * in agda it's simply annoying, but for magit it's causing errors
  ;;   see https://github.com/magit/magit/issues/4766 and
  ;;   https://emacs.stackexchange.com/questions/38771/magit-status-does-not-open-when-using-global-whitespace-mode-1/38778#38778
  ;; * for magit it breaks commit flow
  ;; * for tex mode -- it breaks org-mode tex export
  (whitespace-global-modes '(not magit-mode tex-mode org-mode)))

(use-package pixel-scroll
  :ensure nil
  :custom
  ;; When enabled, this minor mode allows to scroll the display
  ;; precisely, according to the turning of the mouse wheel.
  (pixel-scroll-precision-mode 't))

(use-package treesit
  :ensure nil)

(use-package flymake
  :ensure nil
  :custom (flymake-proc-compilation-prevents-syntax-check nil)
  :hook ((prog-mode-hook LaTeX-mode) . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c n" . flymake-goto-next-error)
              ("C-c p" . flymake-goto-next-error)))

(use-package flyspell
  :ensure nil
  ;; on the fly spell checking
  :hook
  (text-mode-hook . turn-on-flyspell)
  (prog-mode-hook . flyspell-prog-mode)
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-use-global-abbrev-table-p t)
  :bind (:map flyspell-mode-map
              ("C-c k" . compile)))

(use-package reftex
  :ensure nil
  :hook
  (LaTeX-mode . turn-on-reftex)
  :custom
  (reftex-cite-format
   '((?\C-m . "\\cite[]{%l}")
     (?t . "\\citet{%l}")
     (?p . "\\citep[]{%l}")
     (?a . "\\autocite{%l}")
     (?A . "\\textcite{%l}")
     (?P . "[@%l]")
     (?T . "@%l [p. ]")
     (?x . "[]{%l}")
     (?X . "{%l}"))))

(use-package delsel
  :ensure nil
  ;; remove the selection when you start typing with an active selection
  :hook (after-init . delete-selection-mode))

(use-package dbus
  :ensure nil
  :functions liesnikov/set-theme liesnikov/detect-and-switch-theme
  :init
  ;; not significant that's not in :config, we just don't need anything from dbus
  (defun liesnikov/set-theme (&optional name)
    "Automatically switch themes.
Detect xfce4 system theme (or NAME) and switch Emacs theme accordingly."
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
  :config
  (defun liesnikov/detect-and-switch-theme (servname setpath themename)
    (if
        (string= setpath
                 "/Net/ThemeName")
        (liesnikov/set-theme (car themename))))
  (dbus-register-signal
   :session
   nil ; service name, nil is a wildcard
   "/org/xfce/Xfconf" ; path
   "org.xfce.Xfconf" ; interface
   "PropertyChanged" ; message
   #'liesnikov/detect-and-switch-theme))

(use-package savehist
  ;; The built-in savehist package keeps a record of user inputs
  ;; and stores them across sessions.
  ;; Thus, the user will always see their latest choices closer to the top
  ;; (such as with M-x).
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrepz)
  :config
  :config
  (add-to-list 'xref-search-program-alist
               '(ripgrepz . "xargs -0 rg <C> --null -nH --no-heading --no-messages -g '!*/' -z -e <R>")))

(use-package which-key
  :ensure nil
  :defer t
  ;; provide a popup when you press a button with all bindings that follow
  :custom
  (which-key-min-display-lines 10)
  (which-key-mode t))

(use-package faces
  :ensure nil
  :custom-face
  (default ((t (:height 125
                :width condensed
                :foundry "ADBO"
                :family "Source Code Pro"))))
  (fixed-pitch ((t (:height 1.0
                    :foundry "ADBO"
                    :family "Source Code Pro"))))
  (variable-pitch ((t (:height 1.0
                       :foundry "ADBO"
                       :family "Source Sans 3")))))

;; re-evaluate this on restart if emacs gets stuck with wrong colours
;; to select the whole sexpr put carriage on the first parenthesis and press C-M-space
(use-package emacs
  :ensure nil
  ;; catch-all package for all the things that don't have their own package
  :custom
  ;; personal info
  (user-full-name "Bohdan Liesnikov")
  (user-mail-address "bohdan@liesnikov")

  ;; don't show startup emacs screen
  (inhibit-startup-screen t)
  ;; If the value is nil and ‘inhibit-startup-screen’ is nil, show the startup screen.
  ;; If t, open the ‘*scratch*’ buffer.
  (initial-buffer-choice t)
  ;; text present in the scratch buffer by default
  (initial-scratch-message nil)

  ;; If the value is greater than 100, redisplay will never recenter point,
  ;; but will always scroll just enough text to bring point into view,
  ;; even if you move far away.
  (scroll-conservatively 10000)

  ;; The number of lines to try scrolling a window by when point moves out.
  (scroll-step 1)

  ;; pressing tab always indents
  (tab-always-indent t)
  ;; display tab width
  (tab-width 2)

  ;; disable tool-bar
  (tool-bar-mode nil)

  ;; make mode-line line indicator be line-number:colon-number
  (mode-line-position (list "L%4l:C%3c")))

;; end of built-in packages

;;;# Installed packages

;;;## Package managment

;;;## Visual things

(use-package doom-themes
  ;; color theme
  :custom
  ;; Global settings (defaults)
  ;; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ;; if nil, bold is universally disabled
  (doom-themes-enable-italic t)
  :config
  ;; (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; actually set the correct theme when loading
  (liesnikov/set-theme))

(use-package minions
  :custom (minions-prominent-modes '(flymake-mode))
  ;; hide minor modes under a drop-down menu
  :config (minions-mode 1))

(use-package moody
  ;; modeline as tabs
  :custom
  (x-underline-at-descent-line t)
  (moody-mode-line-height 45)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package unicode-whitespace
  :requires whitespace
  :custom
  (unicode-whitespace-soft-space-mappings
   ;; this is the default except for "Space", initially it's also Middle dot
   '(("Space" ("Space"))
     ("Em Quad"
      ("Circled Bullet" "Middle Dot" "."))
     ("Em Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("En Quad"
      ("Circled Bullet" "Middle Dot" "."))
     ("En Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Figure Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Four-Per-Em Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Hair Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Ideographic Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Medium Mathematical Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Mongolian Vowel Separator"
      ("Circled Bullet" "Middle Dot" "."))
     ("Ogham Space Mark"
      ("Circled Bullet" "Middle Dot" "."))
     ("Punctuation Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Six-Per-Em Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Tag Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Thin Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Three-Per-Em Space"
      ("Circled Bullet" "Middle Dot" "."))
     ("Zero Width Space"
      ("Circled Bullet" "Middle Dot" "."))))
 :config
  (unicode-whitespace-setup 'subdued-faces))

(use-package emojify
  ;; enable emoji rendering where they are typeset as text :nonexistent: or :wave:
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode github))
  :hook
  (after-init-hook . global-emojify-mode))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "welcome back 👋")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content 't)
  (dashboard-show-shortcuts 't)
  (dashboard-projects-backend 'project-el)
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items))
  (dashboard-items '((projects . 5)
                     (recents  . 5)
                     (bookmarks . 5)
                     (registers . 5)))
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :config
  (dashboard-setup-startup-hook))

;; a minor mode to enable errors appearing next to the code
(use-package sideline
  :ensure t
  :hook (flymake-mode-hook . sideline-mode)
        (eglot-mode-hook . sideline-mode))

;; because there's no sideline for flymake by default
(use-package sideline-flymake
  :custom
  (sideline-flymake-display-mode 'line) ; 'point to show errors only on point
                                        ; 'line to show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

(use-package sideline-eglot
  :ensure t
  :custom
  (sideline-backends-right '(sideline-eglot)))

(use-package ultra-scroll
  :vc (:fetcher github
       :repo "jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 101) ; important!
  (scroll-margin 0)
  (ultra-scroll-mode 1))

;;;## General goodies

;;autocomplete
(use-package company
  :custom
  (global-company-mode 1)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t))

(use-package company-box
  :hook (company-mode-hook . company-box-mode))

(use-package company-quickhelp
  :custom
  (company-quickhelp-delay 1)
  :config
  (company-quickhelp-mode))

(use-package color-moccur
  ;; provide colours in occur mode
  :bind (("M-s O" . moccur)))

(use-package transpose-frame
  ;; turn frame around, somehow not available by default
  :defer t)

(use-package gnu-elpa-keyring-update
  ;; because elpa keys are expiring sometimes
  )

(use-package ibuffer-project
  ;; group ibuffer entries by the project
  :after project
  :hook
  (ibuffer-hook . (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative)))))

(use-package rg
  :defer t
  :commands rg
  ;; search package instead of grep
  :ensure-system-package (rg . ripgrep))

(use-package ivy
  ;; ivy is an autocompletion framework
  :bind (("C-x b" . ivy-switch-buffer))
  :custom
  ;; enable ivy
  (ivy-mode t)
  ;; use columns in ivy
  (ivy-read-action-format-function 'ivy-read-action-format-columns)
  ;; technically not an ivy variable, but useful to have for some reasons atm unknown to me
  (enable-recursive-minibuffers t)
  ;; add recent files and/or bookmarks to ‘ivy-switch-buffer’.
  (ivy-use-virtual-buffers t)

  :functions issue-1755-fix
  :hook (shell-mode-hook  . issue-1755-fix)
        (eshell-mode-hook . issue-1755-fix)
  :init
  (defun issue-1755-fix ()
    ;; to override ivy-mode, which provides a problematic
    ;; completion-in-region variation:
    (setq-local completion-in-region-function #'completion--in-region)))
(use-package ivy-hydra
  :defer t
  :requires ivy)
(use-package ivy-rich
  :defer t
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


(use-package expand-region
  ;; expand selection semantically
  :bind
  (("M-=" . 'er/expand-region))
  )

(use-package pdf-tools
  ;; view pdfs in emacs
  :config
  (pdf-tools-install) ; enable pdftools instead of docview

  :hook
  ;; Disable line numbers when entering pdf-tools mode.
  ;; from https://stackoverflow.com/a/6839968
  (pdf-view-mode-hook . (lambda ()
                          (add-hook 'after-change-major-mode-hook
                                    (lambda () (display-line-numbers-mode 0))
                                    :append :local))))

(use-package evil
  :defer t
  :custom
  (evil-mode nil))

(use-package evil-numbers
  ;; increment-decrement numbers as in vim
  :bind (("C-c +"   . evil-numbers/inc-at-pt)
         ("C-c -"   . evil-numbers/dec-at-pt)
         ("C-c C-+" . evil-numbers/inc-at-pt-incremental)
         ("C-c C--" . evil-numbers/dec-at-pt-incremental)))

(use-package avy
  ;; move around efficiently
  :bind (("M-g c"   . avy-goto-char-timer)
         ("M-g g"   . avy-goto-line)
         ("M-g M-g" . avy-goto-line))
  :config
  (avy-setup-default))

(use-package undo-tree
  :bind ("C-/" . undo-tree-undo) ; default binding, but forcing the defer this way
  :config
  (global-undo-tree-mode)
  (defun undo-tree-fix/undo-tree-compress (filename)
    (concat filename ".gz"))
  (advice-add 'undo-tree-make-history-save-file-name :filter-return
              #'undo-tree-fix/undo-tree-compress))

(use-package envrc
  :ensure-system-package direnv
  ;; package for direnv, usefull when working with nix
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

(use-package sort-words
  :commands sort-words liesnikov/sort-split
  :functions liesnikov/sort-split
  :config
  (defun liesnikov/sort-split ()
    "Sort and split words per line.
This function sort words in alphabetical order in the currently selected region
and inserts a newline before every new letter of the alphabet.
So that in the end each line has words starting with the same letter"
    (interactive)
    ;; if the region is not selected choose current line
    (let ((beg (if (region-active-p) (region-beginning) (line-beginning-position)))
          (end (if (region-active-p) (region-end) (line-end-position)))
          (abc (number-sequence 97 122)))
      (require 'sort-words)
      (sort-words beg end)
      (goto-char beg)
      (dolist (letter abc)
        (when (re-search-forward (format " %c" letter) end t)
          (backward-char 2)
          (delete-char 1)
          (open-line 1)
          (forward-line 1))))))

(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package trashed
  :commands trashed
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package yasnippet
  :commands yas-expand
  :bind (:map yas-minor-mode-map
              ("C-c & e" . yas-expand))
  :custom
  (yas-global-mode t)
  :config
  (unbind-key "TAB" yas-minor-mode-map))
(use-package yasnippet-snippets
  :requires yasnippet)
(use-package ivy-yasnippet
  :requires ivy yasnippet
  :bind (:map yas-minor-mode-map
              ("C-c e" . ivy-yasnippet)))

;;;## Writing & reading

;; word processor and markup

(use-package wc-mode
  ;; count words in the buffer
  :commands wc-mode)

(use-package tex
  :defer t
  ;; loading auctex directly doesn't work for some reason
  ;; https://github.com/jwiegley/use-package/issues/379
  :ensure auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))

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
  (latex-mode-hook . turn-on-reftex))

(use-package company-math
  :after company tex-mode
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package company-auctex
  :after company tex-mode
  :config
  (company-auctex-init))

(use-package olivetti
  ;; package for writing mode, introduces margins
  ;; for search purposes: org-mode
  :custom
  (olivetti-body-width 90)
  :commands olivetti
  :config
  (defun olivetti ()
    (interactive)
    (call-interactively #'olivetti-mode )))

(use-package flymake-vale
  :ensure-system-package vale
  :vc (:fetcher github
       :repo "tpeacock19/flymake-vale")
  :hook
  (find-file-hook . flymake-vale-maybe-load)
  :config
  ;; flymake-vale-modes defaults to:
  ;;  => (text-mode latex-mode org-mode markdown-mode message-mode)
  (add-to-list 'flymake-vale-modes 'adoc-mode))

;;(use-package nov
;;  ;; Major mode for reading EPUB documents
;;  :config
;;  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package markdown-mode
  :config
  ;; from https://gist.github.com/kleinschmidt/5ab0d3c423a7ee013a2c01b3919b009a
  ;; define markdown citation formats
  (defvar markdown-cite-format)
  ;; these are the only two formats available:
  ;; https://pandoc.org/MANUAL.html#extension-citations
  (setq markdown-cite-format
        '((?\C-m . "[@%l]")
          (?p    . "[@%l]")
          (?t    . "@%l")))

  ;; wrap reftex-citation with local variables for markdown format
  (defun markdown-reftex-citation ()
    (interactive)
    (let ((reftex-cite-format markdown-cite-format)
          (reftex-cite-key-separator "; @"))
      (reftex-citation)))
  :hook
  ;; bind modified reftex-citation to C-c[, without enabling reftex-mode
  ;; https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
  (markdown-mode-hook . (lambda ()
                          (define-key markdown-mode-map "\C-c["
                            'markdown-reftex-citation))))

(use-package org
  :bind (:map org-mode-map
          ;; some default and non-default mappings
          ;; * to insert current date use org-time-stamp bound to C-c .
          ;; * to org-global-cycle use C-u TAB
          ;; * to store a link to an org heading use C-c l
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
  ;; activate org-indent-mode on org-indent
  (org-mode-hook . org-indent-mode))

(use-package org-download
  :defer t
  :requires org)
(use-package org-present
  :defer t
  :requires org)
(use-package org-modern
  :defer t
  :requires org
  :hook
  ((org-mode-hook . org-modern-mode)
   (org-agenda-finalize-hook . org-modern-agenda)))

(use-package org-modern-indent
  :defer t
  :config ; add late to hook
  ;; because of the depth argument can't use :hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;;;## Programming


;;;### misc
(use-package magit
  ;; emacs git interface
  :custom
  ;; highlight word-differences in diffs
  (magit-diff-refine-hunk (quote all))
  :bind (;; magit open default window binding
         ("C-x g" .  magit-status)))

(use-package git-link
  :bind ("C-c g l" . git-link-dispatch)
  :custom
  ;; Link to a particular revision of a file rather than using the branch name in the URL.
  (git-link-use-commit t))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

;; Language server protocol
(use-package eglot
  :hook
  (haskell-mode-hook . eglot-ensure)
  (sh-mode-hook . eglot-ensure)
  (bash-ts-mode-hook . eglot-ensure)
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  :config
;;    (setq-default eglot-workspace-configuration
;;                '((haskell
;;                   (plugin
;;                    (hlint
;;                     (globalOn . t))))))
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
  )

(use-package eldoc-box
  :hook (eglot-managed-mode-hook . eldoc-box-hover-mode))

;; for breadcrumbs modeline
(use-package breadcrumb
  :config
  (breadcrumb-mode))

;; in some cases more functional than eglot, albeit slower
;;(use-package lsp-mode
;;  :custom
;;  (lsp-file-watch-threshold nil))
;;(use-package lsp-ui)

(use-package eglot-booster
  :after eglot
  :requires lsp-booster
  :vc (:fetcher github :repo "jdtsmith/eglot-booster")
  :ensure-system-package (emacs-lsp-booster . ">&2 echo 'Need to install emacs lsp booster manually'")
  :config (eglot-booster-mode))

(use-package treesit-langs
  :requires treesit)

(use-package treesit-auto
  :requires treesit
  :config
  (global-treesit-auto-mode))

(use-package treesit-ispell
  :requires treesit)

;; alternative to treesit built-in mode
;;(use-package tree-sitter
;;  :custom (global-tree-sitter-mode 't))
;;
;(use-package tree-sitter-langs
;;  :requires tree-sitter)

(use-package dockerfile-mode
  :defer t
  :mode "\\.dockerfile\\'")

(use-package copilot
  :hook (prog-mode-hook . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-<tab>" . copilot-tab))
  :custom
  (copilot-max-char-warning-disable t)
  (copilot-indent-offset-warning-disable t)
  :config
  (defun copilot-tab (arg)
    (interactive "P")
    (if (bound-and-true-p arg)
        (copilot-next-completion)
        (or (copilot-accept-completion)
            (copilot-complete)))))

(use-package ellama
  :custom
  (ellama-language "English")
  (ellama-keymap-prefix "C-c e l")
  (ellama-sessions-directory "~/.cache/emacs/ellama-sessions")
  :bind (:map ellama-command-map
         ("q c" . (lambda () (interactive) (ellama--cancel-current-request)))
         ("q q" . ellama--cancel-current-request-and-quit))
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
     (make-llm-ollama
      :chat-model "llama3.2" :embedding-model "llama3.2")))

(use-package noxml-fold
  :requires nxml-mode
  :hook
  (nxml-mode-hook . (lambda () (noxml-fold-mode 1))))

;;;### rust
(use-package rustic
  :mode ("\\.rs\\'")
  :requires eglot
  :custom (rustic-lsp-client 'eglot))

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
  :custom
  (haskell-process-type 'auto)
  (haskell-compiler-type 'auto)
  :hook
  (haskell-mode-hook . eglot-ensure)
  (haskell-mode-hook . interactive-haskell-mode))

(use-package company-cabal
  :autoload company-cabal
  :init
  (add-to-list 'company-backends 'company-cabal))

;;(use-package lsp-haskell
;;  :requires lsp-mode lsp-ui
;;  :hook
;;    (haskell-mode-hook . lsp-deferred)
;;    (haskell-literate-mode-hook . lsp-deferred)
;;  )

;;;### nix
(use-package nix-mode
  ;; activate nix-mode in .nix files
  :mode "\\.nix\\'")

;;### proof assistants
(use-package idris-mode
  :mode "\\.idr$" "\\.lidr$")
(use-package idris2-mode
  :commands idris2-mode idris2-ipkg-mode
  :mode "\\.idr$" ("\\.ipkg$" . idris2-ipkg-mode)
  :vc (:fetcher github :repo "idris-community/idris2-mode"))

;;;#### coq
(use-package proof-general
  :commands coq-mode
  :mode ("\\.v\\'" . coq-mode)
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

;;(add-to-list
;; due to a weird bug, both tokens from PG and company-coq are used
;; which results in "token undefined" errors when using PG ones
;; 'coq-mode-hook (unicode-tokens-use-shortcuts nil))

;;;#### agda
;; agda2 mode, gets appended by `agda-mode setup`
(defun agda-load ()
  (interactive)
  (load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate"))))

(agda-load)

;;; Commentary:

;; NB:

;; to stop wrapping lines: toggle-truncate-lines

;; occur mode (M-s o)

;; interactive regex building (M-x) bound to re-builder

;; text-mode menu bar (M-`) bound to tmm-menubar

;; artist-mode to draw ASCII diagrams: artist-mode

;; to open two different views on the same buffer: make-indirect-buffer

;;; Bindings:

;;; Code:

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.config/emacs/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init.el)
;;; init.el ends here
