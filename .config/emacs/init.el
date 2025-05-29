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
  :ensure nil
  :demand t
  :custom
  ;;(use-package-verbose 't)
  ;;(use-package-compute-statistics 't)
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
  :defer t
  :commands dired-open-file
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
  :defer t
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
    "Try to open magit status for the current project, or prompt for a project directory."
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
  :defer t
  :ensure nil
  :mode ("\\.\\(xml\\|xsd\\|wsdl\\)\\'")
  :functions xml-pretty-print
  :config
  (defun xml-pretty-print ()
    "Pretty print the XML content in the current buffer."
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
  ("C-g" . liesnikov/keyboard-quit-dwim)
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

(use-package editorconfig
  :ensure nil
  :custom
  (editorconfig-mode t))

(use-package compile
  :ensure nil
  :functions liesnikov/compile-on-save-start
  :defines liesnikov/compile-on-save-mode
  :hook
  (compilation-finish-functions . liesnikov/compile-bury-buffer-if-successful)
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
  (show-paren-mode t)
  ;; Show the matching paren right away
  (show-paren-delay 0))

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
  (whitespace-global-modes '(not magit-mode tex-mode org-mode agda2-mode)))

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
  :hook ((prog-mode-hook latex-mode LaTeX-mode) . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-g n" . flymake-goto-next-error)
              ("M-g p" . flymake-goto-prev-error))
  )

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
  :hook (after-init-hook . delete-selection-mode))

(use-package savehist
  ;; The built-in savehist package keeps a record of user inputs
  ;; and stores them across sessions.
  ;; Thus, the user will always see their latest choices closer to the top
  ;; (such as with M-x).
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package tramp
  :defer t
  :ensure nil
  :custom
  (tramp-fuse-unmount-on-cleanup 't))

(use-package xref
  :ensure nil
  :defer t
  :custom
  (xref-search-program 'ripgrepz)
  :config
  (add-to-list 'xref-search-program-alist
               '(ripgrepz . "xargs -0 rg <C> --null -nH --no-heading --no-messages -g '!*/' -z -e <R>")))

(use-package completion-preview
  :ensure nil
  :custom
  (completion-preview-minimum-symbol-length 3)
  (global-completion-preview-mode 't)
  :bind (:map completion-preview-active-mode-map
              ;; Bindings that take effect when the preview is shown:
              ;; Cycle the completion candidate that the preview shows
              ( ("M-n" . completion-preview-next-candidate)
                ("M-p" . completion-preview-prev-candidate)
                ;; Convenient alternative to C-i after typing one of the above
                ("M-i" . completion-preview-insert)))
  :config
  ;; Non-standard commands to that should show the preview:
  ;; Org mode has a custom `self-insert-command'
  ;; (push 'org-self-insert-command completion-preview-commands)
  ;; Paredit has a custom `delete-backward-char' command
  ;; (push 'paredit-backward-delete completion-preview-commands)
  )

(use-package which-key
  :ensure nil
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

(use-package ispell
  :ensure nil
  :custom
  (ispell-dictionary "en_GB-w_accents"))

;; re-evaluate this on restart if emacs gets stuck with wrong colours
;; to select the whole sexpr put carriage on the first parenthesis and press C-M-space
(use-package emacs
  :ensure nil
  ;; catch-all package for all the things that don't have their own package
  :custom
  ;; personal info
  (user-full-name "Dana Liesnikov")
  (user-mail-address "liesnikov@laptop")

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

  ;; display tab width
  (tab-width 2)

  ;; disable tool-bar
  (tool-bar-mode nil)

  ;; make mode-line line indicator be line-number:colon-number
  (mode-line-position (list "%3l:%2c"))

  (indicate-empty-lines 't)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  ;;(read-extended-command-predicate #'command-completion-default-include-p)
  (read-extended-command-predicate nil)
  )

;; end of built-in packages

;;;# Installed packages

;;;## Package managment

;;;## Visual things

(use-package auto-dark
  :defines
  auto-dark--dbus-xfce
  auto-dark--is-light-mode-xfce
  ;; auto-dark--set-theme ;; this is for me not to forget how to switch themes manually
  :custom
  (auto-dark-themes '((modus-videndi-tinted) (modus-operandi-tinted)))
  (auto-dark-detection-method 'dbus)
  ;; because we re-define auto-dark themes when doom is loaded
  :hook (after-init-hook . (lambda () (auto-dark-mode t)))
  :config
  ;; setup for xfce
  (defconst xfce-light-theme "Arc")
  (defun auto-dark--dbus-xfce (servname setpath themename)
    (if (string= setpath "/Net/ThemeName")
        (let ((appearance (if (string= themename xfce-light-theme)
                              'light 'dark)))
          (auto-dark--set-theme appearance))))
  (defun auto-dark--is-light-mode-xfce ()
    (let ((xfce-theme
           (shell-command-to-string
            "xfconf-query -c xsettings -p /Net/ThemeName")))
      (string= xfce-theme (concat xfce-light-theme "\n"))))
  ;; things to enable for xfce
  ;;(let ((appearance (if (auto-dark--is-light-mode-xfce) 'light 'dark)))
  ;;  (unless (eq appearance auto-dark--last-dark-mode-state)
  ;;    (auto-dark--set-theme appearance)))
  ;;(require dbus)
  ;;(dbus-register-signal
  ;; :session
  ;; nil ; service name, nil is a wildcard
  ;; "/org/xfce/Xfconf" ; path
  ;; "org.xfce.Xfconf" ; interface
  ;; "PropertyChanged" ; message
  ;; #'auto-dark--dbus-xfce))
  )

(use-package doom-themes
  :defer t
  ;; color theme
  :custom
  ;; Global settings (defaults)
  ;; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ;; if nil, bold is universally disabled
  (doom-themes-enable-italic t)
  ;; using auto-dark
  (auto-dark-themes '((doom-one) (doom-one-light)))
  :config
  ;; (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package minions
  :custom
  (minions-prominent-modes '(flymake-mode))
  ;; hide minor modes under a drop-down menu
  (minions-mode 1))

(use-package moody
  ;; modeline as tabs
  :custom
  (x-underline-at-descent-line t)
  (moody-mode-line-height 20)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package unicode-whitespace
  :defer t
  :after whitespace
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
  :hook
  (after-init-hook . dashboard-setup-startup-hook))

;; a minor mode to enable errors appearing next to the code
(use-package sideline
  :hook (flymake-mode-hook . sideline-mode)
        (eglot-mode-hook . sideline-mode))

;; because there's no sideline for flymake by default
(use-package sideline-flymake
  :commands sideline-flymake
  :custom
  (sideline-flymake-display-mode 'line) ; 'point to show errors only on point
                                        ; 'line to show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

(use-package sideline-eglot
  :defer t
  :commands sideline-eglot
  :custom
  (sideline-backends-right '(sideline-eglot)))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 101) ; important!
  (scroll-margin 0)
  (ultra-scroll-mode 1))

;;;## General goodies

;;autocomplete

(use-package corfu
  :custom
  ;; Make the popup appear quicker
  (corfu-popupinfo-delay '(0.5 . 0.5))
  ;; don't vary the width too much
  (corfu-min-width 40)
  (corfu-max-width 80)
  ;; only show a few completions at a tim
  (corfu-count 10)
  (corfu-scroll-margin 2)
  ;; Have Corfu wrap around when going up
  (corfu-cycle t)
  (corfu-preselect-first t)
  ;; Enable Corfu
  (global-corfu-mode t)
  ;; Enable Corfu history mode to act like `prescient'
  (corfu-history-mode t)
  ;; Allow Corfu to show help text next to suggested completion
  (corfu-popupinfo-mode t)
  ;; On the exact match still show the completion
  (corfu-on-exact-match 'show))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :defer nil
  :hook
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (completion-at-point-functions cape-file)
  (completion-at-point-functions cape-elisp-block)
  ;(completion-at-point-functions cape-history)
  )

(use-package color-moccur
  ;; provide colours in occur mode
  :bind (("M-s o" . moccur)))

(use-package transpose-frame
  ;; from emacs 31 it's built-in
  ;; https://p.bauherren.ovh/blog/tech/new_window_cmds
  ;; turn frame around, somehow not available by default
  :defer t)

(use-package gnu-elpa-keyring-update
  ;; because elpa keys are expiring sometimes
  )

(use-package ibuffer-project
  :defer t
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
  (ivy-use-virtual-buffers t))
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
  (counsel-find-file-at-point t)
  :bind
  (:map counsel-mode-map
        (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x f f" . counsel-find-file)
         ("C-x f r" . counsel-recentf))))


(use-package expand-region
  :commands ex/expand-region
  ;; expand selection semantically
  :bind
  (("M-=" . 'er/expand-region)))

(use-package pdf-tools
  ;; view pdfs in emacs
  :defer t
  ;:ensure-system-package elpa-pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  ; enable pdftools instead of docview
  (pdf-tools-install)
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
    "Compress the undo-tree history file on save."
    (concat filename ".gz"))
  (advice-add 'undo-tree-make-history-save-file-name :filter-return
              #'undo-tree-fix/undo-tree-compress))

(use-package envrc
  :ensure-system-package direnv
  ;; package for direnv, usefull when working with nix
  :commands envrc-allow
  :functions liesnikov/envrc-reload-or-clear
  :custom
  (envrc-global-mode t)
  :config
  (defun liesnikov/envrc-reload-or-clear ()
   "Clear the direnv environment and reload, if there's another one."
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
  (eshell-directory-change-hook . liesniov/envrc-reload-or-clear))

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
  :autoload yas--get-snippet-tables
  :bind-keymap ("C-c &" . yas-minor-mode-map)
  :bind ((:map yas-minor-mode-map
              ("C-c & TAB" . yas-expand))
         (:map cape-prefix-map
               ("y" . yas-insert-snippet)))
  :custom
  (unbind-key "TAB" yas-minor-mode-map)
  (yas-snippet-dirs
   (cons
    (concat user-emacs-directory "yasnippets")
    yas-snippet-dirs)))
(use-package yasnippet-snippets
  :defer t)

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
 '(TeX-view-program-selection
   '((output-pdf "xdg-open")
     (output-html "xdg-open")
     ((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv")
     (output-dvi "xdvi")))
  :hook
  (TeX-mode-hook . (lambda () (flyspell-mode t)))
  (LaTeX-mode-hook . LaTeX-math-mode)
  (LaTeX-mode-hook .
              (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "$" "$"))))
  ;; Turn on RefTeX with AUCTeX LaTeX mode and Emacs latex mode
  ((LaTeX-mode-hook latex-mode-hook). turn-on-reftex)
  ((LaTeX-mode-hook latex-mode-hook). flymake-mode)
  ((LaTeX-mode-hook latex-mode-hook). eglot-ensure)
  )

(use-package olivetti
  ;; package for writing mode, introduces margins
  ;; for search purposes: org-mode
  :custom
  (olivetti-body-width 90)
  :commands olivetti
  :config
  (defun olivetti ()
    "Toggle olivetti mode, but interactively."
    (interactive)
    (call-interactively #'olivetti-mode )))

;;(use-package nov
;;  ;; Major mode for reading EPUB documents
;;  :config
;;  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package markdown-mode
  :mode "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
  :hook
  (markdown-mode-hook . flymake-mode)
  (markdown-mode-hook . eglot-ensure)
  :custom (markdown-inline-image-overlays 't)
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

  (defun liesnikov/markdown-reftex-citation ()
    "Wrap reftex-citation with local variables for markdown format."
    (interactive)
    (let ((reftex-cite-format markdown-cite-format)
          (reftex-cite-key-separator "; @"))
      (reftex-citation)))
  ;; bind modified reftex-citation to C-c[, without enabling reftex-mode
  ;; https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
  :bind (:map markdown-mode-map
              ("C-c [" . liesnikov/markdown-reftex-citation)))

(use-package org
  :bind (:map org-mode-map
          ;; some default and non-default mappings
          ;; * to insert current date use org-time-stamp bound to C-c .
          ;; * to org-global-cycle use C-u TAB
          ;; * to store a link to an org heading use C-c l
          ("C-c l" . org-store-link))
  :custom
  (org-export-headline-levels 1)
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

;; Language server protocol
(use-package eglot
  :ensure nil
  :hook
  (sh-mode-hook . eglot-ensure)
  (bash-ts-mode-hook . eglot-ensure)
  (latex-mode-hook . eglot-ensure)
  :bind (:map eglot-mode-map
              (("C-c q" . eglot-code-action-quickfix)
               ("C-c c" . eglot-code-actions)))
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
  (add-to-list 'eglot-server-programs '((markdown-mode rst-mode html-mode org-mode) . ("vale-ls")))
  (add-to-list 'eglot-server-programs '((latex-mode LaTeX-mode tex-mode TeX-mode) . ("texlab")))
  )

(use-package eldoc-box
  :custom (eldoc-box-cleanup-interval 2)
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
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :ensure-system-package (emacs-lsp-booster . ">&2 echo 'Need to install emacs lsp booster manually'")
  :custom
  (eglot-booster-mode t))

(use-package treesit-langs
  :requires treesit
  :config
  (treesit-langs-major-mode-setup))

(use-package treesit-auto
  :requires treesit
  :config
  (global-treesit-auto-mode))

(use-package treesit-ispell
  :defer t
  :requires treesit)

(use-package treesit-fold
  :bind ("<backtab>" . treesit-fold-toggle)
  :hook
  (treesit-fold-mode-hook treesit-fold-indicators-mode)
  )

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
  :bind
  ("C-<tab>" . liesnikov/copilot-tab)
  (:map copilot-mode-map
        ("C-<tab>" . liesnikov/copilot-tab))
  :custom
  (copilot-max-char-warning-disable t)
  (copilot-indent-offset-warning-disable t)
  :config
  (defun liesnikov/copilot-tab (arg)
    "Smarter copilot autocompletion, if ARG is provided, go to the next completion, otherwise accept the current one."
    (interactive "P")
    ;; check if copilot mode is active
    (if (not (bound-and-true-p copilot-mode))
        (progn
          (message "Copilot mode wasn't active, activating now")
          (copilot-mode 't))
      ;; check if the argument is provided
      (if (bound-and-true-p arg)
          (copilot-next-completion)
        (or (copilot-accept-completion)
            (copilot-complete))))))

(use-package llm
  :defer t
  :commands make-llm-ollama)

(use-package ellama
  :defer t
  :custom
  (ellama-language "English")
  (ellama-keymap-prefix "C-c e l")
  (ellama-sessions-directory "~/.cache/emacs/ellama-sessions")
  (ellama-provider (make-llm-ollama :chat-model "llama3.2" :embedding-model "llama3.2"))
  :bind (:map ellama-command-map
         ("q c" . (lambda () (interactive) (ellama--cancel-current-request)))
         ("q q" . ellama--cancel-current-request-and-quit)))

(use-package noxml-fold
  :defer t
  :hook
  (nxml-mode-hook . (lambda () (noxml-fold-mode 1))))

;; (use-package paredit
;;   ;; overrides too many bindings, including M-?
;;   :defer t
;;   :bind (:map paredit-mode-map
;;               ("(" . paredit-open-round)
;;               (")" . paredit-close-round)
;;               ("[" . paredit-open-square)
;;               ("]" . paredit-close-square)
;;               ("{" . paredit-open-curly)
;;               ("}" . paredit-close-curly)
;;               ("\"" . paredit-doublequote)
;;               ("C-d" . paredit-delete-char)
;;               ("C-j" . paredit-newline)
;;               ("C-k" . paredit-kill)
;;               ("RET" . paredit-newline)
;;               ("DEL" . paredit-backward-delete))
;;   :hook ((emacs-lisp-mode-hook
;;           eval-expression-minibuffer-setup-hook
;;           ielm-mode-hook
;;           lisp-interaction-mode-hook
;;           lisp-mode-hook) . paredit-mode))

;; (use-package rainbow-delimiters
;;   ;; makes the colours weird, hard to spot parentheses
;;   :defer t
;;   :hook
;;   (prog-mode-hook . rainbow-delimiters-mode))

(use-package highlight-parentheses
  ;; dynamically highlights the parentheses surrounding point based on nesting-level
  :defer t
  :hook ((minibuffer-setup-hook
          prog-mode-hook) . highlight-parentheses-mode))

;;;### rust
(use-package rust-mode
  :defer t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :after (rust-mode)
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
;; copied from https://codeberg.org/pranshu/haskell-ts-mode
(use-package haskell-ts-mode
  :vc (:url "https://codeberg.org/pranshu/haskell-ts-mode" :rev "f047919915c02c814f8836ff3e2b2cfa326b2318")
  :mode "\\.hs\\'"
  :hook
  (haskell-ts-mode-hook . eglot-ensure)
  :config
  (add-to-list 'treesit-language-source-alist
               '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
  )

;;;### nix
(use-package nix-mode
  ;; activate nix-mode in .nix files
  :mode "\\.nix\\'"
  :hook
  (nix-mode-hook . eglot-ensure)
  :init
  (require 'eglot)
  (add-to-list 'eglot-server-programs '((nix-mode) . ("nixd")))
  )

;;### proof assistants
(use-package idris-mode
  :mode "\\.idr$" "\\.lidr$")
(use-package idris2-mode
  :commands idris2-mode idris2-ipkg-mode
  :mode "\\.idr$" ("\\.ipkg$" . idris2-ipkg-mode)
  :vc (:url "https://idris-community/idris2-mode"))

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

;; don't load eagerly, wait till we drop into an appropriate file
;;(agda-load)
(use-package agda2-mode
  :mode ("\\.l?agda\\'")
  :load-path
  (lambda () (file-name-directory (shell-command-to-string "agda-mode locate"))))

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

;; Screenshot to svg
(defun liesnikov/screenshot-svg ()
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

(defun liesnikov/kill-filename ()
  "Copy current buffer's file path to 'kill-ring'."
  (interactive)
  (kill-new buffer-file-name))

(defun liesnikov/get-filename-line-column (&optional full-path)
  "Get current buffer's file path and line/column location.
If FULL-PATH is non-nil use full path, otherwise relative."
  (require 'magit)
  (let ((line (current-line))
        (column (current-column))
        (filename (if full-path
                      (buffer-file-name)
                      (magit-file-relative-name))))
  (concat filename
          ":"
          (number-to-string line)
          ":"
          (number-to-string column))))

(defun liesnikov/kill-filename-line-column (&optional full-path)
  "Copy current buffer's file path to kill ring.
If FULL-PATH is non-nil use full path, otherwise relative."
  (interactive)
  (kill-new (liesnikov/get-filename-line-column full-path)))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.config/emacs/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init.el)
;;; init.el ends here
