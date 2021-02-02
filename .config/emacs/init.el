;;; package -- summary
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(when (< emacs-major-version 27) (package-initialize))

;;; Custom:
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; Use-package

;; The :ensure-system-package keyword allows you to ensure system binaries exist alongside your package declarations.
(use-package use-package-ensure-system-package
  :ensure t)

;; ensure all packages -- installs them
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;;; Built-in packages, for neatness

(use-package windmove
  :ensure nil
  :bind (;; new bindings to change widnow sizes
         ;; similar bindings to windmove (see below),
         ;; which has S-<arrow> as moving binding
         ("S-C-<left>" . shrink-window-horizontally)
         ("S-C-<right>".  enlarge-window-horizontally)
         ("S-C-<down>" . shrink-window)
         ("S-C-<up>" . enlarge-window))
  :init
  ;; Windmove is a library built into GnuEmacs starting with version 21.
  ;; It lets you move point from window to window using Shift and the arrow keys.
  ;; https://www.emacswiki.org/emacs/WindMove
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

(use-package desktop
  :ensure nil
  :custom
  (desktop-base-lock-name "lock")
  (desktop-path '("~/.cache/emacs/desktop"))
  (desktop-auto-save-timeout 30)
  (desktop-save 0)
  :config
  (add-hook 'after-make-frame-functions
    (lambda (frame)
        (with-selected-frame frame
            (unless desktop-save-mode
                (desktop-save-mode 1)
                (desktop-read))))))

(use-package auto-complete
  :ensure nil
  :custom
  (ac-comphist-file "/home/buzzer/.cache/emacs/ac-comphist.dat"))

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file "~/.cache/emacs/recentf"))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-saved-filter-groups nil)
  (ibuffer-saved-filters nil)
  :bind (;; map C-x C-b to ibuffer instead of default `list-buffers`
         ("C-x C-b" . ibuffer)))


(use-package dired
  :ensure nil
  :config
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  :bind
  (:map dired-mode-map
    ("C-c o" . dired-open-file)))


;;; Installed packages
;; Visual things
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; General goodies
(use-package auto-complete
  :config
  (ac-config-default)
  ;; doesn't really work, enables auto-fill-mode globally)
  (global-auto-complete-mode t)
  (use-package auto-complete-auctex)
  (use-package ac-math))

(use-package which-key
  :config
  (which-key-mode))

(use-package color-moccur
  :bind (("M-s O" . moccur)))
(use-package transpose-frame)

;; because elpa keys are expiring sometimes
(use-package gnu-elpa-keyring-update)

;; projectile organizes buffers in projects
(use-package projectile
  :bind (:map projectile-mode-map
         ("C-x p". projectile-command-map))

  :custom
  (projectile-mode t nil (projectile))
  (projectile-cache-file "/home/buzzer/.cache/emacs/projectile.cache")

  :config
  (use-package ibuffer-projectile
    :config
    (add-hook 'ibuffer-hook
    ;; ibuffer-projectile automatic sorting
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))))

(use-package rg
  :ensure-system-package (rg . ripgrep))

;; helm is an autocompletion framework
(use-package helm
  :custom
  (helm-mode t)
  (helm-completion-style (quote helm-fuzzy))

  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("M-x" . helm-select-action)
         :map helm-find-files-map
         ("<tab>" . helm-execute-persistent-action)
         ("M-x" . helm-select-action))

  :config
  (use-package helm-projectile
    :config (helm-projectile-on))
  (use-package helm-rg)
  (helm-mode 1))

;; view pdfs in emacs
(use-package pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              ; Disable linum (line numbers) when entering pdf-tools mode.
              ; from https://stackoverflow.com/a/6839968
              (add-hook 'after-change-major-mode-hook
                        (lambda () (linum-mode 0))
                        :append :local)))
  ;; enable pdftools instead of docview
  (pdf-tools-install))

;; highlight all whitespaces
(use-package unicode-whitespace
  :custom
  (global-whitespace-mode 1)
  (global-whitespace-newline-mode 1)
  :config
  (setq whitespace-global-modes '(not agda2-mode))
  (unicode-whitespace-setup 'subdued-faces))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  (use-package flycheck-haskell)
  (use-package flycheck-mypy)
  (use-package flycheck-pyflakes))

;; Language server protocol
(use-package lsp-mode
  :config
  (use-package lsp-ui)
  (use-package lsp-haskell
    :config
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'haskell-doc-mode)))

;; word processor and markup
(use-package wc-mode)

;; loading auctex directly doesn't work for some reason
;; https://github.com/jwiegley/use-package/issues/379
(use-package tex-mode
  :ensure auctex
  :custom
  (reftex-plug-into-AUCTeX t)
  :config
  (use-package company-auctex
    :config
    (company-auctex-init))
  (add-hook 'TeX-mode-hook (lambda () (flyspell-mode t)))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook
          (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "\\(" "\\)"))))
  ;; Turn on RefTeX with AUCTeX LaTeX mode
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; with Emacs latex mode
  (add-hook 'latex-mode-hook 'turn-on-reftex))

;; package for writing mode, introduces margins
(use-package olivetti
  :custom
  (olivetti-body-width 90))


(use-package markdown-mode)

(use-package org
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
    (org-bbdb org-bibtex org-docview org-eww org-gnus
     org-info org-irc org-mhe org-rmail org-tempo org-w3m)))
  :config
  (use-package org-download)
  (use-package org-present)
  ;; activate org-indent-mode on org-indent
  (add-hook 'org-mode-hook 'org-indent-mode))


;; Programming
(use-package magit
  :custom
  ;; highlight word-differences in diffs
  (magit-diff-refine-hunk (quote all))
  :bind (;; magit open default window binding
         ("C-x g".  magit-status)))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

;; ocaml
(use-package tuareg
  ;; activate tuareg (ocaml) mode in ml4 files
  ;; (syntax extensions for coq)
  :mode "\\.ml4\\'")
(use-package merlin
  :config ;; from https://github.com/ocaml/merlin/wiki/emacs-from-scratch
  (add-hook 'tuareg-mode-hook 'merlin-mode))

;; haskell
(use-package haskell-mode)

;; nix
(use-package nix-mode
  ;; activate nix-mode in .nix files
  :mode "\\.nix\\'")

;; proof assistants
(use-package idris-mode)

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
  :init
  ;; company-coq is an addon on top of proofgeneral,
  ;; enable it as we enter coq mode
  (add-hook 'coq-mode-hook #'company-coq-mode))

;;; Commentary:
;; NB:
;; - copy current buffer's file path to kill-ring
;;   (kill-new buffer-file-name)
;; - toggle-truncate-lines
;; - occur mode (M-s o)


;;; Bindings:

;;; Code:
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
  "Detect xfce4 system theme and switch Emacs theme accordingly."
  (interactive)
  (let* ((command "xfconf-query -c xsettings -p /Net/ThemeName")
         (newtheme (or (concat name "\n")
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

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))

;; agda2 mode, gets appended by `agda-mode setup`
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.config/emacs/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init.el)
;;; init.el ends here
