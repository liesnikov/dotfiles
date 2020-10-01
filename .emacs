;;; package -- summary
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "/home/buzzer/.emacs.d/elpa/use-package-20200721.2156")
  (require 'use-package))

;;; Use-package
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
  (projectile-mode t nil (projectile)))
(use-package ibuffer-projectile)

(use-package rg)

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
         ("<tab>" . helm-ff-TAB)
         ("M-x" . helm-select-action))
  :config
  (helm-mode 1)
  (require 'helm-config)
  (use-package helm-projectile)
  (use-package helm-rg)
)

;; view pdfs in emacs
(use-package pdf-tools
  :config
  ;; enable pdftools instead of docview
  (pdf-tools-install))

;; highlight all whitespaces
(use-package unicode-whitespace
  :custom
  (global-whitespace-mode t)
  (global-whitespace-newline-mode t)
  :config
  (unicode-whitespace-setup 'subdued-faces))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  (use-package flycheck-haskell)
  (use-package flycheck-mypy)
  (use-package flycheck-pyflakes)
  )

;; Language server protocol
(use-package lsp-mode
  :config
  (use-package lsp-ui)
  (use-package lsp-haskell))

;; word processor and markup
(use-package wc-mode)

;; loading auctex directly doesn't work for some reason
;; https://github.com/jwiegley/use-package/issues/379
(use-package tex-mode
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook '(flyspell-mode t)))
(use-package company-auctex)
;; package for writing mode, introduces margins
(use-package olivetti
  :custom
  (olivetti-body-width 90)
  )


(use-package markdown-mode)
(use-package markdown-mode+)

(use-package pandoc)
(use-package pandoc-mode)

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
  (use-package org-present))


;; Programming
(use-package magit
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
(use-package merlin)

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
(use-package company-coq)

;;; Custom:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Commentary:

;; NB:
;; - copy current buffer's file path to kill-ring
;;   (kill-new buffer-file-name)
;; - toggle-truncate-lines
;; - occur mode (M-s o)


;;; Bindings:
;; map C-x C-b to ibuffer instead of default `list-buffers`
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; new bindings to change widnow sizes
;; similar bindings to windmove (see below), which has S-<arrow> as moving binding
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; Windmove is a library built into GnuEmacs starting with version 21.
;; It lets you move point from window to window using Shift and the arrow keys.
;; https://www.emacswiki.org/emacs/WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;; Code:
;; agda2 mode, gets appended by `agda-mode setup`
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(add-hook
 ;; from https://github.com/ocaml/merlin/wiki/emacs-from-scratch
 'tuareg-mode-hook 'merlin-mode)

(add-hook
  ;; activate org-indent-mode on org-indent
 'org-mode-hook 'org-indent-mode)
(add-hook
 ;; company-coq is an addon on top of proofgeneral,
 ;; enable it as we enter coq mode
 'coq-mode-hook #'company-coq-mode)
;;(add-to-list
 ;; due to a weird bug, both tokens from PG and company-coq are used
 ;; which results in "token undefined" errors when using PG ones
;; 'coq-mode-hook (unicode-tokens-use-shortcuts nil))
(add-hook 'ibuffer-hook
    ;; ibuffer-projectile automatic sorting
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

(defun set-system-theme ()
  "Detect xfce4 system theme and switch Emacs theme accordingly."
  (interactive)
  (let ((command "xfconf-query -c xsettings -p /Net/ThemeName")
        (expected-value "Arc\n")
        (dark-theme 'wombat)
        (light-theme 'leuven))
    (if
        (string= (shell-command-to-string command)
                 expected-value)
        (progn (load-theme light-theme t)
               (disable-theme dark-theme))
        (progn (load-theme dark-theme t)
               (disable-theme light-theme)))))
(set-system-theme)


(defun my-inhibit-global-linum-mode ()
  "Disable linum (line numbers) when entering pdf-tools mode."
  (add-hook
   ;; Counter-act `global-linum-mode'
   'after-change-major-mode-hook
   (lambda () (linum-mode 0))
   :append :local))
(add-hook 'pdf-view-mode-hook 'my-inhibit-global-linum-mode)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide '.emacs)
;;; .emacs ends here
