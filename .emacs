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
(use-package auto-complete)
(use-package auto-complete-auctex)

(use-package color-moccur)
(use-package transpose-frame)

;; because elpa keys are expiring sometimes
(use-package gnu-elpa-keyring-update)

;; helm is buffer search
(use-package helm
  :custom
  (helm-mode t)
  (helm-completion-style (quote helm-fuzzy)))
(use-package helm-projectile)

;; projectile organizes buffers in projects
(use-package projectile
  :custom
  (projectile-mode t nil (projectile)))
(use-package ibuffer-projectile)

;; view pdfs in emacs
(use-package pdf-tools)

;; highlight all whitespaces
(use-package unicode-whitespace)

;; Flycheck
(use-package flycheck)
(use-package flycheck-haskell)
(use-package flycheck-mypy)
(use-package flycheck-pyflakes)

;; Language server protocol
(use-package lsp-haskell)
(use-package lsp-mode)
(use-package lsp-ui)

;; word processor and markup
(use-package wc-mode)

;; loading auctex directly doesn't work for some reason
;; https://github.com/jwiegley/use-package/issues/379
(use-package tex-mode
  :ensure auctex)
(use-package company-auctex)
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
     org-info org-irc org-mhe org-rmail org-tempo org-w3m))))
(use-package org-download)
(use-package org-present)

;; Programming
(use-package magit)
(use-package editorconfig
  :custom
  (editorconfig-mode t))

;; ocaml
(use-package tuareg
  ;; activate tuareg (ocaml) mode in ml4 files
  ;; (syntax extensions for coq)
  :mode "\\.ml4\\'")
(use-package sml-mode)
(use-package merlin)

;; haskell
(use-package haskell-mode)

;; nix
(use-package nix-mode
  :mode "\\.nix\\'" ;; activate nix-mode in .nix files
  )

;; proof assistants
(use-package idris-mode)

(use-package company-coq)
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
;; map C-x C-f to helm-find-files instead of default `find-file`
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; map C-x C-f to helm-mini instead of default `switch-buffers`
(global-set-key (kbd "C-x b") 'helm-mini)
;; map C-x C-b to ibuffer instead of default `list-buffers`
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; magit open default window binding
(global-set-key (kbd "C-x g") 'magit-status)
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

(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)


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

;; detect xfce4 theme and switch emacs theme accordingly
(defun set-system-theme ()
  (interactive)
  (let ((command "xfconf-query -c xsettings -p /Net/ThemeName")
        (expected-value "Arc-Dark\n")
        (dark-theme 'wombat)
        (light-theme 'tsdh-light))
    (if
        (string= (shell-command-to-string command)
                 expected-value)
      (load-theme dark-theme t)
      (load-theme light-theme t))))
(set-system-theme)


;; Disable linum (line numbers) when entering pdf-tools mode.
(defun my-inhibit-global-linum-mode ()
  (add-hook
   ;; Counter-act `global-linum-mode'
   'after-change-major-mode-hook
   (lambda () (linum-mode 0))
   :append :local))
(add-hook 'pdf-view-mode-hook 'my-inhibit-global-linum-mode)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(unicode-whitespace-setup 'subdued-faces)
(global-flycheck-mode)
(ac-config-default)
(global-auto-complete-mode t) ;; doesn't really work, enables auto-fill-mode globally
(pdf-tools-install) ;; enable pdftools instead of docview
(require 'helm-config) ;; enable helm config

(provide '.emacs)
;;; .emacs ends here
