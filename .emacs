;;; package -- summary
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
(use-package helm)
(use-package helm-projectile)

;; projectile organizes buffers in projects
(use-package projectile)
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

(use-package auctex)
(use-package company-auctex)
(use-package markdown-mode)
(use-package markdown-mode+)
(use-package pandoc)
(use-package pandoc-mode)

(use-package org)
(use-package org-download)
(use-package org-present)

;; Programming
(use-package magit)
(use-package editorconfig)

;; ocaml
(use-package tuareg)
(use-package sml-mode)
(use-package merlin)

;; haskell
(use-package haskell-mode)

;; nix
(use-package nix-mode)

;; proof assistants
(use-package idris-mode)

(use-package company-coq)
(use-package proof-general)

;;; Custom:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/.backup"))))
 '(calendar-week-start-day 1)
 '(coq-auto-adapt-printing-width t)
 '(coq-compile-auto-save (quote ask-coq))
 '(coq-compile-before-require nil)
 '(coq-compile-parallel-in-background t)
 '(coq-maths-menu-enable nil)
 '(coq-unicode-tokens-enable nil)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(desktop-save-mode t)
 '(dired-async-mode t)
 '(dired-listing-switches "-al")
 '(eshell-load-hook (quote ((lambda nil (setenv "PAGER" ""))))) ;; for eshell to dump outputs in terminal instead of starting pagers
 '(gdb-many-windows t)
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode t)
 '(helm-completion-style (quote emacs))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   (quote
    (("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(keyboard-coding-system (quote utf-8-unix))
 '(org-agenda-files nil)
 '(org-cycle-include-plain-lists (quote integrate))
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt)))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-eww org-gnus org-info org-irc org-mhe org-rmail org-tempo org-w3m)))
 '(package-selected-packages
   (quote
    (org-present color-moccur ibuffer-projectile helm-projectile projectile ac-math auctex auto-complete auto-complete-auctex company-coq editorconfig flycheck flycheck-haskell flycheck-mypy flycheck-pycheckers flycheck-pyflakes gnu-elpa-keyring-update haskell-mode helm lsp-haskell lsp-mode lsp-ui magit markdown-mode markdown-mode+ merlin nix-mode org org-download pandoc pandoc-mode pdf-tools proof-general rmsbolt sml-mode transpose-frame tuareg unicode-whitespace wc-mode)))
 '(projectile-mode t nil (projectile))
 '(proof-autosend-enable nil)
 '(proof-three-window-enable nil)
 '(proof-toolbar-enable t)
 '(safe-local-variable-values
   (quote
    ((eval
      (lambda nil
        (add-to-list
         (quote auto-mode-alist)
         (quote
          ("\\.h\\'" . c++-mode))))))))
 '(selection-coding-system (quote utf-8))
 '(tab-always-indent t)
 '(tab-width 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :width condensed :foundry "ADBO" :family "Source Code Pro")))))

;;; Commentary:

;; NB:
;; - copy current buffer's file path to kill-ring
;;   (kill-new buffer-file-name)
;; - toggle-truncate-lines
;; - occur mode (M-s o)


;;; Bindings:
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;; map C-x C-f to helm-find-files instead of default `find-file`
(global-set-key (kbd "C-x b") 'helm-mini) ;; map C-x C-f to helm-mini instead of default `switch-buffers`
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; map C-x C-b to ibuffer instead of default `list-buffers`
(global-set-key (kbd "C-x g") 'magit-status) ;; magit open default window binding
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
;; projectile binding



;;; Code:
;; agda2 mode, gets appended by `agda-mode setup`
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(add-hook
 ;; from https://github.com/ocaml/merlin/wiki/emacs-from-scratch
 'tuareg-mode-hook 'merlin-mode)
(add-to-list
 ;; activate nix-mode in .nix files
 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list
 ;; activate tuareg (ocaml) mode in ml4 files
 ;; (syntax extensions for coq)
 'auto-mode-alist '("\\.ml4\\'" . tuareg-mode))
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
        (expected-value "Arc-Dark\n"))
    (if
        (string= (shell-command-to-string command)
                 expected-value)
      (load-theme 'tsdh-dark t)
      (load-theme 'tsdh-light t))))
(set-system-theme)



;; Disable linum (line numbers) when entering pdf-tools mode.
(defun my-inhibit-global-linum-mode ()
  (add-hook
   ;; Counter-act `global-linum-mode'
   'after-change-major-mode-hook
   (lambda () (linum-mode 0))
   :append :local))
(add-hook 'pdf-view-mode-hook 'my-inhibit-global-linum-mode)


(setq inhibit-startup-screen t) ;; remove splash screen on start
(setq scroll-step            1  ;; number of lines screen shifts when scrooling
      scroll-conservatively  10000)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(unicode-whitespace-setup 'subdued-faces)
(global-flycheck-mode)
(ac-config-default)
(global-auto-complete-mode t) ;; doesn't really work, enables auto-fill-mode globally
(menu-bar-mode -1) ;; no menubar (the thing w/ icons)
(show-paren-mode 1)
(global-linum-mode 1) ;; enable global linum (line numbers) mode
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; no scrollbar
(editorconfig-mode 1) ;; enable editorconfig, for respecting editorconfig files
(pdf-tools-install) ;; enable pdftools instead of docview
(require 'helm-config) ;; enable helm config
(helm-mode 1) ;; enable helm mode
(projectile-mode +1) ;; enable projectile
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(provide '.emacs)
;;; .emacs ends here
