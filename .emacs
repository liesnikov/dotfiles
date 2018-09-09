;;; package -- summary
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
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
 '(backup-directory-alist (quote (("." . "~/.emacs.d/.backup"))))
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode t)
 '(indent-tabs-mode nil)
 '(keyboard-coding-system (quote utf-8-unix))
 '(package-selected-packages
   (quote
    (unicode-whitespace flycheck-mypy flycheck-pycheckers flycheck-pyflakes flycheck evil-org transpose-frame markdown-mode+ markdown-mode org ac-math auto-complete auto-complete-auctex color-theme-solarized evil haskell-mode nix-mode origami)))
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

;;; Code:
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)) ;; activate nix-mode in .nix files
(add-hook 'org-mode-hook 'org-indent-mode) ;; activate org-indent-mode on org-indent

(setq inhibit-startup-screen t) ;; remove splash screen on start
(setq scroll-step            1  ;; number of lines screen shifts when scrooling
      scroll-conservatively  10000)

(global-origami-mode 1) ;; add folds as in vim
(unicode-whitespace-setup 'subdued-faces)
(global-flycheck-mode)
(evil-mode 1) ;; vi emulation
(ac-config-default)
(global-auto-complete-mode t) ;; doesn't really work, enables auto-fill-mode globally
(menu-bar-mode 1)
(show-paren-mode 1)
(global-linum-mode 1)
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; no scrollbar
(provide '.emacs)
;;; .emacs ends here
