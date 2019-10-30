;;; package -- summary
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

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
 '(coq-maths-menu-enable nil)
 '(coq-unicode-tokens-enable nil)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(desktop-save-mode t)
 '(dired-async-mode t)
 '(eshell-load-hook (quote ((lambda nil (setenv "PAGER" "")))))
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode t)
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
 '(keyboard-coding-system (quote utf-8-unix))
 '(org-cycle-include-plain-lists (quote integrate))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-selected-packages
   (quote
    (merlin lsp-mode helm flycheck company-coq editorconfig flycheck-pycheckers haskell-mode lsp-ui magit org-download pandoc pandoc-mode pdf-tools wc-mode tuareg sml-mode rmsbolt auctex lsp-haskell flycheck-haskell unicode-whitespace flycheck-mypy flycheck-pyflakes transpose-frame markdown-mode+ markdown-mode org ac-math auto-complete auto-complete-auctex color-theme-solarized nix-mode origami)))
 '(proof-three-window-enable nil)
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

;;; Bindings:
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;; map C-x C-f to helm-find-files instead of default `find-file`
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; map C-x C-b to ibuffer instead of default `list-buffers`
(global-set-key (kbd "C-x g") 'magit-status) ;; magit open default window binding
;; new bindings to change widnow sizes
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;; Code:

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)) ;; activate nix-mode in .nix files
(add-to-list 'auto-mode-alist '("\\.ml4\\'" . tuareg-mode)) ;; activate tuareg (ocaml) mode in ml4 files (syntax extensions for coq)
(add-hook 'org-mode-hook 'org-indent-mode) ;; activate org-indent-mode on org-indent
(add-hook 'coq-mode-hook #'company-coq-mode) ;; company-coq is an addon on top of proofgeneral, enable it as we enter coq mode


;; Disable linum (line numbers) when entering pdf-tools mode.
(defun my-inhibit-global-linum-mode ()
  (add-hook 'after-change-major-mode-hook ; Counter-act `global-linum-mode'
            (lambda () (linum-mode 0))
            :append :local))
(add-hook 'pdf-view-mode-hook 'my-inhibit-global-linum-mode)


(setq inhibit-startup-screen t) ;; remove splash screen on start
(setq scroll-step            1  ;; number of lines screen shifts when scrooling
      scroll-conservatively  10000)

;; Windmove is a library built into GnuEmacs starting with version 21.
;; It lets you move point from window to window using Shift and the arrow keys.
;; https://www.emacswiki.org/emacs/WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-origami-mode 1) ;; add folds as in vim
(unicode-whitespace-setup 'subdued-faces)
(global-flycheck-mode)
(ac-config-default)
(global-auto-complete-mode t) ;; doesn't really work, enables auto-fill-mode globally
(menu-bar-mode -1) ;; no menubar (the thing w/ icons
(show-paren-mode 1)
(global-linum-mode 1) ;; enable global linum (line numbers) mode
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; no scrollbar
(editorconfig-mode 1) ;; enable editorconfig, for respecting editorconfig files
(pdf-tools-install) ;; enable pdftools instead of docview
(require 'helm-config) ;; enable helm config
(helm-mode 1) ;; enable helm mode
(provide '.emacs)
;;; .emacs ends here
