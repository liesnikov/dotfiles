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
 '(custom-enabled-themes (quote (tsdh-light)))
 '(desktop-save-mode t)
 '(dired-async-mode t)
 '(dired-listing-switches "-al")
 '(eshell-load-hook (quote ((lambda nil (setenv "PAGER" "")))))
 '(gdb-many-windows t)
 '(global-linum-mode t)
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(keyboard-coding-system (quote utf-8-unix))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (company-auctex editorconfig company-coq ibuffer-projectile use-package idris-mode org-present color-moccur helm-projectile projectile ac-math auctex auto-complete auto-complete-auctex flycheck flycheck-haskell flycheck-mypy flycheck-pyflakes gnu-elpa-keyring-update haskell-mode helm lsp-haskell lsp-mode lsp-ui magit markdown-mode markdown-mode+ merlin nix-mode org org-download pandoc pandoc-mode pdf-tools proof-general sml-mode transpose-frame tuareg unicode-whitespace wc-mode)))
 '(safe-local-variable-values
   (quote
    ((eval let
           ((idris2-path
             (replace-regexp-in-string "
\\'" ""
(shell-command-to-string "which idris2"))))
           (unless
               (string= "" idris2-path)
             (setq idris-interpreter-path idris2-path)))
     (eval
      (lambda nil
        (add-to-list
         (quote auto-mode-alist)
         (quote
          ("\\.h\\'" . c++-mode))))))))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(selection-coding-system (quote utf-8))
 '(show-paren-mode t)
 '(tab-always-indent t)
 '(tab-width 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :width condensed :foundry "ADBO" :family "Source Code Pro")))))
