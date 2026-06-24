(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auto-save-default nil)
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes '(default))
 '(custom-theme-allow-multiple-selections nil)
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(ellama-providers liesnikov/ellama-providers-alist nil nil "Can't set it earlier since the alist is only evaluated during load-time")
 '(package-selected-packages
   '(aider auctex auto-dark
     breadcrumb
     cape color-moccur consult consult-eglot copilot corfu
     dashboard dired-subtree dockerfile-mode doom-themes
     edit-indirect eglot-booster eglot-x eldoc-box ellama embark embark-consult emojify envrc evil-numbers expand-region
     flymake-collection forge
     git-link gnu-elpa-keyring-update
     haskell-ts-mode highlight-parentheses
     ibuffer-project idris-mode idris2-mode
     llm llm-ollama llm-openai
     magit marginalia markdown-mode merlin minions moody
     nix-mode no-littering noxml-fold
     olivetti orderless org-download org-modern org-modern-indent org-present
     pdf-tools proof-general
     rg rust-mode rustic
     sideline sideline-eglot sideline-flymake sort-words
     terminal-here texfrag transpose-frame trashed treesit-auto treesit-fold treesit-ispell treesit-langs tuareg
     ultra-scroll undo-tree unfill unicode-whitespace use-package-ensure-system-package
     vertico vterm
     wc-mode wgrep
     yasnippet yasnippet-snippets))
 '(package-vc-selected-packages
   '((eglot-x :url "https://github.com/nemethf/eglot-x")
     (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
     (idris2-mode :url
                  "https://github.com/idris-community/idris2-mode")
     (ultra-scroll :vc-backend Git :url
                   "https://github.com/jdtsmith/ultra-scroll")))
 '(safe-local-variable-values
   '((ispell-personal-dictionary
      . "/home/bohdan/delft/thesis/my-thesis/utils/.aspell.en.pws")
     (reftex-default-bibliography
      "/home/bohdan/delft/extended-elab/extended-elab/paper/bib.bib")
     (eval progn (olivetti-mode 't) (flyspell-mode 't)
           (flyspell-buffer) (flycheck-buffer)
           (ispell-change-dictionary "en_GB-w_accents"))
     (eval progn (olivetti-mode 't) (flyspell-mode 't)
           (flyspell-buffer) (flymake-mode)
           (ispell-change-dictionary "en_GB-w_accents"))
     (ispell-personal-dictionary
      . "/home/bohdan/delft/thesis/.aspell.en.pws")
     (ispell-personal-dictionary
      . "/home/bohdan/delft/polarity/spec/.aspell.en.pws")
     (eval progn (olivetti-mode 't) (flyspell-buffer)
           (ispell-change-dictionary "en_GB-w_accents"))
     (eval progn (olivetti-mode 't))
     (reftex-default-bibliography "./ref.bib"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
