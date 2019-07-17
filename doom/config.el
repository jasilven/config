;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; keybindings
(map! (:map override
        "C-\\" #'+treemacs/toggle
        "C-s" #'save-buffer
        "C-S-\"" #'eshell
        :in "C-f" #'swiper
        :in "C-S-f" #'+default/search-project
        :in "C-S-p" #'+ivy/projectile-find-file
        "C-q" #'kill-buffer-and-window
        "M-l" #'sp-forward-slurp-sexp
        "M-h" #'sp-forward-barf-sexp
        :in "C-<tab>" #'switch-to-buffer
        :n "f" #'avy-goto-word-1
        ))

(map! :leader
      "j" #'counsel-imenu
      "e" #'next-error
      "w" #'save-buffer
      )
;; settings
(set-frame-font "Fira Code-14")
(set-frame-name "Editor")
(rainbow-delimiters-mode-disable)
(smartparens-global-mode t)
(evil-smartparens-mode t)
(line-number-mode nil)
;; (evil-smartparens-keybindings-mode t)
(show-smartparens-global-mode t)
(smartparens-strict-mode t)
(custom-set-faces
 '(cider-fringe-good-face ((t (:foreground "#34bd39")))))

;; enable smartparens strict
(add-hook! prog-mode
  (turn-on-smartparens-strict-mode)
  (evil-smartparens-mode))

;; disable rainbow
(add-hook! cider-mode
  (rainbow-delimiters-mode-disable))
(add-hook! clojure-mode
  (rainbow-delimiters-mode-disable))
(add-hook! emacs-lisp-mode
  (flycheck-mode -1)
  (rainbow-delimiters-mode-disable))
;; defaults
(setq-default
 company-idle-delay 0.3
 treemacs-width 28
 truncate-lines t
 doom-modeline-height 14
 doom-modeline-icon nil
 nrepl-hide-special-buffers t
 cljr-warn-on-eval nil
 cider-prompt-for-symbol nil
 nrepl-hide-special-buffers t
 cider-repl-display-help-banner nil
 cider-show-error-buffer 'only-in-repl
 cider-auto-select-error-buffer  t
 cider-stacktrace-default-filters '(tooling dup java REPL)
 cider-save-file-on-load t
 projectile-project-search-path '("~/dev" "~/config"))
