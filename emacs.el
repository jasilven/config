(setq package-enable-at-startup nil)

(setq package-archives '(("org"          . "http://orgmode.org/elpa/")
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; editor modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(hl-line-mode 0)
(global-eldoc-mode -1)

;; editor settings
(set-frame-font "Fira Code-14")
(set-frame-name "Editor")
(defalias 'yes-or-no-p 'y-or-n-p)
(setq blink-cursor-blinks 500)
(setq clean-buffer-list-delay-general 1)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq cursor-in-non-selected-windows nil)
(setq custom-safe-themes t) 
(setq default-fill-column 80)
(setq delete-old-versions -1)
(setq eldoc-echo-area-use-multiline-p nil)
(setq font-lock-builtin-face nil)
(setq font-lock-type-face nil)
(setq font-lock-variable-name-face nil)
(setq indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq mac-right-option-modifier nil)
(setq make-backup-files nil) 
(setq midnight-period 7200) ;; 2 hours
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(6))
(setq ring-bell-function 'ignore)
(setq scalable-fonts-allowed t)
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq sentence-end-double-space nil)
(setq set-default-coding-systems 'utf-8)
(setq set-language-environment "UTF-8")
(setq shell-file-name "zsh")
(setq show-paren-style 'parentheses)
(setq tab-width 4)
(setq truncate-lines t)
(setq vc-follow-symlinks t)
(setq vc-make-backup-files -1)
(setq version-control t)
(global-set-key (kbd "C-q") 'kill-buffer-and-window)

;; themes
(use-package solarized-theme :ensure t)
(use-package doom-themes :ensure t)

;; disable minor modes in modeline
(use-package rich-minority
  :ensure t
  :config 
  (rich-minority-mode 1)
  (setf rm-blacklist ""))

;; company
(use-package company
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind
  (:map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  :hook (after-init . global-company-mode))

;; evil
(use-package evil
  :ensure t
  :init (evil-mode)
  :config
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))
  (setq evil-escape-unordered-key-sequence t)
  (setq evil-move-cursor-back nil))

;; avy
(use-package avy :ensure t)

;; helm
(use-package helm
  :ensure t
  :config
  (helm-autoresize-mode t)
  (setq helm-display-source-at-screen-top nil)
  (set-face-attribute 'helm-source-header nil :height 1.0 :inherit 'font-lock-preprocessor-face)
  (setq helm-display-header-line nil)
  (setq helm-autoresize-max-height 23)
  (setq helm-autoresize-min-height 23)
  (use-package helm-rg :ensure t)
  (use-package helm-swoop :ensure t))

;; clojure and cider
(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (use-package cider
    :ensure t
    :bind
    (:map cider-mode-map ("C-x C-x" . cider-eval-last-sexp))
    (:map cider-mode-map ("C-c h" . cider-doc))
    (:map cider-mode-map ("<f1>" . cider-doc))
    :config
    (setq cider-prompt-for-symbol nil)
    (setq nrepl-hide-special-buffers t)
    (setq cider-repl-display-help-banner nil)
    (setq cider-save-file-on-load t)
    (general-define-key :keymaps 'cider-mode-map :states '(normal) "K" 'cider-doc)
    )
  (use-package clj-refactor)
  (use-package flycheck-joker :ensure t)
  :init 
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'clj-refactor-mode))

;; smartparens
(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside t)
  (smartparens-global-mode)
  (require 'smartparens-config))

;; rustic
(use-package rustic
  :ensure t
  :defer t
  :bind
  (:map rustic-mode-map ("C-c C-k" . rustic-cargo-check))
  (:map rustic-mode-map ("C-c k" . rustic-cargo-check))
  :config
  (setq rustic-rls-pkg 'eglot)
  (setq rustic-format-on-save t)
  (setq buffer-save-without-query t)
  (use-package eglot
    :ensure t
    :config
    (general-define-key :keymaps 'eglot-mode-map :states '(normal) "K" 'eglot-help-at-point)
    (general-define-key :keymaps 'rustic-mode-map :states '(normal) "K" 'eglot-help-at-point)
    :bind
    (:map eglot-mode-map ("C-c h" . eglot-help-at-point))
    (:map eglot-mode-map ("<f1>" . eglot-help-at-point))
    (:map eglot-mode-map ("<f2>" . eglot-rename))
    (:map eglot-mode-map ("<f3>" . xref-find-definitions))
    )
  (use-package flycheck :ensure t)
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (add-hook 'rustic-mode-hook #'flycheck-mode))


;; general keys
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   "C-s"   'save-buffer
   "C-q"   'kill-buffer-and-window
   "C-f"   'swiper
   "C-\""  '(lambda () (interactive (term "zsh")))
   "M-x"   'counsel-M-x
   "M-a"   'counsel-ag
   "M-g"   'counsel-git
   "C-b"   'ivy-switch-buffer
   "C-<tab>"   'ivy-switch-buffer
   "C-<right>"   'sp-forward-slurp-sexp
   "C-<left>"   'sp-forward-barf-sexp
   )
  (general-define-key :states '(normal) "f" 'avy-goto-char)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "w"   'save-buffer
   "o"   'delete-other-windows
   "f"   'counsel-find-files
   "j"   'counsel-imenu
   "TAB" '(switch-to-other-buffer)
   ))

;; git gutter
(use-package git-gutter
    :ensure t
    :custom
    (git-gutter:modified-sign "~")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
    :custom-face
    (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
    (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
    (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
    :config
    (global-git-gutter-mode +1))

;; ivy counsel
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t)
  (ivy-mode 1)
  (use-package counsel
    :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-echo-delay 0 t)
 '(company-idle-delay 0 t)
 '(company-minimum-prefix-length 1 t)
 '(custom-safe-themes
   (quote
    ("9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(package-selected-packages
   (quote
    (flycheck-pos-tip flycheck-clojure flycheck-joker helm-rg cider evil-leader paredit-mode clj-refactor clojure-mode helm avy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#50fa7b" :background "#50fa7b"))))
 '(git-gutter:deleted ((t (:foreground "#ff79c6" :background "#ff79c6"))))
 '(git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c")))))
