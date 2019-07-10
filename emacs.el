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
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; editor modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(hl-line-mode 1)
(line-number-mode 1)
(global-eldoc-mode -1)
(global-auto-revert-mode t)
(pixel-scroll-mode 1)
(display-line-numbers-mode -1)

;; editor settings
(set-frame-font "Fira Code-14")
(set-frame-name "Editor")
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default
 term-scroll-show-maximum-output t
 term-scroll-to-bottom-on-output t
 blink-cursor-blinks 500
 clean-buffer-list-delay-general 1
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 confirm-kill-processes nil
 cursor-in-non-selected-windows nil
 custom-safe-themes t 
 default-fill-column 80
 delete-old-versions -1
 eldoc-echo-area-use-multiline-p nil
 eshell-scroll-show-maximum-output t
 eshell-cmpl-cycle-completions nil
 font-lock-builtin-face nil
 font-lock-type-face nil
 font-lock-variable-name-face nil
 indent-tabs-mode nil
 inhibit-startup-screen t
 initial-scratch-message nil
 mac-right-option-modifier nil
 make-backup-files nil 
 midnight-period 7200
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(3)
 ring-bell-function 'ignore
 scalable-fonts-allowed t
 scroll-conservatively 10000
 scroll-step 1
 sentence-end-double-space nil
 set-default-coding-systems 'utf-8
 set-language-environment "UTF-8"
 shell-file-name "zsh"
 show-paren-style 'parentheses
 tab-width 4
 truncate-lines nil
 vc-follow-symlinks t
 vc-make-backup-files -1
 version-control t
 indicate-empty-lines t
 x-select-enable-clipboard t
 kill-buffer-query-functions nil
 ) 

;; themes
(use-package solarized-theme :ensure t)
(use-package base16-theme :ensure t)
(use-package doom-themes :ensure t)
(use-package zenburn-theme :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)
(load-theme 'base16-tomorrow-night)

;; '(font-lock-doc-face ((t (:foreground "#b5bd68"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#888888"))))
 '(font-lock-doc-face ((t (:foreground "#b5bd68"))))
 '(git-gutter:added ((t (:foreground "#50fa7b" :background "#50fa7b"))))
 '(git-gutter:deleted ((t (:foreground "#ff79c6" :background "#ff79c6"))))
 '(git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
 '(highlight ((t (:background "#282a2e" :underline t :foreground "#fdf4c1" :inverse-video nil))))
 '(ivy-minibuffer-match-face-1 ((t (:weight bold :foreground "#de935f" :background nil))))
 '(ivy-minibuffer-match-face-2 ((t (:weight bold :foreground "#de935f" :background nil))))
 '(ivy-minibuffer-match-face-3 ((t (:weight bold :foreground "#de935f" :background nil))))
 '(ivy-minibuffer-match-face-4 ((t (:weight bold :foreground "#de935f" :background nil)))))

;; yaml
(use-package json-mode :ensure t)

;; highlight-symbol
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.7))

;; neotree
(use-package neotree :ensure t)

;; company
(use-package company
  :ensure t
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
        ("C-n" . company-selection)
        ("<RET>" . company-complete-selection)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-common-or-cycle)
        :map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1))

;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-enable-snippet nil)
  (require 'lsp-clients))
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

;; company lsp
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-insert-state-cursor '((bar . 4)))
  (evil-mode 1)
  :config
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))
  (use-package evil-collection
    :ensure t
    :after evil
    :config (evil-collection-init))
  (setq evil-escape-unordered-key-sequence t)
  (setq evil-move-cursor-back nil))

;; flycheck
(use-package flycheck-inline :ensure t)
(use-package flycheck
  :ensure t)
(use-package flycheck-pos-tip :ensure t)
(use-package flycheck-joker :ensure t)
(use-package flycheck-rust
  :ensure t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; clojure and cider
;;(use-package flycheck-clj-kondo :ensure t)
;; (use-package flycheck-clojure :ensure t)
(use-package flycheck-joker :ensure t)
(use-package clj-refactor :ensure t)
(use-package clojure-mode
  :ensure t
  :config
  (use-package cider
    :ensure t
    :bind
    (:map cider-mode-map ("C-x C-x" . cider-eval-last-sexp))
    (:map cider-mode-map ("C-c h" . cider-doc))
    (:map cider-mode-map ("C-=" . cider-format-buffer))
    (:map cider-mode-map ("<f1>" . cider-doc))
    (:map cider-mode-map ("<f2>" . cljr-rename-symbol))
    :config
    (require 'flycheck-joker)
    (setq nrepl-hide-special-buffers t)
    (setq cljr-warn-on-eval nil)
    (setq cider-prompt-for-symbol nil)
    (setq nrepl-hide-special-buffers t)
    (setq cider-repl-display-help-banner nil)
    (setq cider-auto-select-error-buffer t)
    (setq cider-stacktrace-default-filters '(tooling dup java REPL))
    (setq cider-save-file-on-load t)
    (general-define-key :keymaps 'cider-mode-map :states '(normal) "K" 'cider-doc))
  :init 
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'hl-line-mode)
  (add-hook 'cider-mode-hook #'highlight-symbol-mode)
  (add-hook 'cider-mode-hook #'flycheck-mode)
  (add-hook 'cider-mode-hook #'hs-minor-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'clj-refactor-mode))

;; rust
(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . lsp)
  :bind
  (:map rust-mode-map ("C-c C-k" . cargo-process-check))
  (:map rust-mode-map ("C-c k" . cargo-process-check))
  (:map rust-mode-map ("<f1>" . lsp-ui-doc-show))
  :config
  (highlight-symbol-mode t)
  (general-define-key :keymaps 'rust-mode-map :states '(normal) "K" 'lsp-ui-doc-show)
  (add-hook 'rust-mode-hook #'hs-minor-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'flycheck-pos-tip-mode)
  (setq rust-format-on-save t))
(use-package cargo
    :ensure t
    :hook (rust-mode . cargo-minor-mode)
    :config (setq compilation-ask-about-save nil))

;; avy
(use-package avy :ensure t)

;; restclient
(use-package restclient :ensure t)

;; disable minor modes in modeline
(use-package rich-minority
  :ensure t
  :config 
  (rich-minority-mode 1)
  (setf rm-blacklist ""))

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

;; projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy))
(use-package projectile-ripgrep :ensure t)
(use-package counsel-projectile :ensure t)

;; smartparens
(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside t)
  (smartparens-global-mode)
  (require 'smartparens-config))

;; general keys
(use-package general
  :ensure t
  :bind
  (:map emacs-lisp-mode-map ("C-x C-x" . eval-last-sexp))
  :config
  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-unset-key (kbd "C-w"))
  (global-set-key (kbd "C-q") 'kill-buffer-and-window)
  (global-set-key (kbd "C-w C-w") 'other-window)
  (general-define-key
   :states '(normal visual insert emacs)
   "C-s"   'save-buffer
   "C-x C-n"   'neotree-toggle
   "C-'"  '(lambda () (interactive (ansi-term "zsh")))
   "M-x"   'helm-M-x
   "C-f"   'swiper
   "C-S-f"   'counsel-projectile-rg
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
   "e"   'next-error
   "o"   'delete-other-windows
   "f"   'counsel-git
   "g"   'swiper
   "j"   'counsel-imenu
   ))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; git gutter
(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-echo-delay 0 t)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (json-mode yaml-mode git-gutter smartparens helm-swoop helm-rg rich-minority restclient evil-collection key-chord color-theme-sanityinc-tomorrow gruvbox-theme zenburn-theme doom-themes solarized-theme exec-path-from-shell flycheck-inline base16-theme flycheck-clj-kondo highlight-symbol shell-pop neotree counsel-projectile projectile-ripgrep projectile flychech-pos-tip company-lsp yasnippt flycheck-rust cargo lsp-ui lsp-mode magit flycheck-pos-tip flycheck-clojure flycheck-joker cider evil-leader paredit-mode clj-refactor clojure-mode helm avy general use-package)))
 '(pixel-scroll-mode t))

