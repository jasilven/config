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
(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; editor modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode nil)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(global-eldoc-mode -1)
(global-auto-revert-mode t)
(global-display-line-numbers-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'eshell-mode-hook (hl-line-mode -1))
(add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
;; font
(set-frame-font "Fira Code-14")
(set-frame-name "Editor")
(defalias 'yes-or-no-p 'y-or-n-p)
;; defaults
(setq-default
 gc-cons-upper-limit 536870912
 gc-cons-threshold 16777216
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
 truncate-lines t
 vc-follow-symlinks t
 vc-make-backup-files -1
 version-control t
 indicate-empty-lines t
 x-select-enable-clipboard t
 kill-buffer-query-functions nil
 )

(use-package key-chord :ensure t :config (key-chord-mode 1))
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push '(cider-repl-mode :height 12 :stick t) popwin:special-display-config))

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "~/bin/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package flycheck-plantuml
  :after (flycheck plantuml)
  :ensure t
  :config
  (flycheck-plantuml-setup))

(use-package evil
  :after key-chord
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-insert-state-cursor '((bar . 3) "red"))
  (setq evil-normal-state-cursor '(box "black"))
  (setq evil-emacs-state-cursor '(box "blue"))
  (evil-mode 1)
  :config
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))
  (setq evil-move-cursor-back nil))
;; (setq evil-escape-unordered-key-sequence t)

(use-package json-mode :ensure t :config (hs-minor-mode 1))
(use-package avy :ensure t)
(use-package restclient :ensure t)

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.7)
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode))

(use-package company
  :ensure t
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 1)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-enable-snippet nil)
  (require 'lsp-clients))

(use-package lsp-ui
  :after lsp
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

(use-package company-lsp
  :after company
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode))

;; (use-package flycheck-inline
;;   :after flycheck
;;   :ensure t
;;   :init
;;   (add-hook 'prog-mode-hook #'flycheck-inline-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'flycheck-pos-tip-mode))

(use-package clj-refactor :ensure t
  :after cider
  :config
  (setq cljr-warn-on-eval nil)
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package cider
  :after clojure-mode
  :ensure t
  :config
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer t)
  (setq cider-stacktrace-default-filters '(tooling dup java REPL))
  (setq cider-save-file-on-load t)
  (setq nrepl-hide-special-buffers t)
  (setq cider-clojure-cli-global-options "-A:bench")
  (define-key evil-normal-state-map (kbd "C-x C-x")
    (lambda () (interactive) (end-of-line) (cider-eval-last-sexp)))
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'hl-line-mode)
  (add-hook 'cider-mode-hook #'highlight-symbol-mode)
  (add-hook 'cider-mode-hook #'hs-minor-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'cider-repl-mode-hook #'evil-smartparens-mode)
  (use-package flycheck-joker :ensure t))

(use-package cargo
  :ensure t
  :after rust-mode
  :config (setq compilation-ask-about-save nil)
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :after rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . lsp)
  :config
  (highlight-symbol-mode t)
  (setq rust-format-on-save t)
  :init
  (add-hook 'rust-mode-hook #'hs-minor-mode))

;; disable minor modes in modeline
;; (use-package rich-minority
;;   :ensure t
;;   :config
;;   (rich-minority-mode 1)
;;   (setf rm-blacklist ""))

;; (set-face-attribute 'helm-source-header nil :height 1.0 :inherit 'font-lock-preprocessor-face)
(use-package helm
  :ensure t
  :config
  (helm-autoresize-mode t)
  (setq helm-display-source-at-screen-top nil)
  (setq helm-display-header-line nil)
  (setq helm-autoresize-max-height 23)
  (setq helm-autoresize-min-height 23)
  (use-package helm-ag :ensure t)
  (use-package helm-swoop :ensure t))

(use-package projectile
  :requires helm
  :ensure t
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'helm))

(use-package projectile-ripgrep :after projectile :ensure t)
(use-package helm-projectile :after projectile :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
  :config
  (setq sp-show-pair-from-inside t)
  :bind
  ("M-l" . sp-forward-slurp-sexp)
  ("M-h"  . sp-forward-barf-sexp)
  ("C-<left>"  . sp-backward-slurp-sexp)
  ("C-<right>"  . sp-backward-barf-sexp)
  ("C-M-k" . sp-kill-sexp)
  ("C-k"   . sp-kill-hybrid-sexp)
  ("M-k"   . sp-backward-kill-sexp)
  ("M-<backspace>" . backward-kill-word)
  ("C-<backspace>" . sp-backward-kill-word)
  ([remap sp-backward-kill-word] . backward-kill-word))

(use-package evil-smartparens
  :ensure t
  :after smartparens
  :config
  (evil-smartparens-mode 1)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package magit :ensure t)

(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-"))

;; global keys
(global-set-key (kbd "C-x p") 'projectile-switch-project)
(global-set-key (kbd "C-x C-p") 'projectile-switch-project)
(global-set-key (kbd "C-q") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-<tab>") 'helm-mini)
(global-set-key (kbd "C-\\") 'treemacs)
(global-set-key (kbd "C-S-f") 'helm-projectile-ag)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-p") 'evil-paste-pop)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-\"") 'eshell)
(define-key evil-normal-state-map (kbd "C-f") 'helm-swoop)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(define-key evil-insert-state-map (kbd "M-h") 'paredit-forward-barf-sexp)
(define-key evil-visual-state-map "gc" 'comment-dwim)
(define-key evil-normal-state-map "gcc" 'comment-line)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "f") 'avy-goto-char)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
(define-key evil-insert-state-map (kbd "C-p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "<SPC> i") 'helm-imenu)

(use-package which-key :ensure t :config (which-key-mode))
(use-package avy :ensure t)
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-project-follow-cleanup 1)
  (setq treemacs-no-png-images t)
  (setq treemacs-eldoc-display t)
  (treemacs-follow-mode 1)
  (setq treemacs-width 28)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  )

(use-package treemacs-projectile
  :after treemacs
  :ensure t)
(use-package treemacs-evil
  :after treemacs
  :ensure t)
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 23)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))

;; indent, auto remove whitespace and save buffer
(defun my/save-buffer()
  "Indent whole buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))
(add-hook 'before-save-hook 'my/save-buffer)

;; themes
(use-package solarized-theme :ensure t)
(use-package doom-themes :ensure t)

;; solarized theme
(defun my/dark-theme()
  "Load dark theme with my modifications."
  (interactive "*")
  (disable-theme 'github)
  (load-theme 'solarized-dark t)
  (setq evil-insert-state-cursor '((bar . 3) "#DC322F"))
  (setq evil-normal-state-cursor '(box "#EEE8D5"))
  (custom-set-faces
   '(sp-pair-overlay-face ((t (:background "#002B36"))))
   '(sp-wrap-overlay-face ((t (:background "#002B36"))))
   '(highlight-symbol-face ((t (:foreground "#6C71C4" :weight bold))))
   '(flycheck-fringe-error ((t (:bold t :foreground "red"))))
   '(flycheck-fringe-warning ((t (:bold t :foreground "orange"))))
   '(flycheck-fringe-info ((t (:bold t :foreground "green"))))
   '(show-paren-match ((t (:bold t :foreground "red" :background "#f8ee77"))))
   '(font-lock-function-name-face ((t (:foreground "#B58900"))))
   '(font-lock-constant-face ((t (:foreground "#268bd2" :weight normal))))
   '(font-lock-keyword-face ((t (:foreground "#859900" :weight normal))))
   '(cider-fringe-good-face ((t (:foreground "#34bd39"))))
   '(cider-test-error-face ((t (:background "red"))))
   '(cider-test-failure-face ((t (:background "orange"))))
   '(cider-test-success-face ((t (:background "green"))))
   '(cider-result-overlay-face ((t (:inherit cider-traced-face))))
   '(helm-non-file-buffer ((t (:foreground "#586E75"))))
   '(helm-ff-file ((t (:foreground "#B58900"))))
   '(helm-buffer-file ((t (:foreground "#B58900"))))
   '(helm-ff-dotted-directory ((t (:inherit helm-ff-directory))))
   '(helm-ff-symlink ((t (:inherit helm-ff-file ))))
   '(helm-buffer-directory ((t (:inherit helm-ff-directory))))
   '(helm-ff-dotted-symlink-directory ((t (:inherit helm-ff-directory))))
   '(helm-buffer-process ((t (:inherit helm-ff-file))))
   '(helm-swoop-target-line-face ((t (:background "green"))))
   '(helm-swoop-target-word-face ((t (:background "green" :foreground "#000000" :weight bold))))
   '(ivy-minibuffer-match-face-1 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-2 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-3 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-4 ((t (:background "#f8eec7"))))
   '(avy-lead-face ((t (:foreground "red" :background "#f8eec7"))))
   '(line-number ((t (:foreground "#264F59" :background "#002B36"))))
   '(doom-modeline-bar ((t (:background "#859900" :height 0.7))))
   '(doom-modeline-buffer-file ((t (:inherit 'doom-modeline-project-dir))))
   '(mode-line-buffer-id ((t (:height 1.0 :foreground "#222222" :box nil))))
   '(mode-line ((t (:height 1.0 :underline nil :overline nil :foreground "#839496" :background "#073642" :box nil))))
   '(mode-line-inactive ((t (:height 0.9 :underline nil :overline nil :background "#06323D" :box nil))))))
;; (my/dark-theme)

;; github theme
(use-package github-theme :ensure t)
(defun my/light-theme()
  "Load light theme with my modifications."
  (interactive "*")
  (disable-theme 'solarized-dark)
  (load-theme 'github t)
  (custom-set-faces
   '(flycheck-fringe-error ((t (:bold t :foreground "red" :background "#ffffff"))))
   '(flycheck-fringe-warning ((t (:bold t :foreground "orange" :background "#ffffff"))))
   '(flycheck-fringe-info ((t (:bold t :foreground "green" :background "#ffffff"))))
   '(show-paren-match ((t (:bold t :foreground "red" :background "#f8ee77"))))
   ;; '(font-lock-function-name-face ((t (:foreground "#5B369C" :weight bold))))
   '(font-lock-function-name-face ((t (:foreground "#333333" :weight bold))))
   '(highlight-symbol-face ((t (:inherit highlight))))
   '(cider-fringe-good-face ((t (:foreground "#34bd39"))))
   '(cider-test-error-face ((t (:background "red"))))
   '(cider-test-failure-face ((t (:background "orange"))))
   '(cider-test-success-face ((t (:background "green"))))
   '(cider-result-overlay-face ((t (:inherit cider-traced-face))))
   '(helm-non-file-buffer ((t (:foreground "#999999"))))
   '(helm-ff-dotted-directory ((t (:inherit helm-ff-directory))))
   '(helm-ff-dotted-symlink-directory ((t (:inherit helm-ff-directory))))
   '(helm-buffer-file ((t (:foreground "#183691"))))
   '(helm-ff-file ((t (:foreground "#183691"))))
   '(helm-ff-symlink ((t (:inherit default ))))
   '(helm-buffer-directory ((t (:inherit default :weight bold))))
   '(helm-buffer-modified ((t (:inherit helm-buffer-file))))
   '(helm-swoop-target-line-face ((t (:background "green"))))
   '(helm-swoop-target-word-face ((t (:background "green" :foreground "#000000" :weight bold))))
   '(ivy-minibuffer-match-face-1 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-2 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-3 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-4 ((t (:background "#f8eec7"))))
   '(avy-lead-face ((t (:foreground "red" :background "#f8eec7"))))
   '(sp-pair-overlay-face ((t (:background "#ffffff"))))
   '(company-tooltip ((t (:background "#f0f0f0" :foreground "#000000"))))
   '(company-tooltip-common ((t (:background "#f0f0f0" :foreground "#000000" :weight bold))))
   '(company-tooltip-common-selection ((t (:background "#B0CDE7" :foreground "#000000" :weight bold))))
   '(company-tooltip-search ((t (:background "#f0f0f0" :foreground "#000000" :weight normal))))
   '(company-preview-search ((t (:foreground "#e0e0e0" :background "#B0CDE7" :weight bold))))
   '(line-number ((t (:foreground "#073642" :background "#ffffff"))))
   '(mode-line-buffer-id ((t (:height 1.0 :foreground "#222222" :box nil))))
   '(doom-modeline-bar ((t (:background "#183691" :height 0.7))))
   '(doom-modeline-buffer-file ((t (:inherit 'doom-modeline-project-dir))))
   '(mode-line ((t (:underline nil :overline "#e0e0e0" :height 0.9 :foreground "#333333" :background "#e0e0e0" :box nil))))
   '(mode-line-inactive ((t (:underline nil :overline "#e0e0e0" :height 0.9 :background "#f0f0f0" :box nil))))))
(my/light-theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(package-selected-packages
   (quote
    (popwin treemacs-evil which-key lispy flycheck-plantuml plantuml-mode projectile-rg evil-smartparens smartparens-evil treemacs-projectile treemacs helm-ag helm-projectile color-theme-zenburn zenburn json-mode yaml-mode git-gutter smartparens helm-swoop helm-rg rich-minority restclient evil-collection key-chord color-theme-sanityinc-tomorrow gruvbox-theme zenburn-theme doom-themes solarized-theme exec-path-from-shell flycheck-inline base16-theme flycheck-clj-kondo highlight-symbol shell-pop neotree counsel-projectile projectile-ripgrep projectile flychech-pos-tip company-lsp yasnippt flycheck-rust cargo lsp-ui lsp-mode magit flycheck-pos-tip flycheck-clojure flycheck-joker cider evil-leader paredit-mode clj-refactor clojure-mode helm avy general use-package)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7"))))
