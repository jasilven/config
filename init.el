(setq package-enable-at-startup nil)

(setq package-archives '(
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
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode t)
(global-eldoc-mode -1)
(global-auto-revert-mode t)
(global-display-line-numbers-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'eshell-mode-hook (hl-line-mode -1))
(add-hook 'shell-mode-hook (hl-line-mode -1))
(add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'shell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'term-mode-hook (lambda () (text-scale-decrease 1)))
(add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1)))
(add-hook 'cider-repl-mode-hook (lambda () (text-scale-decrease 1)))
(defalias 'yes-or-no-p 'y-or-n-p)

;; defaults
(setq-default
 gc-cons-upper-limit 536870912
 gc-cons-threshold 16777216
 term-scroll-show-maximum-output t
 term-scroll-to-bottom-on-output t
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
  (push '("*eshell*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*shell*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*cider-doc*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*cider-result*" :height 12 :stick t :position bottom :noselect t) popwin:special-display-config)
  (push '("*cider-error*" :height 16 :position bottom) popwin:special-display-config)
  (push '("*Flycheck errors*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '(cider-repl-mode :height 12 :stick t) popwin:special-display-config))

(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
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
  (evil-mode 1)
  :config
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))
  (setq evil-move-cursor-back nil))

(use-package json-mode :ensure t)
(use-package avy :ensure t)
(use-package restclient :ensure t)
(use-package expand-region :ensure t)

(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

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
  (define-key cider-mode-map (kbd "C-s") #'my/save-buffer)
  (setq cider-print-fn (quote fipp))
  (setq cider-print-quota 100000)
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-stacktrace-default-filters '(tooling dup java REPL))
  (setq cider-save-file-on-load t)
  (setq nrepl-hide-special-buffers t)
  (setq cider-clojure-cli-global-options "-A:bench:dev")
  (define-key evil-normal-state-map (kbd "<SPC> x")
    (lambda () (interactive) (cider-eval-sexp-at-point)))
  (define-key evil-normal-state-map (kbd "C-x C-x")
    (lambda () (interactive) (end-of-line) (cider-eval-sexp-at-point)))
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

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d "))

(use-package counsel
  :ensure t
  :config (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

(use-package counsel-projectile :ensure t)

(use-package projectile
  :requires ivy
  :ensure t
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package projectile-ripgrep :after projectile :ensure t)

(use-package smartparens
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
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
(use-package evil-magit :after magit :ensure t)

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
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-<tab>") 'ivy-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-S-f") 'counsel-projectile-ag)
(global-set-key (kbd "C-S-r") 'projectile-replace)
(global-set-key (kbd "C-\\") 'treemacs)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-p") 'evil-paste-pop)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "<f8>") 'shell-pop)
(global-set-key (kbd "C-M-=") 'my/indent-buffer)
(define-key evil-normal-state-map (kbd "C-f") 'swiper)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(define-key evil-insert-state-map (kbd "M-h") 'paredit-forward-barf-sexp)
(define-key evil-visual-state-map "gc" 'comment-dwim)
(define-key evil-normal-state-map "gcc" 'comment-line)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "f") 'avy-goto-char)
(define-key evil-normal-state-map (kbd "m") 'sp-down-sexp)
(define-key evil-normal-state-map (kbd "M") 'sp-backward-sexp)
(define-key evil-normal-state-map (kbd "gh") 'beginning-of-line)
(define-key evil-normal-state-map (kbd "ga") 'beginning-of-line)
(define-key evil-normal-state-map (kbd "gl") 'end-of-line)
(define-key evil-normal-state-map (kbd "ge") 'end-of-line)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
(define-key evil-insert-state-map (kbd "C-p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "C-n") 'evil-buffer-new)
(define-key evil-insert-state-map (kbd "C-n") 'evil-buffer-new)
(define-key evil-normal-state-map (kbd "<SPC> i") 'counsel-imenu)
(define-key evil-normal-state-map (kbd "<SPC> j") 'counsel-imenu)
(define-key evil-normal-state-map (kbd "<SPC> e") 'flycheck-list-errors)
(define-key evil-normal-state-map (kbd "<SPC> w") 'save-buffer)
(define-key evil-normal-state-map (kbd "<SPC> <tab>") 'my/switch-to-last-buffer)
(define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'er/expand-region)
(global-set-key (kbd "C-<backspace>") 'my/switch-to-last-buffer)

(use-package which-key :ensure t :config (which-key-mode))
(use-package avy :ensure t)
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-show-cursor nil)
  (setq treemacs-project-follow-cleanup 1)
  (treemacs-resize-icons 17)
  (setq treemacs-eldoc-display t)
  (treemacs-follow-mode t)
  (setq treemacs-width 22)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-projectile :after treemacs :ensure t)
(use-package treemacs-evil :after treemacs :ensure t)
(use-package all-the-icons :ensure t)
;; (use-package solaire-mode
;;   :ensure t
;;   :hook
;;   ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;   (minibuffer-setup . solaire-mode-in-minibuffer)
;;   :config
;;   ;; (setq solaire-mode-remap-modeline nil)
;;   (setq solaire-mode-remap-fringe nil)
;;   (solaire-global-mode +1)
;;   (solaire-mode-swap-bg))

(use-package doom-modeline
  :ensure t
  :config
  (custom-set-faces
   '(mode-line ((t (:height 0.9))))
   '(mode-line-inactive ((t (:height 0.9)))))
  (setq doom-modeline-height 10)
  (setq doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))

;; (use-package doom-themes

(use-package almost-mono-themes
  :ensure t
  :config
  ;; (setq font-lock-builtin-face nil)
  ;; (setq font-lock-type-face nil)
  ;; (setq font-lock-variable-name-face nil)
  (load-theme 'almost-mono-white t)
  (custom-set-faces
   ;; '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#3B4252" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil))))
   ;;  '(font-lock-comment-face ((t (:foreground "gray45"))))
   ;; '(font-lock-constant-face ((t (:foreground "#005cc5"))))
   '(font-lock-constant-face ((t (:foreground "#0065BB" :slant normal :weight bold))))
   '(font-lock-type-face ((t (:foreground "#0065BB" :weight bold :slant normal))))
   '(font-lock-doc-face ((t (:foreground nil :inherit 'font-lock-string-face))))
   '(font-lock-function-name-face ((t (:foreground "Blue1"))))
   '(font-lock-keyword-face ((t (:foreground "firebrick"))))
   '(show-paren-match ((t (:weight bold :foreground "black" :background "#fda505"))))
   ;;  '(font-lock-string-face ((t (:foreground "#22863a"))))
   ;;  '(highlight-symbol-face ((t (:inherit lazy-highlight))))
   ;; '(hl-line ((t (:background "gray95"))))
   '(mode-line ((t (:height 0.9))))
   '(mode-line-inactive ((t (:height 0.9))))
   )
  )

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package smex :ensure t)
(use-package ivy-rich
  :after ivy
  :ensure t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

;; font
(if (memq window-system '(mac ns))
    (set-frame-font "Fira Code-18")
  (set-frame-font "Fira Code Retina-15"))

;; (set-frame-font "-CYRE-Inconsolata-bold-normal-normal-*-20-*-*-*-m-0-iso8859-1")
(set-frame-name "Editor")

;; my stuff
(defun my/save-buffer ()
  "Indent whole buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  (save-buffer))

(defun my/indent-buffer ()
  "Indent whole buffer."
  (interactive "*")
  (indent-region (point-min) (point-max)))

(defun my/date ()
  "Insert date."
  (interactive "*")
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

(defun my/switch-to-last-buffer ()
  "Switch to last buffer."
  (interactive)
  (switch-to-buffer nil))

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(package-selected-packages
   '(almost-mono-themes solaire-mode which-key use-package treemacs-projectile treemacs-evil smex shell-pop restclient projectile-ripgrep popwin lsp-ui key-chord json-mode ivy-rich highlight-symbol git-gutter flycheck-rust flycheck-posframe flycheck-pos-tip flycheck-plantuml flycheck-joker expand-region exec-path-from-shell evil-smartparens evil-magit evil-collection doom-themes doom-modeline counsel-projectile company-lsp clj-refactor cargo)))

