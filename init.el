;;(setq package-enable-at-startup nil)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu"            . "http://elpa.gnu.org/packages/")
                         ("melpa"          . "https://melpa.org/packages/")
                         ("melpa-stable"   . "https://stable.melpa.org/packages/")
                         ("milkbox"        . "http://melpa.milkbox.net/packages/")
                         ("milkbox-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ))
;;("marmalade"    . "http://marmalade-repo.org/packages/")
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
(require 'use-package)
(use-package exec-path-from-shell :ensure t)
(set-frame-width nil 87)
(set-frame-height nil 30)

;; editor modes
;;(global-so-long-mode 1)
(blink-cursor-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode t)
(size-indication-mode -1)
;;(global-display-line-numbers-mode -1)
(global-eldoc-mode t)
(global-auto-revert-mode t)

;; hooks
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'clojure-mode-hook #'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil) (hl-line-mode -1)))
(add-hook 'shell-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'shell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)
                            (text-scale-decrease 1)))
(add-hook 'flycheck-error-list-mode-hook (lambda () (text-scale-decrease 1)))
;; (add-hook 'compilation-mode-hook (lambda () (text-scale-decrease 1)))
(add-hook 'cargo-process-mode-hook (lambda () (goto-char (point-max))))
(add-hook 'cider-popup-buffer-mode-hook (lambda () (display-line-numbers-mode -1)))
(defalias 'yes-or-no-p 'y-or-n-p)

;; global keys
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-j") 'shell-pop)
(global-set-key (kbd "C-<tab>") 'my/switch-to-last-buffer)
(global-set-key (kbd "C-M-=") 'my/indent-buffer)
(global-set-key (kbd "C-M-w") 'ace-delete-window)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-j") 'forward-paragraph)
(global-set-key (kbd "M-k") 'backward-paragraph)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-m") 'previous-error)
(global-set-key (kbd "C-'") 'next-error)
(global-set-key (kbd "C-\"") 'previous-error)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(global-set-key (kbd "C-c C-y") 'company-yasnippet)
(global-set-key (kbd "M-o") (lambda () (end-of-line) (electric-newline-and-maybe-indent)))
(global-set-key (kbd "<C-mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<C-mouse-5>") 'scroll-up-line)

;; defaults
(setq-default
 set-frame-name "Editor"
 compilation-ask-about-save nil
 hl-line-sticky-flag nil
 global-hl-line-sticky-flag nil
 undo-tree-auto-save-history t
 display-line-numbers-width 3
 text-scale-mode-step 1.1
 tramp-default-method "ssh"
 ;; gc-cons-upper-limit 536870912
 ;; gc-cons-threshold 16777216
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
 mouse-wheel-scroll-amount '(3 ((shift) . 1))
 ring-bell-function #'ignore
 scalable-fonts-allowed t
 scroll-conservatively 100000
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
 indicate-empty-lines nil)

;; (use-package move-text :ensure t :config (move-text-default-bindings))
;; (use-package restclient :ensure t)
;; (use-package expand-region :ensure t)
(use-package fast-scroll :ensure t
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1) ))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1) ))
  (fast-scroll-config)
  (fast-scroll-mode 1))
(use-package gcmh :ensure t :config (gcmh-mode 1))
(use-package undo-tree :ensure t :config (global-undo-tree-mode))
(use-package evil-magit :after magit :ensure t)
(use-package which-key :ensure t :config (which-key-mode))
(use-package treemacs-projectile :after (treemacs projectile) :ensure t)
(use-package treemacs-evil :after (treemacs evil) :ensure t)
(use-package all-the-icons :ensure t)
(use-package smex :ensure t)
(use-package doom-themes :ensure t :init (load-theme 'doom-one))
(use-package key-chord :ensure t :config (key-chord-mode 1))
(use-package projectile-ripgrep :after projectile :ensure t)
(use-package counsel-projectile :after projectile :ensure t)
(use-package yasnippet :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook (lambda () (yas-minor-mode-on))))
(use-package yasnippet-snippets :ensure t :after yasnippet)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package ace-window :ensure t
  :config
  (ace-window-display-mode 1)
  (set-face-attribute 'aw-leading-char-face nil :inherit nil :height 2.0 :foreground "#cb4b16"))

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t insert-directory-program "/usr/local/bin/gls"))
  (setq dired-auto-revert-buffer t)
  (setq dired-recursive-copies t)
  (setq dired-recursive-deletes t)
  :custom
  (dired-listing-switches "-aBhl --group-directories-first"))

(use-package flycheck :ensure t
  :config
  (setq flycheck-display-errors-delay 0.3)
  (setq flycheck-idle-change-delay 0.8)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-set-key (kbd "C-x '") 'flycheck-list-errors)
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))

(use-package avy :ensure t
  :config
  (set-face-attribute 'avy-lead-face nil :weight 'bold :background "#ff2600" :foreground "#ffffff"))

(use-package aggressive-indent :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'rust-mode))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package hide-mode-line :ensure t
  :hook
  ((cider-repl-mode imenu-list-minor-mode treemacs-mode) . hide-mode-line-mode))

(use-package ivy-posframe :ensure t
  :config
  (setq ivy-posframe-min-width 90
        ivy-posframe-font (if (eq window-system 'x) "Fira Code Medium-11" "Monaco-12")
        ivy-posframe-border-width 2
        ivy-posframe-width 90
        ivy-posframe-min-height 10
        ivy-posframe-height 15
        ivy-posframe-parameters '((left-fringe . 1) (right-fringe . 5))
        ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-frame-bottom-window-center)
          (counsel-company . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-center)
          (t               . ivy-posframe-display-at-window-center)))
  (set-face-attribute 'internal-border nil :background "#2AA18E")
  (ivy-posframe-mode 1))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push '("*cider-clojuredocs*" :width 0.40 :position bottom :dedicated t) popwin:special-display-config)
  (push '("*cider-xref*" :width 0.40 :position bottom :dedicated t) popwin:special-display-config)
  (push '("*cider-scratch*" :width 0.30 :position right) popwin:special-display-config)
  (push '("*undo-tree*" :width 0.3 :position right) popwin:special-display-config)
  (push '("*eshell*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*xref*" :height 12 :stick t :position bottom) popwin:special-display-config)
  (push '("*shell*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*cider-out*" :height 7 :stick t :noselect t :tail t :position bottom :dedicated t) popwin:special-display-config)
  (push '("*cider-result*" :width 0.30 :stick t :position right :noselect t) popwin:special-display-config)
  (push '("*cider-test-report*" :width 0.30 :stick t :position right :noselect t) popwin:special-display-config)
  (push '("*cider-inspect*" :width 0.30 :stick t :position right :noselect t) popwin:special-display-config)
  (push '("*cider-error*" :width 0.40 :position right) popwin:special-display-config)
  (push '("*Flycheck errors*" :height 12 :stick t :position bottom) popwin:special-display-config)
  (push '("^*Cargo" :regexp t :width 40 :position right :dedicated f :stick t :noselect t) popwin:special-display-config)
  (push '(compilation-mode :width 40 :position right :dedicated f :stick t :noselect t) popwin:special-display-config)
  ;; (push '("^*Cargo Run" :regexp t :height 14 :position bottom :tail t) popwin:special-display-config)
  (push '("*Warnings*" :height 7 :stick t :position bottom :noselect t) popwin:special-display-config)
  (push '(cider-docview-mode :width 0.40 :stick t :position bottom) popwin:special-display-config)
  (push '(cider-popup-buffer-mode :width 0.30 :stick t :position right) popwin:special-display-config)
  (push '(cider-repl-mode :height 7 :stick t :noselect t :tail t :dedicated t) popwin:special-display-config)
  )

(use-package evil
  :after key-chord
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines nil)
  (add-hook 'cider--debug-mode-hook
            (lambda () (evil-make-overriding-map cider--debug-mode-map 'normal)
              (evil-normalize-keymaps)))
  (add-to-list 'evil-emacs-state-modes 'cider-inspector-mode)
  (add-to-list 'evil-emacs-state-modes 'flycheck-list-errors-mode)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key evil-normal-state-map (kbd "<escape>") 'keyboard-quit)
  (define-key evil-normal-state-map (kbd "C-w k") 'ace-delete-window)
  (define-key evil-normal-state-map (kbd "C-w C-k") 'ace-delete-window)
  (define-key evil-motion-state-map (kbd "C-o") 'my/jump-back)
  (define-key evil-normal-state-map (kbd "C-f") 'swiper)
  (define-key evil-normal-state-map (kbd "C-F") 'counsel-projectile-rg)
  (define-key evil-insert-state-map (kbd "M-h") 'paredit-forward-barf-sexp)
  (define-key evil-visual-state-map "gc" 'comment-dwim)
  (define-key evil-normal-state-map "gcc" 'comment-line)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "f") 'avy-goto-char)
  (define-key evil-visual-state-map (kbd "f") 'avy-goto-char)
  (define-key evil-normal-state-map (kbd "m") 'sp-down-sexp)
  (define-key evil-normal-state-map (kbd "M") 'sp-backward-sexp)
  (define-key evil-normal-state-map (kbd "zA") 'hs-hide-all)
  (define-key evil-normal-state-map (kbd "gm") 'evil-jump-item)
  (define-key evil-normal-state-map (kbd "gw") 'ace-window)
  (define-key evil-normal-state-map (kbd "go") 'other-window)
  (define-key evil-normal-state-map (kbd "gq") nil)
  (define-key evil-normal-state-map (kbd "gh") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "ga") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "gl") 'end-of-line)
  (define-key evil-normal-state-map (kbd "ge") 'end-of-line)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-insert-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
  (define-key evil-insert-state-map (kbd "C-n") 'neotree-toggle)
  (define-key evil-insert-state-map (kbd "M-y") 'company-yasnippet)
  (define-key evil-normal-state-map (kbd "<SPC> i") 'counsel-imenu)
  (define-key evil-normal-state-map (kbd "<SPC> j") 'counsel-imenu)
  (define-key evil-normal-state-map (kbd "<SPC> f") 'counsel-projectile-find-file)
  (define-key evil-normal-state-map (kbd "<SPC> b") 'ivy-switch-buffer)
  (define-key evil-normal-state-map (kbd "<SPC> s") 'swiper)
  (define-key evil-normal-state-map (kbd "<SPC> w") 'save-buffer)
  (define-key evil-normal-state-map (kbd "<SPC> o") 'delete-other-windows)
  (define-key evil-normal-state-map (kbd "<SPC> k") 'ace-delete-window)
  (define-key evil-normal-state-map (kbd "C-<tab>") 'my/switch-to-last-buffer)
  (define-key evil-normal-state-map (kbd "<SPC> <tab>") 'my/switch-to-last-buffer)
  (define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'er/expand-region))


(use-package shell-pop
  :ensure t
  :config
  (define-key term-raw-map (kbd "C-j") 'shell-pop)
  (evil-set-initial-state 'term-mode 'emacs)
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.9)
  (add-hook 'emacs-elisp-mode-hook #'highlight-symbol-mode)
  (add-hook 'clojure-mode-hook #'highlight-symbol-mode))

(use-package company
  :ensure t
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :config
  (add-hook 'emacs-elisp-mode-hook #'company-mode)
  (add-hook 'prog-mode-hook #'company-mode)
  (setq company-tooltip-timer 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)
  ;; (setq company-echo-delay 0.5)
  (define-key company-active-map (kbd "C-j") 'newline-and-indent)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  (define-key company-search-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering))

(use-package flycheck-clj-kondo :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  (setq clojure-align-forms-automatically t)
  (set-face-attribute 'clojure-keyword-face nil :inherit 'font-lock-function-name-face))

(use-package cider
  :ensure t
  :pin melpa-stable
  :init
  (use-package flycheck-joker :ensure t)
  :config
  (defun add-clj-format-before-save () (interactive)
         (add-hook 'before-save-hook
                   'cider-format-buffer
                   t t))
  (add-hook 'clojure-mode-hook
            'add-clj-format-before-save)
  (add-hook 'cider-repl-mode-hook '(lambda () (setq scroll-conservatively 1)))
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider--debug-mode-hook 'evil-normalize-keymaps)
  (set-face-attribute 'cider-fringe-good-face nil :foreground "#2aa198")
  (set-face-attribute 'clojure-keyword-face nil :inherit 'font-lock-function-name-face)
  ;; (define-key cider-mode-map (kbd "C-s") #'my/save-buffer)
  (define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-newline-and-indent)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-clojure-cli-global-options nil)
  (setq cider-print-fn 'fipp)
  (setq cider-print-quota 1000000)
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-history-recenter nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-save-file-on-load t)
  (setq cider-show-error-buffer nil)
  (setq cider-stacktrace-default-filters '(tooling dup java REPL))
  (setq cljr-warn-on-eval nil)
  (setq clojure-toplevel-inside-comment-form t)
  (setq nrepl-hide-special-buffers t)
  (evil-define-key 'normal clojure-mode-map "K" 'cider-doc)
  (evil-define-key 'normal clojure-mode-map "gd" 'cider-find-var)
  (evil-define-key 'normal clojure-mode-map (kbd "<SPC> ck") 'cider-eval-buffer)
  (evil-define-key 'normal clojure-mode-map (kbd "<SPC> ci") 'cider-pprint-eval-last-sexp-to-comment)
  (evil-define-key 'normal clojure-mode-map (kbd "<SPC> cp") 'cider-pprint-eval-last-sexp)
  (evil-define-key 'normal clojure-mode-map (kbd "<SPC> h") 'cider-clojuredocs)
  (evil-define-key 'normal clojure-mode-map (kbd "<SPC> e") 'cider-eval-last-sexp)
  (evil-define-key 'normal clojure-mode-map (kbd "<SPC> x") 'cider-eval-defun-at-point)
  (evil-define-key 'normal clojure-mode-map (kbd "<SPC> r") 'cider-jump-to-compilation-error)
  (evil-define-key 'normal clojure-mode-map (kbd "C-x C-x") 'cider-eval-defun-at-point)
  (evil-define-key 'normal clojure-mode-map (kbd "C-c C-z") 'my/switch-to-repl-and-back)
  (evil-define-key 'normal clojure-mode-map (kbd "C-j") 'my/switch-to-repl-and-back)
  (add-hook 'cider-docview-mode-hook
            (lambda () (text-scale-decrease 1) (display-line-numbers-mode -1) (setq-local global-hl-line-mode nil)
              (visual-line-mode t)))
  (add-hook 'cider-repl-mode-hook
            (lambda () (text-scale-decrease 1) (display-line-numbers-mode -1) (setq-local global-hl-line-mode nil)))
  (add-hook 'cider-popup-buffer-mode-hook
            (lambda () (text-scale-decrease 1) (setq-local global-hl-line-mode nil))))

(use-package ivy
  :ensure t
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x C-b" . ivy-switch-buffer))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format ""))

(use-package counsel
  :ensure t
  :bind
  (( "M-x" . counsel-M-x)
   ( "C-x C-f" . counsel-find-file)
   ( "C-s" . swiper)
   ( "M-p" . counsel-yank-pop)
   ( "C-x C-r" . counsel-recentf))
  :config (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

(use-package projectile
  :requires ivy
  :ensure t
  :bind
  (("C-x p" . projectile-switch-project)
   ("C-S-r" . projectile-replace)
   ("C-x C-p" . projectile-switch-project))
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))

(use-package smartparens
  :ensure t
  :hook
  ((after-init . smartparens-global-mode)
   (prog-mode . smartparens-mode)
   (cider-repl-mode . smartparens-mode)
   (prog-mode . turn-on-smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (sp-pair "\"" "\"")
  (setq sp-show-pair-from-inside t)
  :bind
  ("M-l" . sp-forward-slurp-sexp)
  ("M-h"  . sp-forward-barf-sexp)
  ("C-<left>"  . sp-backward-slurp-sexp)
  ("C-<right>"  . sp-backward-barf-sexp)
  ("C-M-k" . sp-kill-sexp)
  ("C-k"   . sp-kill-hybrid-sexp)
  ("M-<backspace>" . backward-kill-word)
  ("C-<backspace>" . sp-backward-kill-word)
  ([remap sp-backward-kill-word] . backward-kill-word))

(use-package evil-smartparens
  :ensure t
  :after smartparens
  :config
  (evil-smartparens-mode 1)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  :hook ((prog-mode . diff-hl-margin-mode)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(use-package neotree
  :ensure t
  :hook
  (neotree-mode . (lambda () (text-scale-decrease 1)
                    (setq-local display-line-numbers nil)
                    (hide-mode-line-mode)))
  :bind
  (( "C-\\" . neotree-toggle)
   ( "C-<return>" . neotree-toggle))
  :config
  (setq neo-window-fixed-size nil)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq-local tab-width 0)
  (setq neo-smart-open nil)
  (setq neo-autorefresh nil)
  (setq neo-window-width 25)
  (setq neo-click-changes-root nil)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq all-the-icons-color-icons nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "go") 'other-window)
              (define-key evil-normal-state-local-map (kbd "\t") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "v") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-def-modeline 'my-simple-line
    '(matches buffer-info remote-host parrot selection-info)
    '(misc-info input-method buffer-encoding major-mode process vcs))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-simple-line 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-buffer-modification-icon -1)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :family "Noto Sans" :height 0.9)
  (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 0.9)
  :hook (after-init . doom-modeline-mode))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x
                        (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 33))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 45))))))
  (ivy-rich-mode 1))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("/Pipfile.lock\\'" . json-mode)))

;; (use-package treemacs
;;   :ensure t
;;   :bind
;;   (( "C-\\" . treemacs)
;;    ( "C-<return>" . treemacs))
;;   :hook
;;   (treemacs-mode . (lambda () (text-scale-decrease 1)
;;                      (setq-local display-line-numbers nil)))
;;   :config
;;   (setq treemacs-show-cursor nil)
;;   (setq treemacs-project-follow-cleanup 1)
;;   (treemacs-resize-icons 11)
;;   (setq treemacs-eldoc-display t)
;;   (treemacs-follow-mode t)
;;   (setq treemacs-width 22)
;;   (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :config
;;   (require 'lsp-clients)
;;   (setq lsp-auto-guess-root t
;; 	    lsp-prefer-flymake nil
;; 	    lsp-enable-indentation nil
;; 	    lsp-enable-on-type-formatting nil)
;;   (add-to-list 'lsp-file-watch-ignored "\\.vscode$"))

;; (use-package lsp-ui
;;   :ensure t
;;   :hook ((lsp-mode . lsp-ui-mode)
;; 	     (lsp-after-open . (lambda () (lsp-ui-flycheck-enable 1)
;;                              (set-face-attribute 'lsp-ui-sideline-global nil :height 0.9 :background "#00242e")
;;                              (lsp-ui-doc-mode -1))))
;;   :config
;;   (setq lsp-ui-sideline-show-symbol t)
;;   (setq lsp-ui-sideline-ignore-duplicate t)
;;   (setq lsp-ui-doc-use-webkit -1)
;;   (setq lsp-ui-flycheck-enable t)
;;   (setq lsp-ui-doc-include-signature t)
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-mode -1)
;;   (set-face-attribute 'lsp-ui-doc-background nil :background "#002B36")
;;   (set-face-attribute 'markdown-code-face nil :background "#002B36")
;;   )

;; (defun my/cargo-run-bin ()
;;   "Run cargo run."
;;   (interactive)
;;   (let ((bin-name (file-name-sans-extension (buffer-name))))
;;     (delete-other-windows)
;;     (if (string= "main" bin-name)
;;         (cargo-process-run)
;;       (cargo-process-run-bin bin-name))))

;; (defun my/cargo-process-test()
;;   "Run cargo test."
;;   (interactive)
;;   (delete-other-windows)
;;   (cargo-process-test))

;; (defun my/cargo-process-check()
;;   "Run cargo check."
;;   (interactive)
;;   (delete-other-windows)
;;   (cargo-process-check))

;; (use-package rust-mode
;;   :ensure t
;;   :mode "\\.rs\\'"
;;   :hook (rust-mode . lsp)
;;   :config
;;   ;; (flycheck-inline-mode -1)
;;   (electric-pair-local-mode t)
;;   (define-key rust-mode-map (kbd "C-c C-r") 'my/cargo-run-bin)
;;   (define-key rust-mode-map (kbd "C-c C-t") 'my/cargo-process-test)
;;   (define-key rust-mode-map (kbd "C-c C-k") 'my/cargo-process-check)
;;   (evil-define-key 'normal rust-mode-map (kbd "<SPC> r") 'my/cargo-run-bin)
;;   (evil-define-key 'normal rust-mode-map (kbd "<SPC> t") 'my/cargo-process-test)
;;   (evil-define-key 'normal rust-mode-map (kbd "<SPC> c") 'my/cargo-process-check)
;;   (evil-define-key 'normal rust-mode-map "K" 'lsp-ui-doc-glance)
;;   (require 'lsp-clients)
;;   (setq rust-format-on-save t))

;; (use-package flycheck-rust
;;   :ensure t
;;   :after flycheck
;;   :commands flycheck-rust-setup
;;   :init
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package cargo
;;   :ensure t
;;   :commands cargo-minor-mode
;;   :config
;;   (define-key cargo-process-mode-map (kbd "go") 'other-window)
;;   (add-hook 'cargo-process-mode-hook
;;             (lambda ()
;;               (text-scale-decrease 1)
;;               (visual-line-mode 1)
;;               (setq-local global-hl-line-mode nil)))
;;   :hook (rust-mode . cargo-minor-mode))

;; (use-package toml-mode
;;   :ensure t
;;   :mode (("\\.toml\\'" . toml-mode)
;; 	     ("/Pipfile\\'" . toml-mode)))

;; my stuff


;; (defun my/modeline-adjust ()
;;   "My adjust modeline."
;;   (interactive)
;;   (doom-modeline-mode)
;;   (set-face-attribute 'doom-modeline-buffer-modified nil :inherit 'doom-modeline-battery-warning :weight 'bold)
;;   ;; (setq doom-modeline-height (/ (face-attribute 'default :height) 100))
;;   (setq doom-modeline-bar-width 3)
;;   ;; (set-face-attribute 'mode-line nil :inherit nil :height (-  (face-attribute 'default :height) 10 ))
;;   (set-face-attribute 'mode-line nil :inherit nil :height 0.90)
;;   ;; (set-face-attribute 'mode-line-inactive nil :inherit nil :height (- (face-attribute 'default :height) 40))
;;   (set-face-attribute 'mode-line-inactive nil :inherit nil :height 0.90)
;;   )

;; (defun my/common-faces ()
;;   (set-face-attribute 'font-lock-builtin-face nil :foreground nil)
;;   (set-face-attribute 'font-lock-variable-name-face nil :foreground nil)
;;   (set-face-attribute 'font-lock-variable-name-face nil :inherit nil)
;;   (set-face-attribute 'font-lock-doc-face nil :slant 'normal)
;;   (set-face-attribute 'warning nil :height 0.90)
;;   (set-face-attribute 'error nil :height 0.90)
;;   (set-face-attribute 'success nil :height 0.90)
;;   (set-face-attribute 'font-lock-doc-face nil :slant 'normal)
;;   (set-face-attribute 'font-lock-preprocessor-face nil :foreground nil :inherit 'default :weight 'normal)
;;   (set-face-attribute 'line-number-current-line nil :inherit 'line-number)
;;   (set-face-attribute 'font-lock-constant-face nil :foreground nil :inherit 'normal))

;; (defun my/theme-solarized-dark ()
;;   "My solarized dark."
;;   (interactive "*")
;;   (load-theme 'doom-solarized-dark)
;;   (set-face-attribute 'error nil :foreground "#e66f6d" :weight 'bold)
;;   (set-face-attribute 'font-lock-constant-face nil :foreground "#839496")
;;   (set-face-attribute 'region nil :background "#055633")
;;   (set-face-attribute 'hl-line nil :background "#083a4a")
;;   (set-face-attribute 'line-number-current-line nil :background "#083a4a" :foreground "#cccccc")
;;   (set-face-attribute 'font-lock-doc-face nil :slant 'normal)
;;   (set-face-attribute 'compilation-error nil :inherit 'fringe :weight 'bold :height 0.9 :foreground "#56697A" :slant 'italic)
;;   (set-face-attribute 'highlight-symbol-face nil :background nil :underline t :foreground "#bbaaaa")
;;   (setq beacon-color "#f1fa8c")
;;   (my/common-faces)
;;   (my/modeline-adjust))

;; (defun my/theme-doom-one-light ()
;;   "My one light."
;;   (interactive)
;;   (load-theme 'doom-one-light)
;;   (set-face-attribute 'compilation-error nil :inherit 'fringe :weight 'bold :height 0.9 :foreground "#56697A")
;;   (set-face-attribute 'ivy-posframe nil :background "#e9e9fa" :foreground "#000000")
;;   (set-face-attribute 'highlight-symbol-face nil :underline t :foreground "#000000" :background "#dddddd")
;;   (set-face-attribute 'default nil :background "#f2f2f2")
;;   (set-face-attribute 'hl-line nil :background "#CDFDC7" :inherit nil)
;;   (set-face-attribute 'line-number-current-line nil :background "#CDFDC7" :foreground "#333333")
;;   (set-face-attribute 'show-paren-match nil :background "#F3FF4A")
;;   (setq beacon-color "#268bd2")
;;   (my/common-faces)
;;   (my/modeline-adjust))

;; (defun my/save-buffer ()
;;   "Indent whole buffer."
;;   (interactive "*")
;;   (delete-trailing-whitespace)
;;   (indent-region (point-min) (point-max))
;;   (save-buffer))

(defun my/switch-to-repl-and-back ()
  "Switch to repl and back."
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-switch-to-last-clojure-buffer))

(defun my/jump-back ()
  "My jump back based on mode."
  (interactive)
  (if (eq major-mode 'clojure-mode)
      (cider-pop-back)
    (evil-jump-backward)))

(defun my/set-font ()
  "My default font."
  (interactive)
  (if (eq window-system 'x)
      (set-frame-font "Fira Code Medium-12")
    (set-frame-font "Monaco-15")))

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

(defun er-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; my initialize
(defun my/initialize ()
  "Initialize."
  (interactive "*")
  (when (memq window-system '(mac ns x))
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'meta)
    (exec-path-from-shell-initialize))
  (my/set-font)
  (put 'downcase-region 'disabled nil))

(my/initialize)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray22")))))
