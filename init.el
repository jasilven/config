;;(setq package-enable-at-startup nil)

(setq package-archives '(
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")))
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
(require 'use-package)
(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; editor modes
(global-so-long-mode 1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode t)
(global-eldoc-mode -1)
(global-auto-revert-mode t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'eshell-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'shell-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local global-hl-line-mode nil)))
(add-hook 'shell-mode-hook (lambda ()
                             (setq-local global-hl-line-mode nil)))
(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))
(add-hook 'term-mode-hook (lambda () (text-scale-decrease 1)))
(add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1)
                                (setq-local display-line-numbers nil)))
(add-hook 'cider-repl-mode-hook (lambda () (text-scale-decrease 1)
                                  (setq-local global-hl-line-mode nil)))
(defalias 'yes-or-no-p 'y-or-n-p)

;; defaults
(setq-default
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
 mouse-wheel-scroll-amount '(3)
 ring-bell-function #'ignore
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
 dired-listing-switches "-aoht"
 )

(use-package key-chord :ensure t :config (key-chord-mode 1))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push '("*eshell*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*xref*" :height 12 :stick t :position bottom) popwin:special-display-config)
  (push '("*shell*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*cider-doc*" :height 15 :stick t :position bottom) popwin:special-display-config)
  (push '("*cider-result*" :height 12 :stick t :position bottom :noselect t) popwin:special-display-config)
  (push '("*cider-error*" :height 16 :position bottom) popwin:special-display-config)
  (push '("*Flycheck errors*" :height 12 :stick t :position bottom) popwin:special-display-config)
  (push '("*Cargo Run*" :height 12 :stick t :position bottom :noselect t) popwin:special-display-config)
  (push '("*Cargo Test*" :height 12 :stick t :position bottom) popwin:special-display-config)
  (push '("*Cargo Check*" :height 12 :stick t :position bottom :noselect t) popwin:special-display-config)
  (push '("*Racer Help*" :height 12 :stick t :position bottom :noselect t) popwin:special-display-config)
  (push '("*Warnings*" :height 7 :stick t :position bottom :noselect t) popwin:special-display-config)
  (push '(cider-repl-mode :height 9 :stick t :noselect t) popwin:special-display-config))

;; (use-package plantuml-mode
;;   :ensure t
;;   :init
;;   (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
;;   (setq plantuml-default-exec-mode 'jar))

;; (use-package flycheck-plantuml
;;   :after (flycheck plantuml)
;;   :ensure t
;;   :config
;;   (flycheck-plantuml-setup))

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
;; (use-package restclient :ensure t)
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
  (setq highlight-symbol-idle-delay 0.9)
  :hook (prog-mode . highlight-symbol-mode)
  )

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

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :hook (rust-mode . lsp)
;;   :config
;;   (setq lsp-enable-snippet nil)
;;   (set-face-attribute 'lsp-lens-face nil :height 0.9))

;; (use-package lsp-ui
;;   :after lsp
;;   :ensure t
;;   :hook (rust-mode . lsp-ui-mode)
;;   :config
;;   (evil-local-set-key 'normal "K" 'lsp-ui-doc-show)
;;   (evil-local-set-key 'normal (kbd "<SPC> k") 'lsp-ui-doc-hide)
;;   (setq lsp-ui-sideline-enable nil)
;;   (setq lsp-ui-doc-enable nil))

;; (use-package company-lsp
;;   :after company
;;   :ensure t
;;   :config
;;   (push 'company-lsp company-backends))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :ensure t
  :hook (prog-mode . flycheck-pos-tip-mode)
  )

(use-package clj-refactor :ensure t
  :after cider
  :config
  (setq cljr-warn-on-eval nil)
  :hook (cider-mode . clj-refactor-mode)
  )

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . cider-mode)
  :config
  (set-face-attribute 'clojure-keyword-face nil :inherit 'font-lock-function-name-face)
  )

(use-package cider
  :after clojure-mode
  :ensure t
  :config
  ;; (set-face-attribute 'cider-fringe-good-face nil :foreground nil)
  ;; (set-face-attribute 'cider-fringe-good-face nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'cider-fringe-good-face nil :foreground "#e45649")
  (define-key cider-mode-map (kbd "C-s") #'my/save-buffer)
  (setq clojure-toplevel-inside-comment-form t)
  (setq cider-clojure-cli-global-options nil)
  (setq cider-print-fn (quote fipp))
  (setq cider-print-quota 1000000)
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-stacktrace-default-filters '(tooling dup java REPL))
  (setq cider-save-file-on-load t)
  (setq nrepl-hide-special-buffers t)
  (define-key evil-normal-state-map (kbd "<SPC> h")
    (lambda () (interactive) (cider-clojuredocs)))
  (define-key evil-normal-state-map (kbd "<SPC> e")
    (lambda () (interactive) (cider-eval-last-sexp)))
  (define-key evil-normal-state-map (kbd "C-x C-x")
    (lambda () (interactive) (cider-eval-sexp-at-point)))
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
  :hook (rust-mode . cargo-minor-mode)
  :config (setq compilation-ask-about-save nil)
  )

(use-package flycheck-rust
  :after rust-mode
  :ensure t
  :hook (rust-mode . flycheck-rust-setup)
  )

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . lsp)
  :config
  (set-face-attribute 'rust-string-interpolation-face nil :slant 'normal)
  (set-face-attribute 'rust-string-interpolation-face nil :weight 'bold)
  (setq rust-format-on-save t)
  (evil-local-set-key 'normal "gd" 'lsp-find-definition)
  (evil-local-set-key 'motion "gd" 'lsp-find-definition)
  (evil-local-set-key 'normal (kbd "<SPC> r") 'lsp-find-references)
  :init
  (add-hook 'rust-mode-hook #'eldoc-mode)
  (add-hook 'rust-mode-hook #'hs-minor-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format ""))

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
  :hook ((prog-mode . turn-on-smartparens-strict-mode)
         (markdown-mode . turn-on-smartparens-strict-mode))
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
  :config
  (git-gutter-mode 1)
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
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-S-f") 'counsel-projectile-rg)
(global-set-key (kbd "C-S-r") 'projectile-replace)
(global-set-key (kbd "C-\\") 'treemacs)
(global-set-key (kbd "C-<return>") 'treemacs)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-p") 'evil-paste-pop)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "<f8>") 'shell-pop)
(global-set-key (kbd "C-M-=") 'my/indent-buffer)
(define-key evil-motion-state-map (kbd "C-o") 'my/jump-back)
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
(define-key evil-normal-state-map (kbd "<SPC> f") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "<SPC> w") 'save-buffer)
(define-key evil-normal-state-map (kbd "<SPC> o") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "C-<tab>") 'my/switch-to-last-buffer)
(define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'er/expand-region)
(global-set-key (kbd "C-<backspace>") 'my/switch-to-last-buffer)

(use-package which-key :ensure t :config (which-key-mode))
(use-package avy :ensure t)
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-show-cursor nil)
  (setq treemacs-project-follow-cleanup 1)
  (treemacs-resize-icons 15)
  (setq treemacs-eldoc-display t)
  (treemacs-follow-mode t)
  (setq treemacs-width 22)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-projectile :after treemacs :ensure t)
(use-package treemacs-evil :after treemacs :ensure t)
(use-package all-the-icons :ensure t)

(use-package doom-modeline :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes :ensure t)

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

;; my stuff

(defun my/jump-back ()
  "Jump back based on mode."
  (interactive)
  (if (eq major-mode 'clojure-mode)
      (cider-pop-back)
    (evil-jump-backward)))

(defun my/modeline-adjust (font-size)
  "Adjust modeline."
  (interactive)
  (setq doom-modeline-height 12)
  (setq doom-modeline-bar-width 3)
  (set-face-attribute 'mode-line nil :height (* 10 (- font-size 1)))
  (set-face-attribute 'mode-line-inactive nil :height (* 10 (- font-size 1)))
  ;; (set-face-attribute 'mode-line nil :height 0.9)
  ;; (set-face-attribute 'mode-line-inactive nil :height 0.9)
  (setq doom-modeline-icon t))

(defun my/theme ()
  "Load my theme."
  (interactive)
  (set-frame-name "Editor")
  (load-theme 'doom-one-light)
  ;; (load-theme 'doom-solarized-dark)
  (set-face-attribute 'default nil :background "#faf8f7")
  (set-face-attribute 'font-lock-constant-face nil :foreground nil)
  (set-face-attribute 'font-lock-builtin-face nil :foreground nil)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground nil)
  (set-face-attribute 'font-lock-variable-name-face nil :inherit nil)
  (set-face-attribute 'font-lock-doc-face nil :slant 'normal)
  (set-face-attribute 'font-lock-preprocessor-face nil :weight 'normal)
  (set-face-attribute 'treemacs-root-face nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'treemacs-root-face nil :height 1.1)
  (if (eq window-system 'x)
    (progn (set-frame-font "Fira Code Medium-13")
           (my/modeline-adjust 13))
    (progn (set-frame-font "Monaco-15")
           (my/modeline-adjust 15))))

(set-frame-width nil 87)
(set-frame-height nil 30)
(my/theme)

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
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(package-selected-packages
   '(deft ivy-postframe deadgrep which-key use-package treemacs-projectile treemacs-evil solaire-mode smex shell-pop restclient projectile-ripgrep popwin lsp-ui key-chord json-mode ivy-rich highlight-symbol git-gutter flycheck-rust flycheck-posframe flycheck-pos-tip flycheck-plantuml flycheck-joker expand-region exec-path-from-shell evil-smartparens evil-magit evil-collection doom-themes doom-modeline counsel-projectile company-lsp clj-refactor cargo almost-mono-themes)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
