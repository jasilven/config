;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     rust
     javascript
     clojure
     clojure-lint
     markdown
     evil-cleverparens
     restclient
     html
     (go :variables
         go-use-golangci-lint t
         go-format-before-save t
         compile-command "go build -v"
         gofmt-command "goimports"
         go-play-browse-function "firefox"
         go-tab-width 4)
     helm
     auto-completion
     better-defaults
     emacs-lisp
     themes-megapack
     (version-control :variables
                      version-control-diff-tool 'git-cutter
                      version-control-global-margin t)
     git
     theming
     syntax-checking
     (org :variables
          org-directory "~/Notes"
          org-default-notes-file "~/Notes/notes.org"
          org-agenda-files (list "~/Notes")
          org-capture-templates
          (quote (("n" "note" entry (file "~/Notes/notes.org") "* %? %T\n"))))
     (shell)
     )
   dotspacemacs-additional-packages '(doom-themes beacon idea-darkula-theme doom-modeline all-the-icons doom-themes flycheck-joker)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(persp-mode eyebrowse spaceline highlight-parentheses python-mode yasnippet rainbow-delimiters emoji)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   multi-term-dedicated-close-back-to-open-buffer-p t
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(() ())
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(doom-vibrant doom-one sanityinc-tomorrow-night  sanityinc-tomorrow-day doom-one doom-one-light idea-darkula leuven solarized-dark doom-opera-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code" :size 17)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key ";"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.6
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 100
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil))
(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (set-frame-name "Editor")
  (kill-buffer "*spacemacs*")
  ;; popwin
  (push '("*cider-scratch*" :dedicated t :position bottom :stick t :noselect t :height 0.3) popwin:special-display-config)
  (push '("*cider-result*" :dedicated t :position bottom :stick t :noselect t :height 0.3) popwin:special-display-config)
  (push '("^*cider-repl" :regexp t :dedicated t :position bottom :stick t :noselect t :height 0.15) popwin:special-display-config)
  (push '("^*shell" :regexp t :dedicated nil :position bottom :stick t :noselect t :height 0.20) popwin:special-display-config)
  (push '("^*eshell" :regexp t :dedicated nil :position bottom :stick t :noselect t :height 0.20) popwin:special-display-config)
  (push '("^*ansi-term" :regexp t :dedicated nil :position bottom :stick t :noselect t :height 0.20) popwin:special-display-config)
  (push '("^*Racer" :regexp t :dedicated nil :position bottom :stick t :noselect t :height 0.3) popwin:special-display-config)
  (push '("^*Cargo Run" :regexp t :dedicated nil :position bottom :stick nil :noselect t :height 0.3) popwin:special-display-config)
  (push '("^*Cargo Build" :regexp t :dedicated nil :position bottom :stick nil :noselect nil :height 0.4) popwin:special-display-config)
  (push '("^*godoc" :regexp t :dedicated nil :position bottom :stick nil :noselect nil :height 0.3) popwin:special-display-config)
  (push '("^*Go" :regexp t :dedicated nil :position bottom :stick nil :noselect nil :height 0.3) popwin:special-display-config)
  (push '("^*gud" :regexp t :dedicated nil :position bottom :stick t :height 0.3) popwin:special-display-config)
  (push '("^*compilation" :regexp t :dedicated nil :position bottom :stick t :height 0.3) popwin:special-display-config)
  ;; sexp-fu
  (setq eval-sexp-fu-flash-duration 0.2)
  ;; go-mode
  (defun my/go-mode ()
    (paredit-mode 1)
    (line-number-mode 1)
    (define-key go-mode-map (kbd "C-c C-c") '(lambda () (compile)))
    (define-key go-mode-map (kbd "C-x C-x") '(lambda () (and (save-buffer) (spacemacs/go-run-main)))))
  (add-hook 'go-mode-hook 'my/go-mode)
  ;; rust
  (defun save-and-cargo-build ()
    (interactive)
    (save-buffer)
    (cargo-process-build))
  (defun save-and-cargo-run()
    (interactive)
    (save-buffer)
    (cargo-process-run))
  (defun my/rust-mode ()
    (paredit-mode 1)
    (line-number-mode 1)
    (setq rust-format-on-save t)
    (define-key rust-mode-map (kbd "C-i") 'spacemacs/jump-to-definition)
    (define-key rust-mode-map (kbd "M-,") 'evil-jump-backward)
    (define-key rust-mode-map (kbd "C-c C-c") 'save-and-cargo-build)
    (define-key rust-mode-map (kbd "C-x C-x") 'save-and-cargo-run))
  (add-hook 'rust-mode-hook 'my/rust-mode)
  ;; dired
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (defun my/dired-mode ()
    (line-number-mode -1)
    (define-key dired-mode-map (kbd "C-<up>") 'dired-up-directory))
  (add-hook 'dired-mode-hook 'my/dired-mode)
  ;; general modes
  (setq scalable-fonts-allowed t)
  (beacon-mode 1)
  (global-company-mode 1)
  (global-visual-line-mode 1)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (blink-cursor-mode 1)
  (column-number-mode -1)
  (line-number-mode -1)
  (menu-bar-mode -1)
  (midnight-mode 1)
  (global-hl-line-mode 1)
  (set-default 'truncate-lines t)
  (midnight-delay-set 'midnight-delay "4:30am")
  (spacemacs/toggle-vi-tilde-fringe-off)
  (spacemacs/toggle-transparency)
  (setq-default
   vc-follow-symlinks t
   flycheck-pos-tip-timeout 20
   eldoc-echo-area-use-multiline-p nil
   evil-move-cursor-back nil
   blink-cursor-blinks 50
   clean-buffer-list-delay-general 1
   cursor-in-non-selected-windows nil
   evil-escape-key-sequence "kj"
   evil-escape-unordered-key-sequence t
   font-lock-builtin-face nil
   font-lock-type-face nil
   font-lock-variable-name-face nil
   mac-right-option-modifier nil
   midnight-period 7200 ;; 2 hours
   mouse-wheel-progressive-speed nil
   mouse-wheel-scroll-amount '(6)
   org-image-actual-width (/ (display-pixel-width) 3)
   show-help-function nil
   show-paren-style 'expression
   tab-width 4
   set-default-coding-systems 'utf-8
   set-language-environment "UTF-8"
   )
  ;; helm
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 23)
  (setq helm-autoresize-min-height 23)
  (setq helm-split-window-in-side-p t)
  ;; doom
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-height 15)
  (setq doom-modeline-bar-width 5)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-persp-name nil)
  (doom-themes-neotree-config)
  ;; global keys
  (spacemacs/set-leader-keys "SPC" 'helm-descbinds)
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-q") 'delete-window)
  (global-set-key (kbd "C-*") 'spacemacs/shell-pop-ansi-term)
  (global-set-key (kbd "M-w") 'next-multiframe-window)
  (global-set-key (kbd "C-M-k") 'my/kill-buffer-and-window)
  (global-set-key (kbd "M-j") 'avy-goto-char)
  (global-set-key (kbd "C-<tab>") 'helm-buffers-list)
  (global-set-key (kbd "C-x d") 'dired-jump)
  (global-set-key (kbd "C-x C-d") 'dired-jump)
  (global-set-key (kbd "C-=") 'indent-whole-buffer)
  (global-set-key (kbd "C-s-<up>") 'enlarge-window)
  (global-set-key (kbd "C-s-<down>") 'shrink-window)
  (global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
  (define-key evil-normal-state-map (kbd "f") 'avy-goto-char)
  (define-key evil-normal-state-map (kbd "C-f") 'helm-swoop)
  (define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
  (define-key evil-normal-state-map (kbd "ä") 'evil-forward-paragraph)
  (define-key evil-normal-state-map (kbd "ö") 'evil-backward-paragraph)
  ;; evil
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-normal-state-map "\C-e" 'end-of-line)
  ;; neotree
  (setq neo-window-width 26)
  (setq neo-show-hidden-files nil)
  (add-hook 'neotree-mode-hook '(lambda () (spacemacs/scale-down-font)))
  ;; hooks
  (defun my/elisp-mode ()
    (paredit-mode t))
  (add-hook 'emacs-lisp-mode-hook 'my/elisp-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (defun my/cider-docview-mode ()
    (visual-line-mode 1))
  (add-hook 'cider-docview-mode-hook 'my/cider-docview-mode)
  (defun my/cider-repl-mode ()
    (visual-line-mode 1))
  (add-hook 'cider-repl-mode-hook 'my/cider-repl-mode)
  (defun my/kill-buffer-and-window ()
    (interactive)
    (if (string= "*cider-scratch*" (buffer-name))
        (delete-window)
      (kill-buffer-and-window)))
  (defun my/cider-mode-hook ()
    (spacemacs/toggle-aggressive-indent-on)
    (setq cider-auto-select-error-buffer nil)
    (setq cider-repl-use-pretty-printing t)
    (setq cider-show-error-buffer nil)
    (setq cider-jack-in-default 'clojure-cli)
    (setq cljr-warn-on-eval nil)
    (setq cider-prompt-for-symbol nil)
    (setq nrepl-hide-special-buffers t)
    (setq cider-stacktrace-default-filters '(project))
    (setq cider-stacktrace-fill-column 80)
    (setq cider-save-file-on-load t)
    (setq cider-repl-display-help-banner nil)
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq cider-repl-pop-to-buffer-on-connect 'display-only)
    (paredit-mode 1)
    (define-key cider-mode-map (kbd "C-c s") 'cider-scratch)
    (define-key cider-mode-map (kbd "C-x C-x") 'cider-eval-last-sexp)
    (define-key cider-mode-map (kbd "C-x C-e") 'cider-eval-last-sexp)
    (define-key cider-mode-map (kbd "C-SPC") 'cider-eval-defun-at-point)
    (define-key cider-mode-map (kbd "M-SPC") 'cider-eval-defun-at-point)
    (define-key cider-mode-map (kbd "C-c C-i") 'cider-pprint-eval-last-sexp-to-comment))
  (add-hook 'cider-mode-hook 'my/cider-mode-hook)
  (add-hook 'cider-repl-mode-hook '(lambda ()
                                     (setq scroll-conservatively 101)
                                     (spacemacs/scale-down-font)))
  ;; shell
  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "TAB") #'company-complete)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "TAB") #'company-complete)))
  ;; functions
  (defun indent-whole-buffer ()
    (interactive)
    (indent-region (point-min) (point-max))
    (message "Whole buffer indented"))
  (defun insert-date ()
    (interactive)
    (insert (format-time-string "%Y-%m-%d %a")))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#073642" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (flycheck-gometalinter toml-mode racer flycheck-rust cargo rust-mode beacon auto-dim-other-buffers git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-joker flycheck-clojure parseedn parseclj a org-category-capture alert log4e gntp markdown-mode magit-popup parent-mode haml-mode pos-tip flycheck flx highlight transient git-commit with-editor iedit smartparens anzu evil goto-chg undo-tree f autothemer web-completion-data s know-your-http-well go-mode inflections edn multiple-cursors paredit peg eval-sexp-fu spinner queue pkg-info clojure-mode epl bind-map bind-key packed async auto-complete popup web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode idea-darkula-theme yasnippet dash diff-hl parinfer company restclient doom-themes helm-cider doom-modeline eldoc-eval shrink-path all-the-icons memoize lv cider-hydra sesman org-plus-contrib restclient-helm helm-gitignore helm-css-scss helm-company helm-c-yasnippet helm helm-core cider magit expand-region dumb-jump ace-window ace-link projectile avy hydra zenburn-theme zen-and-art-theme xterm-color ws-butler winum white-sand-theme which-key wgrep web-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs request rebecca-theme rainbow-delimiters railscasts-theme purple-haze-theme pug-mode professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el paradox orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http noctilux-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum linum-relative link-hint light-soap-theme jbeans-theme jazz-theme ivy-hydra ir-black-theme inkpot-theme indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy flycheck-pos-tip flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dracula-theme django-theme diminish define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme counsel-projectile company-web company-statistics company-restclient company-go column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-jump-helm-line ac-ispell)))
 '(popwin-mode t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-debug-code-overlay-face ((t (:foreground "dark orange" :box (:line-width -1 :color "grey" :style released-button)))))
 '(cider-fringe-good-face ((t (:foreground "forest green"))))
 '(cider-result-overlay-face ((t (:foreground "dark orange" :box (:line-width -1 :color "gray") :weight bold))))
 '(eval-sexp-fu-flash ((t (:background "dark olive green" :foreground "white" :weight bold))))
 '(sp-show-pair-match-face ((t (:inherit nil :foreground "red" :weight bold)))))
