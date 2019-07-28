;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; keybindings
(map! (:map override
        "C-\\" #'+treemacs/toggle
        "C-s" #'save-buffer
        "C-u" #'universal-argument
        "C-S-\"" #'eshell
        "C-\"" #'eshell
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

;; font and title
;; (set-frame-font "Fira Code-14")
(set-frame-font "Inconsolata-16")
(set-frame-name "Editor")
(setq evil-insert-state-cursor '((bar . 3) "red")
      evil-normal-state-cursor '(box "black"))
;; theme
(defun github-theme-with-mods ()
  "load github theme with my modifications"
  (interactive "*")
  (load-theme 'github t)
  (custom-set-faces
   '(show-paren-match ((t (:bold t :foreground "red" :background "#f8ee77"))))
   '(font-lock-function-name-face ((t (:foreground "#5B369C"))))
   '(cider-fringe-good-face ((t (:foreground "#34bd39"))))
   '(mode-line-inactive ((t (:height 0.9))))
   '(mode-line ((t (:height 0.9 :background "#F0F0F0" :box nil))))
   '(cider-test-error-face ((t (:background "red"))))
   '(cider-test-failure-face ((t (:background "orange"))))
   '(cider-test-success-face ((t (:background "green"))))
   '(ivy-minibuffer-match-face-1 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-2 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-3 ((t (:background "#f8eec7"))))
   '(ivy-minibuffer-match-face-4 ((t (:background "#f8eec7"))))
   '(avy-lead-face ((t (:foreground "red" :background "#f8eec7"))))
   ;; '(line-number ((t (:foreground "#969896" :background "#ffffff"))))
   '(line-number ((t (:foreground "#CECED0" :background "#ffffff"))))
   '(doom-modeline-bar ((t (:background "#183691" :bold t))))
   '(doom-modeline-buffer-file ((t (:inherit 'doom-modeline-project-dir))))
   '(mode-line-inactive ((t (:height 0.95 :box nil))))))
(github-theme-with-mods)

;; plantuml flycheck
(add-hook! plantuml-mode (flycheck-plantuml-setup))

;; enable smartparens strict
(add-hook! prog-mode (turn-on-smartparens-strict-mode) (evil-smartparens-mode))

;; disable rainbow
(add-hook! cider-mode (rainbow-delimiters-mode-disable))
(add-hook! clojure-mode (rainbow-delimiters-mode-disable))
(add-hook! emacs-lisp-mode (flycheck-mode -1) (rainbow-delimiters-mode-disable))

;; defaults
(setq-default
 term-scroll-show-maximum-output t
 term-scroll-to-bottom-on-output t
 evil-want-C-u-scroll t
 smartparens-global-mode t
 evil-smartparens-mode t
 line-number-mode nil
 column-number-mode nil
 show-smartparens-global-mode t
 smartparens-strict-mode t
 plantuml-jar-path "~/bin/plantuml.jar"
 plantuml-default-exec-mode 'jar
 company-idle-delay 0.3
 treemacs-width 28
 truncate-lines t
 doom-modeline-height 10
 doom-modeline-bar-width 3
 doom-modeline-persp-name nil
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
 projectile-project-search-path '("~/dev/"))

;; remove trailing whitespace automatically
(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; format on save for clojure
(defun cider-format-buffer-back () (interactive)
  (let (p)
    (setq p (point))
    (cider-format-buffer)
    (goto-char p)))

(defun add-clj-format-before-save () (interactive)
       (add-hook 'before-save-hook
                 'cider-format-buffer-back
                 t t))

(add-hook 'clojure-mode-hook 'add-clj-format-before-save)
