(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap if needed
(unless (package-installed-p 'package+)
  (package-install 'package+))

;; package management
(package-manifest
 'adaptive-wrap
 'bind-key
 'buffer-move
 'company
 'company-c-headers
 'diminish
 'expand-region
 'ggtags
 'guide-key
 'haskell-mode
 'helm
 'helm-ag
 'helm-gtags
 'helm-projectile
 'highlight-numbers
 'highlight-symbol
 'markdown-mode
 'monokai-theme
 'move-text
 'package+
 'projectile
 'rainbow-delimiters
 'rainbow-identifiers
 'rainbow-mode
 'rust-mode
 'smart-mode-line
 'smart-tabs-mode
 'undo-tree
 'use-package
 'window-number
 'ws-butler
 )

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; load local settings if applicable
(let ((my/local-settings-file "~/.emacs.d/local.el"))
  (when (file-readable-p my/local-settings-file)
    (load-file my/local-settings-file)))

;;;;;;;;;; GUI SETTINGS ;;;;;;;;;;

(set-default-font "Monospace 10")

(setq monokai-doc-face-as-comment t
      monokai-foreground    "#F5F5F5"
      monokai-background    "#1B1E1C"
      monokai-comments      "#AAAA9A"
      monokai-emphasis      "#FFFAFA"
      monokai-highlight     "#303030"
      monokai-highlight-alt "#2A2921")

(load-theme 'monokai t)                                     ; theme
(set-scroll-bar-mode 'nil)                                  ; no scrollbar
(tool-bar-mode -1)                                          ; no toolbar
(menu-bar-mode -1)                                          ; no menubar

(defun set-wide-frame ()
  (interactive)
  (when (boundp 'my/gui-height)
          (set-frame-size (selected-frame) (+ 2 (* 2 90)) my/gui-height)))

(defun set-narrow-frame ()
  (interactive)
  (when (and (boundp 'my/gui-width) (boundp 'my/gui-height))
          (set-frame-size (selected-frame) my/gui-width my/gui-height)))

(if window-system
    (set-narrow-frame)
  (menu-bar-mode -1))

;; white cursor
(set-cursor-color "#ffffff")
;; do not blink
(blink-cursor-mode 0)

;;;;;;;;;; EDITOR SETTINGS ;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)                           ; stop making me type yes/no
(global-visual-line-mode 1)                                 ; soft word wrap
(show-paren-mode 1)                                         ; highlight matching parens
(global-auto-revert-mode 1)                                 ; auto revert files globally

(setq-default
 frame-title-format "%b - %F"                               ; display file info in window title
 tags-case-fold-search nil                                  ; make etags search case sensitive
 require-final-newline t                                    ; insert newline at the end
 indent-tabs-mode nil                                       ; indent with spaces
 tab-width 2                                                ; tab width to 2
 fill-column 80                                             ; a bit bigger fill column
 )

(setq
 initial-major-mode 'text-mode                              ; set initial mode to text
 initial-scratch-message nil                                ; make scratch buffer empty
 inhibit-splash-screen t                                    ; do not show splash screen
 scroll-error-top-bottom t                                  ; modern page-up/down
 column-number-mode t                                       ; enable column numbers
 backup-directory-alist `((".*" . "~/.backup"))             ; store backup files in one place
 auto-save-file-name-transforms `((".*" "~/.backup/" t))    ; store auto-save files in one place
 scroll-margin 5                                            ; scroll with moderate margin
 scroll-conservatively 101                                  ; smooth scrolling
 scroll-preserve-screen-position 'always                    ; preserve screen position on page up/down
 mouse-wheel-progressive-speed nil                          ; don't accelerate when mouse scrolling
 compilation-ask-about-save nil                             ; auto-save before compilaction
 compilation-scroll-output t                                ; always scroll compilation output
 comment-fill-column 80                                     ; a bit bigger comment fill column
 default-fill-column 80                                     ; a bit bigger default fill column
 js-indent-level 2
 )

;; disable scroll margin in term-mode
(add-hook 'term-mode-hook
          (lambda ()
            (setq-local scroll-margin 0)))


;;;;;;;;;; PACKAGES ;;;;;;;;;;

(use-package adaptive-wrap
  :init
  (progn
    (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)))

(use-package buffer-move
  :bind (("<M-S-up>" . buf-move-up)
         ("<M-S-down>" . buf-move-down)
         ("<M-S-left>" . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

(use-package cc-mode
  :init
  (progn
    ;; open .h and .tcc files as c++
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))

    ;; customize default indentation style
    (c-add-style "mine" '("bsd"
                          (c-basic-offset . 2)
                          (tab-width . 2)
                          (indent-tabs-mode . t)
                          (c-offsets-alist
                           (inextern-lang . 0)
                           (innamespace . 0))
                          ))
    (defun my-cc-mode-setup () (progn
                                 (c-set-style "mine")
                                 (ggtags-mode 1)
                                 (setq-local projectile-tags-command "gtags")
                                 (setq-local completion-at-point-functions nil)))
    (add-hook 'c-mode-hook 'my-cc-mode-setup)
    (add-hook 'c++-mode-hook 'my-cc-mode-setup)))

(use-package company
  :diminish company-mode
  :bind ("<C-tab>" . company-complete)
  :init
  (progn
    (setq company-clang-arguments '("-std=c++14")
          company-idle-delay 0.2
          company-dabbrev-downcase nil
          company-minimum-prefix-length 2)
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    (eval-after-load 'company-etags
      '(progn
         (add-to-list 'company-etags-modes 'haskell-mode)))
    (setq company-backends '(company-nxml
                             company-css
                             company-cmake
                             company-files
                             company-c-headers
                             (company-dabbrev-code company-gtags company-etags company-capf company-keywords)
                             company-dabbrev))))

(use-package company-c-headers
  :config
  (progn
    (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/5.4.0/include/g++-v5/")))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package diminish
  :init
  (progn
    (eval-after-load "abbrev" '(diminish 'abbrev-mode))))

(use-package ggtags
  :bind (:map ggtags-mode-map
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack))
  :diminish ggtags-mode)

(use-package guide-key
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence t
          guide-key/recursive-key-sequence-flag t
          guide-key/popup-window-position 'bottom)
    (guide-key-mode 1)))

(use-package haskell-mode
  :init
  (progn
    (defun my-haskell-mode-setup ()
      (progn
        (setq-local projectile-tags-command
                    "hasktags -e -x -f TAGS *")))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-setup)))

(use-package helm
  :bind (("M-."     . helm-etags-select)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-h g"   . helm-google-suggest)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-s s"   . helm-occur))
  :init
  (progn
    (require 'helm-config)
    (require 'helm)
    (global-set-key (kbd "C-c h") helm-command-prefix))
  :config
  (progn
    (setq helm-ag-insert-at-point 'symbol
          helm-autoresize-max-height 33
          helm-autoresize-min-height 33
          helm-display-header-line nil
          helm-etags-match-part-only 'tag
          helm-ff-file-name-history-use-recentf t
          helm-ff-skip-boring-files t
          helm-split-window-in-side-p t
          helm-prevent-escaping-from-minibuffer t)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; make <tab> work in terminal
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ;; list actions using C-z
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    ;; make history usable
    ;;(define-key helm-map (kbd "<left>") nil)
    ;;(define-key helm-map (kbd "<right>") nil)
    (define-key helm-map (kbd "<XF86Back>") nil)
    (define-key helm-map (kbd "M-<up>") 'previous-history-element)
    (define-key helm-map (kbd "<XF86Forward>") nil)
    (define-key helm-map (kbd "M-<down>") 'next-history-element)

    (helm-mode 1)
    (helm-autoresize-mode 1)
    (helm-projectile-on)

    (define-key projectile-mode-map (kbd "<f8>")   #'projectile-compile-project)
    (define-key projectile-mode-map (kbd "M-<f8>") #'kill-compilation)))

(use-package highlight-numbers
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("C-<f5>" . highlight-symbol)
         ("<f5>"   . highlight-symbol-next)
         ("M-<f5>" . highlight-symbol-prev)
         ("S-<f5>" . highlight-symbol-query-replace))
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)))

(use-package move-text
  :init
  (progn
    (move-text-default-bindings)))

(use-package popwin
  :config
  (progn
    (setq popwin:special-display-config nil)
    (push '("*Backtrace*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*compilation*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("*Compile-Log*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.33)
          popwin:special-display-config)
    (push '("*Help*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Shell Command Output*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '(" *undo-tree*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Warnings*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("^\\*Man .*\\*$"
            :regexp t    :position bottom :stick t :noselect nil :height 0.33)
            popwin:special-display-config)
    (popwin-mode 1)))

(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    ;; Overwrite cabal project with nix-style commands
    (projectile-register-project-type 'haskell-cabal #'projectile-cabal-project-p
                                      :compile "cabal new-build"
                                      :test "cabal new-test"
                                      :test-suffix "Spec")
    (setq projectile-completion-system 'helm
          projectile-enable-caching t
          projectile-indexing-method 'hybrid
          projectile-sort-order 'recentf)
    (projectile-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rainbow-identifiers
  :init
  (progn
    (setq-default rainbow-identifiers-choose-face-function
                  'rainbow-identifiers-cie-l*a*b*-choose-face)
    (setq-default rainbow-identifiers-cie-l*a*b*-lightness 90)
    (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)))

(use-package recentf
  :init
  (progn
    (setq recentf-max-menu-items 50
          recentf-max-saved-items 10000)
    (recentf-mode 1)))

(use-package rust-mode
  :init
  (progn
    (setq rust-indent-offset 2)))

(use-package smart-mode-line
  :init
  (progn
    (setq sml/theme 'respectful
          sml/no-confirm-load-theme t)
    (sml/setup)))

(use-package smart-tabs-mode
  :init
  (progn
    (smart-tabs-insinuate 'c 'c++)))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind (("C-x u" . undo-tree-visualize)
         ("C-z"   . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  :config
  (progn
    (global-undo-tree-mode 1)))

(use-package window-number
  :config
  (progn
    (window-number-mode)
    (window-number-meta-mode)))

(use-package ws-butler
 :diminish ws-butler-mode
 :init
 (progn
   (ws-butler-global-mode 1)))

;;;;;;;;;; KEYBINDINGS ;;;;;;;;;;

;; rebind search and replace
(bind-key "C-S-s" 'query-replace)
(bind-key "C-S-r" 'query-replace-regexp)

;; use cycle-spacing instead of just-one-space
(bind-key "M-SPC" (lambda nil (interactive) (cycle-spacing -1)))
(bind-key "S-M-SPC" (lambda nil (interactive) (cycle-spacing 1)))

;; rebind repeat
(bind-key "C-`" 'repeat)

;; bind shell
(bind-key "C-x m" 'eshell)
;; bind term
(bind-key "C-x C-m" 'term)

;; bind navigation of compilation errors
(bind-key "<f7>" 'next-error)
(bind-key "M-<f7>" 'previous-error)

;; do not minimize window
(unbind-key "C-x C-z")

;;;;;;;;;; SAFE VARIABLES ;;;;;;;;;;

(put 'company-clang-arguments            'safe-local-variable #'listp)
(put 'projectile-tags-command            'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'haskell-indentation-starter-offset 'safe-local-variable #'numberp)
(put 'haskell-indentation-left-offset    'safe-local-variable #'numberp)
(put 'haskell-indentation-layout-offset  'safe-local-variable #'numberp)

;;;;;;;;;; FUNCTIONS ;;;;;;;;;;

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
  Prefixed with negative \\[universal-argument], sorts in reverse.

  The variable `sort-fold-case' determines whether alphabetic case
  affects the sort order.

  See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;;;;;;;;; CUSTOM VARIABLES ;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile helm ws-butler window-number use-package undo-tree smart-tabs-mode smart-mode-line rust-mode rainbow-mode rainbow-identifiers rainbow-delimiters package+ move-text highlight-symbol highlight-numbers helm-projectile helm-ag guide-key expand-region company-c-headers buffer-move adaptive-wrap))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
