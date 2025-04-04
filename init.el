(package-initialize)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ;("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 5)
        ;("MELPA Stable" . 10)
        ("MELPA"        . 0)))

;; bootstrap if needed
(unless (package-installed-p 'package+)
  (package-refresh-contents)
  (package-install 'package+))

;; package management
(package-manifest
 'adaptive-wrap
 'bind-key
 'buffer-move
 'company
 'dhall-mode
 'diminish
 'doom-modeline
 'elm-mode
 'expand-region
 'haskell-mode
 'helm
 'helm-ag
 'helm-projectile
 'helm-xref
 'highlight-numbers
 'highlight-symbol
 'markdown-mode
 'monokai-theme
 'move-text
 'nerd-icons
 'package+
 'projectile
 'rainbow-delimiters
 'rainbow-identifiers
 'rainbow-mode
 'rust-mode
 'smart-tabs-mode
 'undo-tree
 'use-package
 'which-key
 'winum
 'ws-butler
 'yaml-mode
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

(set-frame-font "Monospace 10")

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

(defun mouse-clipboard-yank-at-click (click arg)
  "Insert the clipboard contents at the position clicked on.
Also move point to one end of the text thus inserted (normally the end),
and set mark at the beginning.
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (when select-active-regions
    ;; Without this, confusing things happen upon e.g. inserting into
    ;; the middle of an active region.
    (deactivate-mark))
  (or mouse-yank-at-point (mouse-set-point click))
  (setq this-command 'yank)
  (setq mouse-selection-click-count 0)
  (let ((select-enable-clipboard t)
        ;; Ensure that we defeat the DWIM logic in `gui-selection-value'
        ;; (i.e., that gui--clipboard-selection-unchanged-p returns nil).
        (gui--last-selected-text-clipboard nil))
    (yank arg)))
(global-set-key (kbd "<mouse-2>") #'mouse-clipboard-yank-at-click)

(global-set-key (kbd "S-<insert>") #'clipboard-yank)        ; make shift-insert access clipboard only
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

; fix yank on xwayland
(if (eq window-system 'x)
    (setq select-enable-clipboard nil))

(setq
 frame-resize-pixelwise t                                   ; fix resizing on xwayland
 ispell-dictionary "english"                                ; use english dictionary
 tags-add-tables nil                                        ; don't ask for keeping old TAGS
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
 mouse-wheel-scroll-amount '(5)
 mouse-wheel-progressive-speed nil                          ; don't accelerate when mouse scrolling
 compilation-ask-about-save nil                             ; auto-save before compilaction
 compilation-scroll-output t                                ; always scroll compilation output
 tags-revert-without-query t                                ; don't ask about TAGS reloading
 comment-fill-column 80                                     ; a bit bigger comment fill column
 default-fill-column 80                                     ; a bit bigger default fill column
 large-file-warning-threshold 104857600                     ; 100MB warning threshold
 ring-bell-function 'ignore                                 ; ignore the bell
 js-indent-level 2
 warning-minimum-level :emergency
 )

;; disable scroll margin in term-mode
(add-hook 'term-mode-hook
          (lambda ()
            (setq-local scroll-margin 0)))

;; reindent after jumping to definition
(add-hook 'xref-after-jump-hook
          (lambda ()
            (progn (recenter) (xref-pulse-momentarily) (back-to-indentation))))

;; Fix "Other window" action and respect xref-after-jump-hook
(eval-after-load "helm-xref"
  '(progn
     (defun helm-xref-source ()
       "Return a `helm' source for xref results."
       (helm-build-sync-source "Helm Xref"
         :candidates helm-xref-alist
         :persistent-action (lambda (item)
                              (progn
                                (helm-xref-goto-xref-item item 'switch-to-buffer)
                                (helm-highlight-current-line)))
         :action '(("Switch to buffer" . (lambda (item) (helm-xref-goto-xref-item item 'switch-to-buffer)))
                   ("Other window" . (lambda (item) (helm-xref-goto-xref-item item 'switch-to-buffer-other-window))))
         :candidate-number-limit 9999))
     (defun helm-xref-goto-xref-item (item func)
       "Set buffer and point according to xref-item ITEM.

Use FUNC to display buffer."
       (let* ((summary (xref-item-summary item))
              (location (xref-item-location item))
              (marker (xref-location-marker location))
              (buf (marker-buffer marker))
              (offset (marker-position marker)))
         (funcall func buf)
         (goto-char offset)
         (run-hooks 'xref-after-jump-hook)))))

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
    (defun my-cc-mode-setup () (c-set-style "mine"))
    (add-hook 'c-mode-hook 'my-cc-mode-setup)
    (add-hook 'c++-mode-hook 'my-cc-mode-setup)))

(use-package company
  :diminish company-mode
  :bind ("<C-tab>" . company-complete)
  :init
  (progn
    (setq company-idle-delay 0.2
          company-dabbrev-downcase nil
          company-minimum-prefix-length 3)
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    (setq company-backends '(company-bbdb
                             company-semantic
                             company-cmake
                             ;company-clang
                             company-files
                             (company-dabbrev-code company-gtags company-etags company-capf company-keywords)
                             company-oddmuse
                             company-dabbrev))))

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'"
  :config
  (setq dhall-use-header-line nil
          dhall-format-at-save nil))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 20
        doom-modeline-project-detection 'projectile
        doom-modeline-icon nil
        doom-modeline-unicode-fallback t
        doom-modeline-buffer-file-name-style 'relative-to-project)
  :config
  (progn
    ;; Define your custom doom-modeline
    (doom-modeline-def-modeline 'mine
      '(workspace-name window-number buffer-position modals matches buffer-info remote-host word-count parrot selection-info)
      '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
    ;; Add to `doom-modeline-mode-hook` or other hooks
    (defun setup-custom-doom-modeline ()
      (doom-modeline-set-modeline 'mine 'default))
    (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package diminish
  :init
  (progn
    (eval-after-load "abbrev" '(diminish 'abbrev-mode))))

(use-package haskell-mode
  :init
  (progn
    (defun my-haskell-mode-setup ()
      (progn
        (setq-local projectile-tags-command "ghc-tags -e")))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-setup)))

(use-package helm
  :bind (;;("M-."     . helm-etags-select)
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
    (require 'helm-xref)
    (global-set-key (kbd "C-c h") helm-command-prefix))
  :config
  (progn
    (setq helm-ag-insert-at-point 'symbol
          helm-display-buffer-default-height 1
          helm-autoresize-max-height 33
          helm-autoresize-min-height 33
          helm-display-header-line nil
          helm-etags-fuzzy-match nil
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
    (helm-autoresize-mode 1)))

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

(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-completion-system 'helm
          projectile-enable-caching 'persistent
          ;projectile-enable-idle-timer t
          projectile-indexing-method 'hybrid
          projectile-sort-order 'recentf)

    (projectile-mode)
    (helm-projectile-on)

    ;; Overwrite cabal project with nix-style commands
    (projectile-register-project-type 'haskell-cabal #'projectile-cabal-project-p
                                      :compile "cabal new-build"
                                      :test "cabal new-test"
                                      :test-suffix "Spec")

    (setq helm-ag-base-command "rg -j4 --hidden --no-heading")
    ;; redefine helm-projectile-ag so that ripgrep works
    (defun helm-projectile-ag (&optional options)
      "Helm version of projectile-ag."
      (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
      (if (require 'helm-ag nil  'noerror)
          (if (projectile-project-p)
              (let* ((grep-find-ignored-files (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
                     (grep-find-ignored-directories (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
                     (ignored (mapconcat (lambda (i)
                                           (concat "-g !" i))
                                         (append grep-find-ignored-files grep-find-ignored-directories (cadr (projectile-parse-dirconfig-file)))
                                         " "))
                     (helm-ag-base-command (concat helm-ag-base-command " " ignored " " options))
                     (current-prefix-arg nil))
                (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
            (error "You're not in a project"))
        (error "helm-ag not available")))

    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "<f8>")   #'projectile-compile-project)
    (define-key projectile-mode-map (kbd "M-<f8>") #'kill-compilation)))

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
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
    (global-undo-tree-mode 1)))

(use-package which-key
  :config
  (which-key-mode))

(use-package winum
  :init
  (progn
    (setq winum-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
            (define-key map (kbd "M-1") 'winum-select-window-1)
            (define-key map (kbd "M-2") 'winum-select-window-2)
            (define-key map (kbd "M-3") 'winum-select-window-3)
            (define-key map (kbd "M-4") 'winum-select-window-4)
            (define-key map (kbd "M-5") 'winum-select-window-5)
            (define-key map (kbd "M-6") 'winum-select-window-6)
            (define-key map (kbd "M-7") 'winum-select-window-7)
            (define-key map (kbd "M-8") 'winum-select-window-8)
            (define-key map (kbd "M-9") 'winum-select-window-9)
            map))
    (winum-mode)))

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
(put 'projectile-compilation-command     'safe-local-variable #'stringp)
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
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
