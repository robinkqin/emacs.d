;;; init-onefile.el --- the entry of emacs config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; alias ec='emacsclient -t -a ""'
;; format: format-all
;; definition/reference: ctags, gtags, xref
;; refactor: color-rg, symbol-overlay
;; completion: corfu(vertico, orderless, consult), eglot
;; tools: git, clangd/clang-format, universal-ctags, global, ripgrep
;; dict: sdcv, fanyi
;; gtags: export GTAGSOBJDIRPREFIX=~/.cache/gtags/
;; lsp-bridge: pip install epc orjson sexpdata six paramiko requests cmake-language-server

;;(add-to-list 'load-path (concat user-emacs-directory "lisp"))
;;(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;;(defconst my/eglot-enable nil)
;;(defconst my/lsp-bridge-enable nil)

(defconst sys/win32p (eq system-type 'windows-nt))
(defconst sys/macp (eq system-type 'darwin))

(when sys/macp
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defalias 'list-buffers 'ibuffer)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless (fboundp 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package package
  :init
  (setq use-package-enable-imenu-support t
        use-package-expand-minimally t
        use-package-verbose t))

(use-package gnu-elpa-keyring-update
  :ensure t)
(use-package diminish
  :ensure t)

(use-package time
  :init (setq display-time-24hr-format t
              display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

;;(use-package display-line-numbers
;;  :init
;;  (setq-default display-line-numbers-width 3)
;;  (setq display-line-numbers-width-start t)
;;  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode))

(use-package files
  :init
  (setq auto-save-visited-interval 3
        save-silently t
        confirm-nonexistent-file-or-buffer nil
        large-file-warning-threshold (* 32 1024 1024)
        read-process-output-max (* 1024 1024))
  :hook (after-init . auto-save-visited-mode))

(use-package simple
  :init
  (setq column-number-mode t
        line-number-mode t
        ;;kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  (setq-default save-interprogram-paste-before-kill t
                auto-window-vscroll nil
                inhibit-startup-screen t  ; disable the startup screen splash
                isearch-allow-motion t
                isearch-lazy-count t
                make-backup-files nil   ; disable backup file
                use-short-answers t)

  (setq-default indent-tabs-mode nil
                word-wrap t
                fill-column 80
                tab-width 4
                c-basic-offset 4
                c-default-style "linux")

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun my/enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . my/enable-trailing-whitespace)))

(use-package desktop
  :init (desktop-save-mode t))

(use-package saveplace
  :init
  (setq save-place-forget-unreadable-files t)
  :hook (after-init . save-place-mode))

(use-package recentf
  :bind (("C-c r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 200
              recentf-auto-cleanup 'never
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              ;;savehist-file (expand-file-name ".cache/history" user-emacs-directory)
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 30))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package paren
  :init
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
  :hook (after-init . show-paren-mode))

(use-package elec-pair
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :hook ((after-init . electric-pair-mode)
         (after-init . electric-indent-mode)
         (after-init . minibuffer-electric-default-mode)))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package dired
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "open")
          ("\\.docx\\'" "open")
          ("\\.\\(?:djvu\\|eps\\)\\'" "open")
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
          ("\\.\\(?:xcf\\)\\'" "open")
          ("\\.csv\\'" "open")
          ("\\.tex\\'" "open")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
           "open")
          ("\\.\\(?:mp3\\|flac\\)\\'" "open")
          ("\\.html?\\'" "open")
          ("\\.md\\'" "open")))

  (put 'dired-find-alternate-file 'disabled nil)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always))

(use-package ediff
  :hook((ediff-prepare-buffer . outline-show-all)
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

(use-package xref
  :init
  (setq xref-search-program 'ripgrep
        xref-auto-jump-to-first-definition 'show
        xref-auto-jump-to-first-xref 'show))



(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pyim-wbdict
  :ensure t)
(use-package pyim-basedict
  :ensure t)
(use-package pyim
  :after pyim-wbdict pyim-basedict
  :ensure t
  :init
  (setq default-input-method "pyim")
  :config
  (pyim-wbdict-v86-enable)
  (setq pyim-default-scheme 'wubi)
  ;;(pyim-basedict-enable)
  ;;(setq pyim-default-scheme 'quanpin)
  (setq pyim-page-length 8)
  (setq pyim-scheme--enable-assistant-p t))

(use-package beginend
  :ensure t
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))

(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)
         ("<home>" . mwim-beginning-of-line-or-code)
         ("<end>" . mwim-end-of-line-or-code)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :ensure t
  :init
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package wgrep
  :ensure t
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(when (or sys/macp sys/win32p)
  (use-package exec-path-from-shell
    :ensure t))
(require 'color-rg)
(setq color-rg-search-no-ignore-file nil)

(use-package rg
  :ensure t
  :hook (after-init . rg-enable-default-bindings)
  :init (setq rg-group-result t
              rg-show-columns t)
  :config (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package browse-kill-ring
  :ensure t
  :bind ("C-c C-k" . browse-kill-ring)
  :hook (after-init . browse-kill-ring-default-keybindings)
  :init (setq browse-kill-ring-separator "────────────────"
              browse-kill-ring-separator-face 'shadow))

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 4)

  ;;;; Add prompt indicator to `completing-read-multiple'.
  ;;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;;(defun crm-indicator (args)
  ;;  (cons (format "[CRM%s] %s"
  ;;              (replace-regexp-in-string
  ;;               "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;               crm-separator)
  ;;              (car args))
  ;;        (cdr args)))
  ;;(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;;
  ;;;; Do not allow the cursor in the minibuffer prompt
  ;;(setq minibuffer-prompt-properties
  ;;      '(read-only t cursor-intangible t face minibuffer-prompt))
  ;;(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)
  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        ;;completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :init
  (setq vertico-count 17
        vertico-resize nil
        vertico-cycle t)
  :bind (:map vertico-map
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              :map minibuffer-local-map
              ("C-w" . backward-kill-word)))

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(require 'vertico-repeat)
(global-set-key (kbd "C-c C-r") 'vertico-repeat)
(global-set-key (kbd "<f2>") 'vertico-repeat)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(use-package marginalia
  :ensure t
  ;;;; Either bind `marginalia-cycle' globally or only in the minibuffer
  ;;:bind (("M-A" . marginalia-cycle)
  ;;       :map minibuffer-local-map
  ;;       ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package consult
  :ensure t
  :after vertico
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)

         ([remap Info-search]        . consult-info)
         ([remap imenu]              . consult-imenu)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ("M-s b" . consult-bookmark)
         ;;("M-s d" . consult-find)
         ;;("M-s D" . consult-locate)
         ("M-s G" . consult-grep)
         ("M-s g" . consult-git-grep)
         ;;("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;;("M-s k" . consult-keep-lines)
         ;;("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;;("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . (lambda ()
                    "Insert the currunt symbol."
                    (interactive)
                    (insert (save-excursion
                              (set-buffer (window-buffer (minibuffer-selected-window)))
                              (or (thing-at-point 'symbol t) "")))))
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   ;;consult-theme :preview-key '(:debounce 0.2 any)
   ;;consult-ripgrep consult-git-grep consult-grep
   ;;consult-bookmark consult-recent-file consult-xref
   ;;consult--source-bookmark consult--source-file-register
   ;;consult--source-recent-file consult--source-project-recent-file
   ;;;; :preview-key "M-."
   ;;:preview-key '(:debounce 0.4 any)
   consult-goto-line
   consult-theme :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package embark
  :ensure t
  :after vertico
  :bind
  (("C-c e e" . embark-export)
   ("C-c e a" . embark-act)         ;; pick some comfortable binding
   ("C-c e d" . embark-dwim)        ;; good alternative: M-.)
   ("C-c e b" . embark-bindings) ;; alternative for `describe-bindings'
   ([remap describe-bindings] . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)
              ("C-c C-c" . embark-act))
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(global-unset-key (kbd "M-i"))
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.2 . 0.1))
  ;;(corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; nil: Never quit, even if there is no match
  (corfu-preview-current nil)
  ;;;;(corfu-preselect-first nil)    ;; Disable candidate preselection
  ;;(corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;;(corfu-scroll-margin 5)        ;; Use scroll margin

  ;;;; Use TAB for cycling, default is `corfu-complete'.
  ;;:bind
  ;;(:map corfu-map
  ;;      ("TAB" . corfu-next)
  ;;      ([tab] . corfu-next)
  ;;      ("S-TAB" . corfu-previous)
  ;;      ([backtab] . corfu-previous))

  ;;;; Enable Corfu only for certain modes.
  ;;:hook ((prog-mode . corfu-mode)
  ;;       (c-mode . corfu-mode)
  ;;       (c++-mode . corfu-mode)
  ;;       (shell-mode . corfu-mode)
  ;;       (eshell-mode . corfu-mode))

  ;;;; Recommended: Enable Corfu globally.
  ;;;; This is recommended since Dabbrev can be used globally (M-/).
  ;;;; See also `corfu-excluded-modes'.
  ;;:init
  ;;(global-corfu-mode)

  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  ;;:bind ("M-/" . completion-at-point)
  :bind ("M-i M-i" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (;;("M-i M-i" . completion-at-point) ;; capf
         ("M-i i" . completion-at-point) ;; capf
         ("M-i p" . completion-at-point) ;; capf
         ("M-i t" . complete-tag)        ;; etags
         ("M-i d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-i h" . cape-history)
         ("M-i f" . cape-file)
         ("M-i k" . cape-keyword)
         ("M-i s" . cape-elisp-symbol)
         ("M-i e" . cape-elisp-block)
         ("M-i a" . cape-abbrev)
         ("M-i l" . cape-line)
         ("M-i w" . cape-dict)
         ("M-i \\" . cape-tex)
         ("M-i _" . cape-tex)
         ("M-i ^" . cape-tex)
         ("M-i &" . cape-sgml)
         ("M-i r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-etymon-provider
                     fanyi-longman-provider)))

(require 'sdcv)
(setq sdcv-say-word-p t)
(if sys/win32p
    (setq sdcv-dictionary-data-dir "C:\\software\\stardict\\stardict")
  (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic"))
(setq sdcv-dictionary-simple-list
      '("kdic-ec-11w"))
(setq sdcv-dictionary-complete-list
      '("kdic-ec-11w"
        "oxford-gb-formated"  ;; *.ifo bookname=xxx
        "quick_eng-zh_CN"))

(when sys/macp
  (use-package osx-dictionary
    :ensure t))


(use-package symbol-overlay
  :ensure t
  :diminish
  :config (define-key symbol-overlay-map (kbd "h") 'nil)
  :init (symbol-overlay-mode 1))

(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

(use-package format-all
  :ensure t
  :diminish format-all-mode
  :bind
  ("C-c f" . #'format-all-region-or-buffer))

(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(use-package eldoc
  :diminish
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package cmake-mode
  :ensure t)

;;(use-package flymake
;;  :hook (prog-mode . flymake-mode)
;;  :bind (("M-n" . #'flymake-goto-next-error)
;;   ("M-p" . #'flymake-goto-prev-error)))

;;(use-package eglot
;;  :hook (prog-mode . eglot-ensure))

(use-package puni
  :ensure t
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'vterm-mode-hook #'puni-disable-puni-mode)
  (add-hook 'eww-mode-hook #'puni-disable-puni-mode)
  (add-hook 'minibuffer-mode-hook #'puni-disable-puni-mode))

(use-package citre
  :ensure t
  :diminish
  :bind (:map prog-mode-map
              ("C-x c j" . citre-jump+)
              ("C-x c p" . citre-peek)
              ("C-x c a" . citre-ace-peek))
  :init
  ;;(require 'citre-config)
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (when sys/win32p
    (setq citre-gtags-args '("--compact")))

  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (let* ((xref-prompt-for-identifier nil))
               (call-interactively #'xref-find-definitions)))))

  (defun my--push-point-to-xref-marker-stack (&rest r)
    (message "push mark")
    (xref-push-marker-stack (point-marker)))
  (dolist (func '(find-function
                  consult-imenu consult-imenu-multi
                  consult-line consult-grep consult-git-grep consult-ripgrep
                  ;;consult-outline consult-eglot-symbols
                  ;;beginning-of-buffer end-of-buffer jump-to-register mark-whole-buffer
                  ;;beginend-prog-mode-goto-end beginend-prog-mode-goto-beginning
                  ;;mwim-beginning-of-code-or-line mwim-end-of-code-or-line
                  ;;next-buffer previous-buffer switch-to-buffer describe-function
                  ;;describe-variable find-file-at-point xref-find-definitions
                  ;;session-jump-to-last-change avy-goto-word-1 avy-goto-word-2
                  embark-act keyboard-escape-quit
                  embark-next-symbol embark-previous-symbol
                  citre-jump))
    (advice-add func :before 'my--push-point-to-xref-marker-stack)))

(use-package gruvbox-theme
  :ensure t)
(load-theme 'gruvbox t)

(use-package keyfreq
  :ensure t
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line)))

(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode))



(defun my/c-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook #'my/c-hook)
(add-hook 'c++-mode-hook #'my/c-hook)

(defun my/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f12>") 'my/open-init-file)

(defun my/project-root-dir ()
  "Return root directory of the current project."
  (let ((project (project-current)))
    (if project
        (cond
         ((fboundp 'project-root) (project-root project))
         ((fboundp 'project-roots) (car (project-roots project))))
      default-directory)))

(defun my/color-rg-search-symbol-in-project-from-ynak ()
  (interactive)
  (color-rg-search-input (substring-no-properties (car kill-ring))
                         (my/project-root-dir)))


(defun my/xref-find-references-at-point ()
  (interactive)
  (xref-find-references (thing-at-point 'symbol)))
(defun my/xref-find-references-from-yank ()
  (interactive)
  (xref-find-references (substring-no-properties (car kill-ring))))


(defalias 'my/highlight-symbol-at-point 'highlight-symbol-at-point)
(defun my/unhighlight-symbol-at-point ()
  "Remove highlight of symbol at point."
  (interactive)
  (unhighlight-regexp (concat "\\_<" (thing-at-point 'symbol t) "\\_>")))
(defun my/unhighlight-symbol-all ()
  "Remove all highlight symbols."
  (interactive)
  (unhighlight-regexp t))


(defun my/copy-symbol-at-point ()
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(defun my/grep-from-ynak ()
  (interactive)
  (consult-ripgrep (my/project-root-dir)
                   (substring-no-properties (car kill-ring))))

(defun my/consult-line-from-ynak ()
  (interactive)
  (consult-line (substring-no-properties (car kill-ring))))

(defun my/bookmark-at-point ()
  (interactive)
  (consult-bookmark (thing-at-point 'symbol)))

;; https://emacs-china.org/t/xxx-thing-at-point/18047
(defvar my/fly-commands
  '(query-replace-regexp
    flush-lines keep-lines
    consult-line consult-grep consult-git-grep consult-ripgrep consult-man
    ;;consult-citre consult-eglot-symbols
    xref-find-references xref-find-apropos))
(defvar my/fly-back-commands
  '(self-insert-command
    ;;delete-forward-char kill-word kill-sexp
    ;;end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
    yank yank-pop org-yank))
(defun my/fly-back-to-present ()
  (remove-hook 'pre-command-hook 'my/fly-back-to-present t)
  (cond ((and (memq last-command my/fly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command my/fly-back-commands)
         (delete-region (point) (point-max)))))
(defun my/fly-time-travel ()
  (when (memq this-command my/fly-commands)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some
                                    (lambda (thing) (thing-at-point thing t))
                                    '(region url symbol))
                                   ;; '(symbol url region sexp))
                                   ""))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'my/fly-back-to-present nil t)))
(add-hook 'minibuffer-setup-hook #'my/fly-time-travel)


(define-key key-translation-map (kbd "M-p") (kbd "C-p"))
(define-key key-translation-map (kbd "M-n") (kbd "C-n"))
(define-key key-translation-map (kbd "M-q") (kbd "C-v"))
(define-key key-translation-map (kbd "M-c") (kbd "C-g"))
(define-key key-translation-map (kbd "M-<SPC>") (kbd "M-x"))

(global-set-key (kbd "M-s M-s") 'consult-git-grep)
(global-set-key (kbd "M-s s") 'consult-grep)

(global-set-key (kbd "M-s v") symbol-overlay-map)

(global-set-key (kbd "M-s ]") 'diff-hl-next-hunk)
(global-set-key (kbd "M-s [") 'diff-hl-previous-hunk)

(global-set-key (kbd "M-s f") 'find-file)
(global-set-key (kbd "M-s e") 'consult-buffer)
(global-set-key (kbd "M-s r") 'consult-recent-file)

(global-set-key (kbd "M-s x") 'kill-current-buffer)
(global-set-key (kbd "M-s u") 'vundo)

(global-set-key (kbd "M-s i") 'consult-imenu)

(global-set-key (kbd "M-s ;") 'my/bookmark-at-point)
(global-set-key (kbd "M-s '") 'consult-bookmark)

(global-set-key (kbd "M-s ,") 'my/copy-symbol-at-point)
(global-set-key (kbd "M-s j") 'my/grep-from-ynak)
(global-set-key (kbd "M-s k") 'my/consult-line-from-ynak)
(global-set-key (kbd "M-s l") 'my/color-rg-search-symbol-in-project-from-ynak)

(global-set-key (kbd "M-s y") 'fanyi-dwim2)
(if sys/macp
    (global-set-key (kbd "M-s d") 'osx-dictionary-search-pointer)
  (global-set-key (kbd "M-s d") 'sdcv-search-pointer))

(global-set-key (kbd "M-o") 'project-find-file)

(global-set-key (kbd "M-j") 'consult-ripgrep)
(global-set-key (kbd "M-k") 'consult-line)
(global-set-key (kbd "M-l") 'color-rg-search-symbol-in-project) ;; M-\\
(global-set-key (kbd "M-r") 'rg-menu)
(global-set-key (kbd "M-t") 'format-all-buffer)
(global-set-key (kbd "M-;") 'avy-goto-char-timer)
(global-set-key (kbd "M-'") 'avy-goto-char-in-line)

(global-set-key (kbd "M-m") 'symbol-overlay-put)
(global-set-key (kbd "M-u") 'symbol-overlay-remove-all)

(global-set-key (kbd "M-/") 'my/xref-find-references-at-point)
(global-set-key (kbd "M-s /") 'my/xref-find-references-from-yank)

(when (package-installed-p 'citre)
  (global-set-key (kbd "M-.") 'citre-jump+)
  (global-set-key (kbd "M-/") 'citre-jump-to-reference)
  ;;(global-set-key (kbd "M-]") 'xref-find-definitions)
  (message "citre keymaps"))

;;(setq custom-file (locate-user-emacs-file "custom.el"))
;;(when (file-exists-p custom-file)
;;  (load custom-file))

(provide 'init-onefile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-onefile.el ends here
