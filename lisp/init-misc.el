;;; init-misc.el --- misc configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; misc configurations.
;;

;;; Code:

(use-package benchmark-init
  :ensure t
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package which-key
  :diminish
  :init
  (which-key-mode 1))

;; Compatibility
(use-package compat :demand t)

;; Hanlde minified code
(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;;;; Show line numbers
;;(use-package display-line-numbers
;;  :ensure nil
;;  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
;;  :init
;;  (setq-default display-line-numbers-width 3)
;;  (setq display-line-numbers-width-start t))

;; hideshow
(use-package hideshow
  :diminish hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode))

;; History
(use-package desktop
  :ensure nil
  :init (desktop-save-mode t))

(use-package saveplace
  :ensure nil
  :init
  ;;(setq save-place-file (expand-file-name ".cache/places" user-emacs-directory))
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  ;;:bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 200
              ;; disable recentf-cleanup on Emacs start, because it can cause
              ;; problems with remote files
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
  :ensure nil
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

(use-package time
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook ((after-init . electric-pair-mode)
         (after-init . electric-indent-mode))
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  ;;(setq dired-listing-switches "-alh")
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

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always))

;;;; `find-dired' alternative using `fd'
;;(when (executable-find "fd")
;;  (use-package fd-dired))

;;;; Highlight the current line
;;(use-package hl-line
;;  :ensure nil
;;  :hook ((after-init . global-hl-line-mode)
;;         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
;;          (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (with-no-warnings
    ;; Display matching line for off-screen paren.
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit highlight)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; Check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; Rebind `minibuffer-message' called by `blink-matching-open'
        ;; to handle the overlay display.
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg ))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))


(use-package better-defaults)

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . (lambda ()
                        (windmove-default-keybindings 'super))))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy)
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)
         ("<home>" . mwim-beginning-of-line-or-code)
         ("<end>" . mwim-end-of-line-or-code)))


;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
              ("c" . rg-dwim-current-dir)
              ("f" . rg-dwim-current-file)
              ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("M-=" . er/expand-region))

;; Kill & Mark things easily
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Interactively insert and edit items from kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring)
  :hook (after-init . browse-kill-ring-default-keybindings)
  :init (setq browse-kill-ring-separator "────────────────"
              browse-kill-ring-separator-face 'shadow))

(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

;;(use-package pyim-wbdict)
(use-package pyim-basedict)
(use-package pyim
  :init
  (setq default-input-method "pyim")
  :config
  ;;(pyim-wbdict-v86-enable)
  ;;(setq pyim-default-scheme 'wubi)
  (pyim-basedict-enable)
  (setq pyim-default-scheme 'quanpin)
  (setq pyim-page-length 8)
  (setq pyim-scheme--enable-assistant-p t))

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

(use-package keyfreq
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line)))

(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")  ; libtool-bin
           (executable-find "make"))
  (use-package vterm
    :init (setq vterm-always-compile-module t)))

(use-package format-all)

(use-package symbol-overlay
  :diminish
  :config
  (define-key symbol-overlay-map (kbd "h") 'nil)
  :init
  (symbol-overlay-mode 1))

(when (or sys/macp sys/win32p)
  (use-package exec-path-from-shell))
(require 'color-rg)
(setq color-rg-search-no-ignore-file nil)

;;(require 'markmacro)
;;(global-set-key (kbd "s-/") 'markmacro-mark-words)
;;(global-set-key (kbd "s-?") 'markmacro-mark-lines)
;;(global-set-key (kbd "s-:") 'markmacro-mark-chars)
;;(global-set-key (kbd "s-L") 'markmacro-mark-imenus)
;;(global-set-key (kbd "s-<") 'markmacro-apply-all)
;;(global-set-key (kbd "s->") 'markmacro-apply-all-except-first)
;;(global-set-key (kbd "s-M") 'markmacro-rect-set)
;;(global-set-key (kbd "s-D") 'markmacro-rect-delete)
;;(global-set-key (kbd "s-F") 'markmacro-rect-replace)
;;(global-set-key (kbd "s-I") 'markmacro-rect-insert)
;;(global-set-key (kbd "s-C") 'markmacro-rect-mark-columns)
;;(global-set-key (kbd "s-S") 'markmacro-rect-mark-symbols)

;;(setq thing-edit-show-message-p nil)
;;(require 'thing-edit)
;;(defvar peng-thing-edit-map (make-sparse-keymap)
;;  "Keybinding for `thing-edit'")
;;;; Copy.
;;(define-key peng-thing-edit-map (kbd "w") 'thing-copy-word)
;;(define-key peng-thing-edit-map (kbd "s") 'thing-copy-symbol)
;;(define-key peng-thing-edit-map (kbd "m") 'thing-copy-email)
;;(define-key peng-thing-edit-map (kbd "f") 'thing-copy-filename)
;;(define-key peng-thing-edit-map (kbd "u") 'thing-copy-url)
;;(define-key peng-thing-edit-map (kbd "x") 'thing-copy-sexp)
;;(define-key peng-thing-edit-map (kbd "t") 'thing-copy-page)
;;(define-key peng-thing-edit-map (kbd "g") 'thing-copy-sentence)
;;(define-key peng-thing-edit-map (kbd "o") 'thing-copy-whitespace)
;;(define-key peng-thing-edit-map (kbd "i") 'thing-copy-list)
;;(define-key peng-thing-edit-map (kbd "c") 'thing-copy-comment)
;;(define-key peng-thing-edit-map (kbd "h") 'thing-copy-defun)
;;(define-key peng-thing-edit-map (kbd "p") 'thing-copy-parentheses)
;;(define-key peng-thing-edit-map (kbd "l") 'thing-copy-line)
;;(define-key peng-thing-edit-map (kbd "a") 'thing-copy-to-line-beginning)
;;(define-key peng-thing-edit-map (kbd "e") 'thing-copy-to-line-end)
;;;; Replace
;;(define-key peng-thing-edit-map (kbd "W") 'thing-replace-word)
;;(define-key peng-thing-edit-map (kbd "S") 'thing-replace-symbol)
;;(define-key peng-thing-edit-map (kbd "M") 'thing-replace-email)
;;(define-key peng-thing-edit-map (kbd "F") 'thing-replace-filename)
;;(define-key peng-thing-edit-map (kbd "U") 'thing-replace-url)
;;(define-key peng-thing-edit-map (kbd "X") 'thing-replace-sexp)
;;(define-key peng-thing-edit-map (kbd "T") 'thing-replace-page)
;;(define-key peng-thing-edit-map (kbd "G") 'thing-replace-sentence)
;;(define-key peng-thing-edit-map (kbd "O") 'thing-replace-whitespace)
;;(define-key peng-thing-edit-map (kbd "I") 'thing-replace-list)
;;(define-key peng-thing-edit-map (kbd "C") 'thing-replace-comment)
;;(define-key peng-thing-edit-map (kbd "H") 'thing-replace-defun)
;;(define-key peng-thing-edit-map (kbd "P") 'thing-replace-parentheses)
;;(global-set-key (kbd "M-h") peng-thing-edit-map)

;;;; Enforce rules for popups
;;(use-package popper
;;  :defines popper-echo-dispatch-actions
;;  :autoload popper-group-by-directory
;;  :bind (:map popper-mode-map
;;         ("C-h z"     . popper-toggle-latest)
;;         ("C-<tab>"   . popper-cycle)
;;         ("C-M-<tab>" . popper-toggle-type))
;;  :hook (emacs-startup . popper-mode)
;;  :init
;;  (setq popper-group-function #'popper-group-by-directory)
;;  (setq popper-reference-buffers
;;        '("\\*Messages\\*"
;;          "Output\\*$" "\\*Pp Eval Output\\*$"
;;          "^\\*eldoc.*\\*$"
;;          "\\*Compile-Log\\*"
;;          "\\*Completions\\*"
;;          "\\*Warnings\\*"
;;          "\\*Async Shell Command\\*"
;;          "\\*Apropos\\*"
;;          "\\*Backtrace\\*"
;;          "\\*Calendar\\*"
;;          "\\*Finder\\*"
;;          "\\*Kill Ring\\*"
;;          "\\*Go-Translate\\*"
;;          "\\*Embark \\(Collect\\|Live\\):.*\\*"
;;
;;          bookmark-bmenu-mode
;;          comint-mode
;;          compilation-mode
;;          help-mode helpful-mode
;;          tabulated-list-mode
;;          Buffer-menu-mode
;;
;;          flymake-diagnostics-buffer-mode
;;          flycheck-error-list-mode flycheck-verify-mode
;;
;;          gnus-article-mode devdocs-mode
;;          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
;;          youdao-dictionary-mode osx-dictionary-mode fanyi-mode
;;
;;          "^\\*Process List\\*" process-menu-mode
;;          list-environment-mode cargo-process-mode
;;
;;          "^\\*eshell.*\\*.*$"       eshell-mode
;;          "^\\*shell.*\\*.*$"        shell-mode
;;          "^\\*terminal.*\\*.*$"     term-mode
;;          "^\\*vterm[inal]*.*\\*.*$" vterm-mode
;;
;;          "\\*DAP Templates\\*$" dap-server-log-mode
;;          "\\*ELP Profiling Restuls\\*" profiler-report-mode
;;          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
;;          "\\*[Wo]*Man.*\\*$"
;;          "\\*ert\\*$" overseer-buffer-mode
;;          "\\*gud-debug\\*$"
;;          "\\*lsp-help\\*$" "\\*lsp session\\*$"
;;          "\\*quickrun\\*$"
;;          "\\*tldr\\*$"
;;          "\\*vc-.*\\*$"
;;          "^\\*macro expansion\\**"
;;
;;          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
;;          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
;;          "\\*docker-.+\\*"
;;          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
;;          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
;;          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))
;;
;;  (setq popper-echo-dispatch-actions t)
;;  :config
;;  (popper-echo-mode 1)
;;
;;  (with-no-warnings
;;    (defun my-popper-fit-window-height (win)
;;      "Determine the height of popup window WIN by fitting it to the buffer's content."
;;      (fit-window-to-buffer
;;       win
;;       (floor (frame-height) 3)
;;       (floor (frame-height) 3)))
;;    (setq popper-window-height #'my-popper-fit-window-height)
;;
;;    (defun popper-close-window-hack (&rest _)
;;      "Close popper window via `C-g'."
;;      ;; `C-g' can deactivate region
;;      (when (and (called-interactively-p 'interactive)
;;                 (not (region-active-p))
;;                 popper-open-popup-alist)
;;        (let ((window (caar popper-open-popup-alist)))
;;          (when (window-live-p window)
;;            (delete-window window)))))
;;    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))


;;;; Persistent the scratch buffer
;;(use-package persistent-scratch
;;  :diminish
;;  :bind (:map persistent-scratch-mode-map
;;              ([remap kill-buffer] . (lambda (&rest _)
;;                                       (interactive)
;;                                       (user-error "Scratch buffer cannot be killed")))
;;              ([remap revert-buffer] . persistent-scratch-restore)
;;              ([remap revert-this-buffer] . persistent-scratch-restore))
;;  :hook ((after-init . persistent-scratch-autosave-mode)
;;         (lisp-interaction-mode . persistent-scratch-mode))
;;  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
;;              persistent-scratch-backup-directory
;;              (expand-file-name "persistent-scratch" user-emacs-directory)))

;;(when (not emacs/>=29p)
;;  (use-package sqlite3))

;; Sqlite
(when (fboundp 'sqlite-open)
  (use-package emacsql-sqlite-builtin))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start)
              (message "server-started"))))


(use-package doom-themes)
(use-package gruvbox-theme)

(load-theme 'gruvbox t)
;;(load-theme 'doom-dark+ t)
;;(load-theme 'doom-one t)


(provide 'init-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
