;;; init-misc.el --- misc configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pyim
  :ensure t
  :init
  (use-package pyim-wbdict
    :ensure t
    :config
    (pyim-wbdict-v86-enable))
  (use-package pyim-basedict
    :ensure t
    :config
    (pyim-basedict-enable))
  (setq pyim-default-scheme 'wubi)
  (setq default-input-method "pyim")
  (setq pyim-page-length 9))

;;(use-package rime
;;  :ensure t
;;  :init
;;  (setq default-input-method "rime")
;;  (setq rime-user-data-dir "~/.config/ibus/rime/"))
;;
;;(if sys/win32p
;;    (setq default-input-method "pyim")
;;  (setq default-input-method "rime"))

(use-package beginend
  :ensure t
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))

;; Move to the beginning/end of line or code
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(use-package ace-window
  :ensure t
  ;;:config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

(use-package avy
  :ensure t
  :init
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package ace-pinyin
  :diminish
  :ensure t
  :hook (after-init . ace-pinyin-global-mode))

(use-package wgrep
  :ensure t
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;;(when (or sys/macp sys/win32p)
;;  (use-package exec-path-from-shell
;;    :ensure t))
(when (or sys/macp sys/linuxp (daemonp))
  (use-package exec-path-from-shell
    :ensure t
    :custom (exec-path-from-shell-arguments '("-l"))
    :init (exec-path-from-shell-initialize)))

(require 'color-rg)
(setq color-rg-search-no-ignore-file nil)

(require 'markmacro)

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

;;;; Increase selected region by semantic units
;;(use-package expand-region
;;  :ensure t
;;  :bind ("C-=" . er/expand-region)
;;  :config
;;  (when (my/treesit-available-p)
;;    (defun treesit-mark-bigger-node ()
;;      "Use tree-sitter to mark regions."
;;      (let* ((root (treesit-buffer-root-node))
;;             (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
;;             (node-start (treesit-node-start node))
;;             (node-end (treesit-node-end node)))
;;        ;; Node fits the region exactly. Try its parent node instead.
;;        (when (and (= (region-beginning) node-start) (= (region-end) node-end))
;;          (when-let ((node (treesit-node-parent node)))
;;            (setq node-start (treesit-node-start node)
;;                  node-end (treesit-node-end node))))
;;        (set-mark node-end)
;;        (goto-char node-start)))
;;    (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node)))

;;;; Smartly select region, rectangle, multi cursors
;;(use-package smart-region
;;  :ensure t
;;  :hook (after-init . smart-region-on))

;;;; Hungry deletion
;;(use-package hungry-delete
;;  :ensure t
;;  :diminish
;;  :hook (after-init . global-hungry-delete-mode)
;;  :init (setq hungry-delete-chars-to-skip " \t\f\v"
;;              hungry-delete-except-modes
;;              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))
;;
;;;; Goto last change
;;(use-package goto-chg
;;  :bind ("C-," . goto-last-change))
;;
;;;; Handling capitalized subwords in a nomenclature
;;(use-package subword
;;  :ensure nil
;;  :diminish
;;  :hook ((prog-mode . subword-mode)
;;         (minibuffer-setup . subword-mode)))

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

;; Workspace configurations.
;;(use-package tabspaces)

(use-package symbol-overlay
  :ensure t
  :diminish
  :config (define-key symbol-overlay-map (kbd "h") 'nil)
  :init (symbol-overlay-mode 1))

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

(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")  ; libtool-bin
           (executable-find "make"))
  (use-package vterm
    :ensure t
    :init (setq vterm-always-compile-module t)))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))
(use-package doom-themes
  :ensure t)
(use-package gruvbox-theme
  :ensure t)

(if (display-graphic-p)
    (load-theme 'gruvbox t)
  (load-theme 'gruvbox t))

;; Enforce rules for popups
(use-package popper
  :ensure t
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  :bind (:map popper-mode-map
         ("C-h z"       . popper-toggle)
         ("C-<tab>"     . popper-cycle)
         ("C-M-<tab>"   . popper-toggle-type))
  :hook (emacs-startup . popper-echo-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          "\\*Async Shell Command\\*$"
          "\\*Apropos\\*$"
          "\\*Backtrace\\*$"
          "\\*Calendar\\*$"
          "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
          "\\*Kill Ring\\*$"
          "\\*Embark \\(Collect\\|Live\\):.*\\*$"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode sdcv-mode

          "^\\*Process List\\*$" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\**"
          "\\*diff-hl\\**"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))

  :config
  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

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

(provide 'init-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
