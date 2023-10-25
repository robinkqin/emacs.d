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

(use-package ace-pinyin
  :diminish
  :ensure t
  :hook (after-init . ace-pinyin-global-mode))

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

;;(when (and module-file-suffix           ; dynamic module
;;           (executable-find "cmake")
;;           (executable-find "libtool")  ; libtool-bin
;;           (executable-find "make"))
;;  (use-package vterm
;;    :ensure t
;;    :init (setq vterm-always-compile-module t)))

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
