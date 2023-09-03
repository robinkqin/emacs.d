;;; init-basic.el --- Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs Configuration.
;;


;;; Code:

;; Personal information
(setq user-full-name "Robin"
      user-mail-address "robinkqin@163.com")

(when sys/macp
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(when emacs/<29p
  (setq package-native-compile nil))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max (* 1024 1024))
(setq large-file-warning-threshold (* 32 1024 1024))

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default indent-tabs-mode nil
              fill-column 80
              tab-width 4
              c-basic-offset 4
              c-default-style "linux")

(setq delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      save-interprogram-paste-before-kill t ; Save clipboard contents into kill-ring before replace them
      make-backup-files nil
      auto-save-default nil
      sentence-end-double-space nil
      word-wrap-by-category t)

(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s ")

(setq auto-save-visited-interval 2)
(add-hook 'after-init-hook #'auto-save-visited-mode) ; auto save buffers after modify

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(when (not (display-graphic-p))
  (setq
   browse-url-browser-function 'eww-browse-url        ; Use eww as the default browser
   eww-search-prefix "https://www.bing.com")      ; Use another engine for searching
  (when sys/linuxp
    (setq eww-search-prefix "https://www.google.com")))

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
