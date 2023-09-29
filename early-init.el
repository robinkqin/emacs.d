;;; early-init.el --- Emacs 27 introduces early-init.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(unless (>= emacs-major-version 27)
  (error "ONLY EMACS v27+ IS SUPPORTED!"))

;; For speed up the startup, please do NOT forget reset it to default
;; after Emacs after-init-hook, or it may cause freezes.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Prevent unwanted runtime compilation for native-comp users
(setq inhibit-automatic-native-compilation t)

;; Package initialize occurs automatically, before `user-init-file' is loaded
;; but after `early-init-file'. If you want to handle package initialization,
;; you can prevent Emacs from doing it early by uncomment next line!
(setq package-enable-at-startup nil)

;; [From DOOM Emacs]
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Clean GUI
(push '(menu-bar-mode . nil) default-frame-alist)
(push '(tool-bar-mode . nil) default-frame-alist)
(push '(scroll-bar-mode . nil) default-frame-alist)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Misc settings
(setq mode-line-compact t)

(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
