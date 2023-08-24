;;; init-basic.el --- Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs Configuration.
;;


;;; Code:

(when sys/macp
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(setq gc-cons-threshold (* 512 1024 1024))
(setq read-process-output-max (* 16 1024 1024))
(setq large-file-warning-threshold (* 32 1024 1024))

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-auto-revert-mode t)

(winner-mode t)

;;(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)
(show-paren-mode t)

(desktop-save-mode t)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again

(setq dired-dwim-target t)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alh --group-directories-first")
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")

(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s ")

(setq auto-save-visited-interval 1)
(add-hook 'after-init-hook #'auto-save-visited-mode) ; auto save buffers after modify

(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'global-so-long-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (setq show-trailing-whitespace 1)))

(add-hook 'prog-mode-hook #'hs-minor-mode)


(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
