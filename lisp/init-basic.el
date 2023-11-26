;;; init-basic.el --- Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(when sys/macp
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defalias 'list-buffers 'ibuffer)

;;(setq
;; eww-search-prefix "https://www.bing.com"
;; ;;eww-search-prefix "https://www.google.com"
;; browse-url-browser-function 'eww-browse-url)

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
  (setq auto-save-visited-interval 2
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
                use-file-dialog nil
                use-dialog-box nil
                inhibit-startup-screen t
                inhibit-default-init t
                initial-scratch-message nil
                isearch-allow-motion t
                isearch-lazy-count t
                make-backup-files nil
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

;;(use-package elec-pair
;;  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
;;  :hook ((after-init . electric-pair-mode)
;;         (after-init . electric-indent-mode)
;;         (after-init . minibuffer-electric-default-mode)))

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

(use-package so-long
  :hook (after-init . global-so-long-mode))

(put 'narrow-to-region 'disabled nil)
(defun narrow-to-region-pop-mark (_ _) (pop-mark))
(advice-add #'narrow-to-region :after #'narrow-to-region-pop-mark)

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
