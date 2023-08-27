;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Version control systems.
;;

;;; Code:

;;(use-package magit
;;  :commands (magit-status))

;; Git
;; See `magit-maybe-define-global-key-bindings'
(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;;;; Access Git forges from Magit
  ;;(use-package forge
  ;;  :demand t
  ;;  :defines (forge-database-connector forge-topic-list-columns)
  ;;  :custom-face
  ;;  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  ;;  :init
  ;;  (setq forge-database-connector (if (and (require 'emacsql-sqlite-builtin nil t)
  ;;                                          (functionp 'emacsql-sqlite-builtin)
  ;;                                          (functionp 'sqlite-open))
  ;;                                     'sqlite-builtin
  ;;                                   'sqlite)
  ;;        forge-topic-list-columns
  ;;        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
  ;;          ("Title" 60 t nil title  nil)
  ;;          ("State" 6 t nil state nil)
  ;;          ("Updated" 10 t nil updated nil))))
  ;;
  ;;;; Show TODOs in magit
  ;;(use-package magit-todos
  ;;  :defines magit-todos-nice
  ;;  :commands magit-todos--scan-with-git-grep
  ;;  :init
  ;;  (setq magit-todos-nice (if (executable-find "nice") t nil))
  ;;  (setq magit-todos-scanner #'magit-todos--scan-with-git-grep)
  ;;  (let ((inhibit-message t))
  ;;    (magit-todos-mode 1))
  ;;  (when sys/win32p
  ;;    (setq magit-todos-exclude-globs '(patterns)))
  ;;  :config
  ;;  (with-eval-after-load 'magit-status
  ;;    (transient-append-suffix 'magit-status-jump '(0 0 -1)
  ;;      '("t " "Todos" magit-todos-jump-to-todos))))
  )

;;;; Walk through git revisions of a file
;;(use-package git-timemachine
;;  :custom-face
;;  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
;;  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
;;  :bind (:map vc-prefix-map
;;              ("t" . git-timemachine))
;;  :hook ((git-timemachine-mode . (lambda ()
;;                                   "Improve `git-timemachine' buffers."
;;                                   ;; Display different colors in mode-line
;;                                   (if (facep 'mode-line-active)
;;                                       (face-remap-add-relative 'mode-line-active 'custom-state)
;;                                     (face-remap-add-relative 'mode-line 'custom-state))
;;
;;                                   ;; Highlight symbols in elisp
;;                                   (and (derived-mode-p 'emacs-lisp-mode)
;;                                        (fboundp 'highlight-defined-mode)
;;                                        (highlight-defined-mode t))
;;
;;                                   ;; Display line numbers
;;                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
;;                                        (fboundp 'display-line-numbers-mode)
;;                                        (display-line-numbers-mode t))))
;;         (before-revert . (lambda ()
;;                            (when (bound-and-true-p git-timemachine-mode)
;;                              (user-error "Cannot revert the timemachine buffer"))))))
;;
;;;; Pop up last commit information of current line
;;(use-package git-messenger
;;  :bind (:map vc-prefix-map
;;              ("p" . git-messenger:popup-message)
;;              :map git-messenger-map
;;              ("m" . git-messenger:copy-message))
;;  :init (setq git-messenger:show-detail t
;;              git-messenger:use-magit-popup t))
;;
;;;; Resolve diff3 conflicts
;;(use-package smerge-mode
;;  :ensure nil
;;  :diminish
;;  :hook ((find-file . (lambda ()
;;                        (save-excursion
;;                          (goto-char (point-min))
;;                          (when (re-search-forward "^<<<<<<< " nil t)
;;                            (smerge-mode 1)))))))
;;
;;
;;;; Git configuration modes
;;(use-package git-modes)

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :bind (:map diff-hl-command-map
			  ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if sys/linuxp #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))


(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
