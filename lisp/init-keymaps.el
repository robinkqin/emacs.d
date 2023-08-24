;;; init-keymaps.el --- Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs Configuration.
;;


;;; Code:


;;C-.
;;C-x c
;;C-x g
;;C-x j
;;C-x w
;;C-x y
;;C-x C-a
;;C-x C-g
;;C-x C-y

;;(define-key key-translation-map (kbd "your-key") (kbd "target-key"))
;;(define-key some-minor-mode-map (kbd "your-key") 'your-command)
;;(local-set-key (kbd "your-key") 'your-command)
;;(add-hook 'some-major-mode-hook '(lambda () (local-set-key ...)))
;;(local-unset-key (kbd "your-key")
;;(global-set-key (kbd "your-key") 'your-command)
;;(global-unset-key (kbd "your-key")

;; M-j M-k M-l M-u M-i M-o M-p M-n M-m M-c M-] M-<SPC> M-\ M-h M-q
(define-key key-translation-map (kbd "M-p") (kbd "C-p"))
(define-key key-translation-map (kbd "M-n") (kbd "C-n"))
(define-key key-translation-map (kbd "M-l") (kbd "C-l"))
(define-key key-translation-map (kbd "M-q") (kbd "C-v"))

(define-key key-translation-map (kbd "M-c") (kbd "C-g"))
(define-key key-translation-map (kbd "M-<SPC>") (kbd "M-x"))

(define-key key-translation-map (kbd "M-] M-]") (kbd "C-x o"))
(define-key key-translation-map (kbd "M-] ]") (kbd "C-x o"))
(define-key key-translation-map (kbd "M-] d") (kbd "C-x 0"))
(define-key key-translation-map (kbd "M-] o") (kbd "C-x 1"))
(define-key key-translation-map (kbd "M-] h") (kbd "C-x 2"))
(define-key key-translation-map (kbd "M-] v") (kbd "C-x 3"))

(global-set-key (kbd "M-o") 'project-find-file)

(when (package-installed-p 'consult)
  (global-set-key (kbd "M-s M-s") 'consult-ripgrep)
  (global-set-key (kbd "M-s s") 'consult-grep)
  (message "consult keymaps"))

(global-set-key (kbd "M-j") 'my/grep-at-point)
(global-set-key (kbd "M-k") 'my/isearch-at-point)

(global-set-key (kbd "M-\\") 'color-rg-search-symbol-in-project)

(global-set-key (kbd "M-r") 'rg-menu)

(when (package-installed-p 'citre)
  (global-set-key (kbd "M-.") 'citre-jump+)
  (global-set-key (kbd "M-,") 'citre-jump-back+)
  (global-set-key (kbd "M-/") 'citre-jump-to-reference)
  ;;(global-set-key (kbd "M-/") 'citre-ace-peek)
  ;;(global-set-key (kbd "M-/") 'consult-citre)
  (message "citre keymaps"))

(global-set-key (kbd "M-/") 'my/xref-find-references-at-point)
(global-set-key (kbd "M-s /") 'my/xref-find-references-from-yank)

(global-set-key (kbd "M-t") 'format-all-buffer)

(global-set-key (kbd "M-;") 'avy-goto-char-timer)
(global-set-key (kbd "M-'") 'avy-goto-char-in-line)

(global-set-key (kbd "M-m") 'symbol-overlay-put)
(global-set-key (kbd "M-u") 'symbol-overlay-remove-all)

(global-set-key (kbd "M-s v") symbol-overlay-map)


(global-set-key (kbd "M-s t") 'my/create-tags-project)

(global-set-key (kbd "M-s i") 'my/imenu)

(global-set-key (kbd "M-s ]") 'diff-hl-next-hunk)
(global-set-key (kbd "M-s [") 'diff-hl-previous-hunk)

(global-set-key (kbd "M-s f") 'find-file)
(global-set-key (kbd "M-s e") 'my/switch-buffer)
(global-set-key (kbd "M-s r") 'my/recent-file)

(global-set-key (kbd "M-s x") 'kill-current-buffer)
(global-set-key (kbd "M-s u") 'vundo)

(global-set-key (kbd "M-s ;") 'my/bookmark-at-point)
(global-set-key (kbd "M-s '") 'my/bookmark)

(global-set-key (kbd "M-s ,") 'my/copy-symbol-at-point)
(global-set-key (kbd "M-s j") 'my/grep-from-ynak)
(global-set-key (kbd "M-s k") 'my/isearch-from-ynak)
(global-set-key (kbd "M-s \\") 'my/color-rg-search-symbol-in-project-from-ynak)

(global-set-key (kbd "M-s y") 'fanyi-dwim2)
(when (or sys/linuxp sys/win32p)
  (global-set-key (kbd "M-s d") 'sdcv-search-pointer))
(when sys/macp
  (global-set-key (kbd "M-s d") 'osx-dictionary-search-pointer))


(provide 'init-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keymaps.el ends here
