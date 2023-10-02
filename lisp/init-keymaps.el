;;; init-keymaps.el --- Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;(define-key key-translation-map (kbd "M-p") (kbd "C-p"))
;;(define-key key-translation-map (kbd "M-n") (kbd "C-n"))
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
(global-set-key (kbd "M-l") 'color-rg-search-symbol-in-project)
(global-set-key (kbd "M-r") 'rg-menu)
(global-set-key (kbd "M-t") 'format-all-buffer)
(global-set-key (kbd "M-;") 'avy-goto-char-timer)
(global-set-key (kbd "M-'") 'avy-goto-char-in-line)

(global-set-key (kbd "M-m") 'symbol-overlay-put)
(global-set-key (kbd "M-u") 'symbol-overlay-remove-all)

(global-set-key (kbd "M-\\") 'my/xref-find-references-at-point)
(global-set-key (kbd "M-s \\") 'my/xref-find-references-from-yank)

(when (package-installed-p 'citre)
  (global-set-key (kbd "M-.") 'citre-jump+)
  (global-set-key (kbd "M-/") 'citre-jump-to-reference)
  (global-set-key (kbd "M-]") 'consult-citre)
  (message "citre keymaps"))

(provide 'init-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keymaps.el ends here
