;;; init-keymaps.el --- Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-thing-edit)
(message "init thing-edit done")

;;(define-key key-translation-map (kbd "M-a") (kbd "C-a"))
;;(define-key key-translation-map (kbd "M-e") (kbd "C-e"))
;;(define-key key-translation-map (kbd "M-p") (kbd "C-p"))
;;(define-key key-translation-map (kbd "M-n") (kbd "C-n"))
;;(define-key key-translation-map (kbd "M-l") (kbd "C-l"))
;;(define-key key-translation-map (kbd "M-h") (kbd "C-h"))
;;(define-key key-translation-map (kbd "M-q") (kbd "C-v"))
;;(define-key key-translation-map (kbd "M-c") (kbd "C-g"))
;;(define-key key-translation-map (kbd "M-<SPC>") (kbd "C-<SPC>"))

(define-key key-translation-map (kbd "M-s p") (kbd "C-x p"))

(define-key key-translation-map (kbd "M-s 0") (kbd "C-x 0"))
(define-key key-translation-map (kbd "M-s 1") (kbd "C-x 1"))
(define-key key-translation-map (kbd "M-s 2") (kbd "C-x 2"))
(define-key key-translation-map (kbd "M-s 3") (kbd "C-x 3"))

(global-set-key (kbd "M-j") 'consult-ripgrep)
(global-set-key (kbd "M-k") 'consult-line)
;;("M-l" . dumb-jump-go)
(global-set-key (kbd "M-\\") 'color-rg-search-symbol-in-project)

(global-set-key (kbd "M-r") 'rg-menu)

(global-set-key (kbd "M-;") 'avy-goto-char-2)
;;(global-set-key (kbd "M-;") 'avy-goto-char-timer)
;;(global-set-key (kbd "M-t") 'avy-goto-char-in-line)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-i") 'consult-buffer)

;;(global-set-key (kbd "M-\\") 'my/xref-find-references-at-point)
;;(global-set-key (kbd "M-s \\") 'my/xref-find-references-from-yank)

(when (my/lsp-bridge-available-p)
  (global-set-key (kbd "M-.") 'lsp-bridge-find-def)
  (global-set-key (kbd "M-,") 'lsp-bridge-find-def-return)
  (global-set-key (kbd "M-/") 'lsp-bridge-find-references)
  (global-set-key (kbd "M-]") 'lsp-bridge-workspace-list-symbols))

(when (my/eglot-available-p)
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-/") 'xref-find-references)
  (global-set-key (kbd "M-]") 'consult-eglot-symbols))

(when (and (package-installed-p 'citre)
           (my/citre-available-p))
  (global-set-key (kbd "M-.") 'citre-jump+)
  (global-set-key (kbd "M-/") 'citre-jump-to-reference)
  (global-set-key (kbd "M-]") 'consult-citre)
  (message "citre keymaps"))

(global-set-key (kbd "M-s M-s") 'consult-git-grep)
(global-set-key (kbd "M-s s") 'consult-grep)

(global-set-key (kbd "M-s ]") 'diff-hl-next-hunk)
(global-set-key (kbd "M-s [") 'diff-hl-previous-hunk)

(global-set-key (kbd "M-s f") 'project-find-file)
(global-set-key (kbd "M-s e") 'find-file)
(global-set-key (kbd "M-s r") 'consult-recent-file)

(global-set-key (kbd "M-s x") 'kill-current-buffer)
(global-set-key (kbd "M-s u") 'vundo)

(global-set-key (kbd "M-s t") 'format-all-buffer)

(global-set-key (kbd "M-s ;") 'my/bookmark-at-point)
(global-set-key (kbd "M-s '") 'consult-bookmark)

(global-set-key (kbd "M-s i") 'consult-imenu)

(global-set-key (kbd "M-s ,") 'my/copy-symbol-at-point)

(global-set-key (kbd "M-s j") 'my/grep-from-ynak)
(global-set-key (kbd "M-s k") 'my/consult-line-from-ynak)
(global-set-key (kbd "M-s \\") 'my/color-rg-search-symbol-in-project-from-ynak)

(global-set-key (kbd "M-s y") 'fanyi-dwim2)
(if sys/macp
    (global-set-key (kbd "M-s d") 'osx-dictionary-search-pointer)
  (global-set-key (kbd "M-s d") 'sdcv-search-pointer))

(global-set-key (kbd "M-s v") symbol-overlay-map)
;;(global-set-key (kbd "M-m") 'symbol-overlay-put)
;;(global-set-key (kbd "M-u") 'symbol-overlay-remove-all)
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m M-m") 'symbol-overlay-put)
(global-set-key (kbd "M-m M-n") 'symbol-overlay-remove-all)
(global-set-key (kbd "M-m m") 'symbol-overlay-put)
(global-set-key (kbd "M-m n") 'symbol-overlay-remove-all)
(global-set-key (kbd "M-m w") 'markmacro-mark-words)
(global-set-key (kbd "M-m l") 'markmacro-mark-lines)
(global-set-key (kbd "M-m c") 'markmacro-mark-chars)
(global-set-key (kbd "M-m i") 'markmacro-mark-imenus)
(global-set-key (kbd "M-m a") 'markmacro-apply-all)
(global-set-key (kbd "M-m b") 'markmacro-apply-all-except-first)
(global-set-key (kbd "M-m r") 'markmacro-rect-set)
(global-set-key (kbd "M-m d") 'markmacro-rect-delete)
(global-set-key (kbd "M-m f") 'markmacro-rect-replace)
(global-set-key (kbd "M-m k") 'markmacro-rect-insert)
(global-set-key (kbd "M-m j") 'markmacro-rect-mark-columns)
(global-set-key (kbd "M-m s") 'markmacro-rect-mark-symbols)

(provide 'init-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keymaps.el ends here
