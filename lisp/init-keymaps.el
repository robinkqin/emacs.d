;;; init-keymaps.el --- Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;(define-key key-translation-map (kbd "M-a") (kbd "C-a"))
;;(define-key key-translation-map (kbd "M-e") (kbd "C-e"))
;;(define-key key-translation-map (kbd "M-p") (kbd "C-p"))
;;(define-key key-translation-map (kbd "M-n") (kbd "C-n"))
;;(define-key key-translation-map (kbd "M-l") (kbd "C-l"))
;;(define-key key-translation-map (kbd "M-h") (kbd "C-h"))
;;(define-key key-translation-map (kbd "M-q") (kbd "C-v"))
;;(define-key key-translation-map (kbd "M-c") (kbd "C-g"))
;;(define-key key-translation-map (kbd "M-<SPC>") (kbd "C-<SPC>"))

(global-set-key (kbd "M-j") 'consult-ripgrep)
(global-set-key (kbd "M-k") 'consult-line)
(global-set-key (kbd "M-\\") 'color-rg-search-symbol-in-project)

(global-set-key (kbd "M-r") 'rg-menu)

(global-set-key (kbd "M-;") 'avy-goto-char-2)
;;(global-set-key (kbd "M-;") 'avy-goto-char-timer)
;;(global-set-key (kbd "M-t") 'avy-goto-char-in-line)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-i") 'consult-buffer)

;;(global-set-key (kbd "M-m") 'symbol-overlay-put)
;;(global-set-key (kbd "M-u") 'symbol-overlay-remove-all)

;;(global-set-key (kbd "M-\\") 'my/xref-find-references-at-point)
;;(global-set-key (kbd "M-s \\") 'my/xref-find-references-from-yank)

(when (package-installed-p 'citre)
  (global-set-key (kbd "M-.") 'citre-jump+)
  (global-set-key (kbd "M-/") 'citre-jump-to-reference)
  (global-set-key (kbd "M-]") 'consult-citre)
  (message "citre keymaps"))


(defvar my/thing-edit-map (make-sparse-keymap)
  "Keybinding for `thing-edit'")
;; Copy.
(define-key my/thing-edit-map (kbd "w") 'thing-copy-word)
(define-key my/thing-edit-map (kbd "s") 'thing-copy-symbol)
(define-key my/thing-edit-map (kbd "m") 'thing-copy-email)
(define-key my/thing-edit-map (kbd "f") 'thing-copy-filename)
(define-key my/thing-edit-map (kbd "u") 'thing-copy-url)
(define-key my/thing-edit-map (kbd "x") 'thing-copy-sexp)
(define-key my/thing-edit-map (kbd "t") 'thing-copy-page)
(define-key my/thing-edit-map (kbd "v") 'thing-copy-sentence)
;;(define-key my/thing-edit-map (kbd "g") 'thing-copy-block)
(define-key my/thing-edit-map (kbd "o") 'thing-copy-whitespace)
(define-key my/thing-edit-map (kbd "i") 'thing-copy-list)
(define-key my/thing-edit-map (kbd "c") 'thing-copy-comment)
(define-key my/thing-edit-map (kbd "h") 'thing-copy-defun)
(define-key my/thing-edit-map (kbd "p") 'thing-copy-parentheses)
(define-key my/thing-edit-map (kbd "l") 'thing-copy-region-or-line)
(define-key my/thing-edit-map (kbd "a") 'thing-copy-to-line-beginning)
(define-key my/thing-edit-map (kbd "e") 'thing-copy-to-line-end)
;; Cut.
(define-key my/thing-edit-map (kbd "SPC w") 'thing-cut-word)
(define-key my/thing-edit-map (kbd "SPC s") 'thing-cut-symbol)
(define-key my/thing-edit-map (kbd "SPC m") 'thing-cut-email)
(define-key my/thing-edit-map (kbd "SPC f") 'thing-cut-filename)
(define-key my/thing-edit-map (kbd "SPC u") 'thing-cut-url)
(define-key my/thing-edit-map (kbd "SPC x") 'thing-cut-sexp)
(define-key my/thing-edit-map (kbd "SPC t") 'thing-cut-page)
(define-key my/thing-edit-map (kbd "SPC v") 'thing-cut-sentence)
;;(define-key my/thing-edit-map (kbd "SPC g") 'thing-cut-block)
(define-key my/thing-edit-map (kbd "SPC o") 'thing-cut-whitespace)
(define-key my/thing-edit-map (kbd "SPC i") 'thing-cut-list)
(define-key my/thing-edit-map (kbd "SPC c") 'thing-cut-comment)
(define-key my/thing-edit-map (kbd "SPC h") 'thing-cut-defun)
(define-key my/thing-edit-map (kbd "SPC l") 'thing-cut-region-or-line)
(define-key my/thing-edit-map (kbd "SPC p") 'thing-cut-parentheses)
(define-key my/thing-edit-map (kbd "SPC a") 'thing-cut-to-line-beginning)
(define-key my/thing-edit-map (kbd "SPC e") 'thing-cut-to-line-end)
;; Replace
(define-key my/thing-edit-map (kbd "W") 'thing-replace-word)
(define-key my/thing-edit-map (kbd "S") 'thing-replace-symbol)
(define-key my/thing-edit-map (kbd "M") 'thing-replace-email)
(define-key my/thing-edit-map (kbd "F") 'thing-replace-filename)
(define-key my/thing-edit-map (kbd "U") 'thing-replace-url)
(define-key my/thing-edit-map (kbd "X") 'thing-replace-sexp)
(define-key my/thing-edit-map (kbd "T") 'thing-replace-page)
(define-key my/thing-edit-map (kbd "V") 'thing-replace-sentence)
;;(define-key my/thing-edit-map (kbd "G") 'thing-replace-block)
(define-key my/thing-edit-map (kbd "O") 'thing-replace-whitespace)
(define-key my/thing-edit-map (kbd "I") 'thing-replace-list)
(define-key my/thing-edit-map (kbd "C") 'thing-replace-comment)
(define-key my/thing-edit-map (kbd "H") 'thing-replace-defun)
(define-key my/thing-edit-map (kbd "P") 'thing-replace-parentheses)
(define-key my/thing-edit-map (kbd "L") 'thing-replace-region-or-line)
(define-key my/thing-edit-map (kbd "A") 'thing-paste-to-line-beginning)
(define-key my/thing-edit-map (kbd "E") 'thing-paste-to-line-end)

(global-set-key (kbd "M-h") my/thing-edit-map)


(defvar my/avy-thing-edit-map (make-sparse-keymap)
  "Keybinding for `thing-edit'")
;; Copy.
(define-key my/avy-thing-edit-map (kbd "w") 'avy-thing-paste-word)
(define-key my/avy-thing-edit-map (kbd "s") 'avy-thing-paste-symbol)
(define-key my/avy-thing-edit-map (kbd "m") 'avy-thing-paste-email)
(define-key my/avy-thing-edit-map (kbd "f") 'avy-thing-paste-filename)
(define-key my/avy-thing-edit-map (kbd "u") 'avy-thing-paste-url)
(define-key my/avy-thing-edit-map (kbd "x") 'avy-thing-paste-sexp)
(define-key my/avy-thing-edit-map (kbd "t") 'avy-thing-paste-page)
(define-key my/avy-thing-edit-map (kbd "v") 'avy-thing-paste-sentence)
;;(define-key my/avy-thing-edit-map (kbd "g") 'avy-thing-paste-block)
(define-key my/avy-thing-edit-map (kbd "g") 'avy-thing-paste-region)
(define-key my/avy-thing-edit-map (kbd "o") 'avy-thing-paste-whitespace)
(define-key my/avy-thing-edit-map (kbd "i") 'avy-thing-paste-list)
(define-key my/avy-thing-edit-map (kbd "c") 'avy-thing-paste-comment)
(define-key my/avy-thing-edit-map (kbd "h") 'avy-thing-paste-defun)
(define-key my/avy-thing-edit-map (kbd "p") 'avy-thing-paste-parentheses)
(define-key my/avy-thing-edit-map (kbd "l") 'avy-thing-paste-region-or-line)
(define-key my/avy-thing-edit-map (kbd "a") 'avy-thing-paste-to-line-beginning)
(define-key my/avy-thing-edit-map (kbd "e") 'avy-thing-paste-to-line-end)
;; Cut.
(define-key my/avy-thing-edit-map (kbd "SPC w") 'avy-thing-cut-word)
(define-key my/avy-thing-edit-map (kbd "SPC s") 'avy-thing-cut-symbol)
(define-key my/avy-thing-edit-map (kbd "SPC m") 'avy-thing-cut-email)
(define-key my/avy-thing-edit-map (kbd "SPC f") 'avy-thing-cut-filename)
(define-key my/avy-thing-edit-map (kbd "SPC u") 'avy-thing-cut-url)
(define-key my/avy-thing-edit-map (kbd "SPC x") 'avy-thing-cut-sexp)
(define-key my/avy-thing-edit-map (kbd "SPC t") 'avy-thing-cut-page)
(define-key my/avy-thing-edit-map (kbd "SPC v") 'avy-thing-cut-sentence)
;;(define-key my/avy-thing-edit-map (kbd "SPC g") 'avy-thing-cut-block)
(define-key my/avy-thing-edit-map (kbd "SPC o") 'avy-thing-cut-whitespace)
(define-key my/avy-thing-edit-map (kbd "SPC i") 'avy-thing-cut-list)
(define-key my/avy-thing-edit-map (kbd "SPC c") 'avy-thing-cut-comment)
(define-key my/avy-thing-edit-map (kbd "SPC h") 'avy-thing-cut-defun)
(define-key my/avy-thing-edit-map (kbd "SPC p") 'avy-thing-cut-parentheses)
(define-key my/avy-thing-edit-map (kbd "SPC l") 'avy-thing-cut-region-or-line)
(define-key my/avy-thing-edit-map (kbd "SPC a") 'avy-thing-cut-to-line-beginning)
(define-key my/avy-thing-edit-map (kbd "SPC e") 'avy-thing-cut-to-line-end)
;; Replace
(define-key my/avy-thing-edit-map (kbd "W") 'avy-thing-replace-word)
(define-key my/avy-thing-edit-map (kbd "S") 'avy-thing-replace-symbol)
(define-key my/avy-thing-edit-map (kbd "M") 'avy-thing-replace-email)
(define-key my/avy-thing-edit-map (kbd "F") 'avy-thing-replace-filename)
(define-key my/avy-thing-edit-map (kbd "U") 'avy-thing-replace-url)
(define-key my/avy-thing-edit-map (kbd "X") 'avy-thing-replace-sexp)
(define-key my/avy-thing-edit-map (kbd "T") 'avy-thing-replace-page)
(define-key my/avy-thing-edit-map (kbd "V") 'avy-thing-replace-sentence)
;;(define-key my/avy-thing-edit-map (kbd "G") 'avy-thing-replace-block)
(define-key my/avy-thing-edit-map (kbd "O") 'avy-thing-replace-whitespace)
(define-key my/avy-thing-edit-map (kbd "I") 'avy-thing-replace-list)
(define-key my/avy-thing-edit-map (kbd "C") 'avy-thing-replace-comment)
(define-key my/avy-thing-edit-map (kbd "H") 'avy-thing-replace-defun)
(define-key my/avy-thing-edit-map (kbd "P") 'avy-thing-replace-parentheses)
(define-key my/avy-thing-edit-map (kbd "L") 'avy-thing-replace-region-or-line)
(define-key my/avy-thing-edit-map (kbd "A") 'avy-thing-paste-to-line-beginning)
(define-key my/avy-thing-edit-map (kbd "E") 'avy-thing-paste-to-line-end)

(global-set-key (kbd "M-'") my/avy-thing-edit-map)


(global-unset-key (kbd "M-m"))
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


(define-key key-translation-map (kbd "M-s p") (kbd "C-x p"))

(define-key key-translation-map (kbd "M-s 0") (kbd "C-x 0"))
(define-key key-translation-map (kbd "M-s 1") (kbd "C-x 1"))
(define-key key-translation-map (kbd "M-s 2") (kbd "C-x 2"))
(define-key key-translation-map (kbd "M-s 3") (kbd "C-x 3"))

(global-set-key (kbd "M-s y") 'fanyi-dwim2)
(if sys/macp
    (global-set-key (kbd "M-s d") 'osx-dictionary-search-pointer)
  (global-set-key (kbd "M-s d") 'sdcv-search-pointer))

(global-set-key (kbd "M-s M-s") 'consult-git-grep)
(global-set-key (kbd "M-s s") 'consult-grep)

(global-set-key (kbd "M-s v") symbol-overlay-map)

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


(provide 'init-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keymaps.el ends here
