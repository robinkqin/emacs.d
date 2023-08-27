;;; init-functions.el --- functions configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; functions configurations.
;;

;;; Code:

(defun my/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f12>") 'my/open-init-file)

;; Dos2Unix/Unix2Dos
(defun my/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun my/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(defun my/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun my/save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))


(defun my/project-root-dir ()
  "Return root directory of the current project."
  (let ((project (project-current)))
    (if project
        (cond
         ((fboundp 'project-root) (project-root project))
         ((fboundp 'project-roots) (car (project-roots project))))
      default-directory)))

(defun my/xref-find-references-at-point ()
  (interactive)
  (xref-find-references (thing-at-point 'symbol)))

(defun my/xref-find-references-from-yank ()
  (interactive)
  (xref-find-references (substring-no-properties (car kill-ring))))


(defalias 'my/highlight-symbol-at-point 'highlight-symbol-at-point)

(defun my/unhighlight-symbol-at-point ()
  "Remove highlight of symbol at point."
  (interactive)
  (unhighlight-regexp (concat "\\_<" (thing-at-point 'symbol t) "\\_>")))

(defun my/unhighlight-symbol-all ()
  "Remove all highlight symbols."
  (interactive)
  (unhighlight-regexp t))


(defun my/man ()
  (interactive)
  (man (thing-at-point 'word)))

(defun my/color-rg-search-symbol-in-project-from-ynak ()
  (interactive)
  (color-rg-search-input (substring-no-properties (car kill-ring))
                         (my/project-root-dir)))

(defun my/copy-symbol-at-point ()
  (interactive)
  (kill-new (thing-at-point 'symbol)))


(defun my/grep-at-point ()
  (interactive)
  (consult-ripgrep (my/project-root-dir) (thing-at-point 'symbol)))

(defun my/grep-from-ynak ()
  (interactive)
  (consult-ripgrep (my/project-root-dir)
                   (substring-no-properties (car kill-ring))))

(defun my/isearch-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun my/isearch-from-ynak ()
  (interactive)
  (consult-line (substring-no-properties (car kill-ring))))

(defun my/bookmark-at-point ()
  (interactive)
  (consult-bookmark (thing-at-point 'symbol)))
(defalias 'my/bookmark 'consult-bookmark)

(defalias 'my/switch-buffer 'consult-buffer)
(defalias 'my/recent-file 'consult-recent-file)
(defalias 'my/imenu 'consult-imenu)


(defvar mcfly-commands
  '(consult-line
    consult-grep
    consult-git-grep
    consult-ripgrep
    consult-man
    consult-outline
    consult-citre
    consult-eglot-symbols
    ;;citre-jump
    ;;citre-jump+
    xref-find-references
	xref-find-apropos))

(defvar mcfly-back-commands
  '(self-insert-command
    yank
    yank-pop
    org-yank))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region (point) (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some
                                    (lambda (thing) (thing-at-point thing t))
					                '(region url symbol))
					               ;; '(symbol url region sexp))
			                       ""))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))

(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)


(provide 'init-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-functions.el ends here
