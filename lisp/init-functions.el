;;; init-functions.el --- functions configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f12>") 'my/open-init-file)

(defun my/project-root-dir ()
  "Return root directory of the current project."
  (let ((project (project-current)))
    (if project
        (cond
         ((fboundp 'project-root) (project-root project))
         ((fboundp 'project-roots) (car (project-roots project))))
      default-directory)))

(defun my/color-rg-search-symbol-in-project-from-ynak ()
  (interactive)
  (color-rg-search-input (substring-no-properties (car kill-ring))
                         (my/project-root-dir)))


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


(defun my/copy-symbol-at-point ()
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(defun my/grep-from-ynak ()
  (interactive)
  (consult-ripgrep (my/project-root-dir)
                   (substring-no-properties (car kill-ring))))

(defun my/consult-line-from-ynak ()
  (interactive)
  (consult-line (substring-no-properties (car kill-ring))))

(defun my/bookmark-at-point ()
  (interactive)
  (consult-bookmark (thing-at-point 'symbol)))

;; https://emacs-china.org/t/xxx-thing-at-point/18047
(defvar my/fly-commands
  '(query-replace-regexp
    flush-lines keep-lines
    consult-line consult-grep consult-git-grep consult-ripgrep consult-man
    ;;consult-citre consult-eglot-symbols
    xref-find-references xref-find-apropos))
(defvar my/fly-back-commands
  '(self-insert-command
    ;;delete-forward-char kill-word kill-sexp
    ;;end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
    yank yank-pop org-yank))
(defun my/fly-back-to-present ()
  (remove-hook 'pre-command-hook 'my/fly-back-to-present t)
  (cond ((and (memq last-command my/fly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command my/fly-back-commands)
         (delete-region (point) (point-max)))))
(defun my/fly-time-travel ()
  (when (memq this-command my/fly-commands)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some
                                    (lambda (thing) (thing-at-point thing t))
                                    '(region url symbol))
                                   ;; '(symbol url region sexp))
                                   ""))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'my/fly-back-to-present nil t)))
(add-hook 'minibuffer-setup-hook #'my/fly-time-travel)

(provide 'init-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-functions.el ends here
