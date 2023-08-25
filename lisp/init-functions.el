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


(when (package-installed-p 'counsel)

  (defun my/grep-at-point ()
    (interactive)
    (counsel-git-grep (thing-at-point 'symbol) (my/project-root-dir)))

  (defun my/grep-from-ynak ()
    (interactive)
    (counsel-git-grep (substring-no-properties (car kill-ring)) (my/project-root-dir)))

  (defun my/isearch-at-point ()
    (interactive)
    (swiper (thing-at-point 'symbol)))

  (defun my/isearch-from-ynak ()
    (interactive)
    (swiper (substring-no-properties (car kill-ring))))

  (defalias 'my/bookmark-at-point 'counsel-bookmark)
  (defalias 'my/bookmark 'counsel-bookmark)
  (defalias 'my/switch-buffer 'counsel-switch-buffer)
  (defalias 'my/recent-file 'counsel-recentf)
  (defalias 'my/imenu 'counsel-imenu)
  (message "counsel functions"))

(when (package-installed-p 'consult)

  (defun my/grep-at-point ()
    (interactive)
    (consult-git-grep (my/project-root-dir) (thing-at-point 'symbol)))

  (defun my/grep-from-ynak ()
    (interactive)
    (consult-git-grep (my/project-root-dir)
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
  (message "consult functions"))


(defvar mcfly-commands
  '(consult-line
    consult-grep
    consult-git-grep
    consult-ripgrep
    consult-man
    consult-outline
    consult-citre
    consult-eglot-symbols
    swiper
    counsel-ag
    counsel-ack
    counsel-pt
    counsel-rg
    counsel-grep
    counsel-git-grep
    counsel-locate
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
