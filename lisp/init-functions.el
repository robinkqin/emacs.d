;;; init-functions.el --- functions configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-dos-eol ()
  "Delete `' characters in current region or buffer.
Same as '`replace-string' `C-q' `C-m' `RET' `RET''."
  (interactive)
  (save-excursion
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "\r" nil t)
        (replace-match "" nil t)
        (setq count (1+ count)))
      (message "Removed %d " count))
    (widen)))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
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

;; Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

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
