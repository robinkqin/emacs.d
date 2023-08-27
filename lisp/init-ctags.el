;;; init-ctags.el --- ctags configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; ctags configurations.
;;

;;; Code:

(require 'init-citre)
(message "init citre done")

(setq my/path-to-ctags "ctags")

(defun my/create-dir-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (async-shell-command
   (format "%s -f TAGS -e -R %s"
		   my/path-to-ctags
		   (directory-file-name dir-name))))

;;(project-async-shell-command)
(defun my/create-project-tags ()
  "Create tags file."
  (interactive)
  ;;(message (format "%s -f %s/TAGS -e -R %s"
  ;;				   my/path-to-ctags
  ;;				   (my/project-root-dir)
  ;;				   (my/project-root-dir)))
  (async-shell-command
   (format "%s -f %s/TAGS -e -R %s"
		   my/path-to-ctags
		   (my/project-root-dir)
		   (my/project-root-dir))))

;;;;see complete-tag
;;(defun my-tags-complete ()
;;  "Completing read for a tag."
;;  (interactive)
;;  (let ((start (save-excursion (skip-syntax-backward "w_") (point)))
;;        (end (point)))
;;    (unless (and tags-file-name (get-file-buffer tags-file-name))
;;      (visit-tags-table (funcall default-tags-table-function)))
;;    (when-let (result
;;               (completing-read "Tag: "
;;                                (my-tags-completion-table (buffer-substring-no-properties start end))
;;                                nil t))
;;      (delete-region start end)
;;      (insert result))))
;;
;;(defun my-tags-completion-table (input)
;;  "Create a tags completion table starting with INPUT."
;;  (with-current-buffer (get-file-buffer tags-file-name)
;;    (let (table
;;          (progress-reporter
;;           (make-progress-reporter
;;            (format "Making tags completion table for %s..." buffer-file-name)
;;            (point-min) (point-max)))
;;          (case-fold-search nil)
;;          (re (concat "\177\\(" (regexp-quote input) "[^\001]*\\)\001")))
;;      (save-excursion
;;        (goto-char (point-min))
;;        (while (re-search-forward re nil t)
;;          (progress-reporter-update progress-reporter (point))
;;          (push (buffer-substring (match-beginning 1) (match-end 1)) table)))
;;      table)))
;;

(provide 'init-ctags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ctags.el ends here
