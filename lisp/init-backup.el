;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-


(defun enable-minibuffer-auto-search-at-point()
  ;; https://github.com/lynnux/.emacs.d/blob/ac552c1bd4bd2763a8dfbaa9e80b1461b3b0845d/settings/package_extra.el#L1225-L1299
  ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
  ;; https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  ;; https://emacs-china.org/t/xxx-thing-at-point/18047
  (defvar my/fly-commands
    '(query-replace-regexp
      flush-lines keep-lines
      consult-line consult-ripgrep consult-grep consult-git-grep consult-man
      xref-find-references xref-find-apropos))

  (defvar my/fly-back-commands
    '(self-insert-command
      delete-forward-char kill-word kill-sexp
      end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
      yank yank-pop))

  (defvar-local my/fly--travel nil)
  (defun my/fly-back-to-present ()
    (cond ((and (memq last-command my/fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command my/fly-back-commands)
               (equal (this-command-keys-vector) (kbd "M-n")))
           (unless my/fly--travel
             (delete-region (point) (point-max))
             (when (memq this-command '(delete-forward-char
                                        kill-word kill-sexp
                                        end-of-line mwim-end-of-line
                                        mwim-end-of-code-or-line
                                        mwim-end-of-line-or-code))
               (when (featurep 'vertico)
                 (insert (substring-no-properties (or (car-safe vertico--input) ""))))
               (when (memq this-command '(delete-forward-char
                                          kill-word kill-sexp))
                 (beginning-of-line)))
             (setq my/fly--travel t)))))

  (defvar disable-for-vertico-repeat nil)
  (defun my/fly-time-travel ()
    (unless disable-for-vertico-repeat
      (when (memq this-command my/fly-commands)
        (insert (propertize
                 (save-excursion
		           (set-buffer (window-buffer (minibuffer-selected-window)))
		           (or (seq-some (lambda (thing) (thing-at-point thing t))
				                 '(region symbol)) ;; url sexp
		               "")
                   )
                 'face 'shadow))
        (add-hook 'pre-command-hook 'my/fly-back-to-present nil t)
        (beginning-of-line))))

  (add-hook 'minibuffer-setup-hook #'my/fly-time-travel)
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my/fly-back-to-present t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
