;;; init-matchit.el --- matchit configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; matchit configurations.
;;

;;; Code:

;; Use puni-mode globally and disable it for term-mode.
(use-package puni
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'vterm-mode-hook #'puni-disable-puni-mode)
  (add-hook 'eww-mode-hook #'puni-disable-puni-mode)
  (add-hook 'minibuffer-mode-hook #'puni-disable-puni-mode))

;;(defun my/kill-line ()
;;  "Kill a line forward while keeping expressions balanced.
;;If nothing can be deleted, kill backward.  If still nothing can be
;;deleted, kill the pairs around point."
;;  (interactive)
;;  (let ((bounds (puni-bounds-of-list-around-point)))
;;    (if (eq (car bounds) (cdr bounds))
;;        (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
;;          (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
;;      (if (eq (point) (cdr bounds))
;;          (puni-backward-kill-line)
;;        (puni-kill-line)))))
;;;;(global-set-key (kbd "C-k") 'my/kill-line)

(provide 'init-matchit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-matchit.el ends here
