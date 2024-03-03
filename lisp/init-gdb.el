;;; init-gdb.el --- gdb configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;(when emacs/>=29p
;;  (use-package dape
;;    :ensure t
;;    :bind (("<f5>" . dape))
;;    :custom (dape-buffer-window-arrangment 'right)
;;    :config
;;    ;; Save buffers on startup, useful for interpreted languages
;;    (add-hook 'dape-on-start-hooks
;;              (defun dape--save-on-start ()
;;                (save-some-buffers t t)))))

(global-set-key [f5] 'gud-cont)
(global-set-key [f6] 'gud-finish)
(global-set-key [f7] 'gud-step)
(global-set-key [f8] 'gud-next)
(global-set-key [f9] 'gud-break)

;; set gdb multi-windows when open
(setq gdb-many-windows t)

;;;; customize the gdb multi-windows
(defadvice gdb-setup-windows (after my/setup-gdb-windows activate)
  "MY GDB UI"
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil nil 'left))  ;; code and output
        (win2 (split-window-below (/ (* (window-height) 3) 4)))  ;; stack
        )
    (select-window win2)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win3 (split-window nil (/ (* (window-height) 3) 4))))  ;; io
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3))
    (select-window win0)
    ))

(provide 'init-gdb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gdb.el ends here
