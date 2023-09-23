;; init-flymake.el --- Initialize flymake configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . #'flymake-goto-next-error)
         ("M-p" . #'flymake-goto-prev-error)))

(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
