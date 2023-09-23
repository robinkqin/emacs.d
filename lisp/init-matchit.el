;;; init-matchit.el --- matchit configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package puni
  :ensure t
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'vterm-mode-hook #'puni-disable-puni-mode)
  (add-hook 'eww-mode-hook #'puni-disable-puni-mode)
  (add-hook 'minibuffer-mode-hook #'puni-disable-puni-mode))

(provide 'init-matchit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-matchit.el ends here
