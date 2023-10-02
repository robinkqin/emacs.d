;;; init-program.el --- program configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
			  ("C-c c" . compile))
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                c-basic-offset 4
                c-default-style "linux"))

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :init (modern-c++-font-lock-global-mode t))

(defun my/c-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook #'my/c-hook)
(add-hook 'c++-mode-hook #'my/c-hook)

(use-package cmake-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;(use-package dumb-jump
;;  :ensure t
;;  :config
;;  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;(use-package ggtags
;;  :ensure t
;;  :config
;;  (add-hook 'c-mode-hook 'ggtags-mode)
;;  (add-hook 'c++-mode-hook 'ggtags-mode)
;;  :init
;;  (setq ggtags-enable-navigation-keys nil)
;;  (setq ggtags-navigation-mode nil)
;;  (setq ggtags-navigation-mode-lighter nil)
;;  (setq ggtags-highlight-tag nil)
;;  (setq ggtags-global-ignore-case t)
;;  (setq ggtags-sort-by-nearness t))
;;
;;(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;;(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;;(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;;(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;;(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;;(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;;(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
