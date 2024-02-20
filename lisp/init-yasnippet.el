;; init-yasnippet.el --- Initialize yasnippet configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Yet another snippet extension
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :ensure t
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  :after yasnippet)

(use-package consult-yasnippet
  :ensure t
  :after yasnippet
  :bind ("M-g y" . consult-yasnippet))

(provide 'init-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here
