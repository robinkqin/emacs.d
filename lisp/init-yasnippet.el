;; init-yasnippet.el --- Initialize yasnippet configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Yasnippet configurations.
;;

;;; Code:

;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide 'init-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here