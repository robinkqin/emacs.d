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
                c-default-style "linux")
  :config
  (use-package modern-cpp-font-lock
    :ensure t
    :diminish modern-c++-font-lock-mode
    :init (modern-c++-font-lock-global-mode t)))

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

(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
