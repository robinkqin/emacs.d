;;; init-c.el --- c/c++ configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; c/c++ configurations.
;;

;;; Code:

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
			  ("C-c c" . compile))
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default c-basic-offset 4)
  (setq-default c-default-style "linux")
  :config
  (use-package modern-cpp-font-lock
    :ensure
    :diminish
    :init (modern-c++-font-lock-global-mode t)))


(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
