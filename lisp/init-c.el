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
  (setq-default indent-tabs-mode nil
                tab-width 4
                c-basic-offset 4
                c-default-style "linux")
  :config
  (use-package modern-cpp-font-lock
    :diminish modern-c++-font-lock-mode
    :init (modern-c++-font-lock-global-mode t)))


(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
