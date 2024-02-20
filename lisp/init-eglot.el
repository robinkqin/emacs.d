;;; init-eglot.el --- eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(require 'init-flymake)
(message "init flymake done")

;;(use-package eglot
;;  :hook ((c-mode . eglot-ensure)
;;         (c++-mode . eglot-ensure)))
;;
;;(use-package consult-eglot
;;  :ensure t
;;  :bind (:map eglot-mode-map
;;              ("C-M-." . consult-eglot-symbols)))

(use-package eglot
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5)
  :config
  (use-package consult-eglot
    :ensure t
    :bind (:map eglot-mode-map
                ("M-]" . consult-eglot-symbols))))

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
