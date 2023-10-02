;;; init-eglot.el --- eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(require 'init-flymake)
(message "init flymake done")

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

(use-package consult-eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
