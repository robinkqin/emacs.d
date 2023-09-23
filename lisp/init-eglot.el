;;; init-eglot.el --- eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(require 'init-flymake)
(message "init flymake done")

(use-package eglot
  :hook (prog-mode . eglot-ensure))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("C-M-." . consult-eglot-symbols))))

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
