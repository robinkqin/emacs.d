;;; init-basic.el --- basic configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; basic configurations.
;;

;;; Code:

;; compile_commands.json

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

(setq eldoc-echo-area-use-multiline-p nil)

(use-package consult-eglot)

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
