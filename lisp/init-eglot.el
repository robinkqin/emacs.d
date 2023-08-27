;;; init-basic.el --- basic configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; basic configurations.
;;

;;; Code:

;; compile_commands.json

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("C-M-." . consult-eglot-symbols))))

;;(use-package eglot
;;  :hook ((c-mode . eglot-ensure)
;;         (c++-mode . eglot-ensure)))

;;(setq eldoc-echo-area-use-multiline-p nil)

;;(use-package consult-eglot)


(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
