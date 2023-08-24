;;; init-program.el --- program configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; program configurations.
;;

;;; Code:

(require 'init-citre)
(message "init citre done")

(when my/lsp-bridge-enable

  ;; flycheck
  ;; treesitter

  (use-package markdown-mode
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown")
    :bind (:map markdown-mode-map
                ("C-c C-e" . markdown-do)))

  ;; Yet another snippet extension
  (use-package yasnippet
    :hook (after-init . yas-global-mode))
  ;; Collection of yasnippet snippets
  (use-package yasnippet-snippets)

  (message "init lsp-bridge")
  (setq lsp-bridge-enable-debug t)
  (require 'lsp-bridge)
  (global-lsp-bridge-mode))


(when my/eglot-enable

  (require 'init-flymake)
  (message "init flymake done")

  (require 'init-eglot)
  (message "init eglot done")

  ;; company
  (use-package company
    :init
    ;;(global-company-mode 1)
    (setq company-show-quick-access 1)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3))

  (add-hook 'after-init-hook 'global-company-mode)

  (use-package consult-company))


(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
