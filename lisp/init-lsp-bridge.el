;;; init-lsp-bridge.el --- lsp-bridge configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(require 'init-yasnippet)
(message "init yasnippet done")

;;(setq lsp-bridge-enable-debug t)
(setq lsp-bridge-enable-log nil)
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(message "init lsp-bridge done")

(provide 'init-lsp-bridge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp-bridge.el ends here
