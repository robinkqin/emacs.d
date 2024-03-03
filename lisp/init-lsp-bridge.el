;;; init-lsp-bridge.el --- lsp-bridge configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; compile_commands.json

(require 'init-yasnippet)
(message "init yasnippet done")

(add-to-list 'load-path my/lsp-bridge-path)
(setq lsp-bridge-enable-debug t)
(setq lsp-bridge-enable-log nil)
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(message "init lsp-bridge done")

(provide 'init-lsp-bridge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp-bridge.el ends here
