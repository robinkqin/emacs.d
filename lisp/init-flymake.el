;; init-flymake.el --- Initialize flymake configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Flymake configurations.
;;

;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe
              elisp-flymake-byte-compile-load-path load-path))

(use-package flymake-diagnostic-at-point
  :commands flymake-diagnostic-at-point-mode
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
