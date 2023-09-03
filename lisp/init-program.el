;;; init-program.el --- program configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; program configurations.
;;

;;; Code:

(when my/eglot-enable

  (message "eglot enable")

  (require 'init-flymake)
  (message "init flymake done")

  (require 'init-eglot)
  (message "init eglot done"))

(when my/lsp-bridge-enable

  (message "lsp-bridge enable")

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
  (message "init lsp-bridge done"))

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish)

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;;;; Search tool
;;(use-package grep
;;  :ensure nil
;;  :autoload grep-apply-setting
;;  :init
;;  (cond
;;   ((executable-find "ugrep")
;;    (grep-apply-setting
;;     'grep-command "ugrep --color=auto -0In -e ")
;;    (grep-apply-setting
;;     'grep-template "ugrep --color=auto -0In -e <R> <D>")
;;    (grep-apply-setting
;;     'grep-find-command '("ugrep --color=auto -0Inr -e ''" . 30))
;;    (grep-apply-setting
;;     'grep-find-template "ugrep <C> -0Inr -e <R> <D>"))
;;   ((executable-find "rg")
;;    (grep-apply-setting
;;     'grep-command "rg --color=auto --null -nH --no-heading -e ")
;;    (grep-apply-setting
;;     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
;;    (grep-apply-setting
;;     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
;;    (grep-apply-setting
;;     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))))

;;;; Cross-referencing commands
;;(use-package xref
;;  :init
;;  ;; Use faster search tool
;;  (setq xref-search-program (cond
;;                             ((executable-find "ugrep") 'ugrep)
;;                             ((executable-find "rg") 'ripgrep)
;;                             (t 'grep)))
;;
;;  ;; FIXME ???
;;  ;; Select from xref candidates in minibuffer
;;  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
;;        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;;;; Code styles
;;(use-package editorconfig
;;  :diminish
;;  :hook (after-init . editorconfig-mode))

;;(use-package csv-mode)

(use-package cmake-mode)

;;(use-package dumb-jump
;;  :init
;;  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;  (setq dumb-jump-selector 'completing-read))

(defun endless/c-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook #'endless/c-hook)
(add-hook 'c++-mode-hook #'endless/c-hook)


(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
