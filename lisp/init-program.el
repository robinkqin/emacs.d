;;; init-program.el --- program configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; program configurations.
;;

;;; Code:

(when my/lsp-bridge-enable

  (message "lsp-bridge-enable")

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

  (message "eglot enable")

  (require 'init-flymake)
  (message "init flymake done")

  (require 'init-eglot)
  (message "init eglot done")

  ;;;; company
  ;;(use-package company
  ;;  :init
  ;;  ;;(global-company-mode 1)
  ;;  (setq company-show-quick-access 1)
  ;;  (setq company-idle-delay 0)
  ;;  (setq company-minimum-prefix-length 3))
  ;;
  ;;(add-hook 'after-init-hook 'global-company-mode)
  ;;
  ;;(use-package consult-company)
  )


;;;; Show function arglist or variable docstring
;;(use-package eldoc
;;  :ensure nil
;;  :diminish)
;;
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

(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
