;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; alias ec='emacsclient -t -a ""'
;; format: format-all
;; definition/reference: ctags, gtags, xref
;; refactor: color-rg, symbol-overlay
;; completion: corfu(vertico, orderless, consult), eglot
;; tools: git, clangd/clang-format, universal-ctags, global, ripgrep
;; dict: sdcv, fanyi
;; gtags: export GTAGSOBJDIRPREFIX=~/.cache/gtags/
;; lsp-bridge: pip install epc orjson sexpdata six paramiko requests cmake-language-server

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(defconst my/eglot-enable nil)
(defconst my/lsp-bridge-enable nil)

(require 'init-const)
(message "init const done")

(require 'init-package)
(message "init package done")

(require 'init-basic)
(message "init basic done")

(require 'init-misc)
(message "init misc done")

(require 'init-fonts)
(message "init fonts done")

(require 'init-gdb)
(message "init gdb done")

(require 'init-vcs)
(message "init vcs done")

(require 'init-vertico)
(message "init vertico done")

(require 'init-dict)
(message "init dict done")

(require 'init-matchit)
(message "init matchit done")

(require 'init-citre)
(message "init citre done")

(require 'init-program)
(message "init program done")

(require 'init-functions)
(message "init functions  done")

(require 'init-keymaps)
(message "init keymaps done")

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
