;;; init.el --- Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs Configuration.
;;


;;; Code:

;; consult-ripgrep: #require#!init !sdcv !color
;; consult-line:  require !init

;; format: format-all(clang-format, shfmt)
;; definition: ctags/etags, global, xref
;; reference: ctags/etags, global, xref
;; refactor: color-rg, symbol-overlay
;; completion: corfu(vertico, orderless, consult), company, eglot(clangd)
;; completion: ivy/swiper/counsel
;; check: flycheck/flymake
;; project: built-in
;; tools: git, clangd/clang-format, ripgrep, universal-ctags, fd, global, silversearcher-ag
;; dict: sdcv, sqlite3, fanyi
;; gtags: export GTAGSOBJDIRPREFIX=~/.cache/gtags/
;; lsp-bridge: pip install epc orjson sexpdata six paramiko requests cmake-language-server

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory) t)
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory) t)

(require 'init-const)
(message "init const done")

(require 'init-basic)
(message "init basic done")

(require 'init-package)
(message "init package done")

(require 'init-fonts)
(message "init fonts done")

(require 'init-misc)
(message "init misc done")

(require 'init-gdb)
(message "init gdb done")

(require 'init-dict)
(message "init dict done")

(require 'init-matchit)
(message "init matchit done")

(require 'init-ivy)
(message "init ivy done")
;;(require 'init-vertico)
;;(message "init vertico done")

(require 'init-c)
(message "init c done")

(require 'init-ctags)
(message "init ctags done")

(require 'init-functions)
(message "init functions  done")

(require 'init-keymaps)
(message "init keymaps done")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
