;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; alias ec='emacsclient -t -a ""'
;; format: format-all
;; definition/reference: lsp-bridge, eglot, dumb-jump, ctags, gtags, xref
;; refactor: lsp-bridge, eglot, color-rg, symbol-overlay
;; completion: acm, corfu(vertico, orderless, consult), eglot
;; tools: git, clangd/clang-format, universal-ctags, global, ripgrep, cmake, bear, compiledb
;; dict: sdcv, fanyi
;; gtags: export GTAGSOBJDIRPREFIX=~/.cache/gtags/
;; lsp-bridge: pip install epc orjson sexpdata six paramiko requests cmake-language-server

;; python mode: pip install pyflakes autopep8

;; compile_commands.json: pip install compiledb; apt install bear cmake
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1
;; compiledb -n make
;; bear -- make

;;export PATH=$HOME/.local/bin:${PATH}
;;export TERM=xterm-256color
;;export COLORTERM=truecolor
;;export GTAGSOBJDIRPREFIX=~/.cache/gtags/

;; upgrade eglot
;; install tree-sitter LANG
;; check for clangd: clang++ main.cpp; clang test.c; clang++ -v; libstdc++.a; clang -v

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

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

(require 'init-vcs)
(message "init vcs done")

(require 'init-dict)
(message "init dict done")

(require 'init-vertico)
(message "init vertico done")

(require 'init-program)
(message "init program done")

(when (my/lsp-bridge-available-p)
  (require 'init-lsp-bridge)
  (message "init lsp-bridge done"))

(when (my/eglot-available-p)
  (require 'init-eglot)
  (message "init eglot done"))

(when (my/citre-available-p)
  (require 'init-citre)
  (message "init citre done"))

(require 'init-functions)
(message "init functions  done")

(require 'init-keymaps)
(message "init keymaps done")

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
