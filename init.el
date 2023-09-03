;;; init.el --- Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs Configuration.
;;


;;; Code:

;;mkdir -p ~/.config/systemd/user
;;cp /usr/lib/systemd/user/emacs.service ~/.config/systemd/user
;;systemctl --user enable emacs
;;systemctl --user start emacs

;;~/.bashrc
;;alias ec='emacsclient -t -a ""'
;;alias sec='sudo emacsclient -t -a ""'
;;alias vim='emacsclient -t -a ""'
;;alias emacs='emacsclient -t -a ""'
;;emacsclient -t -a "" $*
;;emacsclient -t -a "" "$@"

;; consult-ripgrep: #require#!init !sdcv !color
;; consult-line:  require !init

;; format: format-all(clang-format, shfmt)
;; definition: ctags/etags, global, xref
;; reference: ctags/etags, global, xref
;; refactor: color-rg, symbol-overlay
;; completion: corfu(vertico, orderless, consult), company, eglot(clangd)
;; check: flycheck/flymake
;; project: built-in
;; tools: git, clangd/clang-format, universal-ctags, global, ripgrep/ugrep, fzf, fd, aspell
;; dict: sdcv, sqlite3, fanyi
;; gtags: export GTAGSOBJDIRPREFIX=~/.cache/gtags/
;; lsp-bridge: pip install epc orjson sexpdata six paramiko requests cmake-language-server

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

(defconst my/lsp-bridge-enable nil)
(defconst my/eglot-enable nil)

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

(require 'init-vcs)
(message "init vcs done")

(require 'init-vertico)
(message "init vertico done")

;;(require 'init-org)
;;(message "init org done")

(require 'init-dict)
(message "init dict done")

(require 'init-matchit)
(message "init matchit done")

(require 'init-c)
(message "init c done")

(require 'init-ctags)
(message "init ctags done")

(require 'init-program)
(message "init program done")

(require 'init-functions)
(message "init functions  done")

(require 'init-keymaps)
(message "init keymaps done")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
