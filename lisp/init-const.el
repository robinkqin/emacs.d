;;; init-const.el --- Define constants.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a Windows system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above?")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above?")

(defconst emacs/<28p
  (< emacs-major-version 28)
  "Emacs is lower then 28?")

(defconst emacs/<29p
  (< emacs-major-version 29)
  "Emacs is lower then 29?")

(defconst my/lsp-bridge-path "~/workbench/emacs/lsp-bridge")
(defun my/lsp-bridge-available-p ()
  "Check whether lsp-bridge is available."
  (and (file-exists-p my/lsp-bridge-path)
       (executable-find "python")
       (display-graphic-p)
       emacs/>=28p))

(defun my/eglot-available-p ()
  "Check whether eglot is available.
Native eglot is introduced since 29.1."
  (and (fboundp 'eglot)
       (not (my/lsp-bridge-available-p))))

(defun my/citre-available-p ()
  "Check whether citre is available."
  nil)

(defun my/treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       nil))

(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
