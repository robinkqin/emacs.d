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

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/<29p
  (< emacs-major-version 29)
  "Emacs is lower then 29.")

(defun my/treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
