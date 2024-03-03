;;; init-program.el --- program configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-gdb)
(message "init gdb done")

;;(require 'init-matchit)
;;(message "init matchit done")

;; Tree-sitter support
(when (my/treesit-available-p)
  (use-package treesit-auto
    :ensure t
    :hook (after-init . global-treesit-auto-mode)
    :init (setq treesit-auto-install 'prompt))
  (use-package c-ts-mode
    :init (setq c-ts-mode-indent-offset 4)))

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
			  ("C-c c" . compile))
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                c-basic-offset 4
                c-default-style "linux"))

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :init (modern-c++-font-lock-global-mode t))

(defun my/c-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook #'my/c-hook)
(add-hook 'c++-mode-hook #'my/c-hook)

;; Python Mode
;; Install: pip install pyflakes autopep8
(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))

;; Jump to definition
(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read)
  :bind (("M-l" . dumb-jump-go)
         ("M-L" . dumb-jump-go-other-window)))

(use-package cmake-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :bind (:map hl-todo-mode-map
              ([C-f3]    . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur)
              ("C-c t r" . hl-todo-rg-project)
              ("C-c t i" . hl-todo-insert))
  :hook ((after-init . global-hl-todo-mode)
         (hl-todo-mode . (lambda ()
                           (add-hook 'flymake-diagnostic-functions
                                     #'hl-todo-flymake nil t))))
  :init (setq hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#e45649")))
  (dolist (keyword '("TRICK" "WORKAROUND"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#d0bf8f")))
  (dolist (keyword '("DEBUG" "STUB"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#7cb8bb")))

  (defun hl-todo-rg (regexp &optional files dir)
    "Use `rg' to find all TODO or similar keywords."
    (interactive
     (progn
       (unless (require 'rg nil t)
         (error "`rg' is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
         (list regexp
               (rg-read-files)
               (read-directory-name "Base directory: " nil default-directory t)))))
    (rg regexp files dir))

  (defun hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords in current project."
    (interactive)
    (unless (require 'rg nil t)
      (error "`rg' is not installed"))
    (rg-project (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)) "everything")))

;; Code styles
(use-package editorconfig
  :ensure t
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package lua-mode
  :ensure t)

(provide 'init-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-program.el ends here
