;;; init-vertico.el --- vertico configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 4)

  ;;;; Add prompt indicator to `completing-read-multiple'.
  ;;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;;(defun crm-indicator (args)
  ;;  (cons (format "[CRM%s] %s"
  ;;              (replace-regexp-in-string
  ;;               "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;               crm-separator)
  ;;              (car args))
  ;;        (cdr args)))
  ;;(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;;
  ;;;; Do not allow the cursor in the minibuffer prompt
  ;;(setq minibuffer-prompt-properties
  ;;      '(read-only t cursor-intangible t face minibuffer-prompt))
  ;;(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)
  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; support pinyin first character match for orderless, avy etc.
(use-package pinyinlib
  :ensure t)

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        ;;completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; make completion support pinyin, refer to
  ;; https://emacs-china.org/t/vertico/17913/2
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :init
  (setq vertico-count 17
        vertico-resize nil
        vertico-cycle t)
  :bind (:map vertico-map
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              :map minibuffer-local-map
              ("C-w" . backward-kill-word)))

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(require 'vertico-repeat)
(global-set-key (kbd "C-c C-r") 'vertico-repeat)
(global-set-key (kbd "<f2>") 'vertico-repeat)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(when (display-graphic-p)
  (use-package vertico-posframe
    ;;:config
    ;;(vertico-posframe-mode 1)
    :ensure t))

(use-package marginalia
  :ensure t
  ;;;; Either bind `marginalia-cycle' globally or only in the minibuffer
  ;;:bind (("M-A" . marginalia-cycle)
  ;;       :map minibuffer-local-map
  ;;       ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package consult
  :ensure t
  :after vertico
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)

         ([remap Info-search]        . consult-info)
         ([remap imenu]              . consult-imenu)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ("M-s b" . consult-bookmark)
         ;;("M-s d" . consult-find)
         ;;("M-s D" . consult-locate)
         ("M-s G" . consult-grep)
         ("M-s g" . consult-git-grep)
         ;;("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;;("M-s k" . consult-keep-lines)
         ;;("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;;("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . (lambda ()
                    "Insert the currunt symbol."
                    (interactive)
                    (insert (save-excursion
                              (set-buffer (window-buffer (minibuffer-selected-window)))
                              (or (thing-at-point 'symbol t) "")))))
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   ;;consult-theme :preview-key '(:debounce 0.2 any)
   ;;consult-ripgrep consult-git-grep consult-grep
   ;;consult-bookmark consult-recent-file consult-xref
   ;;consult--source-bookmark consult--source-file-register
   ;;consult--source-recent-file consult--source-project-recent-file
   ;;;; :preview-key "M-."
   ;;:preview-key '(:debounce 0.4 any)
   consult-goto-line
   consult-theme :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package embark
  :ensure t
  :after vertico
  :bind
  (("C-c e e" . embark-export)
   ("C-c e a" . embark-act)         ;; pick some comfortable binding
   ("C-c e d" . embark-dwim)        ;; good alternative: M-.)
   ("C-c e b" . embark-bindings) ;; alternative for `describe-bindings'
   ([remap describe-bindings] . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)
              ("C-c C-c" . embark-act))
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(global-unset-key (kbd "M-i"))
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.2 . 0.1))
  ;;(corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; nil: Never quit, even if there is no match
  (corfu-preview-current nil)
  ;;;;(corfu-preselect-first nil)    ;; Disable candidate preselection
  ;;(corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;;(corfu-scroll-margin 5)        ;; Use scroll margin

  ;;;; Use TAB for cycling, default is `corfu-complete'.
  ;;:bind
  ;;(:map corfu-map
  ;;      ("TAB" . corfu-next)
  ;;      ([tab] . corfu-next)
  ;;      ("S-TAB" . corfu-previous)
  ;;      ([backtab] . corfu-previous))

  ;;;; Enable Corfu only for certain modes.
  ;;:hook ((prog-mode . corfu-mode)
  ;;       (c-mode . corfu-mode)
  ;;       (c++-mode . corfu-mode)
  ;;       (shell-mode . corfu-mode)
  ;;       (eshell-mode . corfu-mode))

  ;;;; Recommended: Enable Corfu globally.
  ;;;; This is recommended since Dabbrev can be used globally (M-/).
  ;;;; See also `corfu-excluded-modes'.
  ;;:init
  ;;(global-corfu-mode)

  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  ;;:bind ("M-/" . completion-at-point)
  :bind ("M-i M-i" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (;;("M-i M-i" . completion-at-point) ;; capf
         ("M-i i" . completion-at-point) ;; capf
         ("M-i p" . completion-at-point) ;; capf
         ("M-i t" . complete-tag)        ;; etags
         ("M-i d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-i h" . cape-history)
         ("M-i f" . cape-file)
         ("M-i k" . cape-keyword)
         ("M-i s" . cape-elisp-symbol)
         ("M-i e" . cape-elisp-block)
         ("M-i a" . cape-abbrev)
         ("M-i l" . cape-line)
         ("M-i w" . cape-dict)
         ("M-i \\" . cape-tex)
         ("M-i _" . cape-tex)
         ("M-i ^" . cape-tex)
         ("M-i &" . cape-sgml)
         ("M-i r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-vertico)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vertico.el ends here
