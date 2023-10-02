;;; init-citre.el --- citre configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package citre
  :ensure t
  :diminish
  :bind (:map prog-mode-map
              ("C-x c j" . citre-jump+)
              ("C-x c p" . citre-peek)
              ("C-x c a" . citre-ace-peek))
  :init
  ;;(require 'citre-config)
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (when sys/win32p
    (setq citre-gtags-args '("--compact")))

  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (let* ((xref-prompt-for-identifier nil))
               (call-interactively #'xref-find-definitions)))))

  (defun my--push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))
  (dolist (func '(find-function
                  consult-imenu consult-imenu-multi
                  consult-line consult-grep consult-git-grep consult-ripgrep
                  ;;consult-outline consult-eglot-symbols
                  ;;beginning-of-buffer end-of-buffer jump-to-register mark-whole-buffer
                  ;;beginend-prog-mode-goto-end beginend-prog-mode-goto-beginning
                  ;;mwim-beginning-of-code-or-line mwim-end-of-code-or-line
                  ;;next-buffer previous-buffer switch-to-buffer describe-function
                  ;;describe-variable find-file-at-point xref-find-definitions
                  ;;session-jump-to-last-change avy-goto-word-1 avy-goto-word-2
                  embark-act keyboard-escape-quit
                  embark-next-symbol embark-previous-symbol
                  citre-jump))
    (advice-add func :before 'my--push-point-to-xref-marker-stack))

  (if my/eglot-enable
      (with-no-warnings
        (message "citre with eglot")
        ;; Use Citre xref backend as a fallback
        (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
          (let ((fetcher (apply fn args))
                (citre-fetcher
                 (let ((xref-backend-functions '(citre-xref-backend t)))
                   (ignore xref-backend-functions)
                   (apply fn args))))
            (lambda ()
              (or (with-demoted-errors "%s, fallback to citre"
                    (funcall fetcher))
                  (funcall citre-fetcher)))))

        ;; Combine completions from Citre and lsp
        (defun lsp-citre-capf-function ()
          "A capf backend that tries lsp first, then Citre."
          (let ((lsp-result (cond
                             ((bound-and-true-p lsp-mode)
                              (and (fboundp #'lsp-completion-at-point)
                                   (lsp-completion-at-point)))
                             ((bound-and-true-p eglot--managed-mode)
                              (and (fboundp #'eglot-completion-at-point)
                                   (eglot-completion-at-point))))))
            (if (and lsp-result
                     (try-completion
                      (buffer-substring (nth 0 lsp-result)
                                        (nth 1 lsp-result))
                      (nth 2 lsp-result)))
                lsp-result
              (citre-completion-at-point))))

        (defun enable-lsp-citre-capf-backend ()
          "Enable the lsp + Citre capf backend in current buffer."
          (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

        (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend))
    (with-no-warnings
      (message "citre without eglot")
      (add-to-list 'completion-at-point-functions#'citre-completion-at-point))))


(require 'citre)
(require 'consult)
(require 'consult-xref)

(defun consult-citre-readtags--build-cmd
    (tagsfile &optional name match case-fold filter sorter action)
  "Build readtags command.
See `citre-readtags-get-tags' to know about NAME, MATCH, CASE-FOLD,
FILTER, and SORTER.  ACTION can be nil, to get regular tags, or
any valid actions in readtags, e.g., \"-D\", to get pseudo tags."
  (let* ((match (or match 'exact))
         (extras (concat
                  "-Ene"
                  (pcase match
                    ('exact "")
                    ('prefix "p")
                    (_ (error "Unexpected value of MATCH")))
                  (if case-fold "i" "")))
         (tagsfile (substring-no-properties tagsfile))
         (name (when name (substring-no-properties name)))
         (filter (citre-readtags--strip-text-property-in-list filter))
         (sorter (citre-readtags--strip-text-property-in-list sorter))
         inhibit-message
         cmd)
    ;; Program name
    (push (or citre-readtags-program "readtags") cmd)
    ;; Read from this tags file
    (push "-t" cmd)
    (push (file-local-name tagsfile) cmd)
    ;; Filter expression
    (when filter (push "-Q" cmd) (push (format "%S" filter) cmd))
    (when sorter (push "-S" cmd) (push (format "%S" sorter) cmd))
    ;; Extra arguments
    (push extras cmd)
    ;; Action
    (if action (push action cmd)
      (if (or (null name) (string-empty-p name))
          (push "-l" cmd)
        (push "-" cmd)
        (push name cmd)))
    (nreverse cmd)))

(defun consult-citre-readtags--builder (input)
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
    (setq re (consult--join-regexps re 'extended))
    (cons
     (append (consult-citre-readtags--build-cmd
              (citre-tags-file-path)
              nil nil t
              `((string->regexp ,re :case-fold true) $name)
              nil
              (car-safe opts))
             (cdr-safe opts))
     hl)
    ))

(defun consult-citre-readtags--format (info lines)
  (mapcar (lambda (line)
            (let* ((tag (citre-readtags--parse-line
                         line
                         info
                         '(name input pattern line kind) '() '()
                         '(ext-abspath ext-kind-full) '() '() t))
                   (group (citre-get-tag-field 'ext-abspath tag))
                   (line (citre-get-tag-field 'line tag))
                   (cand (consult--format-file-line-match
                          group
                          line
                          (citre-make-tag-str tag nil
                                              '(annotation :prefix "(" :suffix ")"
                                                           ;; In xref buffer, we may want to jump to
                                                           ;; the tags with these anonymous names.
                                                           :full-anonymous-name t)
                                              '(content :ensure t)))))
              (add-text-properties 0 (length cand) `(consult-citre-tag ,tag consult--prefix-group ,group) cand)
              cand))
          lines))

;;;###autoload
(defun consult-citre (initial)
  "Read a tag from minibuffer and jump to the tag."
  (interactive "P")
  (let ((info (citre-readtags-tags-file-info (citre-tags-file-path))))
    (xref-pop-to-location
     (consult--read
      (consult--async-command
          #'consult-citre-readtags--builder
        (consult--async-transform consult-citre-readtags--format info)
        (consult--async-highlight #'consult-citre-readtags--builder))
      :prompt "Tag: "
      :keymap consult-async-map
      :require-match t
      :category 'consult-citre
      :initial (consult--async-split-initial initial)
      :group #'consult--prefix-group
      :state (consult-xref--preview #'switch-to-buffer)
      :lookup (lambda (&rest args)
                (when-let ((tag (apply #'consult--lookup-prop 'consult-citre-tag args)))
                  (citre-xref--make-object tag)))))))

(with-eval-after-load 'embark
  (defvar embark-exporters-alist)

  (defun consult-citre--embark-export-xref (items)
    "Create an xref buffer listing ITEMS."
    (let ((xrefs))
      (dolist-with-progress-reporter (item items)
          "Exporting Xrefs..."
        (redisplay)
        (push  (citre-xref--make-object (get-text-property 0 'consult-citre-tag item))
               xrefs))
      (set-buffer
       (xref--show-xref-buffer
        (lambda () nil)
        `((fetched-xrefs . ,xrefs)
          (window . ,(embark--target-window))
          (auto-jump . ,xref-auto-jump-to-first-xref)
          (display-action))))))
  (setf (alist-get 'consult-citre embark-exporters-alist)
        #'consult-citre--embark-export-xref))

(provide 'init-citre)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-citre.el ends here
