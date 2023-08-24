;;; init-ivy.el --- ivy Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs Configuration.
;;


;;; Code:

(use-package counsel)
(ivy-mode)
(counsel-mode)
(setq ivy-height 17)
(setq ivy-fixed-height-minibuffer t)
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")
(setq ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful .+\\*"
                           "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*"
                           "\\`flycheck_.+"))
(setq ivy-initial-inputs-alist nil)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq swiper-action-recenter t)

;;(cond
;; ((executable-find "ugrep")
;;  (setq counsel-grep-base-command "ugrep --color=never -n -e '%s' '%s'"))
;; ((executable-find "rg")
;;  (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' '%s'")))
;;
;;(when (executable-find "fd")
;;  (setq counsel-fzf-cmd
;;        "fd --type f --hidden --follow --exclude .git --color never '%s'"))

;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f2>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Enhance M-x
(use-package amx
  :init (setq amx-history-length 20))

(use-package ivy-rich)
(ivy-rich-mode)

(use-package counsel-etags
  :ensure t
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(use-package ivy-xref
  :init
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

;;(use-package counsel-gtags)
;;(add-hook 'c-mode-hook 'counsel-gtags-mode)
;;(add-hook 'c++-mode-hook 'counsel-gtags-mode)
;;;;(with-eval-after-load 'counsel-gtags
;;;;  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
;;;;  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
;;;;  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
;;;;  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))


;; Support pinyin in Ivy
;; Input prefix ':' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package pinyinlib
  :autoload pinyinlib-build-regexp-string
  :init
  (with-no-warnings
    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) "!")
             (mapconcat
              #'my-pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'my-pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string "!" "" str )
                            "")))
              ""))
            (t nil)))

    (defun my-ivy--regex-pinyin (fn str)
      "The regex builder advice to support pinyin."
      (or (pinyin-to-utf8 str)
          (funcall fn str)))
    (advice-add #'ivy--regex-plus :around #'my-ivy--regex-pinyin)
    (advice-add #'ivy--regex-ignore-order :around #'my-ivy--regex-pinyin)))


(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init_ivy.el ends here
