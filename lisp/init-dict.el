;; init-dict.el --- Initialize dictionaries.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-etymon-provider
                     fanyi-longman-provider)))

(require 'sdcv)
(setq sdcv-say-word-p t)
(if sys/win32p
    (setq sdcv-dictionary-data-dir "C:\\software\\stardict\\stardict")
  (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic"))
(setq sdcv-dictionary-simple-list
      '("kdic-ec-11w"))
(setq sdcv-dictionary-complete-list
      '("kdic-ec-11w"
        "oxford-gb-formated"  ;; *.ifo bookname=xxx
        "quick_eng-zh_CN"))

(when sys/macp
  (use-package osx-dictionary
    :ensure t))

;;(use-package go-translate
;;  :ensure t
;;  :bind (("C-c d g" . gts-do-translate))
;;  :init (setq gts-translate-list '(("en" "zh") ("zh" "en"))))

;;;; On-the-fly spell checker
;;(use-package flyspell
;;  :diminish
;;  :if (executable-find "aspell")
;;  :hook (((text-mode outline-mode) . flyspell-mode)
;;         ;; (prog-mode . flyspell-prog-mode)
;;         (flyspell-mode . (lambda ()
;;                            (dolist (key '("C-;" "C-," "C-."))
;;                              (unbind-key key flyspell-mode-map)))))
;;  :init (setq flyspell-issue-message-flag nil
;;              ispell-program-name "aspell"
;;              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;;(when (my/eglot-available-p)
;;  (use-package corfu-english-helper
;;    :commands toggle-corfu-english-helper))

(provide 'init-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
