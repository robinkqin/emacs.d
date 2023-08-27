;; init-dict.el --- Initialize dictionaries.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Multiple dictionaries.
;;
;;; Code:

(use-package fanyi
  :custom
  (fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-etymon-provider
                     fanyi-longman-provider)))

(when sys/linuxp
  (require 'sdcv)
  (setq sdcv-say-word-p t)
  (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic")
  (setq sdcv-dictionary-simple-list
        '("KDic11万英汉词典"
          "懒虫简明英汉词典"
          "懒虫简明汉英词典"))
  (setq sdcv-dictionary-complete-list
        '("牛津英汉双解美化版"
          "英汉汉英专业词典"
          "quick_eng-zh_CN"
          "KDic11万英汉词典"
          "朗道汉英字典5.0"
          "朗道英汉字典5.0"
          "懒虫简明英汉词典"
          "懒虫简明汉英词典")))

(when sys/win32p
  ;; copy sdcv.exe libtermcap-0.dll libreadline8.dll from msys2 env to PATH
  (require 'sdcv)
  (setq sdcv-say-word-p t)
  (setq sdcv-dictionary-data-dir "C:\\software\\stardict\\stardict")
  (setq sdcv-dictionary-simple-list
        '("kdic-ec-11w"))
  (setq sdcv-dictionary-complete-list
        '("kdic-ec-11w"
          "oxford-gb-formated"  ;; modify *.ifo bookname=xxx
          "quick_eng-zh_CN")))

(when sys/macp
  (use-package osx-dictionary))

;;;; On-the-fly spell checker
;;(use-package flyspell
;;  :ensure nil
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


(provide 'init-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
