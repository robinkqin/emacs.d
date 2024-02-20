;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "proxy.com:8080")
;;         ("https" . "proxy.com:8080")))
;;
;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;;                   (cons "Input your LDAP UID !"
;;                         (base64-encode-string "USERNAME:PASSWORD")))))

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; More options
(setq package-install-upgrade-built-in t)

(unless (fboundp 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-enable-imenu-support t
      use-package-expand-minimally t
      ;;use-package-always-defer t
      ;;use-package-always-ensure t
      use-package-verbose t)

;;(require 'use-package)

;; Required by `use-package'
(use-package diminish
  :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update
  :ensure t)

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
