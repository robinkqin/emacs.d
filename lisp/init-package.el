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

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless (fboundp 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t
      use-package-expand-minimally t
      use-package-verbose t)

(require 'use-package)

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package diminish
  :ensure t)

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
