;; packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)

(defun install-pkg (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(install-pkg 'company)
(install-pkg 'slime)


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)
(setq make-backup-files nil)
(show-paren-mode t)
(global-company-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(setq inferior-lisp-program "/usr/bin/sbcl")
