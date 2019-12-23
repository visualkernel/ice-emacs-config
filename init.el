;; set packages site
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)

;; base settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)
(setq make-backup-files nil)
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
;;(load-theme 'wombat t)
;;(set-default-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(delete-selection-mode t)

;; install packages
(defmacro install-pkg (pkg &rest body)
  `(unless (package-installed-p ',pkg)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (package-install ',pkg)
     ,@body))

;; package: company
(install-pkg company)
(global-company-mode 1)

;; package: slime
(install-pkg slime)
(setq inferior-lisp-program "/usr/bin/sbcl")

;; package: magit
(install-pkg magit)

;; highlight-parenttheses
(install-pkg highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'lisp-mode-hook 'highlight-parentheses-mode)

;; hungry-delete
(install-pkg hungry-delete)
(global-hungry-delete-mode t)

;; swiper
(install-pkg swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
