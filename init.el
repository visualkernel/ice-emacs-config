;; ------------------------------------------
;; packages
;; ------------------------------------------
(when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives 
		 '("melpa" . "http://melpa.org/packages/") 
		 t))

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar ice/packages 
  '(;; --- Auto-completion ---
    company
    monokai-theme
    hungry-delete
    smartparens
    expand-region
    swiper
    aggressive-indent    
    ) "Default packages")

(setq package-selected-packages ice/packages)

(defun ice/packages-installed-p ()
  (loop for pkg in ice/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (ice/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ice/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


;; ------------------------------------------
;; mode
;; ------------------------------------------
;; close toolbar
(tool-bar-mode -1)
;; close scrollbar
(scroll-bar-mode -1)
;; show line number
(global-linum-mode 1)

(setq column-number-mode t)
(setq line-number-mode t)

(delete-selection-mode t)

(global-hl-line-mode 1)

(show-paren-mode t)

(global-auto-revert-mode t)

(require 'recentf)
(recentf-mode 1)

(global-company-mode 1)

(require 'hungry-delete)
(global-hungry-delete-mode)

(require 'smartparens-config)
(smartparens-global-mode t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(global-aggressive-indent-mode 1)

;; ------------------------------------------
;; function
;; ------------------------------------------
(defun open-init-file()
  "open init configure file <init.el>"
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; ------------------------------------------
;; keymap
;; ------------------------------------------

;; press <f2> to open <init.el> file
(global-set-key (kbd "<f2>") 'open-init-file)

;;expand-region key-binding
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; ------------------------------------------
;; settings
;; -----------------------------------------
(setq-default cursor-type 'bar)
(setq make-backup-files nil)
(setq recentf-max-menu-item 10)

;; (load-theme 'monokai 1)

(setq-default c-default-style "linux")
