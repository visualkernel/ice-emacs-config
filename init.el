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
    green-screen-theme
    company
    hungry-delete
    smartparens
    expand-region
    swiper
    aggressive-indent
    json-mode
    sr-speedbar
    tabbar
    powerline
    helm
    ggtags
    slime
    haskell-mode
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
(savehist-mode 1)
(tooltip-mode -1) ;; disable tooltip mode
(tool-bar-mode -1) ;; close toolbar
(menu-bar-mode -1) ;; close menu
(scroll-bar-mode -1) ;; close scrollbar
(global-linum-mode 1) ;; show line number
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline nil) ;;remove underline when hl-line-mode enable
(fset 'yes-or-no-p 'y-or-n-p) ;y/n=>yes/no
(blink-cursor-mode -1) ;disable cursor blink
(transient-mark-mode 1)
(setq use-dialog-box nil) ;never pop dialog
(setq inhibit-startup-screen t) ;inhibit start screen
(setq initial-scratch-message "")
(setq mouse-yank-at-point t)
(setq default-major-mode 'text-mode)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(setq column-number-mode t)
(setq line-number-mode t)
(delete-selection-mode t)
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

(setq-default indent-tabs-mode nil)

;; (tabbar-mode 1)
;; (sr-speedbar-open)

;; (require 'helm-config)
;; (helm-mode 1)

;; (powerline-default-theme)
;; (powerline-center-theme)

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

(global-set-key [?\C-c ?\C-c] 'comment-or-uncomment-region)

(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; ------------------------------------------
;; settings
;; -----------------------------------------

;; theme settings
;; (load-theme 'deeper-blue t)
(load-theme 'green-screen t)

;; (setq frame-title-format "%f")
(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))
(setq-default cursor-type 'bar)
(setq make-backup-files nil)
(setq recentf-max-menu-item 10)

;; font settings
(set-default-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")

;; (setq-default default-tab-width 4)
;; (setq-default tab-width 4)
;; (setq-default indent-tabs-mode t)
;; (setq-default c-default-style "linux")
;; (setq-default c-basic-offset 4)

;; set c-mode hook for c-programming
(defun my-c-mode-hook()
  (c-set-style "linux")
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)
  (hs-minor-mode 1)
  (ggtags-mode 1)
  (if (and (buffer-file-name)
           (string-match ".c$\\|.cpp$" (buffer-file-name)))
      (hs-hide-all))
  (define-key c-mode-base-map (kbd "C-o") 'hs-toggle-hiding)
  (define-key c-mode-base-map [remap comment-region]
    'comment-or-uncomment-region))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;; slime for common lisp
;; (setq inferior-lisp-program "/usr/bin/sbcl")

;; sr-speedbar
(setq speedbar-use-images nil
      sr-speedbar-right-side nil)

;; maximize-window
(toggle-frame-maximized)
(put 'narrow-to-region 'disabled nil)
