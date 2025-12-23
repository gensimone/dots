;;; Startup
;;; PACKAGE LIST
(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")))
;;; BOOTSTRAP USE-PACKAGE (package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)) (eval-when-compile (require 'use-package))

;;; UNDO
;; Vim style undo not needed for emacs 28
(use-package undo-fu)

;;; Vim Bindings
(use-package evil
  :demand t :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; UI
(use-package adwaita-dark-theme
  :config
  (load-theme 'adwaita-dark))
(customize-set-variable 'menu-bar-mode nil)   ;; Remove menu-bar
(customize-set-variable 'tool-bar-mode nil)   ;; Remove tool-bar
(customize-set-variable 'scroll-bar-mode nil) ;; Remove scroll-bar
(setq inhibit-splash-screen t)                ;; Remove splash-screen
(setq use-file-dialog nil)                    ;; Do not use GUI dialog

;; Font
(add-to-list 'default-frame-alist '(font . "Hack Nerd Font Mono-12" ))
(set-face-attribute 'default t :font "Hack Nerd Font Mono-12" )

;; Remeber and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessages)
