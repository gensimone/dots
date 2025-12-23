(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)) (eval-when-compile (require 'use-package))

;; Vim Bindings
(use-package evil
  :demand t :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; Magit
(use-package magit)

;; Feebleline
(use-package feebleline
  :config
  (feebleline-mode 1))

;; Ivy
(use-package ivy
  :config
  (ivy-mode)
  (setopt ivy-use-virtual-buffers t)
  (setopt enable-recursive-minibuffers t))

;; Tmux Integration
(use-package tmux-pane
  :config
  (tmux-pane-mode))

;; Custom theme
(use-package adwaita-dark-theme
  :config
  (load-theme 'adwaita-dark :no-confirm))

;; Keymaps
(evil-define-key 'normal 'global
  (kbd "C-h") #'windmove-left
  (kbd "C-j") #'windmove-down
  (kbd "C-k") #'windmove-up
  (kbd "C-l") #'windmove-right
  (kbd "SPC bd") #'kill-current-buffer
  (kbd "SPC fb") #'switch-to-buffer
  (kbd "SPC fd") #'describe-function
  (kbd "SPC ff") #'find-file
  (kbd "SPC fp") #'project-switch-project)
  (kbd "SPC n") #'switch-to-next-buffer
  (kbd "SPC p") #'switch-to-prev-buffer

;; Compile command
(setq compile-command "make")

;; Font
(add-to-list 'default-frame-alist '(font . "ProFont Nerd Font-12" ))
(set-face-attribute 'default t :font "ProFont Nerd Font-12" )

;; Remeber and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessages)

(customize-set-variable 'menu-bar-mode nil)   ;; Remove menu-bar
(customize-set-variable 'scroll-bar-mode nil) ;; Remove scroll-bar
(customize-set-variable 'tool-bar-mode nil)   ;; Remove tool-bar
(global-auto-revert-mode 1)                   ;; Revert buffers when the underlying file has changed
(global-display-line-numbers-mode +1)         ;; ..
(setq display-line-numbers-type 'relative)    ;; Relative line numbers
(setq inhibit-splash-screen t)                ;; Remove splash-screen
(setq initial-scratch-message "")             ;; Remove scratch-buf message
(setq use-file-dialog nil)                    ;; Do not use GUI dialog
(toggle-truncate-lines 0)                     ;; Do not truncate lines
