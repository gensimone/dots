(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)) (eval-when-compile (require 'use-package))

;; Packages
(use-package evil
  :demand t :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))
(use-package evil-commentary
  :config
  (evil-commentary-mode))
(use-package magit)
(use-package feebleline
  :config
  (feebleline-mode 1))
(use-package ivy
  :config
  (ivy-mode)
  (setopt ivy-use-virtual-buffers t)
  (setopt enable-recursive-minibuffers t))
(use-package tmux-pane
  :config
  (tmux-pane-mode))
(use-package minimal-dashboard
    :init
    (setq initial-buffer-choice #'minimal-dashboard)
    :custom
    (minimal-dashboard-buffer-name "Dashboard")
    (minimal-dashboard-image-path "~/.config/emacs/logo.svg")
    (minimal-dashboard-text ""))
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))
(add-hook 'pdf-view-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))
(use-package vterm
    :ensure t)
(add-hook 'image-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))
;; Org Mode
(require 'org)
(use-package org-modern)
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))
(setq
 org-auto-align-tags nil
org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")
(global-org-modern-mode)

;; Backup files 
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Keymaps
(evil-define-key 'normal 'global
  (kbd "C-h") #'windmove-left
  (kbd "C-j") #'windmove-down
  (kbd "C-k") #'windmove-up
  (kbd "C-l") #'windmove-right
  (kbd "C--") #'maximize-window)

;; Dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-e") #'wdired-change-to-wdired-mode))

;; Compile command
(setq compile-command "make")

;; Font
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12" ))
(set-face-attribute 'default t :font "JetBrainsMono Nerd Font-12" )

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessages)

;; (global-visual-line-mode 1)                
(transient-mark-mode 1)
(setq-default truncate-lines t)
(customize-set-variable 'menu-bar-mode nil)   
(customize-set-variable 'scroll-bar-mode nil) 
(customize-set-variable 'tool-bar-mode nil)   
(setq use-file-dialog nil)                    
(global-auto-revert-mode 1)                   
(global-display-line-numbers-mode +1)         
(setq display-line-numbers-type 'relative)    
(save-place-mode 1)                           
(setq inhibit-startup-message t
      inhibit-startup-screen t
      ;; inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(load-theme 'manoj-dark :no-confirm)
