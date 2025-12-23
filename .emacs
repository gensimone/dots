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

;; Dashboard
(use-package minimal-dashboard
    :init
    (setq initial-buffer-choice #'minimal-dashboard)
    :custom
    (minimal-dashboard-buffer-name "Dashboard")
    (minimal-dashboard-image-path "~/.config/emacs/logo.svg")
    (minimal-dashboard-text ""))

; Option 2: Globally
(with-eval-after-load 'org (global-org-modern-mode))

;; Option 2: Globally
(with-eval-after-load 'org (global-org-modern-mode))
;; Add frame borders and window dividers
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
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
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
  (kbd "SPC bd") #'kill-current-buffer
  (kbd "SPC fb") #'switch-to-buffer
  (kbd "SPC fd") #'describe-function
  (kbd "SPC ff") #'find-file
  (kbd "SPC fp") #'project-switch-project
  (kbd "SPC n") #'switch-to-next-buffer
  (kbd "SPC p") #'switch-to-prev-buffer
  (kbd "SPC e") #'dired)

;; Compile command
(setq compile-command "make")

;; Font
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12" ))
(set-face-attribute 'default t :font "JetBrainsMono Nerd Font-12" )

;; Remeber and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessages)

;; (global-visual-line-mode 1)                ;; Alternative to truncate
(set-default 'truncate-lines nil)             ;; Do not truncate lines
(customize-set-variable 'menu-bar-mode nil)   ;; Remove menu-bar
(customize-set-variable 'scroll-bar-mode nil) ;; Remove scroll-bar
(customize-set-variable 'tool-bar-mode nil)   ;; Remove tool-bar
(setq use-file-dialog nil)                    ;; Do not use GUI dialog
(global-auto-revert-mode 1)                   ;; Revert buffers when the underlying file has changed
(global-display-line-numbers-mode +1)         ;; ..
(setq display-line-numbers-type 'relative)    ;; Relative line numbers

;; Make emacs minimal
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Enable transient mark mode
(transient-mark-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
