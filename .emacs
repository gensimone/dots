;;; --- Package Management ---
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;;; --- UI Configuration ---
;; Theme
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12"))
(set-face-attribute 'default t :font "JetBrainsMono Nerd Font-12")

;; Gruvbox
(use-package gruvbox-theme :ensure t)
(load-theme 'gruvbox-dark-medium :no-confirm)

;; Window appearance
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; UI elements
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(setq use-file-dialog nil)

;; Line numbers
(global-display-line-numbers-mode +1)
(setq display-line-numbers-type 'relative)

;;; --- Core Configuration ---
;; Custom file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessages)

;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Global modes
(transient-mark-mode 1)
(setq-default truncate-lines t)
(global-auto-revert-mode 1)
(save-place-mode 1)

;; Startup behavior
(setq inhibit-startup-message t
      inhibit-startup-screen t
      ;; inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-e") #'wdired-change-to-wdired-mode))

;; Compile command
(setq compile-command "make")

;;; --- Programming ---
;; Formatting
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

;;; --- Packages Configuration ---
;; Evil package suite
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
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

;; Interface packages
(use-package magit)

(use-package ivy
  :config
  (ivy-mode)
  (setopt ivy-use-virtual-buffers t)
  (setopt enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t))

(use-package tmux-pane
  :config
  (tmux-pane-mode))

;; Application packages
(use-package pdf-tools
  :ensure t
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (pdf-tools-install))

(use-package vterm
  :ensure t
  :hook (image-mode . (lambda () (display-line-numbers-mode 0))))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" "sk-...")
  :custom
  (aidermacs-default-chat-mode 'code)
  (aidermacs-default-model "openrouter/tngtech/deepseek-r1t2-chimera:free"))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

(use-package nerd-icons)

;;; --- Org Mode Configuration ---
(use-package org
  :ensure nil  ; built-in package
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-agenda-tags-column 0)
  (org-ellipsis "…"))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (global-org-modern-mode))

;;; --- Keybindings ---
;; Evil mode navigation
(evil-define-key 'normal 'global
  (kbd "C-h") #'windmove-left
  (kbd "C-j") #'windmove-down
  (kbd "C-k") #'windmove-up
  (kbd "C-l") #'windmove-right
  (kbd "C--") #'maximize-window)

;; Global keys
(global-set-key (kbd "C-x C-c") #'ignore)

;; Frame transparency
(set-frame-parameter nil 'alpha-background 100) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth
