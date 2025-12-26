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

;; Window rules
(setq display-buffer-alist
      '(("\\*compilation\\*"
         display-buffer-at-bottom
         (window-height . 0.3))

        ("\\*Help\\*"
         display-buffer-reuse-window
         display-buffer-in-side-window
         (side . right)
         (window-width . 0.33))

        ("\\*.*\\*"
         display-buffer-reuse-window
         display-buffer-in-side-window
         (side . bottom)
         (window-height . 0.25))))

;;; --- Core Configuration ---
;; Custm file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessages)

;; Backup files
(let ((auto-saves-dir (expand-file-name "auto-saves" user-emacs-directory)))
  ;; Create the directory if it doesn't exist
  (unless (file-directory-p auto-saves-dir)
    (make-directory auto-saves-dir t))

  ;; Put auto-save files there
  (setq auto-save-file-name-transforms
        `((".*" ,(concat auto-saves-dir "/\\1") t)))

  ;; Put backup files there (files ending with ~)
  (setq backup-directory-alist
        `((".*" . ,auto-saves-dir)))

  ;; Optional: keep numbered backups
  (setq version-control t
        kept-new-versions 10
        kept-old-versions 2
        delete-old-versions t))

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
(defun my/compile-no-prompt ()
  (interactive)
  (let ((compilation-read-command nil))
    (call-interactively #'compile)))

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

(use-package nerd-icons)

;;; --- Snippets
(use-package yasnippet
  :config
  ;; Set custom snippet directories
  (setq yas-snippet-dirs '("~/.snippets"))
  ;; Enable yasnippet globally
  (yas-global-mode 1))

;;; --- Auto Insert Mode
(require 'autoinsert)
(auto-insert-mode 1)
(setq auto-insert-query nil)  ;; ask before inserting

;; Function to get Git username
(defun my/git-username ()
  "Return the Git username for the current repository or fallback to ENV."
  (or
   (string-trim (shell-command-to-string "git config user.name"))
   (getenv "USER")
   "Unknown Author"))

;; Function to choose license text
(defun my/choose-license ()
  "Prompt the user to choose a license and return the text."
  (let ((choice (completing-read "Choose license: "
                                 '("MIT" "GPLv3" "BSD-3-Clause" "None")
                                 nil t)))
    (pcase choice
      ("MIT"
       "Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction...")
      ("GPLv3"
       "This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version...")
      ("BSD-3-Clause"
       "Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met...")
      ("None" ""))))

;; Template function.
(defun my/insert-c-template ()
  "Insert a C file template with optional copyright/license header."
  (let ((license-text (my/choose-license)))
    (when (> (length license-text) 0)
      (insert "/*\n")
      (insert " * File: " (file-name-nondirectory buffer-file-name) "\n")
      (insert " * Author: " (my/git-username) "\n")
      (insert " * Created: " (format-time-string "%Y-%m-%d") "\n")
      (insert " * Copyright (c) " (format-time-string "%Y") " " (my/git-username) ". All rights reserved.\n")
      (insert " * License: \n")
      (insert (mapconcat (lambda (line) (concat " * " line))
                         (split-string license-text "\n") "\n")))
    (insert " */\n\n")))

;; Auto-insert definitions
(define-auto-insert
  '("\\.c\\'" . "C source file")
  (lambda () (my/insert-c-template)))

(define-auto-insert
  '("\\.h\\'" . "C header file")
  (lambda () (my/insert-h-template)))

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
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "C-h") #'windmove-left
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-right
    (kbd "C--") #'maximize-window)
  (evil-define-key '(normal insert) 'global
    (kbd "C-c C-c") #'my/compile-no-prompt
    (kbd "C-x C-r") #'recentf))

;; Global keys
(global-set-key (kbd "C-x C-c") #'ignore)
