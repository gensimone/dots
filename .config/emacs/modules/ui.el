;;; ui.el -*- lexical-binding: t; -*-

(use-package gruvbox-theme
  :config (load-theme 'gruvbox t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package dashboard
  :init
  ;; Set dashboard as the startup buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Basic Doom-like configuration
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))

  :config
  (dashboard-setup-startup-hook))

(setq display-line-numbers-type 'relative)
(setq-default truncate-lines t)

(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font-12"))

(provide 'ui)
