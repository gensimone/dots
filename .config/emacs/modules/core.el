;;; core.el -*- lexical-binding: t; -*-

(setq inhibit-startup-message t
      ring-bell-function 'ignore
      use-dialog-box nil
      confirm-kill-emacs 'y-or-n-p)

(setq make-backup-files nil
      auto-save-default nil)

(global-display-line-numbers-mode 'relative)
(column-number-mode 1)

(setq scroll-margin 8
      scroll-conservatively 101)

(provide 'core)
