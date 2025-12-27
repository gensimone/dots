;; init.el

;; straight bootstrap code
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; integration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; load modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/completion" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq exec-path (append exec-path '("~/.local/bin")))

(require 'core)
(require 'evil)
(require 'code-format)
(require 'git)
(require 'leader)
(require 'recentf)
(require 'ui)
(require 'vertico)
(require 'ai)
