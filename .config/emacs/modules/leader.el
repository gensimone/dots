;;; leader.el -*- lexical-binding: t; -*-

(use-package general
  :config
  (general-create-definer doom/leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (doom/leader
    ;; files
    "f" '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf :which-key "recent")
    "fp" '(project-switch-project :which-key "projects")
    "fg" '(consult-ripgrep :which-key "ripgrep")

    ;; buffers
    "b" '(:ignore t :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch")
    "bd" '(kill-current-buffer :which-key "kill")

    ;; git
    "g" '(magit :which-key "magit")))

(use-package which-key
  :init (which-key-mode)
  :custom (which-key-idle-delay 0.5))

(provide 'leader)
