(use-package aidermacs
  ;; :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" "sk-...")
  :custom
  (aidermacs-default-chat-mode 'code)
  (aidermacs-default-model "openrouter/tngtech/deepseek-r1t2-chimera:free"))

(shell-command-to-string "aider --version")

(provide 'ai)
