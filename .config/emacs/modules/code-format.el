(use-package format-all
  :commands (format-all-buffer format-all-mode)
  :hook ((prog-mode . format-all-ensure-formatter))
  :config
  (add-hook 'prog-mode-hook 'format-all-mode))

(provide 'code-format)
