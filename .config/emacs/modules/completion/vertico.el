(use-package vertico
  :init
  (vertico-mode)  ;; Enable Vertico globally
  :custom
  (vertico-count 15)         ;; Max candidates shown
  (vertico-cycle t))         ;; Wrap around at top/bottom

;; Optional: add richer metadata display
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))          ;; Show additional info like file size/type

;; Optional: flexible completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package consult)

(use-package recentf
  :straight nil   ;; built-in
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 25)
  (recentf-auto-cleanup 'never)
  :config
  ;; Exclude junk (same spirit as Doom)
  (add-to-list 'recentf-exclude "\\.git/.*")
  (add-to-list 'recentf-exclude "/tmp/")
  (add-to-list 'recentf-exclude "recentf"))

(provide 'vertico)
