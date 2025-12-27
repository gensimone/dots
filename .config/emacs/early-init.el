;; early-init.el

;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
