;; Increase font size
(set-face-attribute 'default nil :height 150)

;; Stolen from someone smart
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; Column numbers
(column-number-mode)
(global-display-line-numbers-mode)

(dolist (mode '(eshell-mode-hook
		term-mode-hook
		shell-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

