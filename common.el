(set-language-environment "UTF-8")

;; Open help buffer always in vsplit
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Maximize emacs on startup
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Do nothing on alerts
(setq ring-bell-function 'ignore)

;; Escape is not a command
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Git is enough for backups
(setq make-backup-files nil)
(setq auto-save-default nil)
