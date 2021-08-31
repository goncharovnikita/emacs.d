;;; init.el --- Main configuration
;;; Commentary:

;; This configuration uses evil mode, ivy & counsel and some web packages

;; Speed up startup

;; -- Increase GC treshold --
(setq gc-cons-threshold 100000000)

;; ---- Straight.el ----

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(load-file "~/.emacs.d/common.el")
(load-file "~/.emacs.d/ui.el")

;; --- UsePackage ---
(straight-use-package 'use-package)

;; ---- Configuring packages ----

;; ---- Installing packages ----

;; --- Evil mode ---
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil)

;; --- Which key ---
(use-package which-key
  :straight t
  :init (which-key-mode 1)
  :config (evil-collection-init))

;; --- Ivy ---
(use-package ivy
  :straight t
  :init (ivy-mode 1))

;; --- Counsel ---
(use-package counsel
  :straight t
  :config
  (setq ivy-initial-input-alist nil))

;; --- CounselProjectile ---
(use-package counsel-projectile
  :straight t
  :config (counsel-projectile-mode))

;; --- IvyRich ---
(use-package ivy-rich
  :straight t
  :init (ivy-rich-mode 1))


;; --- Page break line ---
(straight-use-package 'page-break-lines)

;; --- Projectile ---
(use-package projectile
  :straight t
  :config
    (projectile-mode 1)
    (setq projectile-sort-order 'recently-active)
    (setq projectile-indexing-method 'hybrid)
    (setq projectile-enable-caching t)
  :custom
  ((projectile-completion-system 'ivy)))


;; ---- All the icons ----
(use-package all-the-icons
  :straight t)

;; ---- Dashboard ----
(use-package dashboard
  :straight t
  :init (dashboard-setup-startup-hook))

;; ---- Themes ----

;; --- Doom ---
(use-package doom-themes
  :straight t
  :config
    (load-theme 'doom-one t)
    (doom-themes-neotree-config)
    (doom-themes-org-config))

;; --- Doom Modeline ---
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

;; --- RainbowDelimiters --
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; --- Helpful ---
(use-package helpful
  :straight t
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))
    

;; --- RestartEmacs ---
(use-package restart-emacs
  :straight t)

  
;; --- General ---
(use-package general
  :straight t
  :config
  (general-create-definer ng/leader-key-def
			  :keymaps '(normal visual)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (ng/leader-key-def
    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    "bs" '(counsel-switch-buffer :which-key "buffer/switch")
    ;; File
    "f" '(:ignore t :which-key "file")
    "ff" '(counsel-find-file :which-key "file/find")
    ;; Search
    "s" '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "search/curent-file")
    ;; Project
    "p" '(:ignore t :which-key "project")
    "ps" '(counsel-projectile-switch-project :which-key "project/switch")
    "pf" '(projectile-find-file :which-key "project/find-file")
    "pr" '(counsel-projectile-rg :which-key "project/search")
    "pb" '(:ignore t :which-key "project/buffer")
    "pbs" '(counsel-projectile-switch-to-buffer :which-key "project/buffer/switch")
    ;; Service
    "z" '(:ignore t :which-key "service")
    "zz" '(restart-emacs :which-key "service/restart")
    ;; Help
    "h" '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "help/function")
    "hv" '(describe-variable :which-key "help/variable")
    "hk" '(describe-key :which-key "help/key")
    ;; Window
    "w" '(:ignore t :which-key "window")
    "wq" '(evil-quit :which-key "window/close")
    "wh" '(evil-window-left :which-key "window/left")
    "wj" '(evil-window-down :which-key "window/down")
    "wk" '(evil-window-up :which-key "window/up")
    "wl" '(evil-window-right :which-key "window/right")
    "ws" '(evil-window-split :which-key "window/split")
    "wv" '(evil-window-vsplit :which-key "window/vsplit")
    ;; Error checking
    "e" '(:ignore t :which-key "errors")
    "ec" '(flycheck-buffer :which-key "errors/check")
    "en" '(flycheck-next-error :which-key "errors/next")
    "ep" '(flycheck-previous-error :which-key "errors/prev")
    ;; Meta
    ";" '(counsel-M-x :which-key "M-x")
    ))
  
;; --- Load Env Vars ---
(use-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; --- Typescript Mode ---
(use-package typescript-mode
  :straight t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

;; --- Web mode ---
(use-package web-mode
  :straight t)

;; --- Flycheck ---
(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save))
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode))

;; --- Add node modules into path --
(use-package add-node-modules-path
  :straight t
  :hook ((js-mode . add-node-modules-path)
	 (typescript . add-node-modules-path)
	 (typescript-tsx . add-node-modules-path)))

;; --- JSON mode ---
(use-package json-mode
  :straight t)

(provide 'init)

;;; init.el ends here
