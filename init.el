;;  __  __      __  __
;; |  \/  | ___|  \/  | __ _  ___ ___
;; | |\/| |/ _ \ |\/| |/ _` |/ __/ __|
;; | |  | |  __/ |  | | (_| | (__\__ \
;; |_|  |_|\___|_|  |_|\__,_|\___|___/
;;
;; Light Emacs Distribution

(defun ensure-use-package-is-installed ()
  "Installs use package if it is not installed"
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun autocompile-init-file nil
  "Check if buffer is the init file and compile it"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (file-truename user-init-file)))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))

(defun install-language-server-hooks ()
  "Automatically start lsp for certain programming languages"
  (use-package lsp-mode
    :ensure t
    :config
    (add-hook 'php-mode-hook #'lsp)
    (add-hook 'c-mode-hook #'lsp)
    (add-hook 'c++-mode-hook #'lsp)
    (add-hook 'js-mode-hook #'lsp)
    )
  )

(defun setup-editor-for-programming ()
  "Prepare editor for programming"
  (global-linum-mode 1)
  (ido-mode 1)
  (set-face-attribute 'default nil :height 150)
  ;; indentation
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq indent-line-function 'insert-tab)
  )

(defun start-garbage-collector()
  ;; don't show gcmh as minor mode
  (eval-after-load 'gcmh
  '(setq minor-mode-alist (assq-delete-all 'gcmh-mode minor-mode-alist)))
  ;; enable garbage collection freeing unused memory
  (use-package gcmh
    :ensure t  
    :config
    (gcmh-mode 1)
    )
  )

(defun start-emacs-server ()
  "start emacs server"
  (use-package server
    :config
    (unless (server-running-p) ;; only start the server if it isn't already running
      (server-start)))  
  )

(defun async-startup ()
  "Run in the background asynchronously"
  (start-garbage-collector)  
  (start-emacs-server)
 )

(require 'package) ; load package managment
;; add sources to load our packages from
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ; load and activate the installed packages

(ensure-use-package-is-installed)

;; remove toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

(setup-editor-for-programming)

;; move save files to a central space
;; avoiding cluttering project directories
(setq backup-directory-alist
      `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves" t)))

;; Dashboard Configuration
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-footer nil)
  (setq dashboard-startup-banner "~/.emacs.d/logo.svg")
  (setq dashboard-banner-logo-title "MeMacs")
  (setq dashboard-init-info "Keep things light"))

(install-language-server-hooks)

;; Setup Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t   
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; don't show undo-tree as minor mode
(eval-after-load 'undo-tree
  '(setq minor-mode-alist (assq-delete-all 'undo-tree-mode minor-mode-alist)))
;; globaly enable undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(add-hook 'after-save-hook 'autocompile-init-file)
(run-with-idle-timer 0 nil #'async-startup)
