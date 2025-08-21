(defun ensure-package-is-installed (package)
  "Installs the specified package if it is not installed"
  (unless (package-installed-p package)
    (unless memacs/package-refreshed-p
      (package-refresh-contents)
      (setq memacs/package-refreshed-p t)
    )
    (package-install package))
  )

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
    (add-hook 'typescript-mode-hook #'lsp)    
    )
  )

(defun setup-editor-for-programming ()
  "Prepare editor for programming"
  (ido-mode 1)
  (setq display-line-numbers t)
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
  (ensure-package-is-installed 'gcmh) 
  (use-package gcmh
    :ensure t  
    :config
    (gcmh-mode 1)
    )
  )

(defun start-emacs-server ()
  "start emacs server"
  (ensure-package-is-installed 'server) 
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

(defun customize-dashboard ()
(ensure-package-is-installed 'dashboard) 
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents   . 15)
                        (bookmarks . 5)
                        (projects  . 5)))
  (setq dashboard-startupify-list '(
    dashboard-insert-banner
    dashboard-insert-newline
    dashboard-insert-banner-title
    dashboard-insert-newline
    dashboard-insert-navigator
    dashboard-insert-newline
    dashboard-insert-init-info
    dashboard-insert-items))
  (setq dashboard-startup-banner "~/.emacs.d/logo.svg")
  (setq dashboard-banner-logo-title "MeMacs")
  (setq dashboard-init-info "Keep things light"))
)

(defun customize-window ()
;; remove toolbar and menu bar
  (tool-bar-mode -1)
  (menu-bar-mode -1)
)

(defun enable-command-line-clipboard ()
  "Enable clip board on command line emacs"
  (when (not (display-graphic-p))
    (xterm-mouse-mode 1)
    (ensure-package-is-installed 'xclip)
    (xclip-mode 1))
  )
