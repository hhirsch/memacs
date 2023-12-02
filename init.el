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

(require 'package) ; load package managment
; add sources to load our packages from
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ; load and activate the installed packages

(ensure-use-package-is-installed)

(tool-bar-mode -1)
(menu-bar-mode -1)
(use-package gcmh
  :ensure t  
  :config
  (gcmh-mode 1)
)

(global-linum-mode 1)
(ido-mode 1)
(set-face-attribute 'default nil :height 150)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(setq backup-directory-alist
      `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves" t)))

(use-package server
  :config
(unless (server-running-p)
(server-start)))

; Dashboard Configuration
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-footer nil)
  (setq dashboard-startup-banner "~/.emacs.d/logo.svg")
  (setq dashboard-banner-logo-title "MeMacs")
  (setq dashboard-init-info "Keep things light"))

(install-language-server-hooks)

; Setup Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-hook 'after-save-hook 'autocompile-init-file)
