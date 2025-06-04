;;  __  __      __  __
;; |  \/  | ___|  \/  | __ _  ___ ___
;; | |\/| |/ _ \ |\/| |/ _` |/ __/ __|
;; | |  | |  __/ |  | | (_| | (__\__ \
;; |_|  |_|\___|_|  |_|\__,_|\___|___/
;;
;; Light Emacs Distribution
(global-display-line-numbers-mode t)
(defun ensure-package-is-installed (package)
  "Installs the specified package if it is not installed"
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

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

(require 'package) ; load package managment
;; add sources to load our packages from
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ; load and activate the installed packages

(ensure-package-is-installed 'use-package)

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
(ensure-package-is-installed 'dashboard) 
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-footer nil)
  (setq dashboard-startup-banner "~/.emacs.d/logo.svg")
  (setq dashboard-banner-logo-title "MeMacs")
  (setq dashboard-init-info "Keep things light"))

;; Language Server Hooks
(install-language-server-hooks)

;; Go Support
(ensure-package-is-installed 'go-mode) 
(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'before-save-hook #'lsp-format-buffer))
(ensure-package-is-installed 'company) 
(use-package company
  :ensure t
  :config
  (add-hook 'completion-at-point-functions 'go-complete-at-point)
	(setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))
(ensure-package-is-installed 'lsp-ui) 
(use-package lsp-ui
  :ensure t
  :config
(add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Builder Mode
(defun no-electric-indent ()
  (setq-local electric-indent-functions '(lambda (char) 'no-indent)))

(define-derived-mode builder-mode fundamental-mode "Builder Mode"
  "Major mode for Builder scripts."
  (setq font-lock-defaults '((builder-font-lock-keywords))))

(defvar builder-font-lock-keywords
  '(("^\\(builder-mode\\|remove\\|purge\\|execute\\|listContains\\|listFiles\\|assertTrue\\|assertFalse\\|step\\|done\\|upload\\|function\\|setUser\\|connect\\|setupHost\\|setHost\\|setTargetUser\\|pushFile\\|executable\\|ensureExecutable\\|ensureService\\|listPackages\\|dumpPackages\\|include\\|alias\\|print\\|ensurePackage\\|ensureCapabilityConnection\\|executeAndPrint\\|saveDatabase\\)\\s-+\\(.*\\)$" 
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
     ("//.*$" 
     (0 font-lock-comment-face)
     )))

(font-lock-add-keywords
 'builder-mode
 `((,(concat "^systemInfo\\s-*$") 0 font-lock-keyword-face)))

(add-hook 'builder-mode-hook 'turn-on-font-lock)
(add-hook 'builder-mode-hook 'no-electric-indent)
(add-to-list 'auto-mode-alist '("\\.bld\\'" . builder-mode))

;; Setup Theme
(ensure-package-is-installed 'doom-themes) 
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t   
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-atom")
)

;; don't show undo-tree as minor mode
(eval-after-load 'undo-tree
  '(setq minor-mode-alist (assq-delete-all 'undo-tree-mode minor-mode-alist)))
;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
;; globaly enable undo tree
(ensure-package-is-installed 'undo-tree) 
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(ensure-package-is-installed 'org-ref) 
(use-package org-ref)

;; color for source code listings in org mode latex exports
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-listings 'minted) 

(add-hook 'after-save-hook 'autocompile-init-file)
(run-with-idle-timer 0 nil #'async-startup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company dashboard doom-themes gcmh git-gutter go-complete lsp-mode
             lsp-ui org-ref terraform-mode undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
