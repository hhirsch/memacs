;;  __  __      __  __
;; |  \/  | ___|  \/  | __ _  ___ ___
;; | |\/| |/ _ \ |\/| |/ _` |/ __/ __|
;; | |  | |  __/ |  | | (_| | (__\__ \
;; |_|  |_|\___|_|  |_|\__,_|\___|___/
;;
;; Light Emacs Distribution
(load "~/.emacs.d/memacs.el")
(global-display-line-numbers-mode t)
(defun nolinum ()
  (display-line-numbers-mode 0)
)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)
(setq lsp-disabled-clients '(ts-ls)) ;; deno instead of node
(defvar memacs/package-refreshed-p nil
  "Non-nil if package archives have been refreshed this Emacs session.")

(enable-command-line-clipboard)

(require 'package) ; load package managment
;; add sources to load our packages from
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ; load and activate the installed packages

(ensure-package-is-installed 'use-package)

(customize-window)

(setup-editor-for-programming)

;; move save files to a central space
;; avoiding cluttering project directories
(setq backup-directory-alist
      `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves" t)))

;; Dashboard Configuration
(customize-dashboard)
;; Language Server Hooks
(install-language-server-hooks)
;;(require 'languages)
(load "~/.emacs.d/languages.el")
(add-language-support)
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

;; Terminal
(ensure-package-is-installed 'eat)
(use-package eat)
(add-hook 'eat-mode-hook 'nolinum)

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
(setq org-latex-src-block-backend 'minted) 

;;iedit
(ensure-package-is-installed 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)

;; Snippets
(ensure-package-is-installed 'dirvish)
(ensure-package-is-installed 'yasnippet) 
(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
    :bind (:map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil)
         ("C-c C-l" . yas-insert-snippet)
         ("C-c C-w" . yas-expand)         
         )
    :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(add-hook 'after-save-hook 'autocompile-init-file)
(run-with-idle-timer 0 nil #'async-startup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
