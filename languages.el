(defun add-language-support ()
  "Enable syntax highlighting and lsp for additional languages"

  ;; eglot everything
  (use-package eglot
    :hook (prog-mode . eglot-ensure))
  
  ;; toml
  (ensure-package-is-installed 'toml-mode)
  (use-package toml-mode)
  (add-to-list 'auto-mode-alist '("\\.container\\'" . toml-mode))
  (add-to-list 'auto-mode-alist '("\\.network\\'" . toml-mode))

  ;; lua
  (ensure-package-is-installed 'lua-mode)
  (use-package lua-mode)
  
  ;; zig
  (ensure-package-is-installed 'zig-mode)
  (use-package zig-mode)

  ;; caddyfile
  (ensure-package-is-installed 'caddyfile-mode)
  (use-package caddyfile-mode
  :ensure t
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))
  
  ;; go
  (ensure-package-is-installed 'go-mode)
  (use-package go-mode
    :ensure t
    :config
    (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'go-mode-hook #'yas-reload-all)
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
  
  ;; TypeScript Support
  (ensure-package-is-installed 'typescript-mode)
  (use-package typescript-mode
    :ensure t
    :config
    (add-hook 'typescript-mode-hook #'yas-reload-all)
    (add-hook 'before-save-hook #'lsp-format-buffer))
  
  ;; Nix Support
  (ensure-package-is-installed 'nix-mode)
  (use-package nix-mode
    :mode "\\.nix\\'")

  ;; Puppet Support
  (ensure-package-is-installed 'puppet-mode)
  (use-package puppet-mode)

  ;; Builder Mode
  (defun no-electric-indent ()
    (setq-local electric-indent-functions '(lambda (char) 'no-indent)))
  (define-derived-mode builder-mode fundamental-mode "Builder Mode"
    "Major mode for Builder scripts."
    (setq font-lock-defaults '((builder-font-lock-keywords))))
  (defvar builder-font-lock-keywords
    '(("^\\(builder-mode\\|remove\\|purge\\|execute\\|listContains\\|listFiles\\|assertTrue\\|assertFalse\\|step\\|done\\|upload\\|function\\|setUser\\|connect\\|import\\|setupHost\\|setHost\\|setTargetUser\\|pushFile\\|executable\\|ensureExecutable\\|ensureService\\|listPackages\\|dumpPackages\\|include\\|alias\\|print\\|ensurePackage\\|ensureCapabilityConnection\\|executeAndPrint\\|saveDatabase\\)\\s-+\\(.*\\)$"
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

  ;; PHP
  (ensure-package-is-installed 'php-mode)
  (ensure-package-is-installed 'phpactor)
  (ensure-package-is-installed 'company-phpactor)  
  (use-package phpactor :ensure t)
  (use-package company-phpactor :ensure t)
  (use-package php-mode
  ;;
  :hook ((php-mode . (lambda () (set (make-local-variable 'company-backends)
       '(;; list of backends
         company-phpactor
         company-files
         ))))))

  ;; Elisp
  (ensure-package-is-installed 'corfu)
  (use-package corfu :ensure t)
  (add-hook 'emacs-lisp-mode-hook #'corfu-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode)  
  (ensure-package-is-installed 'cape)
  (use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  )
  (setq corfu-auto t)
  (setq-local completion-at-point-functions
  (mapcar #'cape-company-to-capf
          (list #'company-files #'company-keywords #'company-dabbrev))))
