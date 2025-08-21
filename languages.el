(defun add-language-support ()
  "Enable syntax highlighting and lsp for additional languages"
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

  )
