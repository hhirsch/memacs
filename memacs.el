(defun ensure-package-is-installed (package)
  "Installs the specified package if it is not installed"
  (unless (package-installed-p package)
    (unless memacs/package-refreshed-p
      (package-refresh-contents)
      (setq memacs/package-refreshed-p t)
    )
    (package-install package))
  )

(defun markdown-php-fenced-block-region ()
  "Return (BEG . END) of the current fenced PHP block (excluding fence lines), or nil."
  (save-excursion
    (let ((orig (point)))
      (when (or (search-backward "```php" nil t)
                (progn (goto-char orig) (search-forward "```php" nil t)))
        (goto-char (match-beginning 0))
        (forward-line 1)
        (let ((beg (point)))
          (if (search-forward-regexp "^```[ \t]*$" nil t)
              (progn (beginning-of-line) (cons beg (point)))
            (cons beg (point-max))))))))

(defun run-command (command)
  (interactive)
  (shell-command command "*shell-output*" "*shell-error*")
      (display-buffer
       (get-buffer "*shell-output*")
       '((display-buffer-reuse-window display-buffer-in-side-window)
         (inhibit-same-window . t)
         (side . bottom)
         (window-height . 0.25)
         (no-select . t)))
      )

(defun run-command-at-point ()
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (if (string-prefix-p "#x " line)
        (run-command (replace-regexp-in-string "\\`#x " "" line))
      (message "Line not executable"))))

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd "C-c C-c") #'run-command-at-point))

(defun markdown-run-php-fenced-block-via-shell-command ()
  "Send current PHP fenced block to `php' via `shell-command-on-region'.
If the block does not start with a PHP opening tag, prepend \"<?php\\n\" before sending."
  (interactive)
  (let ((range (markdown-php-fenced-block-region)))
    (let* ((beg (car range))
           (end (cdr range))
           (code (buffer-substring-no-properties beg end))
           (needs-tag (not (string-match-p "\\`\\s-*<\\?php\\b" code)))
           (tmp (make-temp-file "md-php-" nil ".php")))
      ;; Write code to temp file, prepending <?php if needed
      (with-temp-file tmp
        (when needs-tag
          (insert "<?php\n"))
        (insert code))
      ;; Run php on the temp file and show output
      (shell-command (format "php %s" (shell-quote-argument tmp)) "*php-shell-output*" "*php-shell-error*")
      (shell-command (format "rm %s" (shell-quote-argument tmp)))
      (display-buffer
       (get-buffer "*php-shell-output*")
       '((display-buffer-reuse-window display-buffer-in-side-window)
         (inhibit-same-window . t)
         (side . bottom)
         (window-height . 0.25)
         (no-select . t)))
      )))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-c") #'markdown-run-php-fenced-block-via-shell-command))

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
    (setq dashboard-startup-banner "~/.emacs.d/logo.txt")
(when (display-graphic-p)
 (setq dashboard-startup-banner "~/.emacs.d/logo.svg"))
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
