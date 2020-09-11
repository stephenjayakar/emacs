(load-file "~/.emacs.d/stephen-init.el")

;; Custom Functions
(defun copy-region-to-clipboard-mac ()
  "Copies the selected region to system clipboard"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy"))

;; Projectile Config
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; End Projectile Config

;; Moving backup files out of working directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; UI
(load-file "~/.emacs.d/stephen-ui.el")

(global-undo-tree-mode)

;; LSP MODE

(setq lsp-keymap-prefix "C-x C-k")
(add-hook 'web-mode-hook #'lsp)
(add-hook '-mode-hook #'lsp)
(setq lsp-rust-server 'rust-analyzer)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; END LSP MODE

(ivy-mode 1)

(add-hook 'term-mode-hook
   (lambda ()
     ;; C-x is the prefix command, rather than C-c
     (term-set-escape-char ?\C-x)
     (define-key term-raw-map "\M-y" 'yank-pop)
     (define-key term-raw-map "\M-w" 'kill-ring-save)))
(exec-path-from-shell-initialize)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(setq custom-file "~/.emacs.d/garbage.el")

;; Keybinds
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-x u") 'undo-tree-undo)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c C-c") 'copy-region-to-clipboard)
(global-set-key (kbd "C-x C-j") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)
(global-set-key (kbd "C-x f") 'fzf)
(global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-e") 'eval-buffer)
(global-set-key (kbd "C-c RET") 'yafolding-toggle-element)
;; (define-key yafolding-mode-map (kbd "C-c <C-RET>") 'yafolding-toggle-element)
;; END Keybinds

