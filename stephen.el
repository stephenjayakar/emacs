(load-file "~/.emacs.d/stephen-init.el")
;; End Init

;; Custom macros
;(fset 'require-to-import
;      (kmacro-lambda-form [?\C-\[ ?d ?i ?m ?p ?o ?r ?t ?\C-s ?r ?e ?q ?u ?i ?r ?e ?\C-m ?\C-\[ ?\C-? ?\C-? ?\C-? ?f ?r ?o ?m ?\C-d ?  ?\C-s ?\) ?\C-m ?\C-b ?\C-k ?\; ?\C-s ?r ?e ?q ?u ?i ?r ?e ?\C-m ?\C-a] 0 "%d"))

;; Custom Functions
(defun copy-region-to-clipboard ()
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
(require 'lsp-mode)
(add-hook 'web-mode-hook #'lsp)
(add-hook '-mode-hook #'lsp)
(setq lsp-rust-server 'rust-analyzer)

(company-mode +1)
(add-hook 'after-init-hook 'global-company-mode)
(ivy-mode 1)

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(add-hook 'term-mode-hook
   (lambda ()
     ;; C-x is the prefix command, rather than C-c
     (term-set-escape-char ?\C-x)
     (define-key term-raw-map "\M-y" 'yank-pop)
     (define-key term-raw-map "\M-w" 'kill-ring-save)))
(exec-path-from-shell-initialize)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-hook 'rust-mode 'flymake-mode)
(add-hook 'rust-mode 'lsp)

(setq custom-file "~/.emacs.d/garbage.el")

;; Keybinds
(global-set-key (kbd "M-x") 'smex)
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

