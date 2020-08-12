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

;; Disabling backup files
(setq make-backup-files nil)

;; UI
(load-file "~/.emacs.d/stephen-ui.el")

(global-undo-tree-mode)

;; Keybinds
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-x u") 'undo-tree-undo)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c C-c") 'copy-region-to-clipboard)
(global-set-key (kbd "C-x C-j") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)
(global-set-key (kbd "C-x f") 'fzf)

;; END Keybinds

;; LSP MODE
(setq lsp-keymap-prefix "C-x C-k")
(require 'lsp-mode)
(add-hook 'web-mode-hook #'lsp)
(add-hook '-mode-hook #'lsp)

(company-mode +1)
(add-hook 'after-init-hook 'global-company-mode)
(ivy-mode 1)

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

(setq custom-file "~/.emacs.d/garbage.el")