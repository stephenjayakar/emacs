;; Fixing path to mirror the one of my zsh shell
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))
;; setting emacs garbage collection threshold to a modern device level
(setq gc-cons-threshold 100000000)

;; package setup
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  :ensure t
)

(use-package tide
  :ensure t
  :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      (company-mode +1)
    )
  :hook (web-mode . setup-tide-mode)
  :bind (
    ("C-c t" . tide-jump-to-definition)
  )
)

(use-package web-mode
  :ensure t
  :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-auto-indentation nil)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
)

(use-package rust-mode
  :ensure t
)

(use-package yaml-mode
  :ensure t
)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config (
    setq lsp-rust-server 'rust-analyzer
         lsp-auto-guess-root nil
  )
)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
)

(use-package ivy
  :ensure t
  :config (
    ivy-mode 1
  )
)

(use-package undo-tree
  :ensure t
  :bind (
    ("C-x u" . undo-tree-undo)
  )
  :config (
    global-undo-tree-mode
  )
)

(use-package restart-emacs
  :ensure t
)

(use-package flycheck
  :ensure t
  :config
    (flycheck-add-mode 'typescript-tslint 'web-mode)
)

(use-package company
  :ensure t
  :config
    ;; Optionally enable completion-as-you-type behavior.
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
)

(use-package string-inflection
  :ensure t
)

(use-package yafolding
  :ensure t
  :bind (
    ("C-c RET" . yafolding-toggle-element)
  )
)

(use-package fzf
  :ensure t
  :bind (
    ("C-x f" . fzf-git)
  )
)

(use-package projectile
  :ensure t
  :config
    (projectile-mode +1)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuring Built-in packages
(setq js-indent-level 2)

;; Custom Functions
(defun copy-region-to-clipboard-mac ()
  "Copies the selected region to system clipboard"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy"))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Moving backup files out of working directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq custom-file "~/.emacs.d/garbage.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  KEYBINDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-h") 'backward-kill-word)
;; TODO: somehow only bind this on Mac OSX
(global-set-key (kbd "C-c C-c") 'copy-region-to-clipboard-mac)
(global-set-key (kbd "C-x C-j") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)
(global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-e") 'eval-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display line numbers
(global-linum-mode)
(setq linum-format "%d ")

;; disable dumb stuff
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Type 'y' for yes and 'n' for no
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default c-basic-offset 2)
;; Whitespace
(setq-default show-trailing-whitespace t)

;; Loading my custom theme, and telling Emacs it's safe.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0060308491490f94b8ce88b1c61a3a4d0ca205b7ee83ea3ac872d54045b135ed" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'fuck)
