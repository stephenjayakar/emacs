(setq debug-on-error 't)

;; Fixing path to mirror the one of my zsh
;; (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))
(exec-path-from-shell-initialize)

;; setting emacs garbage collection threshold to a modern device level
(setq gc-cons-threshold 100000000)

;; package setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :ensure t
  :config (
    add-hook 'web-mode-hook 'prettier-js-mode)
)

(use-package yasnippet
  :ensure t
)

(use-package yasnippet-snippets
  :ensure t
)

(use-package prettier-js
  :ensure t
)

(use-package go-mode
  :ensure t
  :init
    (setq gofmt-command '"goimports")
  :config (
    add-hook 'before-save-hook 'gofmt-before-save
  )
)

(use-package web-mode
  :ensure t
  :custom
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
  :config
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
)

(use-package rust-mode
  :ensure t
)

(use-package protobuf-mode
  :ensure t
)

(use-package elixir-mode
  :ensure t
)

(use-package yaml-mode
  :ensure t
)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
    (go-mode . lsp-deferred)
    (web-mode . lsp-deferred)
    (rust-mode . lsp-deferred)
    (yaml-mode . lsp-deferred)
  :config (
    setq lsp-rust-server 'rust-analyzer
         lsp-auto-guess-root nil
  )
  :bind (:map lsp-mode-map
    ("C-c t" . lsp-find-definition)
	("C-x a" . completion-at-point)
  )
)

;; (use-package company
;;   :ensure t
;;   :defer 2
;;   :diminish
;;   :custom
;; 	(company-begin-commands '(self-insert-command))
;; 	(company-idle-delay .1)
;; 	(company-minimum-prefix-length 2)
;; 	(company-show-numbers t)
;; 	(company-tooltip-align-annotations 't)
;; )

(use-package dap-mode
  :ensure t
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

(use-package switch-window
  :ensure t
  :bind (
    ("C-x o" . switch-window)
  )
  :config
	(setq switch-window-multiple-frames t)
	(setq switch-window-shortcut-style 'qwerty)
	(setq switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "r" "u" "i" "o" "q" "t" "y" "p"))
	(setq switch-window-increase 3)
	(setq switch-window-threshold 3)
)

(use-package ace-jump-mode
  :ensure t
  :bind (
    ("C-c SPC" . ace-jump-word-mode)
    ("C-c C-x SPC" . ace-jump-char-mode)
  )
)

(use-package restclient
  :ensure t
  :config
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
)

(use-package magit
  :ensure t
)

(use-package projectile
  :ensure t
  :config
    (projectile-mode +1)
  :bind-keymap
    ("C-c p" . projectile-command-map)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Configuring Built-in packages
(setq js-indent-level 2)
(add-hook 'python-mode-hook 'lsp)

;; Custom Functions
(defun copy-region-to-clipboard-mac ()
  "Copies the selected region to system clipboard"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy"))

;; Moving backup files out of working directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq custom-file "~/.emacs.d/garbage.el")

(delete-selection-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  KEYBINDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-h") 'backward-kill-word)
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
(scroll-bar-mode -1)

;; yas
(yas-global-mode)

;; Type 'y' for yes and 'n' for no
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default c-basic-offset 2)
;; Whitespace
(setq-default show-trailing-whitespace t)

;; Radon's magical jira command
(setq bug-reference-bug-regexp "\\(\\)\\([A-Z]+-[0-9]+\\)")
(setq bug-reference-url-format "https://jira.plaid.com/browse/%s")
(define-globalized-minor-mode bug-reference-global-mode
  bug-reference-mode bug-reference-mode)
(bug-reference-global-mode +1)

;; font and telling emacs my theme is safe and loading it
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5e1f1e8effb6454f616a35fabcdaaa2438c2f506ac67d96a7811b529d70de7d3" default))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
; (custom-set-face
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fira Code")))))
(load-theme 'deeper-blue)
