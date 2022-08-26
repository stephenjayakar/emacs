;; package setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :ensure t
)

(use-package yasnippet
  :ensure t
)

(use-package yasnippet-snippets
  :ensure t
)

(use-package go-mode
  :ensure t
  :init
    (setq gofmt-command '"goimports")
  :config
    (add-hook 'before-save-hook 'gofmt-before-save)
    (define-key go-mode-map (kbd "C-c C-d") nil)
)

(use-package terraform-mode
  :ensure t
)

(use-package web-mode
  :ensure t
  :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
    (define-key web-mode-map (kbd "C-c C-d") nil)
)

(use-package add-node-modules-path
  :ensure t
)

(use-package prettier-js
  :ensure t
)

(use-package tide
  :ensure t
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

(use-package jenkinsfile-mode
  :ensure t
)

(use-package git-link
  :ensure t
  :config
    (setq git-link-default-branch "master")
  :bind
    ("C-c g l" . git-link)
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
         lsp-ui-doc-enable nil
         lsp-completion-mode t
  )
  :bind (:map lsp-mode-map
    ("C-c t" . lsp-find-definition)
    ("C-c C-t" . lsp-ui-peek-find-implementation)
    ("C-x C-a" . lsp-execute-code-action)
  )
)

(use-package company
  :ensure t
)

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
  :bind
    ("C-M-y" . counsel-yank-pop)
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

;; Trying out another window switching paradigm
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") 'previous-window-any-frame)
;; TODO: Undo this once you rewire muscle memory
(global-set-key (kbd "C-x o") nil)
;; (use-package switch-window
;;   :ensure t
;;   :bind (
;;     ("C-x o" . switch-window)
;;   )
;;   :config
;; 	(setq switch-window-multiple-frames t)
;; 	(setq switch-window-shortcut-style 'qwerty)
;; 	(setq switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "r" "u" "i" "o" "q" "t" "y" "p"))
;; 	(setq switch-window-increase 3)
;; 	(setq switch-window-threshold 3)
;; )

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

(setq-default markdown-fontify-code-blocks-natively t)
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

;; prettier config
(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)))

;;; Advice
;; I'm not really sure what advice is
;; https://www.reddit.com/r/emacs/comments/rlli0u/whats_your_favorite_defadvice/
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Single line killed")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun ad-kmacro-change-modebar ()
  (set-face-attribute 'mode-line nil
                      :background "Firebrick"
                      :box `(:line-width -1 :color "salmon" :style released-button)))

(defun ad-kmacro-restore-modebar ()
  (set-face-attribute 'mode-line nil
                      :background "RoyalBlue4"
                      :box `(:line-width -1 :color "RoyalBlue1" :style released-button)))

(defadvice kmacro-start-macro (before kmacro-hl-modeline activate)
  "Alters `kmacro-start-macro' so it highlights the modeline when
  recording begins."
  (ad-kmacro-change-modebar))

(defadvice kmacro-keyboard-quit (before kmacro-rem-hl-modeline activate)
  "Alters `kmacro-keyboard-quit' so it highlights the modeline when
  recording begins."
  (ad-kmacro-restore-modebar))

(defadvice kmacro-end-macro (before kmacro-rem-hl-modeline activate)
  "Alters `kmacro-end-macro' so it highlights the modeline when
  recording begins."
  (ad-kmacro-restore-modebar))

(defadvice backward-kill-word (around delete-pair activate)
  (if (eq (char-syntax (char-before)) ?\()
      (progn
        (backward-char 1)
        (save-excursion
          (forward-sexp 1)
          (delete-char -1))
        (forward-char 1)
        (append-next-kill)
        (kill-backward-chars 1))
    ad-do-it))

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
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c d") (lambda () (interactive) (point-to-register 'f)))
(global-set-key (kbd "C-c C-d") (lambda () (interactive) (jump-to-register 'f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display line numbers
(global-linum-mode)
(setq linum-format "%d ")
(setq column-number-mode t)

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
   '("1c039d8c53227074439c425ec195a9ec725397b398afdb69ea23a5d087fec13f" "3cc2699107f09d2fd63caad608d68931669060d8512bbaab869734614076e389" "5e1f1e8effb6454f616a35fabcdaaa2438c2f506ac67d96a7811b529d70de7d3" default))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((bug-reference))))
; (custom-set-face
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fira Code")))))
(load-theme 'deeper-blue)
(set-face-attribute 'default nil :height 140)
