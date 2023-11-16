;;; stephen.el --- Stephen's emacs config

(require 'package)
;;; Code:

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             t)
(package-initialize)

(setq debug-on-error 't)

;; (exec-path-from-shell-initialize)

;; setting emacs garbage collection threshold to a modern device level
(setq gc-cons-threshold 100000000)

(load-file "~/.emacs.d/tiling.el")

(setq custom-file "~/.emacs.d/custom.el")

(require 'bind-key)

;; markdown drag and drop config
(load-file "~/.emacs.d/markdown-dnd-images.el")
(setq dnd-save-directory "images")
(setq dnd-view-inline t)
(setq dnd-save-buffer-name nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  PRIVATE KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-api-key-from-file (filepath)
  "Read the content of FILEPATH and return it as a string.  If the file does not exist, return an empty string."
  (if (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents-literally filepath)
        (buffer-substring-no-properties (point-min)
                                        (point-max)))
    ""))

(defvar openai-api-key (read-api-key-from-file "~/.emacs.d/secret/openai-key"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package adaptive-wrap :ensure t)

(use-package exec-path-from-shell :ensure t)

(use-package yasnippet :ensure t)

(use-package yasnippet-snippets :ensure t)

(use-package go-mode
  :ensure t
  :init (setq gofmt-command '"goimports"):config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (define-key go-mode-map (kbd "C-c C-d") nil))

(use-package handlebars-mode :ensure t)

(use-package glsl-mode :ensure t)

(use-package docker-compose-mode :ensure t)

(use-package terraform-mode :ensure t)

(use-package add-node-modules-path :ensure t)

(use-package prettier-js
  :ensure t
  :hook
  (typescript-ts-base-mode . prettier-js-mode)
  (typescript-ts-mode . prettier-js-mode))

(use-package tide :ensure t)

(use-package rust-mode :ensure t)

(use-package protobuf-mode
  :ensure t
  :config
  (add-hook 'protobuf-mode-hook
    (lambda ()
      (local-set-key (kbd "C-c n") 'proto-renumber))))

(use-package elixir-mode :ensure t)

(use-package jenkinsfile-mode :ensure t)

(use-package git-link
  :ensure t
  :config (setq git-link-default-branch "master"):bind
  ("C-c g l" . git-link))

(use-package yaml-mode :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred):hook
  (go-mode . lsp-deferred)
  ;; (web-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  :config (setq lsp-rust-server 'rust-analyzer lsp-auto-guess-root
                nil lsp-ui-doc-enable nil lsp-completion-mode
                t):bind
  (:map lsp-mode-map
        ("C-c t" . lsp-find-definition)
        ("C-c C-t" . lsp-ui-peek-find-implementation)
        ("C-x C-a" . lsp-execute-code-action)))

(use-package company :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package dap-mode :ensure t)

(use-package lsp-ui :ensure t
  :commands lsp-ui-mode)

(use-package counsel :ensure t)

(use-package selectrum
  :ensure t
  :config (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config (selectrum-prescient-mode +1))

(use-package undo-tree
  :ensure t
  :bind (("C-x u" . undo-tree-undo)):config
  (global-undo-tree-mode))

(use-package restart-emacs :ensure t)

(use-package flycheck
  :ensure t
  :config (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package string-inflection :ensure t)

(use-package yafolding
  :ensure t
  :bind (("C-c RET" . yafolding-toggle-element)))

(use-package fzf
  :ensure t
  :bind (("C-x f" . fzf-git)))

(use-package material-theme
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c C-x SPC" . ace-jump-char-mode)))

(use-package restclient
  :ensure t
  :config (add-to-list 'auto-mode-alist
                       '("\\.http\\'" . restclient-mode)))

(use-package magit
  :ensure t
  :bind ("C-c g m" lambda
         ()
         (interactive)
         (magit-find-file "master"
                          (magit-file-relative-name))))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (projectile-mode +1):bind-keymap
  ("C-c p" . projectile-command-map))

(use-package gruvbox-theme :ensure t)

(use-package srefactor
  :ensure t
  :config (require 'srefactor-lisp))

(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key openai-api-key)
  (setq-default gptel-model "gpt-4-1106-preview"))

(use-package visual-fill-column
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs 29 -- Set up tsx-ts-mode
(add-to-list 'auto-mode-alist
             '("\\.ts\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist
             '("\\.tsx\\'" . tsx-ts-mode))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Configuring Built-in packages
(setq js-indent-level 2)
(add-hook 'python-mode-hook 'lsp)
;; (define-key python-mode-map (kbd "C-c C-d") nil)

;; Custom Functions
(defun copy-region-to-clipboard-mac ()
  "Copies the selected region to system clipboard."
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end)
                           "pbcopy"))

;; Moving backup files out of working directory
(setq backup-directory-alist '((".*" . "~/.Trash")))

;; Search term: temporary files
;; Disabling creating lockfiles
(setq create-lockfiles nil)
;; Moving the backup files
(defvar user-temporary-file-directory (concat temporary-file-directory user-login-name
                                              "/"))
(make-directory user-temporary-file-directory
                t)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,user-temporary-file-directory)
                               (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t)))

(delete-selection-mode t)

;; prettier config
;; (eval-after-load 'web-mode
;;   '(progn
;;      (add-hook 'web-mode-hook #'add-node-modules-path)))

;;; Advice
;; I'm not really sure what advice is
;; https://www.reddit.com/r/emacs/comments/rlli0u/whats_your_favorite_defadvice/
(defadvice kill-ring-save
    (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active
                   (list (region-beginning)
                         (region-end))
                 (message "Single line killed")
                 (list (line-beginning-position)
                       (line-beginning-position 2)))))

(defadvice kill-region
    (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive (if mark-active
                   (list (region-beginning)
                         (region-end))
                 (list (line-beginning-position)
                       (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  KEYBINDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key (kbd "C-<tab>")
          'next-window-any-frame)
(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "C-<tab>") nil))
(bind-key (kbd "C-S-<tab>")
                'previous-window-any-frame)
(bind-key (kbd "C-x o")
                nil)

;; Others
(global-set-key (kbd "C-j")
                'newline-and-indent)
(global-set-key (kbd "M-h")
                'backward-kill-word)
(global-set-key (kbd "C-x C-j")
                'previous-buffer)
(global-set-key (kbd "C-x C-l")
                'next-buffer)
(global-set-key (kbd "C-x l")
                'goto-line)
(global-set-key (kbd "C-x C-r")
                'revert-buffer)
(global-set-key (kbd "C-x C-e")
                'eval-last-sexp)
(global-set-key (kbd "C-c r")
                'replace-string)
(global-set-key (kbd "C-c d")
                (lambda ()
                  (interactive)
                  (point-to-register 'f)))
(global-set-key (kbd "C-c C-d")
                (lambda ()
                  (interactive)
                  (jump-to-register 'f)))
(global-set-key (kbd "C-x M-s")
                (subword-mode))

;; Tab management
(global-set-key (kbd "C-M-<tab>")
                'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-M-S-<tab>")
                'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}")
                'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-{")
                'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-t")
                'tab-bar-new-tab)
(global-set-key (kbd "s-w")
                'tab-bar-close-tab)
(global-set-key (kbd "C-\\")
                'tiling-cycle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display line numbers
(global-display-line-numbers-mode 1)

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

;; MARKDOWN mode configuration -- I want this to look similar to Notion
(setq-default markdown-fontify-code-blocks-natively
              t)

(defun clear-whitespace-and-newline-and-indent ()
  (interactive)
  (markdown-enter-key)
  (previous-line)
  (end-of-line)
  (delete-horizontal-space)
  (next-line))

(defun proto-add-rpc (name)
  "Create a template for adding gRPC services and messages to .proto files"
  (interactive "sEnter the function name: ")
  (insert
   (format "%s %s(%sRequest) returns (%sResponse);\n"
           "rpc"
           name
           name
           name))
  (goto-char (point-max))
  (newline)
  (insert
   (format "message %sRequest {}\n" name))
  (insert
   (format "message %sResponse {}\n" name)))

(defun proto-renumber ()
  "Renumber selected protobuf fields in ascending order."
  (interactive)
  (if (use-region-p)
      (let ((counter 1))
        (save-excursion
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (re-search-forward "= \\([0-9]+\\);" nil t)
            (replace-match (format "= %d;" counter) nil nil)
            (setq counter (1+ counter))))
        (widen))
    (message "You must select a region first!")))

;; Defer custom loading if there's a daemon -- otherwise just load it.
(defun load-custom (frame)
  (select-frame frame)
  (load-file "~/.emacs.d/custom.el"))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-custom)
  (load-file "~/.emacs.d/custom.el"))

;; TODO: Figure out if I can move the font configurations -> customize
(defun my-markdown-mode-hook ()
  "Set a specific font for `markdown-mode'."
  (face-remap-add-relative 'default :family "Helvetica Neue"
                           :height 200)
  (visual-line-mode 1)
  (visual-fill-column-mode)
  (setq fill-column 100)
  (setq visual-fill-column-center-text t)
  (markdown-display-inline-images)
  (setq markdown-display-remote-images t)
  (setq markdown-max-image-size '(800 . 800))
  (display-line-numbers-mode 0)
  (local-set-key (kbd "RET") 'clear-whitespace-and-newline-and-indent)
  (adaptive-wrap-prefix-mode)
  (define-key markdown-mode-map (kbd "C-c C-d") nil))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(set-face-attribute 'markdown-header-face-1
                    nil :height 1.3
                    :weight 'bold)
(set-face-attribute 'markdown-header-face-2
                    nil :height 1.2
                    :weight 'bold)
(set-face-attribute 'markdown-header-face-3
                    nil :height 1.1
                    :weight 'bold)
(set-face-attribute 'markdown-header-delimiter-face
                    nil :weight 'semibold)
(set-face-attribute 'markdown-inline-code-face
                    nil
                    :box '(:line-width 1))

(provide 'stephen)

;;; stephen.el ends here
