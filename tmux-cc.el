;;; tmux-cc.el --- Compatibility loader for submodule package -*- lexical-binding: t; -*-

;;; Commentary:

;; The actual tmux control mode package lives in the
;; site-lisp/emacs-tmux-control-mode submodule. This file remains as a thin
;; compatibility loader for any local code that still loads ~/.emacs.d/tmux-cc.el
;; directly.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "site-lisp/emacs-tmux-control-mode" user-emacs-directory))
(require 'tmux-cc)

;;; tmux-cc.el ends here
