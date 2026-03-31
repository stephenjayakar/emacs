;; Custom keybinds added interactively.
(require 'bind-key)

(defconst my/keybinds-file (expand-file-name "~/.emacs.d/keybinds.el")
  "Path to the persisted custom keybindings file.")

(defun my/bind-key--append-entry (entry-text)
  "Append ENTRY-TEXT to `my/keybinds-file'.
If the file is already visiting a buffer, update that buffer and save it."
  (let ((buffer (find-buffer-visiting my/keybinds-file)))
    (if buffer
        (with-current-buffer buffer
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert entry-text)
          (save-buffer))
      (append-to-file entry-text nil my/keybinds-file))))

(defun my/bind-key (key-seq command-sym)
  "Bind KEY-SEQ to COMMAND-SYM now and persist it in `keybinds.el'."
  (interactive
   (let* ((raw-key (read-key-sequence-vector "Press key sequence to bind: "))
          (key-str (key-description raw-key))
          (cmd (read-command (format "Command to bind to '%s': " key-str))))
     (list key-str cmd)))
  (unless (commandp command-sym)
    (error "Not an interactive command: %s" command-sym))
  (unless (file-exists-p my/keybinds-file)
    (write-region ";; Custom keybinds added interactively.\n" nil my/keybinds-file))
  (let ((entry-text (format ";; Added on %s\n(bind-key %S #'%s)\n"
                            (format-time-string "%Y-%m-%d %H:%M")
                            key-seq
                            command-sym)))
    (my/bind-key--append-entry entry-text)
    ;; Apply immediately so the new binding is live in the active session.
    (bind-key key-seq command-sym)
    (message "Bound %s to %s and saved it to %s"
             key-seq
             command-sym
             my/keybinds-file)))

(defalias 'i-bind-key #'my/bind-key)

;; Added on 2025-11-07 10:38
(bind-key "s-m" 'gptel-menu)

;; Added on 2025-11-07 17:18
(bind-key "s-r" 'restart-emacs)

;; Added on 2025-11-12 16:50
(bind-key "s-g" 'gptel)
