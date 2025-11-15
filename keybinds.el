;; Custom Keybinds Dynamically added
(defun i-bind-key (key-seq command-sym)
  "Prompts for a key sequence and a command, activates it globally now,
and appends a (bind-key) directive to ~/.emacs.d/keybinds.el for future sessions."
  (interactive
   (let* ((raw-key (read-key-sequence-vector "Press key sequence to bind: "))
          (key-str (key-description raw-key))
          (cmd (read-command (format "Command to bind to '%s': " key-str))))
     (list key-str cmd)))
  (let ((keybind-file (expand-file-name "~/.emacs.d/keybinds.el")))
    (global-set-key (kbd key-seq) command-sym)
    (unless (file-exists-p keybind-file)
      (write-region ";; Auto-generated keybinds file\n" nil keybind-file))
    (let ((entry-text (format "\n;; Added on %s\n(bind-key \"%s\" '%s)\n"
                              (format-time-string "%Y-%m-%d %H:%M")
                              key-seq
                              command-sym)))
      (append-to-file entry-text nil keybind-file)
      (message "Bound '%s' to '%s' globally and saved to %s"
               key-seq command-sym keybind-file))))

;; Added on 2025-11-07 10:38
(bind-key "s-m" 'gptel-menu)

;; Added on 2025-11-07 17:18
(bind-key "s-r" 'restart-emacs)

;; Added on 2025-11-12 16:50
(bind-key "s-g" 'gptel)
