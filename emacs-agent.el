;;; emacs-agent-tools.el --- Tools for agentic Emacs control -*- lexical-binding: t; -*-

(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "Read the entire contents of an Emacs buffer. Returns the full text of the buffer."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to read"))
 :category "emacs")

(gptel-make-tool
 :name "read_buffer_region"
 :function (lambda (buffer start-line end-line)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (save-excursion
                 (goto-char (point-min))
                 (forward-line (1- start-line))
                 (let ((start (point)))
                   (forward-line (- end-line start-line 1))
                   (end-of-line)
                   (buffer-substring-no-properties start (point))))))
 :description "Read a specific region of a buffer by line numbers. Line numbers are 1-indexed."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to read from")
             '(:name "start_line"
               :type number
               :description "The starting line number (1-indexed)")
             '(:name "end_line"
               :type number
               :description "The ending line number (1-indexed, inclusive)"))
 :category "emacs")

(gptel-make-tool
 :name "list_buffers"
 :function (lambda ()
             (let ((buffers (mapcar #'buffer-name (buffer-list))))
               (string-join buffers "\n")))
 :description "List all open buffers in Emacs. Returns buffer names separated by newlines."
 :args nil
 :category "emacs")

(gptel-make-tool
 :name "edit_buffer"
 :function (lambda (buffer old-text new-text)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (save-excursion
                 (goto-char (point-min))
                 (let ((count 0)
                       (case-fold-search nil))
                   (while (search-forward old-text nil t)
                     (setq count (1+ count)))
                   (cond
                    ((= count 0)
                     (error "Text not found in buffer"))
                    ((> count 1)
                     (error "Text appears %d times in buffer" count))
                    (t
                     (goto-char (point-min))
                     (search-forward old-text)
                     (replace-match new-text t t)
                     (format "Successfully replaced text in buffer %s" buffer)))))))
 :description "Replace old-text with new-text in buffer. Requires exact text matching including whitespace. Fails if text is not found or appears multiple times."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to edit")
             '(:name "old_text"
               :type string
               :description "The exact text to replace")
             '(:name "new_text"
               :type string
               :description "The replacement text"))
 :category "emacs")

(gptel-make-tool
 :name "open_file"
 :function (lambda (filepath)
             (let ((buffer (find-file-noselect (expand-file-name filepath))))
               (format "Opened file %s in buffer '%s'" filepath (buffer-name buffer))))
 :description "Open a file in Emacs. Creates a buffer for the file if not already open."
 :args (list '(:name "filepath"
               :type string
               :description "The path to the file to open"))
 :category "emacs")

(gptel-make-tool
 :name "save_buffer"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (if (buffer-file-name)
                   (progn
                     (save-buffer)
                     (format "Saved buffer %s to %s" buffer (buffer-file-name)))
                 (error "Buffer %s is not associated with a file" buffer))))
 :description "Save a buffer to its associated file on disk."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to save"))
 :category "emacs")

(gptel-make-tool
 :name "write_file"
 :function (lambda (filepath content)
             (let ((expanded-path (expand-file-name filepath)))
               (with-temp-buffer
                 (insert content)
                 (write-file expanded-path))
               (format "Wrote %d characters to %s" (length content) expanded-path)))
 :description "Write content to a file, creating it if it doesn't exist or overwriting it if it does."
 :args (list '(:name "filepath"
               :type string
               :description "The path where the file should be written")
             '(:name "content"
               :type string
               :description "The complete content to write to the file"))
 :category "emacs")

(gptel-make-tool
 :name "search_in_buffer"
 :function (lambda (buffer pattern)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (let ((results '())
                     (case-fold-search nil))
                 (save-excursion
                   (goto-char (point-min))
                   (while (re-search-forward pattern nil t)
                     (let ((line-num (line-number-at-pos))
                           (line-text (string-trim
                                      (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position)))))
                       (push (format "Line %d: %s" line-num line-text) results))))
                 (if results
                     (string-join (nreverse results) "\n")
                   "No matches found"))))
 :description "Search for a regular expression pattern in a buffer. Returns matching lines with line numbers."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to search in")
             '(:name "pattern"
               :type string
               :description "Regular expression pattern to search for"))
 :category "emacs")

(gptel-make-tool
 :name "grep_project"
 :function (lambda (pattern)
             (let* ((project (project-current))
                    (default-directory (if project
                                          (project-root project)
                                        default-directory))
                    (result (shell-command-to-string
                            (format "rg --line-number --no-heading --color never '%s' 2>&1 || true" pattern))))
               (if (string-empty-p result)
                   "No matches found"
                 result)))
 :description "Search for a pattern across all files in the current project using ripgrep."
 :args (list '(:name "pattern"
               :type string
               :description "Text or regex pattern to search for across the project"))
 :category "emacs")

(gptel-make-tool
 :name "eval_elisp"
 :function (lambda (code)
             (condition-case err
                 (let ((result (eval (read code))))
                   (format "%S" result))
               (error (format "Error: %S" err))))
 :description "Evaluate Emacs Lisp code and return the result."
 :args (list '(:name "code"
               :type string
               :description "Emacs Lisp code to evaluate as a string"))
 :category "emacs")

(gptel-make-tool
 :name "shell_command"
 :function (lambda (command)
             (let ((result (shell-command-to-string command)))
               (if (string-empty-p result)
                   "(no output)"
                 result)))
 :description "Execute a shell command and return its output."
 :args (list '(:name "command"
               :type string
               :description "Shell command to execute"))
 :category "emacs")

(gptel-make-tool
 :name "get_diagnostics"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (if (bound-and-true-p lsp-mode)
                   (let ((diagnostics (lsp-diagnostics)))
                     (if (hash-table-empty-p diagnostics)
                         "No diagnostics found"
                       (let ((results '()))
                         (maphash
                          (lambda (_file diags)
                            (dolist (diag (append diags nil))
                              (let* ((range (lsp:diagnostic-range diag))
                                     (start (lsp:range-start range))
                                     (line (1+ (lsp:position-line start)))
                                     (message (lsp:diagnostic-message diag))
                                     (severity (lsp:diagnostic-severity diag))
                                     (severity-str (pcase severity
                                                    (1 "Error")
                                                    (2 "Warning")
                                                    (3 "Info")
                                                    (4 "Hint")
                                                    (_ "Unknown"))))
                                (push (format "Line %d [%s]: %s" line severity-str message) results))))
                          diagnostics)
                         (if results
                             (string-join (nreverse results) "\n")
                           "No diagnostics found"))))
                 "LSP mode is not active in this buffer")))
 :description "Get LSP diagnostics (errors, warnings, info) for a buffer. Requires LSP mode to be active."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to get diagnostics for"))
 :category "emacs")

(gptel-make-tool
 :name "find_definition"
 :function (lambda (buffer symbol)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (if (bound-and-true-p lsp-mode)
                   (save-excursion
                     (goto-char (point-min))
                     (if (search-forward symbol nil t)
                         (condition-case err
                             (let* ((locations (lsp-request "textDocument/definition"
                                                           (lsp--text-document-position-params)))
                                    (location (if (sequencep locations)
                                                 (seq-first locations)
                                               locations)))
                               (if location
                                   (let* ((uri (lsp:location-uri location))
                                          (range (lsp:location-range location))
                                          (start (lsp:range-start range))
                                          (line (1+ (lsp:position-line start)))
                                          (file (lsp--uri-to-path uri)))
                                     (format "Found in %s at line %d" file line))
                                 "Definition not found"))
                           (error (format "Error finding definition: %S" err)))
                       (format "Symbol '%s' not found in buffer" symbol)))
                 "LSP mode is not active in this buffer")))
 :description "Find the definition of a symbol using LSP. Returns the file and line number where the symbol is defined."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer containing the symbol")
             '(:name "symbol"
               :type string
               :description "The symbol name to find the definition of"))
 :category "emacs")

(gptel-make-tool
 :name "find_references"
 :function (lambda (buffer symbol)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (if (bound-and-true-p lsp-mode)
                   (save-excursion
                     (goto-char (point-min))
                     (if (search-forward symbol nil t)
                         (condition-case err
                             (let ((locations (lsp-request "textDocument/references"
                                                          (lsp--make-reference-params))))
                               (if (and locations (> (length locations) 0))
                                   (let ((results '()))
                                     (seq-doseq (location locations)
                                       (let* ((uri (lsp:location-uri location))
                                              (range (lsp:location-range location))
                                              (start (lsp:range-start range))
                                              (line (1+ (lsp:position-line start)))
                                              (file (lsp--uri-to-path uri)))
                                         (push (format "%s:%d" file line) results)))
                                     (string-join (nreverse results) "\n"))
                                 "No references found"))
                           (error (format "Error finding references: %S" err)))
                       (format "Symbol '%s' not found in buffer" symbol)))
                 "LSP mode is not active in this buffer")))
 :description "Find all references to a symbol using LSP. Returns a list of file:line locations where the symbol is used."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer containing the symbol")
             '(:name "symbol"
               :type string
               :description "The symbol name to find references for"))
 :category "emacs")
