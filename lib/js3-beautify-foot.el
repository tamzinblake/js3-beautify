;;; js3-beautify-foot.el

(eval-when-compile
  (require 'cl))

(defun js3-beautify ()
  "Beautify JavaScript code in the current buffer."
  (interactive)
  (js3-beautify-check-compat)
  (set-syntax-table js3-beautify-syntax-table)
  (use-local-map js3-beautify-map)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq local-abbrev-table js3-beautify-abbrev-table)
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'js3-beautify-indent-line)
  (set (make-local-variable 'indent-tabs-mode) js3-beautify-indent-tabs-mode)

  (set (make-local-variable 'before-save-hook) #'js3-beautify-before-save)
  (set (make-local-variable 'next-error-function) #'js3-beautify-next-error)
  (set (make-local-variable 'beginning-of-defun-function) #'js3-beautify-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'js3-beautify-end-of-defun)
  ;; We un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; this is necessary to make `show-paren-function' work properly
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; needed for M-x rgrep, among other things
  (put 'js3-beautify 'find-tag-default-function #'js3-beautify-find-tag)

  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-buffer-is-cc-mode t
        c-comment-prefix-regexp js3-beautify-comment-prefix-regexp
        c-comment-start-regexp "/[*/]\\|\\s|"
        c-paragraph-start js3-beautify-paragraph-start
        c-paragraph-separate "$"
        comment-start-skip js3-beautify-comment-start-skip
        c-syntactic-ws-start js3-beautify-syntactic-ws-start
        c-syntactic-ws-end js3-beautify-syntactic-ws-end
        c-syntactic-eol js3-beautify-syntactic-eol)
  (if js3-beautify-emacs22
      (c-setup-paragraph-variables))

  (set (make-local-variable 'line-move-ignore-invisible) t)
  (set (make-local-variable 'forward-sexp-function) #'js3-beautify-forward-sexp)
  (setq js3-beautify-buffer-dirty-p t
        js3-beautify-parsing nil)
  (js3-beautify-reparse)
  (js3-beautify-pretty-print)
  (js3-beautify-exit))

(defun js3-beautify-check-compat ()
  "Signal an error if we can't run with this version of Emacs."
  (if (and js3-beautify-must-byte-compile
           (not (byte-code-function-p (symbol-function 'js3-beautify))))
      (error "You must byte-compile js3-beautify before using it."))
  (if (and (boundp 'running-xemacs) running-xemacs)
      (error "js3-beautify is not compatible with XEmacs"))
  (unless (>= emacs-major-version 21)
    (error "js3-beautify requires GNU Emacs version 21 or higher")))

(defun js3-beautify-exit ()
  (interactive)
  (setq js3-beautify-ast nil))

(defun js3-beautify-before-save ()
  "Clean up whitespace before saving file.
You can disable this by customizing `js3-beautify-cleanup-whitespace'."
  (when js3-beautify-cleanup-whitespace
    (let ((col (current-column)))
      (delete-trailing-whitespace)
      ;; don't change trailing whitespace on current line
      (unless (eq (current-column) col)
        (indent-to col)))))

(defsubst js3-beautify-reset-timer ()
  (if js3-beautify-parse-timer
      (cancel-timer js3-beautify-parse-timer))
  (setq js3-beautify-parsing nil)
  (setq js3-beautify-parse-timer
        (run-with-idle-timer js3-beautify-idle-timer-delay nil #'js3-beautify-reparse)))

(defun js3-beautify-reparse (&optional force)
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it.  If FORCE is nil, then the
buffer will only rebuild its `js3-beautify-ast' if the buffer is dirty."
  (let (time
        interrupted-p
        (js3-beautify-compiler-strict-mode js3-beautify-show-strict-warnings))
    (unless js3-beautify-parsing
      (setq js3-beautify-parsing t)
      (unwind-protect
          (when (or js3-beautify-buffer-dirty-p force)
            (js3-beautify-with-unmodifying-text-property-changes
             (setq js3-beautify-buffer-dirty-p nil)
             (if js3-beautify-verbose-parse-p
                 (message "parsing..."))
             (setq time
                   (js3-beautify-time
                    (setq interrupted-p
                          (catch 'interrupted
                            (setq js3-beautify-ast (js3-beautify-parse))
			    ;; if parsing is interrupted, comments and regex
			    ;; literals stay ignored by `parse-partial-sexp'
                            nil))))
             (if interrupted-p
                 (progn
                   ;; unfinished parse => try again
                   (setq js3-beautify-buffer-dirty-p t)
                   (js3-beautify-reset-timer))
               (if js3-beautify-verbose-parse-p
                   (message "Parse time: %s" time)))))
        ;; finally
        (setq js3-beautify-parsing nil)
        (unless interrupted-p
          (setq js3-beautify-parse-timer nil))))))

(defun js3-beautify-remove-suppressed-warnings ()
  "Take suppressed warnings out of the AST warnings list.
This ensures that the counts and `next-error' are correct."
  (setf (js3-beautify-ast-root-warnings js3-beautify-ast)
        (js3-beautify-delete-if
         (lambda (e)
           (let ((key (caar e)))
             (or
              (and (not js3-beautify-strict-trailing-comma-warning)
                   (string-match "trailing\\.comma" key))
              (and (not js3-beautify-strict-cond-assign-warning)
                   (string= key "msg.equal.as.assign"))
              (and js3-beautify-missing-semi-one-line-override
                   (string= key "msg.missing.semi")
                   (let* ((beg (second e))
                          (node (js3-beautify-node-at-point beg))
                          (fn (js3-beautify-find-parent-fn node))
                          (body (and fn (js3-beautify-function-node-body fn)))
                          (lc (and body (js3-beautify-node-abs-pos body)))
                          (rc (and lc (+ lc (js3-beautify-node-len body)))))
                     (and fn
                          (or (null body)
                              (save-excursion
                                (goto-char beg)
                                (and (js3-beautify-same-line lc)
                                     (js3-beautify-same-line rc))))))))))
         (js3-beautify-ast-root-warnings js3-beautify-ast))))

(defun js3-beautify-echo-error (old-point new-point)
  "Called by point-motion hooks."
  (let ((msg (get-text-property new-point 'help-echo)))
    (if msg
        (message msg))))

(defalias #'js3-beautify-echo-help #'js3-beautify-echo-error)

(defun js3-beautify-beginning-of-line ()
  "Toggles point between bol and first non-whitespace char in line.
Also moves past comment delimiters when inside comments."
  (interactive)
  (let (node beg)
    (cond
     ((bolp)
      (back-to-indentation))
     ((looking-at "//")
      (skip-chars-forward "/ \t"))
     ((and (eq (char-after) ?*)
           (setq node (js3-beautify-comment-at-point))
           (memq (js3-beautify-comment-node-format node) '(jsdoc block))
           (save-excursion
             (skip-chars-backward " \t")
             (bolp)))
      (skip-chars-forward "\* \t"))
     (t
      (goto-char (point-at-bol))))))

(defun js3-beautify-end-of-line ()
  "Toggles point between eol and last non-whitespace char in line."
  (interactive)
  (if (eolp)
      (skip-chars-backward " \t")
    (goto-char (point-at-eol))))

(defsubst js3-beautify-inside-string ()
  "Return non-nil if inside a string.
Actually returns the quote character that begins the string."
  (let ((parse-state (save-excursion
                       (parse-partial-sexp (point-min) (point)))))
    (nth 3 parse-state)))

(defsubst js3-beautify-inside-comment-or-string ()
  "Return non-nil if inside a comment or string."
  (or
   (let ((comment-start
          (save-excursion
            (goto-char (point-at-bol))
            (if (re-search-forward "//" (point-at-eol) t)
                (match-beginning 0)))))
     (and comment-start
          (<= comment-start (point))))
   (let ((parse-state (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
     (or (nth 3 parse-state)
         (nth 4 parse-state)))))

(defun js3-beautify-wait-for-parse (callback)
  "Invoke CALLBACK when parsing is finished.
If parsing is already finished, calls CALLBACK immediately."
  (if (not js3-beautify-buffer-dirty-p)
      (funcall callback)
    (push callback js3-beautify-pending-parse-callbacks)
    (add-hook 'js3-beautify-parse-finished-hook #'js3-beautify-parse-finished)))

(defun js3-beautify-parse-finished ()
  "Invoke callbacks in `js3-beautify-pending-parse-callbacks'."
  ;; We can't let errors propagate up, since it prevents the
  ;; `js3-beautify-parse' method from completing normally and returning
  ;; the ast, which makes things mysteriously not work right.
  (unwind-protect
      (dolist (cb js3-beautify-pending-parse-callbacks)
        (condition-case err
            (funcall cb)
          (error (message "%s" err))))
    (setq js3-beautify-pending-parse-callbacks nil)))

(defun js3-beautify-function-at-point (&optional pos)
  "Return the innermost function node enclosing current point.
Returns nil if point is not in a function."
  (let ((node (js3-beautify-node-at-point pos)))
    (while (and node (not (js3-beautify-function-node-p node)))
      (setq node (js3-beautify-node-parent node)))
    (if (js3-beautify-function-node-p node)
        node)))

(defun js3-beautify-customize ()
  (interactive)
  (customize-group 'js3-beautify))

(defun js3-beautify-forward-sexp (&optional arg)
  "Move forward across one statement or balanced expression.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (setq arg (or arg 1))
  (if js3-beautify-buffer-dirty-p
      (js3-beautify-wait-for-parse #'js3-beautify-forward-sexp))
  (let (node end (start (point)))
    (cond
     ;; backward-sexp
     ;; could probably make this "better" for some cases:
     ;;  - if in statement block (e.g. function body), go to parent
     ;;  - infix exprs like (foo in bar) - maybe go to beginning
     ;;    of infix expr if in the right-side expression?
     ((and arg (minusp arg))
      (dotimes (i (- arg))
        (js3-beautify-backward-sws)
        (forward-char -1)  ; enter the node we backed up to
        (setq node (js3-beautify-node-at-point (point) t))
        (goto-char (if node
                       (js3-beautify-node-abs-pos node)
                     (point-min)))))
     (t
      ;; forward-sexp
      (js3-beautify-forward-sws)
      (dotimes (i arg)
        (js3-beautify-forward-sws)
        (setq node (js3-beautify-node-at-point (point) t)
              end (if node (+ (js3-beautify-node-abs-pos node)
                              (js3-beautify-node-len node))))
        (goto-char (or end (point-max))))))))

(defun js3-beautify-find-tag ()
  "Replacement for `find-tag-default'.
`find-tag-default' returns a ridiculous answer inside comments."
  (let (beg end)
    (js3-beautify-with-underscore-as-word-syntax
     (save-excursion
       (if (and (not (looking-at "[A-Za-z0-9_$]"))
                (looking-back "[A-Za-z0-9_$]"))
           (setq beg (progn (forward-word -1) (point))
                 end (progn (forward-word 1) (point)))
         (setq beg (progn (forward-word 1) (point))
               end (progn (forward-word -1) (point))))
       (replace-regexp-in-string
        "[\"']" ""
        (buffer-substring-no-properties beg end))))))

(defun js3-beautify-forward-sibling ()
  "Move to the end of the sibling following point in parent.
Returns non-nil if successful, or nil if there was no following sibling."
  (let* ((node (js3-beautify-node-at-point))
         (parent (js3-beautify-find-enclosing-fn node))
         sib)
    (when (setq sib (js3-beautify-node-find-child-after (point) parent))
      (goto-char (+ (js3-beautify-node-abs-pos sib)
                    (js3-beautify-node-len sib))))))

(defun js3-beautify-backward-sibling ()
  "Move to the beginning of the sibling node preceding point in parent.
Parent is defined as the enclosing script or function."
  (let* ((node (js3-beautify-node-at-point))
         (parent (js3-beautify-find-enclosing-fn node))
         sib)
    (when (setq sib (js3-beautify-node-find-child-before (point) parent))
      (goto-char (js3-beautify-node-abs-pos sib)))))

(defun js3-beautify-beginning-of-defun ()
  "Go to line on which current function starts, and return non-nil.
If we're not in a function, go to beginning of previous script-level element."
  (interactive)
  (let ((parent (js3-beautify-node-parent-script-or-fn (js3-beautify-node-at-point)))
        pos sib)
    (cond
     ((and (js3-beautify-function-node-p parent)
           (not (eq (point) (setq pos (js3-beautify-node-abs-pos parent)))))
      (goto-char pos))
     (t
      (js3-beautify-backward-sibling)))))

(defun js3-beautify-end-of-defun ()
  "Go to the char after the last position of the current function.
If we're not in a function, skips over the next script-level element."
  (interactive)
  (let ((parent (js3-beautify-node-parent-script-or-fn (js3-beautify-node-at-point))))
    (if (not (js3-beautify-function-node-p parent))
        ;; punt:  skip over next script-level element beyond point
        (js3-beautify-forward-sibling)
      (goto-char (+ 1 (+ (js3-beautify-node-abs-pos parent)
                         (js3-beautify-node-len parent)))))))

(defun js3-beautify-mark-defun (&optional allow-extend)
  "Put mark at end of this function, point at beginning.
The function marked is the one that contains point.

Interactively, if this command is repeated,
or (in Transient Mark mode) if the mark is active,
it marks the next defun after the ones already marked."
  (interactive "p")
  (let (extended)
    (when (and allow-extend
               (or (and (eq last-command this-command) (mark t))
                   (and transient-mark-mode mark-active)))
      (let ((sib (save-excursion
                   (goto-char (mark))
                   (if (js3-beautify-forward-sibling)
                       (point))))
            node)
        (if sib
            (progn
              (set-mark sib)
              (setq extended t))
          ;; no more siblings - try extending to enclosing node
          (goto-char (mark t)))))
    (when (not extended)
      (let ((node (js3-beautify-node-at-point (point) t)) ; skip comments
            ast fn stmt parent beg end)
        (when (js3-beautify-ast-root-p node)
          (setq ast node
                node (or (js3-beautify-node-find-child-after (point) node)
                         (js3-beautify-node-find-child-before (point) node))))
        ;; only mark whole buffer if we can't find any children
        (if (null node)
            (setq node ast))
        (if (js3-beautify-function-node-p node)
            (setq parent node)
          (setq fn (js3-beautify-find-enclosing-fn node)
                stmt (if (or (null fn)
                             (js3-beautify-ast-root-p fn))
                         (js3-beautify-find-first-stmt node))
                parent (or stmt fn)))
        (setq beg (js3-beautify-node-abs-pos parent)
              end (+ beg (js3-beautify-node-len parent)))
        (push-mark beg)
        (goto-char end)
        (exchange-point-and-mark)))))

(defun js3-beautify-narrow-to-defun ()
  "Narrow to the function enclosing point."
  (interactive)
  (let* ((node (js3-beautify-node-at-point (point) t))  ; skip comments
         (fn (if (js3-beautify-script-node-p node)
                 node
               (js3-beautify-find-enclosing-fn node)))
         (beg (js3-beautify-node-abs-pos fn)))
    (unless (js3-beautify-ast-root-p fn)
      (narrow-to-region beg (+ beg (js3-beautify-node-len fn))))))

(defalias 'js3r 'js3-beautify-reset)

(provide 'js3-beautify)

;;; js3-beautify-foot.el ends here

;;; js3-beautify.el ends here
