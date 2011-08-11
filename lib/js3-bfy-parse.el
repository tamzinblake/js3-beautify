;;; js3-beautify-parse.el --- JavaScript parser

;; Commentary:

;; This is based on Rhino's parser and tries to follow its code
;; structure as closely as practical, so that changes to the Rhino
;; parser can easily be propagated into this code.  However, Rhino
;; does not currently generate a usable AST representation, at least
;; from an IDE perspective, so we build our own more suitable AST.

;; The AST node structures are defined in `js3-beautify-ast.el'.
;; Every parser function that creates and returns an AST node has
;; the following responsibilities:

;;   1) set the node start to the absolute buffer start position
;;   2) set the node length to include any closing chars (RC, SEMI)
;;   3) fix up any child-node starts to be relative to this node
;;   4) set any field positions (e.g. keywords) relative to this node
;;   5) report any child nodes with `js3-beautify-node-add-children'
;;      (note that this call fixes up start positions by default)

;; The resulting AST has all node start positions relative to the
;; parent nodes; only the root has an absolute start position.

;; Note: fontification is done inline while parsing.  It used to be
;; done in a second pass over the AST, but doing it inline is about
;; twice as fast.  Most of the fontification happens when tokens are
;; scanned, and the parser has a few spots that perform extra
;; fontification.  In addition to speed, a second benefit of inline
;; parsing is that if a long parse is interrupted, everything parsed
;; so far is still fontified.

;; The editing mode that uses this parser, `js3-beautify', directs the
;; parser to check periodically for user input.  If user input
;; arrives, the parse is abandoned, and a re-parse is rescheduled for when Emacs
;; becomes idle again.  This works pretty well, but could be better.
;; In particular, when the user input has not resulted in changes to
;; the buffer (for instance, navigation input), the parse tree built
;; so far should not be discarded, and the parse should continue where
;; it left off.  It will be some work to create what amounts to a
;; continuation, but it should not be unreasonably difficult.

;; TODO:
;; - make non-editing input restart parse at previous continuation
;; - in Eclipse, sibling nodes never overlap start/end ranges
;;   - for getters, prop name and function nodes overlap
;;   - should write a debug tree visitor to look for overlaps
;; - figure out a way not to store value in string/name nodes
;;   - needs a solution for synthetic nodes

;;; Code

(eval-and-compile
  (require 'cl))  ; for delete-if


(defconst js3-beautify-version "1.8.0"
  "Version of JavaScript supported, plus minor js3 version.")

(defsubst js3-beautify-node-end (n)
  "Computes the absolute end of node N.
Use with caution!  Assumes `js3-beautify-node-pos' is -absolute-, which
is only true until the node is added to its parent; i.e., while parsing."
  (+ (js3-beautify-node-pos n)
     (js3-beautify-node-len n)))

(defsubst js3-beautify-record-comment ()
  (push (make-js3-beautify-comment-node :len (- js3-beautify-token-end js3-beautify-token-beg)
                               :format js3-beautify-ts-comment-type)
        js3-beautify-scanned-comments))

;; This function is called depressingly often, so it should be fast.
;; Most of the time it's looking at the same token it peeked before.
(defsubst js3-beautify-peek-token ()
  "Returns the next token without consuming it.
If previous token was consumed, calls scanner to get new token.
If previous token was -not- consumed, returns it (idempotent).

This function will not return a newline (js3-beautify-EOL) - instead, it
gobbles newlines until it finds a non-newline token, and flags
that token as appearing just after a newline.

This function will also not return a js3-beautify-COMMENT.  Instead, it
records comments found in `js3-beautify-scanned-comments'.  If the token
returned by this function immediately follows a jsdoc comment,
the token is flagged as such.

Note that this function always returned the un-flagged token!
The flags, if any, are saved in `js3-beautify-current-flagged-token'."
  (if (/= js3-beautify-current-flagged-token js3-beautify-EOF) ; last token not consumed
      js3-beautify-current-token  ; most common case - return already-peeked token
    (let ((tt (js3-beautify-get-token))          ; call scanner
          saw-eol)
      ;; process comments and whitespace
      (while (or (= tt js3-beautify-EOL)
                 (= tt js3-beautify-COMMENT))
        (if (= tt js3-beautify-EOL)
            (setq saw-eol t)
          (setq saw-eol nil)
          (if js3-beautify-record-comments
              (js3-beautify-record-comment)))
        (setq tt (js3-beautify-get-token)))  ; call scanner
      (setq js3-beautify-current-token tt
            js3-beautify-current-flagged-token (if saw-eol
                                          (logior tt js3-beautify-ti-after-eol)
                                        tt))
      tt)))  ; return unflagged token

(defsubst js3-beautify-peek-flagged-token ()
  "Returns the current token along with any flags set for it."
  (js3-beautify-peek-token)
  js3-beautify-current-flagged-token)

(defsubst js3-beautify-consume-token ()
  (setq js3-beautify-current-flagged-token js3-beautify-EOF))

(defsubst js3-beautify-next-token ()
  (prog1
      (js3-beautify-peek-token)
    (js3-beautify-consume-token)))

(defsubst js3-beautify-next-flagged-token ()
  (js3-beautify-peek-token)
  (prog1 js3-beautify-current-flagged-token
    (js3-beautify-consume-token)))

(defsubst js3-beautify-match-token (match)
  "Consume and return t if next token matches MATCH, a bytecode.
Returns nil and consumes nothing if MATCH is not the next token."
  (if (/= (js3-beautify-peek-token) match)
      nil
    (js3-beautify-consume-token)
    t))

(defsubst js3-beautify-valid-prop-name-token (tt)
  (or (= tt js3-beautify-NAME)
      (and js3-beautify-allow-keywords-as-property-names
           (plusp tt)
           (aref js3-beautify-kwd-tokens tt))))

(defsubst js3-beautify-match-prop-name ()
  "Consume token and return t if next token is a valid property name.
It's valid if it's a js3-beautify-NAME, or `js3-beautify-allow-keywords-as-property-names'
is non-nil and it's a keyword token."
  (if (js3-beautify-valid-prop-name-token (js3-beautify-peek-token))
      (progn
        (js3-beautify-consume-token)
        t)
    nil))

(defsubst js3-beautify-must-match-prop-name (msg-id &optional pos len)
  (if (js3-beautify-match-prop-name)
      t
    (js3-beautify-report-error msg-id nil pos len)
    nil))

(defsubst js3-beautify-peek-token-or-eol ()
  "Return js3-beautify-EOL if the current token immediately follows a newline.
Else returns the current token.  Used in situations where we don't
consider certain token types valid if they are preceded by a newline.
One example is the postfix ++ or -- operator, which has to be on the
same line as its operand."
  (let ((tt (js3-beautify-peek-token)))
    ;; Check for last peeked token flags
    (if (js3-beautify-flag-set-p js3-beautify-current-flagged-token js3-beautify-ti-after-eol)
        js3-beautify-EOL
      tt)))

(defsubst js3-beautify-set-check-for-label ()
  (assert (= (logand js3-beautify-current-flagged-token js3-beautify-clear-ti-mask) js3-beautify-NAME))
  (js3-beautify-set-flag js3-beautify-current-flagged-token js3-beautify-ti-check-label))

(defsubst js3-beautify-must-match (token msg-id &optional pos len)
  "Match next token to token code TOKEN, or record a syntax error.
MSG-ID is the error message to report if the match fails.
Returns t on match, nil if no match."
  (if (js3-beautify-match-token token)
      t
    (js3-beautify-report-error msg-id nil pos len)
    nil))

(defsubst js3-beautify-inside-function ()
  (plusp js3-beautify-nesting-of-function))

(defsubst js3-beautify-set-requires-activation ()
  (if (js3-beautify-function-node-p js3-beautify-current-script-or-fn)
      (setf (js3-beautify-function-node-needs-activation js3-beautify-current-script-or-fn) t)))

(defsubst js3-beautify-check-activation-name (name token)
  (when (js3-beautify-inside-function)
    ;; skip language-version 1.2 check from Rhino
    (if (or (string= "arguments" name)
            (and js3-beautify-compiler-activation-names  ; only used in codegen
                 (gethash name js3-beautify-compiler-activation-names)))
        (js3-beautify-set-requires-activation))))

(defsubst js3-beautify-set-is-generator ()
  (if (js3-beautify-function-node-p js3-beautify-current-script-or-fn)
      (setf (js3-beautify-function-node-is-generator js3-beautify-current-script-or-fn) t)))

(defsubst js3-beautify-push-scope (scope)
  "Push SCOPE, a `js3-beautify-scope', onto the lexical scope chain."
  (assert (js3-beautify-scope-p scope))
  (assert (null (js3-beautify-scope-parent-scope scope)))
  (assert (neq js3-beautify-current-scope scope))
  (setf (js3-beautify-scope-parent-scope scope) js3-beautify-current-scope
        js3-beautify-current-scope scope))

(defsubst js3-beautify-pop-scope ()
  (setq js3-beautify-current-scope
        (js3-beautify-scope-parent-scope js3-beautify-current-scope)))

(defsubst js3-beautify-enter-loop (loop-node)
  (push loop-node js3-beautify-loop-set)
  (push loop-node js3-beautify-loop-and-switch-set)
  (js3-beautify-push-scope loop-node)
  ;; Tell the current labeled statement (if any) its statement,
  ;; and set the jump target of the first label to the loop.
  ;; These are used in `js3-beautify-parse-continue' to verify that the
  ;; continue target is an actual labeled loop.  (And for codegen.)
  (when js3-beautify-labeled-stmt
    (setf (js3-beautify-labeled-stmt-node-stmt js3-beautify-labeled-stmt) loop-node
          (js3-beautify-label-node-loop (car (js3-beautify-labeled-stmt-node-labels
                                     js3-beautify-labeled-stmt))) loop-node)))

(defsubst js3-beautify-exit-loop ()
  (pop js3-beautify-loop-set)
  (pop js3-beautify-loop-and-switch-set)
  (js3-beautify-pop-scope))

(defsubst js3-beautify-enter-switch (switch-node)
  (push switch-node js3-beautify-loop-and-switch-set))

(defsubst js3-beautify-exit-switch ()
  (pop js3-beautify-loop-and-switch-set))

(defun js3-beautify-parse (&optional buf cb)
  "Tells the js3 parser to parse a region of JavaScript.

BUF is a buffer or buffer name containing the code to parse.
Call `narrow-to-region' first to parse only part of the buffer.

The returned AST root node is given some additional properties:
`node-count' - total number of nodes in the AST
`buffer' - BUF.  The buffer it refers to may change or be killed,
so the value is not necessarily reliable.

An optional callback CB can be specified to report parsing
progress.  If `(functionp CB)' returns t, it will be called with
the current line number once before parsing begins, then again
each time the lexer reaches a new line number.

CB can also be a list of the form `(symbol cb ...)' to specify
multiple callbacks with different criteria.  Each symbol is a
criterion keyword, and the following element is the callback to
call

:line  - called whenever the line number changes
:token - called for each new token consumed

The list of criteria could be extended to include entering or
leaving a statement, an expression, or a function definition."
  (if (and cb (not (functionp cb)))
      (error "criteria callbacks not yet implemented"))
  (let ((inhibit-point-motion-hooks t)
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000))
        (max-specpdl-size (max max-specpdl-size 3000))
        (case-fold-search nil)
        ast)
    (or buf (setq buf (current-buffer)))
    (message nil)  ; clear any error message from previous parse
    (save-excursion
      (set-buffer buf)
      (setq js3-beautify-scanned-comments nil
            js3-beautify-parsed-errors nil
            js3-beautify-parsed-warnings nil
            js3-beautify-label-set nil)
      (js3-beautify-init-scanner)
      (setq ast (js3-beautify-with-unmodifying-text-property-changes
                 (js3-beautify-do-parse)))
      (unless js3-beautify-ts-hit-eof
        (js3-beautify-report-error "msg.got.syntax.errors" (length js3-beautify-parsed-errors)))
      (setf (js3-beautify-ast-root-errors ast) js3-beautify-parsed-errors
            (js3-beautify-ast-root-warnings ast) js3-beautify-parsed-warnings)
      (run-hooks 'js3-beautify-parse-finished-hook)
      ast)))

;; Corresponds to Rhino's Parser.parse() method.
(defun js3-beautify-do-parse ()
  "Parse current buffer starting from current point.
Scanner should be initialized."
  (let ((pos js3-beautify-ts-cursor)
        (end js3-beautify-ts-cursor)  ; in case file is empty
        root n tt)
    ;; initialize buffer-local parsing vars
    (setf root (make-js3-beautify-ast-root :buffer (buffer-name) :pos pos)
          js3-beautify-current-script-or-fn root
          js3-beautify-current-scope root
          js3-beautify-current-flagged-token js3-beautify-EOF
          js3-beautify-nesting-of-function 0
          js3-beautify-labeled-stmt nil)
    (while (/= (setq tt (js3-beautify-peek-token)) js3-beautify-EOF)
      (if (= tt js3-beautify-FUNCTION)
          (progn
            (js3-beautify-consume-token)
            (setq n (js3-beautify-parse-function (if js3-beautify-called-by-compile-function
                                            'FUNCTION_EXPRESSION
                                          'FUNCTION_STATEMENT))))
        ;; not a function - parse a statement
        (setq n (js3-beautify-parse-statement)))
      ;; add function or statement to script
      (setq end (js3-beautify-node-end n))
      (js3-beautify-block-node-push root n))
    ;; add comments to root in lexical order
    (when js3-beautify-scanned-comments
      ;; if we find a comment beyond end of normal kids, use its end
      (setq end (max end (js3-beautify-node-end (first js3-beautify-scanned-comments))))
      (dolist (comment js3-beautify-scanned-comments)
        (push comment (js3-beautify-ast-root-comments root))
        (js3-beautify-node-add-children root comment)))
    (setf (js3-beautify-node-len root) (- end pos))
    root))

(defun js3-beautify-function-parser ()
  (js3-beautify-consume-token)
  (js3-beautify-parse-function 'FUNCTION_EXPRESSION_STATEMENT))

(defun js3-beautify-parse-function-closure-body (fn-node)
  "Parse a JavaScript 1.8 function closure body."
  (let ((js3-beautify-nesting-of-function (1+ js3-beautify-nesting-of-function)))
    (if js3-beautify-ts-hit-eof
        (js3-beautify-report-error "msg.no.brace.body" nil
                          (js3-beautify-node-pos fn-node)
                          (- js3-beautify-ts-cursor (js3-beautify-node-pos fn-node)))
      (js3-beautify-node-add-children fn-node
                             (setf (js3-beautify-function-node-body fn-node)
                                   (js3-beautify-parse-expr))))))

(defun js3-beautify-parse-function-body (fn-node)
  (js3-beautify-must-match js3-beautify-LC "msg.no.brace.body"
                  (js3-beautify-node-pos fn-node)
                  (- js3-beautify-ts-cursor (js3-beautify-node-pos fn-node)))
  (let ((pos js3-beautify-token-beg)         ; LC position
        (pn (make-js3-beautify-block-node))  ; starts at LC position
        tt
        end)
    (incf js3-beautify-nesting-of-function)
    (unwind-protect
        (while (not (or (= (setq tt (js3-beautify-peek-token)) js3-beautify-ERROR)
                        (= tt js3-beautify-EOF)
                        (= tt js3-beautify-RC)))
          (js3-beautify-block-node-push pn (if (/= tt js3-beautify-FUNCTION)
                                      (js3-beautify-parse-statement)
                                    (js3-beautify-consume-token)
                                    (js3-beautify-parse-function 'FUNCTION_STATEMENT))))
      (decf js3-beautify-nesting-of-function))
    (setq end js3-beautify-token-end)  ; assume no curly and leave at current token
    (if (js3-beautify-must-match js3-beautify-RC "msg.no.brace.after.body" pos)
        (setq end js3-beautify-token-end))
    (setf (js3-beautify-node-pos pn) pos
          (js3-beautify-node-len pn) (- end pos))
    (setf (js3-beautify-function-node-body fn-node) pn)
    (js3-beautify-node-add-children fn-node pn)
    pn))

(defun js3-beautify-parse-function-params (fn-node pos)
  (if (js3-beautify-match-token js3-beautify-RP)
      (setf (js3-beautify-function-node-rp fn-node) (- js3-beautify-token-beg pos))
    (let (params len param)
      (loop for tt = (js3-beautify-peek-token)
            do
            (cond
             ;; destructuring param
             ((or (= tt js3-beautify-LB) (= tt js3-beautify-LC))
              (push (js3-beautify-parse-primary-expr) params))
             ;; simple name
             (t
              (js3-beautify-must-match js3-beautify-NAME "msg.no.parm")
              (setq param (js3-beautify-create-name-node))
              (js3-beautify-define-symbol js3-beautify-LP js3-beautify-ts-string param)
              (push param params)))
            while
            (js3-beautify-match-token js3-beautify-COMMA))
      (if (js3-beautify-must-match js3-beautify-RP "msg.no.paren.after.parms")
          (setf (js3-beautify-function-node-rp fn-node) (- js3-beautify-token-beg pos)))
      (dolist (p params)
        (js3-beautify-node-add-children fn-node p)
        (push p (js3-beautify-function-node-params fn-node))))))

(defsubst js3-beautify-check-inconsistent-return-warning (fn-node name)
  "Possibly show inconsistent-return warning.
Last token scanned is the close-curly for the function body."
  (when (and js3-beautify-show-strict-warnings
             js3-beautify-strict-inconsistent-return-warning
             (not (js3-beautify-has-consistent-return-usage
                   (js3-beautify-function-node-body fn-node))))
    ;; Have it extend from close-curly to bol or beginning of block.
    (let ((pos (save-excursion
                 (goto-char js3-beautify-token-end)
                 (max (js3-beautify-node-abs-pos (js3-beautify-function-node-body fn-node))
                      (point-at-bol))))
          (end js3-beautify-token-end))
      (if (plusp (js3-beautify-name-node-length name))
          (js3-beautify-add-strict-warning "msg.no.return.value"
                                  (js3-beautify-name-node-name name) pos end)
        (js3-beautify-add-strict-warning "msg.anon.no.return.value" nil pos end)))))

(defun js3-beautify-parse-function (function-type)
  "Function parser.  FUNCTION-TYPE is a symbol."
  (let ((pos js3-beautify-token-beg)  ; start of 'function' keyword
        name
        name-beg
        name-end
        fn-node
        lp
        (synthetic-type function-type)
        member-expr-node)
    ;; parse function name, expression, or non-name (anonymous)
    (cond
     ;; function foo(...)
     ((js3-beautify-match-token js3-beautify-NAME)
      (setq name (js3-beautify-create-name-node t)
            name-beg js3-beautify-token-beg
            name-end js3-beautify-token-end)
      (unless (js3-beautify-match-token js3-beautify-LP)
        (when js3-beautify-allow-member-expr-as-function-name
          ;; function foo.bar(...)
          (setq member-expr-node name
                name nil
                member-expr-node (js3-beautify-parse-member-expr-tail
                                  nil member-expr-node)))
        (js3-beautify-must-match js3-beautify-LP "msg.no.paren.parms")))
     ((js3-beautify-match-token js3-beautify-LP)
      nil)  ; anonymous function:  leave name as null
     (t
      ;; function random-member-expr(...)
      (when js3-beautify-allow-member-expr-as-function-name
        ;; Note that memberExpr can not start with '(' like
        ;; in function (1+2).toString(), because 'function (' already
        ;; processed as anonymous function
        (setq member-expr-node (js3-beautify-parse-member-expr)))
      (js3-beautify-must-match js3-beautify-LP "msg.no.paren.parms")))
    (if (= js3-beautify-current-token js3-beautify-LP)  ; eventually matched LP?
        (setq lp js3-beautify-token-beg))
    (if member-expr-node
	(setq synthetic-type 'FUNCTION_EXPRESSION))
    (if (and (neq synthetic-type 'FUNCTION_EXPRESSION)
             (plusp (js3-beautify-name-node-length name)))
        ;; Function statements define a symbol in the enclosing scope
        (js3-beautify-define-symbol js3-beautify-FUNCTION (js3-beautify-name-node-name name) fn-node))
    (setf fn-node (make-js3-beautify-function-node :pos pos
                                          :name name
                                          :form function-type
                                          :lp (if lp (- lp pos))))
    (if (or (js3-beautify-inside-function) (plusp js3-beautify-nesting-of-with))
        ;; 1. Nested functions are not affected by the dynamic scope flag
        ;;    as dynamic scope is already a parent of their scope.
        ;; 2. Functions defined under the with statement also immune to
        ;;    this setup, in which case dynamic scope is ignored in favor
        ;;    of the with object.
        (setf (js3-beautify-function-node-ignore-dynamic fn-node) t))
    ;; dynamically bind all the per-function variables
    (let ((js3-beautify-current-script-or-fn fn-node)
          (js3-beautify-current-scope fn-node)
          (js3-beautify-nesting-of-with 0)
          (js3-beautify-end-flags 0)
          js3-beautify-label-set
          js3-beautify-loop-set
          js3-beautify-loop-and-switch-set)
      (js3-beautify-parse-function-params fn-node pos)
      (if (and (>= js3-beautify-language-version 180)
               (/= (js3-beautify-peek-token) js3-beautify-LC))
          (js3-beautify-parse-function-closure-body fn-node)
        (js3-beautify-parse-function-body fn-node))
      (if name
          (js3-beautify-node-add-children fn-node name))
      (js3-beautify-check-inconsistent-return-warning fn-node name)
      ;; Function expressions define a name only in the body of the
      ;; function, and only if not hidden by a parameter name
      (if (and name
               (eq synthetic-type 'FUNCTION_EXPRESSION)
               (null (js3-beautify-scope-get-symbol js3-beautify-current-scope
                                           (js3-beautify-name-node-name name))))
          (js3-beautify-define-symbol js3-beautify-FUNCTION
                             (js3-beautify-name-node-name name)
                             fn-node)))
    (setf (js3-beautify-node-len fn-node) (- js3-beautify-ts-cursor pos)
          (js3-beautify-function-node-member-expr fn-node) member-expr-node) ; may be nil
    ;; Rhino doesn't do this, but we need it for finding undeclared vars.
    ;; We wait until after parsing the function to set its parent scope,
    ;; since `js3-beautify-define-symbol' needs the defining-scope check to stop
    ;; at the function boundary when checking for redeclarations.
    (setf (js3-beautify-scope-parent-scope fn-node) js3-beautify-current-scope)
    fn-node))

(defun js3-beautify-parse-statements (&optional parent)
  "Parse a statement list.  Last token consumed must be js3-beautify-LC.

PARENT can be a `js3-beautify-block-node', in which case the statements are
appended to PARENT.  Otherwise a new `js3-beautify-block-node' is created
and returned.

This function does not match the closing js3-beautify-RC: the caller
matches the RC so it can provide a suitable error message if not
matched.  This means it's up to the caller to set the length of
the node to include the closing RC.  The node start pos is set to
the absolute buffer start position, and the caller should fix it
up to be relative to the parent node.  All children of this block
node are given relative start positions and correct lengths."
  (let ((pn (or parent (make-js3-beautify-block-node)))
        tt)
    (setf (js3-beautify-node-pos pn) js3-beautify-token-beg)
    (while (and (> (setq tt (js3-beautify-peek-token)) js3-beautify-EOF)
                (/= tt js3-beautify-RC))
      (js3-beautify-block-node-push pn (js3-beautify-parse-statement)))
    pn))

(defun js3-beautify-parse-statement ()
  (let (tt pn beg end)
    ;; coarse-grained user-interrupt check - needs work
    (and js3-beautify-parse-interruptable-p
         (input-pending-p)
         (throw 'interrupted t))
    (setq pn (js3-beautify-statement-helper))
    ;; no-side-effects warning check
    (unless (js3-beautify-node-has-side-effects pn)
      (setq end (js3-beautify-node-end pn))
      (save-excursion
        (goto-char end)
        (setq beg (max (js3-beautify-node-pos pn) (point-at-bol))))
      (js3-beautify-add-strict-warning "msg.no.side.effects" nil beg end))
    pn))

;; These correspond to the switch cases in Parser.statementHelper
(defconst js3-beautify-parsers
  (let ((parsers (make-vector js3-beautify-num-tokens
                              #'js3-beautify-parse-expr-stmt)))
    (aset parsers js3-beautify-BREAK     #'js3-beautify-parse-break)
    (aset parsers js3-beautify-CONST     #'js3-beautify-parse-const-var)
    (aset parsers js3-beautify-CONTINUE  #'js3-beautify-parse-continue)
    (aset parsers js3-beautify-DEBUGGER  #'js3-beautify-parse-debugger)
    (aset parsers js3-beautify-DO        #'js3-beautify-parse-do)
    (aset parsers js3-beautify-FOR       #'js3-beautify-parse-for)
    (aset parsers js3-beautify-FUNCTION  #'js3-beautify-function-parser)
    (aset parsers js3-beautify-IF        #'js3-beautify-parse-if)
    (aset parsers js3-beautify-LC        #'js3-beautify-parse-block)
    (aset parsers js3-beautify-LET       #'js3-beautify-parse-let-stmt)
    (aset parsers js3-beautify-NAME      #'js3-beautify-parse-name-or-label)
    (aset parsers js3-beautify-RETURN    #'js3-beautify-parse-ret-yield)
    (aset parsers js3-beautify-SEMI      #'js3-beautify-parse-semi)
    (aset parsers js3-beautify-SWITCH    #'js3-beautify-parse-switch)
    (aset parsers js3-beautify-THROW     #'js3-beautify-parse-throw)
    (aset parsers js3-beautify-TRY       #'js3-beautify-parse-try)
    (aset parsers js3-beautify-VAR       #'js3-beautify-parse-const-var)
    (aset parsers js3-beautify-WHILE     #'js3-beautify-parse-while)
    (aset parsers js3-beautify-WITH      #'js3-beautify-parse-with)
    (aset parsers js3-beautify-YIELD     #'js3-beautify-parse-ret-yield)
    parsers)
  "A vector mapping token types to parser functions.")

(defsubst js3-beautify-parse-warn-missing-semi (beg end)
  (and js3-beautify-show-strict-warnings
       js3-beautify-strict-missing-semi-warning
       (js3-beautify-add-strict-warning
        "msg.missing.semi" nil
        ;; back up to beginning of statement or line
        (max beg (save-excursion
                   (goto-char end)
                   (point-at-bol)))
        end)))

(defconst js3-beautify-no-semi-insertion
  (list js3-beautify-IF
        js3-beautify-SWITCH
        js3-beautify-WHILE
        js3-beautify-DO
        js3-beautify-FOR
        js3-beautify-TRY
        js3-beautify-WITH
        js3-beautify-LC
        js3-beautify-ERROR
        js3-beautify-SEMI
        js3-beautify-FUNCTION)
  "List of tokens that don't do automatic semicolon insertion.")

(defconst js3-beautify-autoinsert-semi-and-warn
  (list js3-beautify-ERROR js3-beautify-EOF js3-beautify-RC))

(defun js3-beautify-statement-helper ()
  (let* ((tt (js3-beautify-peek-token))
         (first-tt tt)
         (beg js3-beautify-token-beg)
         (parser (if (= tt js3-beautify-ERROR)
                     #'js3-beautify-parse-semi
                   (aref js3-beautify-parsers tt)))
         pn
         tt-flagged)
    ;; If the statement is set, then it's been told its label by now.
    (and js3-beautify-labeled-stmt
         (js3-beautify-labeled-stmt-node-stmt js3-beautify-labeled-stmt)
         (setq js3-beautify-labeled-stmt nil))
    (setq pn (funcall parser))
    ;; Don't do auto semi insertion for certain statement types.
    (unless (or (memq first-tt js3-beautify-no-semi-insertion)
                (js3-beautify-labeled-stmt-node-p pn))
      (js3-beautify-auto-insert-semicolon pn))
    pn))

(defun js3-beautify-auto-insert-semicolon (pn)
  (let* ((tt-flagged (js3-beautify-peek-flagged-token))
         (tt (logand tt-flagged js3-beautify-clear-ti-mask))
         (pos (js3-beautify-node-pos pn)))
    (cond
     ((= tt js3-beautify-SEMI)
      ;; Consume ';' as a part of expression
      (js3-beautify-consume-token)
      ;; extend the node bounds to include the semicolon.
      (setf (js3-beautify-node-len pn) (- js3-beautify-token-end pos)))
     ((memq tt js3-beautify-autoinsert-semi-and-warn)
      ;; Autoinsert ;
      (js3-beautify-parse-warn-missing-semi pos (js3-beautify-node-end pn)))
     (t
      (if (js3-beautify-flag-not-set-p tt-flagged js3-beautify-ti-after-eol)
          ;; Report error if no EOL or autoinsert ';' otherwise
          (js3-beautify-report-error "msg.no.semi.stmt")
        (js3-beautify-parse-warn-missing-semi pos (js3-beautify-node-end pn)))))))

(defun js3-beautify-parse-condition ()
  "Parse a parenthesized boolean expression, e.g. in an if- or while-stmt.
The parens are discarded and the expression node is returned.
The `pos' field of the return value is set to an absolute position
that must be fixed up by the caller.
Return value is a list (EXPR LP RP), with absolute paren positions."
  (let (pn lp rp)
    (if (js3-beautify-must-match js3-beautify-LP "msg.no.paren.cond")
        (setq lp js3-beautify-token-beg))
    (setq pn (js3-beautify-parse-expr))
    (if (js3-beautify-must-match js3-beautify-RP "msg.no.paren.after.cond")
        (setq rp js3-beautify-token-beg))
    ;; Report strict warning on code like "if (a = 7) ..."
    (if (and js3-beautify-strict-cond-assign-warning
             (js3-beautify-assign-node-p pn))
        (js3-beautify-add-strict-warning "msg.equal.as.assign" nil
                                (js3-beautify-node-pos pn)
                                (+ (js3-beautify-node-pos pn)
                                   (js3-beautify-node-len pn))))
    (list pn lp rp)))

(defun js3-beautify-parse-if ()
  "Parser for if-statement.  Last matched token must be js3-beautify-IF."
  (let ((pos js3-beautify-token-beg)
        cond
        if-true
        if-false
        else-pos
        end
        pn)
    (js3-beautify-consume-token)
    (setq cond (js3-beautify-parse-condition)
          if-true (js3-beautify-parse-statement)
          if-false (if (js3-beautify-match-token js3-beautify-ELSE)
                       (progn
                         (setq else-pos (- js3-beautify-token-beg pos))
                         (js3-beautify-parse-statement)))
          end (js3-beautify-node-end (or if-false if-true))
          pn (make-js3-beautify-if-node :pos pos
                               :len (- end pos)
                               :condition (car cond)
                               :then-part if-true
                               :else-part if-false
                               :else-pos else-pos
                               :lp (js3-beautify-relpos (second cond) pos)
                               :rp (js3-beautify-relpos (third cond) pos)))
    (js3-beautify-node-add-children pn (car cond) if-true if-false)
    pn))

(defun js3-beautify-parse-switch ()
  "Parser for if-statement.  Last matched token must be js3-beautify-SWITCH."
  (let ((pos js3-beautify-token-beg)
        tt
        pn
        discriminant
        has-default
        case-expr
        case-node
        case-pos
        cases
        stmt
        lp
        rp)
    (js3-beautify-consume-token)
    (if (js3-beautify-must-match js3-beautify-LP "msg.no.paren.switch")
        (setq lp js3-beautify-token-beg))
    (setq discriminant (js3-beautify-parse-expr)
          pn (make-js3-beautify-switch-node :discriminant discriminant
                                   :pos pos
                                   :lp (js3-beautify-relpos lp pos)))
    (js3-beautify-node-add-children pn discriminant)
    (js3-beautify-enter-switch pn)
    (unwind-protect
        (progn
          (if (js3-beautify-must-match js3-beautify-RP "msg.no.paren.after.switch")
              (setf (js3-beautify-switch-node-rp pn) (- js3-beautify-token-beg pos)))
          (js3-beautify-must-match js3-beautify-LC "msg.no.brace.switch")
          (catch 'break
            (while t
              (setq tt (js3-beautify-next-token)
                    case-pos js3-beautify-token-beg)
              (cond
               ((= tt js3-beautify-RC)
                (setf (js3-beautify-node-len pn) (- js3-beautify-token-end pos))
                (throw 'break nil))  ; done
               ((= tt js3-beautify-CASE)
                (setq case-expr (js3-beautify-parse-expr))
                (js3-beautify-must-match js3-beautify-COLON "msg.no.colon.case"))
               ((= tt js3-beautify-DEFAULT)
                (if has-default
                    (js3-beautify-report-error "msg.double.switch.default"))
                (setq has-default t
                      case-expr nil)
                (js3-beautify-must-match js3-beautify-COLON "msg.no.colon.case"))
               (t
                (js3-beautify-report-error "msg.bad.switch")
                (throw 'break nil)))
              (setq case-node (make-js3-beautify-case-node :pos case-pos
                                                  :len (- js3-beautify-token-end case-pos)
                                                  :expr case-expr))
              (js3-beautify-node-add-children case-node case-expr)
              (while (and (/= (setq tt (js3-beautify-peek-token)) js3-beautify-RC)
                          (/= tt js3-beautify-CASE)
                          (/= tt js3-beautify-DEFAULT)
                          (/= tt js3-beautify-EOF))
                (setf stmt (js3-beautify-parse-statement)
                      (js3-beautify-node-len case-node) (- (js3-beautify-node-end stmt) case-pos))
                (js3-beautify-block-node-push case-node stmt))
              (push case-node cases)))
          ;; add cases last, as pushing reverses the order to be correct
          (dolist (kid cases)
            (js3-beautify-node-add-children pn kid)
            (push kid (js3-beautify-switch-node-cases pn)))
          pn)  ; return value
      (js3-beautify-exit-switch))))

(defun js3-beautify-parse-while ()
  "Parser for while-statement.  Last matched token must be js3-beautify-WHILE."
  (let ((pos js3-beautify-token-beg)
        (pn (make-js3-beautify-while-node))
        cond
        body)
    (js3-beautify-consume-token)
    (js3-beautify-enter-loop pn)
    (unwind-protect
        (progn
          (setf cond (js3-beautify-parse-condition)
                (js3-beautify-while-node-condition pn) (car cond)
                body (js3-beautify-parse-statement)
                (js3-beautify-while-node-body pn) body
                (js3-beautify-node-len pn) (- (js3-beautify-node-end body) pos)
                (js3-beautify-while-node-lp pn) (js3-beautify-relpos (second cond) pos)
                (js3-beautify-while-node-rp pn) (js3-beautify-relpos (third cond) pos))
          (js3-beautify-node-add-children pn body (car cond)))
      (js3-beautify-exit-loop))
    pn))

(defun js3-beautify-parse-do ()
  "Parser for do-statement.  Last matched token must be js3-beautify-DO."
  (let ((pos js3-beautify-token-beg)
        (pn (make-js3-beautify-do-node))
        cond
        body
        end)
    (js3-beautify-consume-token)
    (js3-beautify-enter-loop pn)
    (unwind-protect
        (progn
          (setq body (js3-beautify-parse-statement))
          (js3-beautify-must-match js3-beautify-WHILE "msg.no.while.do")
          (setf (js3-beautify-do-node-while-pos pn) (- js3-beautify-token-beg pos)
                cond (js3-beautify-parse-condition)
                (js3-beautify-do-node-condition pn) (car cond)
                (js3-beautify-do-node-body pn) body
                end js3-beautify-ts-cursor
                (js3-beautify-do-node-lp pn) (js3-beautify-relpos (second cond) pos)
                (js3-beautify-do-node-rp pn) (js3-beautify-relpos (third cond) pos))
          (js3-beautify-node-add-children pn (car cond) body))
      (js3-beautify-exit-loop))
    ;; Always auto-insert semicolon to follow SpiderMonkey:
    ;; It is required by ECMAScript but is ignored by the rest of
    ;; world; see bug 238945
    (if (js3-beautify-match-token js3-beautify-SEMI)
        (setq end js3-beautify-ts-cursor))
    (setf (js3-beautify-node-len pn) (- end pos))
    pn))

(defun js3-beautify-parse-for ()
  "Parser for for-statement.  Last matched token must be js3-beautify-FOR.
Parses for, for-in, and for each-in statements."
  (let ((for-pos js3-beautify-token-beg)
        pn
        is-for-each
        is-for-in
        in-pos
        each-pos
        tmp-pos
        init  ; Node init is also foo in 'foo in object'
        cond  ; Node cond is also object in 'foo in object'
        incr  ; 3rd section of for-loop initializer
        body
        tt
        lp
        rp)
    (js3-beautify-consume-token)
    ;; See if this is a for each () instead of just a for ()
    (when (js3-beautify-match-token js3-beautify-NAME)
      (if (string= "each" js3-beautify-ts-string)
	  (setq is-for-each t
		each-pos (- js3-beautify-token-beg for-pos)) ; relative
        (js3-beautify-report-error "msg.no.paren.for")))
    (if (js3-beautify-must-match js3-beautify-LP "msg.no.paren.for")
        (setq lp (- js3-beautify-token-beg for-pos)))
    (setq tt (js3-beautify-peek-token))
    ;; parse init clause
    (let ((js3-beautify-in-for-init t))  ; set as dynamic variable
      (cond
       ((= tt js3-beautify-SEMI)
        (setq init (make-js3-beautify-empty-expr-node)))
       ((or (= tt js3-beautify-VAR) (= tt js3-beautify-LET))
        (js3-beautify-consume-token)
        (setq init (js3-beautify-parse-variables tt js3-beautify-token-beg)))
       (t
        (setq init (js3-beautify-parse-expr)))))
    (if (js3-beautify-match-token js3-beautify-IN)
        (setq is-for-in t
              in-pos (- js3-beautify-token-beg for-pos)
              cond (js3-beautify-parse-expr))  ; object over which we're iterating
      ;; else ordinary for loop - parse cond and incr
      (js3-beautify-must-match js3-beautify-SEMI "msg.no.semi.for")
      (setq cond (if (= (js3-beautify-peek-token) js3-beautify-SEMI)
                     (make-js3-beautify-empty-expr-node) ; no loop condition
                   (js3-beautify-parse-expr)))
      (js3-beautify-must-match js3-beautify-SEMI "msg.no.semi.for.cond")
      (setq tmp-pos js3-beautify-token-end
            incr (if (= (js3-beautify-peek-token) js3-beautify-RP)
                     (make-js3-beautify-empty-expr-node :pos tmp-pos)
                   (js3-beautify-parse-expr))))
    (if (js3-beautify-must-match js3-beautify-RP "msg.no.paren.for.ctrl")
        (setq rp (- js3-beautify-token-beg for-pos)))
    (if (not is-for-in)
        (setq pn (make-js3-beautify-for-node :init init
                                    :condition cond
                                    :update incr
                                    :lp lp
                                    :rp rp))
      ;; cond could be null if 'in obj' got eaten by the init node.
      (if (js3-beautify-infix-node-p init)
          ;; it was (foo in bar) instead of (var foo in bar)
          (setq cond (js3-beautify-infix-node-right init)
                init (js3-beautify-infix-node-left init))
        (if (and (js3-beautify-var-decl-node-p init)
                 (> (length (js3-beautify-var-decl-node-kids init)) 1))
            (js3-beautify-report-error "msg.mult.index")))
      (setq pn (make-js3-beautify-for-in-node :iterator init
                                     :object cond
                                     :in-pos in-pos
                                     :foreach-p is-for-each
                                     :each-pos each-pos
                                     :lp lp
                                     :rp rp)))
    (unwind-protect
        (progn
          (js3-beautify-enter-loop pn)
          ;; We have to parse the body -after- creating the loop node,
          ;; so that the loop node appears in the js3-beautify-loop-set, allowing
          ;; break/continue statements to find the enclosing loop.
          (setf body (js3-beautify-parse-statement)
                (js3-beautify-loop-node-body pn) body
                (js3-beautify-node-pos pn) for-pos
                (js3-beautify-node-len pn) (- (js3-beautify-node-end body) for-pos))
          (js3-beautify-node-add-children pn init cond incr body))
      ;; finally
      (js3-beautify-exit-loop))
    pn))

(defun js3-beautify-parse-try ()
  "Parser for try-statement.  Last matched token must be js3-beautify-TRY."
  (let ((try-pos js3-beautify-token-beg)
        try-end
        try-block
        catch-blocks
        finally-block
        saw-default-catch
        peek
        var-name
        catch-cond
        catch-node
        guard-kwd
        catch-pos
        finally-pos
        pn
        block
        lp
        rp)
    (js3-beautify-consume-token)
    (if (/= (js3-beautify-peek-token) js3-beautify-LC)
        (js3-beautify-report-error "msg.no.brace.try"))
    (setq try-block (js3-beautify-parse-statement)
          try-end (js3-beautify-node-end try-block)
          peek (js3-beautify-peek-token))
    (cond
     ((= peek js3-beautify-CATCH)
      (while (js3-beautify-match-token js3-beautify-CATCH)
        (setq catch-pos js3-beautify-token-beg
              guard-kwd nil
              catch-cond nil
              lp nil
              rp nil)
        (if saw-default-catch
            (js3-beautify-report-error "msg.catch.unreachable"))
        (if (js3-beautify-must-match js3-beautify-LP "msg.no.paren.catch")
            (setq lp (- js3-beautify-token-beg catch-pos)))
        (js3-beautify-must-match js3-beautify-NAME "msg.bad.catchcond")
	(js3-beautify-push-scope (make-js3-beautify-scope))
        (setq var-name (js3-beautify-create-name-node))
	(js3-beautify-define-symbol js3-beautify-LET (js3-beautify-name-node-name var-name) var-name t)
        (if (js3-beautify-match-token js3-beautify-IF)
            (setq guard-kwd (- js3-beautify-token-beg catch-pos)
                  catch-cond (js3-beautify-parse-expr))
          (setq saw-default-catch t))
        (if (js3-beautify-must-match js3-beautify-RP "msg.bad.catchcond")
            (setq rp (- js3-beautify-token-beg catch-pos)))
        (js3-beautify-must-match js3-beautify-LC "msg.no.brace.catchblock")
        (setq block (js3-beautify-parse-statements)
              try-end (js3-beautify-node-end block)
              catch-node (make-js3-beautify-catch-node :pos catch-pos
                                              :var-name var-name
                                              :guard-expr catch-cond
                                              :guard-kwd guard-kwd
                                              :block block
                                              :lp lp
                                              :rp rp))
	(js3-beautify-pop-scope)
        (if (js3-beautify-must-match js3-beautify-RC "msg.no.brace.after.body")
            (setq try-end js3-beautify-token-beg))
        (setf (js3-beautify-node-len block) (- try-end (js3-beautify-node-pos block))
              (js3-beautify-node-len catch-node) (- try-end catch-pos))
        (js3-beautify-node-add-children catch-node var-name catch-cond block)
        (push catch-node catch-blocks)))
     ((/= peek js3-beautify-FINALLY)
      (js3-beautify-must-match js3-beautify-FINALLY "msg.try.no.catchfinally"
                      (js3-beautify-node-pos try-block)
                      (- (setq try-end (js3-beautify-node-end try-block))
                         (js3-beautify-node-pos try-block)))))
    (when (js3-beautify-match-token js3-beautify-FINALLY)
      (setq finally-pos js3-beautify-token-beg
            block (js3-beautify-parse-statement)
            try-end (js3-beautify-node-end block)
            finally-block (make-js3-beautify-finally-node :pos finally-pos
                                                 :len (- try-end finally-pos)
                                                 :body block))
      (js3-beautify-node-add-children finally-block block))
    (setq pn (make-js3-beautify-try-node :pos try-pos
                                :len (- try-end try-pos)
                                :try-block try-block
                                :finally-block finally-block))
    (js3-beautify-node-add-children pn try-block finally-block)
    ;; push them onto the try-node, which reverses and corrects their order
    (dolist (cb catch-blocks)
      (js3-beautify-node-add-children pn cb)
      (push cb (js3-beautify-try-node-catch-clauses pn)))
    pn))

(defun js3-beautify-parse-throw ()
  "Parser for throw-statement.  Last matched token must be js3-beautify-THROW."
  (let ((pos js3-beautify-token-beg)
        expr
        pn)
    (js3-beautify-consume-token)
    (if (= (js3-beautify-peek-token-or-eol) js3-beautify-EOL)
        ;; ECMAScript does not allow new lines before throw expression,
        ;; see bug 256617
        (js3-beautify-report-error "msg.bad.throw.eol"))
    (setq expr (js3-beautify-parse-expr)
          pn (make-js3-beautify-throw-node :pos pos
                                  :len (- (js3-beautify-node-end expr) pos)
                                  :expr expr))
    (js3-beautify-node-add-children pn expr)
    pn))

(defsubst js3-beautify-match-jump-label-name (label-name)
  "If break/continue specified a label, return that label's labeled stmt.
Returns the corresponding `js3-beautify-labeled-stmt-node', or if LABEL-NAME
does not match an existing label, reports an error and returns nil."
  (let ((bundle (cdr (assoc label-name js3-beautify-label-set))))
    (if (null bundle)
        (js3-beautify-report-error "msg.undef.label"))
    bundle))

(defun js3-beautify-parse-break ()
  "Parser for break-statement.  Last matched token must be js3-beautify-BREAK."
  (let ((pos js3-beautify-token-beg)
        (end js3-beautify-token-end)
        break-target ; statement to break from
        break-label  ; in "break foo", name-node representing the foo
        labels       ; matching labeled statement to break to
        pn)
    (js3-beautify-consume-token)  ; `break'
    (when (eq (js3-beautify-peek-token-or-eol) js3-beautify-NAME)
      (js3-beautify-consume-token)
      (setq break-label (js3-beautify-create-name-node)
            end (js3-beautify-node-end break-label)
            ;; matchJumpLabelName only matches if there is one
            labels (js3-beautify-match-jump-label-name js3-beautify-ts-string)
            break-target (if labels (car (js3-beautify-labeled-stmt-node-labels labels)))))
    (unless (or break-target break-label)
      ;; no break target specified - try for innermost enclosing loop/switch
      (if (null js3-beautify-loop-and-switch-set)
          (unless break-label
            (js3-beautify-report-error "msg.bad.break" nil pos (length "break")))
        (setq break-target (car js3-beautify-loop-and-switch-set))))
    (setq pn (make-js3-beautify-break-node :pos pos
                                  :len (- end pos)
                                  :label break-label
                                  :target break-target))
    (js3-beautify-node-add-children pn break-label)  ; but not break-target
    pn))

(defun js3-beautify-parse-continue ()
  "Parser for continue-statement.  Last matched token must be js3-beautify-CONTINUE."
  (let ((pos js3-beautify-token-beg)
        (end js3-beautify-token-end)
        label   ; optional user-specified label, a `js3-beautify-name-node'
        labels  ; current matching labeled stmt, if any
        target  ; the `js3-beautify-loop-node' target of this continue stmt
        pn)
    (js3-beautify-consume-token)  ; `continue'
    (when (= (js3-beautify-peek-token-or-eol) js3-beautify-NAME)
      (js3-beautify-consume-token)
      (setq label (js3-beautify-create-name-node)
            end (js3-beautify-node-end label)
            ;; matchJumpLabelName only matches if there is one
            labels (js3-beautify-match-jump-label-name js3-beautify-ts-string)))
    (cond
     ((null labels)  ; no current label to go to
      (if (null js3-beautify-loop-set)  ; no loop to continue to
          (js3-beautify-report-error "msg.continue.outside" nil pos
                            (length "continue"))
        (setq target (car js3-beautify-loop-set))))  ; innermost enclosing loop
     (t
      (if (js3-beautify-loop-node-p (js3-beautify-labeled-stmt-node-stmt labels))
          (setq target (js3-beautify-labeled-stmt-node-stmt labels))
        (js3-beautify-report-error "msg.continue.nonloop" nil pos (- end pos)))))
    (setq pn (make-js3-beautify-continue-node :pos pos
                                     :len (- end pos)
                                     :label label
                                     :target target))
    (js3-beautify-node-add-children pn label)  ; but not target - it's not our child
    pn))

(defun js3-beautify-parse-with ()
  "Parser for with-statement.  Last matched token must be js3-beautify-WITH."
  (js3-beautify-consume-token)
  (let ((pos js3-beautify-token-beg)
        obj body pn lp rp)
    (if (js3-beautify-must-match js3-beautify-LP "msg.no.paren.with")
        (setq lp js3-beautify-token-beg))
    (setq obj (js3-beautify-parse-expr))
    (if (js3-beautify-must-match js3-beautify-RP "msg.no.paren.after.with")
        (setq rp js3-beautify-token-beg))
    (let ((js3-beautify-nesting-of-with (1+ js3-beautify-nesting-of-with)))
      (setq body (js3-beautify-parse-statement)))
    (setq pn (make-js3-beautify-with-node :pos pos
                                 :len (- (js3-beautify-node-end body) pos)
                                 :object obj
                                 :body body
                                 :lp (js3-beautify-relpos lp pos)
                                 :rp (js3-beautify-relpos rp pos)))
    (js3-beautify-node-add-children pn obj body)
    pn))

(defun js3-beautify-parse-const-var ()
  "Parser for var- or const-statement.
Last matched token must be js3-beautify-CONST or js3-beautify-VAR."
  (let ((tt (js3-beautify-peek-token))
        (pos js3-beautify-token-beg)
        expr
        pn)
    (js3-beautify-consume-token)
    (setq expr (js3-beautify-parse-variables tt js3-beautify-token-beg)
          pn (make-js3-beautify-expr-stmt-node :pos pos
                                      :len (- (js3-beautify-node-end expr) pos)
                                      :expr expr))
    (js3-beautify-node-add-children pn expr)
    pn))

(defsubst js3-beautify-wrap-with-expr-stmt (pos expr &optional add-child)
  (let ((pn (make-js3-beautify-expr-stmt-node :pos pos
                                     :len (js3-beautify-node-len expr)
                                     :type (if (js3-beautify-inside-function)
                                               js3-beautify-EXPR_VOID
                                             js3-beautify-EXPR_RESULT)
                                     :expr expr)))
    (if add-child
        (js3-beautify-node-add-children pn expr))
    pn))

(defun js3-beautify-parse-let-stmt ()
  "Parser for let-statement.  Last matched token must be js3-beautify-LET."
  (js3-beautify-consume-token)
  (let ((pos js3-beautify-token-beg)
        expr
        pn)
    (if (= (js3-beautify-peek-token) js3-beautify-LP)
        ;; let expression in statement context
        (setq expr (js3-beautify-parse-let pos 'statement)
              pn (js3-beautify-wrap-with-expr-stmt pos expr t))
      ;; else we're looking at a statement like let x=6, y=7;
      (setf expr (js3-beautify-parse-variables js3-beautify-LET pos)
            pn (js3-beautify-wrap-with-expr-stmt pos expr t)
            (js3-beautify-node-type pn) js3-beautify-EXPR_RESULT))
    pn))

(defun js3-beautify-parse-ret-yield ()
  (js3-beautify-parse-return-or-yield (js3-beautify-peek-token) nil))

(defconst js3-beautify-parse-return-stmt-enders
  (list js3-beautify-SEMI js3-beautify-RC js3-beautify-EOF js3-beautify-EOL js3-beautify-ERROR js3-beautify-RB js3-beautify-RP js3-beautify-YIELD))

(defsubst js3-beautify-now-all-set (before after mask)
  "Return whether or not the bits in the mask have changed to all set.
BEFORE is bits before change, AFTER is bits after change, and MASK is
the mask for bits.  Returns t if all the bits in the mask are set in AFTER
but not BEFORE."
  (and (/= (logand before mask) mask)
       (= (logand after mask) mask)))

(defun js3-beautify-parse-return-or-yield (tt expr-context)
  (let ((pos js3-beautify-token-beg)
        (end js3-beautify-token-end)
        (before js3-beautify-end-flags)
        (inside-function (js3-beautify-inside-function))
        e
        ret
        name)
    (unless inside-function
      (js3-beautify-report-error (if (eq tt js3-beautify-RETURN)
                            "msg.bad.return"
                          "msg.bad.yield")))
    (js3-beautify-consume-token)
    ;; This is ugly, but we don't want to require a semicolon.
    (unless (memq (js3-beautify-peek-token-or-eol) js3-beautify-parse-return-stmt-enders)
      (setq e (js3-beautify-parse-expr)
            end (js3-beautify-node-end e)))
    (cond
     ((eq tt js3-beautify-RETURN)
      (js3-beautify-set-flag js3-beautify-end-flags (if (null e)
                                      js3-beautify-end-returns
                                    js3-beautify-end-returns-value))
      (setq ret (make-js3-beautify-return-node :pos pos
                                      :len (- end pos)
                                      :retval e))
      (js3-beautify-node-add-children ret e)
      ;; See if we need a strict mode warning.
      ;; TODO:  The analysis done by `js3-beautify-has-consistent-return-usage' is
      ;; more thorough and accurate than this before/after flag check.
      ;; E.g. if there's a finally-block that always returns, we shouldn't
      ;; show a warning generated by inconsistent returns in the catch blocks.
      ;; Basically `js3-beautify-has-consistent-return-usage' needs to keep more state,
      ;; so we know which returns/yields to highlight, and we should get rid of
      ;; all the checking in `js3-beautify-parse-return-or-yield'.
      (if (and js3-beautify-strict-inconsistent-return-warning
               (js3-beautify-now-all-set before js3-beautify-end-flags
                                (logior js3-beautify-end-returns js3-beautify-end-returns-value)))
          (js3-beautify-add-strict-warning "msg.return.inconsistent" nil pos end)))
     (t
      (unless (js3-beautify-inside-function)
        (js3-beautify-report-error "msg.bad.yield"))
      (js3-beautify-set-flag js3-beautify-end-flags js3-beautify-end-yields)
      (setq ret (make-js3-beautify-yield-node :pos pos
                                     :len (- end pos)
                                     :value e))
      (js3-beautify-node-add-children ret e)
      (unless expr-context
        (setq e ret
              ret (js3-beautify-wrap-with-expr-stmt pos e t))
        (js3-beautify-set-requires-activation)
        (js3-beautify-set-is-generator))))
    ;; see if we are mixing yields and value returns.
    (when (and inside-function
               (js3-beautify-now-all-set before js3-beautify-end-flags
                                (logior js3-beautify-end-yields js3-beautify-end-returns-value)))
      (setq name (js3-beautify-function-name js3-beautify-current-script-or-fn))
      (if (zerop (length name))
          (js3-beautify-report-error "msg.anon.generator.returns" nil pos (- end pos))
        (js3-beautify-report-error "msg.generator.returns" name pos (- end pos))))
    ret))

(defun js3-beautify-parse-debugger ()
  (js3-beautify-consume-token)
  (make-js3-beautify-keyword-node :type js3-beautify-DEBUGGER))

(defun js3-beautify-parse-block ()
  "Parser for a curly-delimited statement block.
Last token matched must be js3-beautify-LC."
  (let ((pos js3-beautify-token-beg)
        (pn (make-js3-beautify-scope)))
    (js3-beautify-consume-token)
    (js3-beautify-push-scope pn)
    (unwind-protect
        (progn
          (js3-beautify-parse-statements pn)
          (js3-beautify-must-match js3-beautify-RC "msg.no.brace.block")
          (setf (js3-beautify-node-len pn) (- js3-beautify-token-end pos)))
      (js3-beautify-pop-scope))
    pn))

;; for js3-beautify-ERROR too, to have a node for error recovery to work on
(defun js3-beautify-parse-semi ()
  "Parse a statement or handle an error.
Last matched token is js3-beautify-SEMI or js3-beautify-ERROR."
  (let ((tt (js3-beautify-peek-token)) pos len)
    (js3-beautify-consume-token)
    (if (eq tt js3-beautify-SEMI)
        (make-js3-beautify-empty-expr-node :len 1)
      (setq pos js3-beautify-token-beg
            len (- js3-beautify-token-beg pos))
      (js3-beautify-report-error "msg.syntax" nil pos len)
      (make-js3-beautify-error-node :pos pos :len len))))

(defun js3-beautify-record-label (label bundle)
  ;; current token should be colon that `js3-beautify-parse-primary-expr' left untouched
  (js3-beautify-consume-token)
  (let ((name (js3-beautify-label-node-name label))
        labeled-stmt
        dup)
    (when (setq labeled-stmt (cdr (assoc name js3-beautify-label-set)))
      ;; flag both labels if possible when used in editing mode
      (if (and js3-beautify-parse-ide-mode
               (setq dup (js3-beautify-get-label-by-name labeled-stmt name)))
          (js3-beautify-report-error "msg.dup.label" nil
                            (js3-beautify-node-abs-pos dup) (js3-beautify-node-len dup)))
      (js3-beautify-report-error "msg.dup.label" nil
                        (js3-beautify-node-pos label) (js3-beautify-node-len label)))
    (js3-beautify-labeled-stmt-node-add-label bundle label)
    (js3-beautify-node-add-children bundle label)
    ;; Add one reference to the bundle per label in `js3-beautify-label-set'
    (push (cons name bundle) js3-beautify-label-set)))

(defun js3-beautify-parse-name-or-label ()
  "Parser for identifier or label.  Last token matched must be js3-beautify-NAME.
Called when we found a name in a statement context.  If it's a label, we gather
up any following labels and the next non-label statement into a
`js3-beautify-labeled-stmt-node' bundle and return that.  Otherwise we parse an
expression and return it wrapped in a `js3-beautify-expr-stmt-node'."
  (let ((pos js3-beautify-token-beg)
        (end js3-beautify-token-end)
        expr
        stmt
        pn
        bundle
        (continue t))
    ;; set check for label and call down to `js3-beautify-parse-primary-expr'
    (js3-beautify-set-check-for-label)
    (setq expr (js3-beautify-parse-expr))
    (if (/= (js3-beautify-node-type expr) js3-beautify-LABEL)
        ;; Parsed non-label expression - wrap with expression stmt.
        (setq pn (js3-beautify-wrap-with-expr-stmt pos expr t))
      ;; else parsed a label
      (setq bundle (make-js3-beautify-labeled-stmt-node :pos pos))
      (js3-beautify-record-label expr bundle)
      ;; look for more labels
      (while (and continue (= (js3-beautify-peek-token) js3-beautify-NAME))
        (js3-beautify-set-check-for-label)
        (setq expr (js3-beautify-parse-expr))
        (if (/= (js3-beautify-node-type expr) js3-beautify-LABEL)
            (progn
              (setq stmt (js3-beautify-wrap-with-expr-stmt (js3-beautify-node-pos expr) expr t)
                    continue nil)
              (js3-beautify-auto-insert-semicolon stmt))
          (js3-beautify-record-label expr bundle)))
      ;; no more labels; now parse the labeled statement
      (unwind-protect
          (unless stmt
            (let ((js3-beautify-labeled-stmt bundle))  ; bind dynamically
              (setq stmt (js3-beautify-statement-helper))))
        ;; remove the labels for this statement from the global set
        (dolist (label (js3-beautify-labeled-stmt-node-labels bundle))
          (setq js3-beautify-label-set (remove label js3-beautify-label-set))))
      (setf (js3-beautify-labeled-stmt-node-stmt bundle) stmt
            (js3-beautify-node-len bundle) (- (js3-beautify-node-end stmt) pos))
      (js3-beautify-node-add-children bundle stmt)
      bundle)))

(defun js3-beautify-parse-expr-stmt ()
  "Default parser in statement context, if no recognized statement found."
  (js3-beautify-wrap-with-expr-stmt js3-beautify-token-beg (js3-beautify-parse-expr) t))

(defun js3-beautify-parse-variables (decl-type pos)
  "Parse a comma-separated list of variable declarations.
Could be a 'var', 'const' or 'let' expression, possibly in a for-loop initializer.

DECL-TYPE is a token value: either VAR, CONST, or LET depending on context.
For 'var' or 'const', the keyword should be the token last scanned.

POS is the position where the node should start. It's sometimes the
var/const/let keyword, and other times the beginning of the first token
in the first variable declaration.

Returns the parsed `js3-beautify-var-decl-node' expression node."
  (let* ((result (make-js3-beautify-var-decl-node :decl-type decl-type
                                         :pos pos))
         destructuring
         kid-pos
         tt
         init
         name
         end
         nbeg nend
         vi
         (continue t))
    ;; Example:
    ;; var foo = {a: 1, b: 2}, bar = [3, 4];
    ;; var {b: s2, a: s1} = foo, x = 6, y, [s3, s4] = bar;
    (while continue
      (setq destructuring nil
            name nil
            tt (js3-beautify-peek-token)
            kid-pos js3-beautify-token-beg
            end js3-beautify-token-end
            init nil)
      (if (or (= tt js3-beautify-LB) (= tt js3-beautify-LC))
          ;; Destructuring assignment, e.g., var [a, b] = ...
          (setq destructuring (js3-beautify-parse-primary-expr)
                end (js3-beautify-node-end destructuring))
        ;; Simple variable name
        (when (js3-beautify-must-match js3-beautify-NAME "msg.bad.var")
          (setq name (js3-beautify-create-name-node)
                nbeg js3-beautify-token-beg
                nend js3-beautify-token-end
                end nend)
          (js3-beautify-define-symbol decl-type js3-beautify-ts-string name js3-beautify-in-for-init)))
      (when (js3-beautify-match-token js3-beautify-ASSIGN)
        (setq init (js3-beautify-parse-assign-expr)
              end (js3-beautify-node-end init)))
      (setq vi (make-js3-beautify-var-init-node :pos kid-pos
                                       :len (- end kid-pos)
                                       :type decl-type))
      (if destructuring
          (progn
            (if (and (null init) (not js3-beautify-in-for-init))
                (js3-beautify-report-error "msg.destruct.assign.no.init"))
            (setf (js3-beautify-var-init-node-target vi) destructuring))
        (setf (js3-beautify-var-init-node-target vi) name))
      (setf (js3-beautify-var-init-node-initializer vi) init)
      (js3-beautify-node-add-children vi name destructuring init)
      (js3-beautify-block-node-push result vi)
      (unless (js3-beautify-match-token js3-beautify-COMMA)
        (setq continue nil)))
    (setf (js3-beautify-node-len result) (- end pos))
    result))

(defun js3-beautify-parse-let (pos &optional stmt-p)
  "Parse a let expression or statement.
A let-expression is of the form `let (vars) expr'.
A let-statment is of the form `let (vars) {statements}'.
The third form of let is a variable declaration list, handled
by `js3-beautify-parse-variables'."
  (let ((pn (make-js3-beautify-let-node :pos pos))
        beg vars body)
    (if (js3-beautify-must-match js3-beautify-LP "msg.no.paren.after.let")
        (setf (js3-beautify-let-node-lp pn) (- js3-beautify-token-beg pos)))
    (js3-beautify-push-scope pn)
    (unwind-protect
        (progn
          (setq vars (js3-beautify-parse-variables js3-beautify-LET js3-beautify-token-beg))
          (if (js3-beautify-must-match js3-beautify-RP "msg.no.paren.let")
              (setf (js3-beautify-let-node-rp pn) (- js3-beautify-token-beg pos)))
          (if (and stmt-p (eq (js3-beautify-peek-token) js3-beautify-LC))
              ;; let statement
              (progn
                (js3-beautify-consume-token)
                (setf beg js3-beautify-token-beg  ; position stmt at LC
                      body (js3-beautify-parse-statements))
                (js3-beautify-must-match js3-beautify-RC "msg.no.curly.let")
                (setf (js3-beautify-node-len body) (- js3-beautify-token-end beg)
                      (js3-beautify-node-len pn) (- js3-beautify-token-end pos)
                      (js3-beautify-let-node-body pn) body
                      (js3-beautify-node-type pn) js3-beautify-LET))
            ;; let expression
            (setf body (js3-beautify-parse-expr)
                  (js3-beautify-node-len pn) (- (js3-beautify-node-end body) pos)
                  (js3-beautify-let-node-body pn) body))
          (js3-beautify-node-add-children pn vars body))
      (js3-beautify-pop-scope))
    pn))

(defsubst js3-beautify-define-new-symbol (decl-type name node &optional scope)
  (js3-beautify-scope-put-symbol (or scope js3-beautify-current-scope)
                        name
                        (make-js3-beautify-symbol decl-type name node)))

(defun js3-beautify-define-symbol (decl-type name &optional node ignore-not-in-block)
  "Define a symbol in the current scope.
If NODE is non-nil, it is the AST node associated with the symbol."
  (let* ((defining-scope (js3-beautify-get-defining-scope js3-beautify-current-scope name))
         (symbol (if defining-scope
                     (js3-beautify-scope-get-symbol defining-scope name)))
         (sdt (if symbol (js3-beautify-symbol-decl-type symbol) -1)))
    (cond
     ((and symbol ; already defined
           (or (= sdt js3-beautify-CONST) ; old version is const
               (= decl-type js3-beautify-CONST) ; new version is const
               ;; two let-bound vars in this block have same name
               (and (= sdt js3-beautify-LET)
                    (eq defining-scope js3-beautify-current-scope))))
      (js3-beautify-report-error
       (cond
        ((= sdt js3-beautify-CONST) "msg.const.redecl")
        ((= sdt js3-beautify-LET) "msg.let.redecl")
        ((= sdt js3-beautify-VAR) "msg.var.redecl")
        ((= sdt js3-beautify-FUNCTION) "msg.function.redecl")
        (t "msg.parm.redecl"))
       name))
     ((= decl-type js3-beautify-LET)
      (if (and (not ignore-not-in-block)
               (or (= (js3-beautify-node-type js3-beautify-current-scope) js3-beautify-IF)
                   (js3-beautify-loop-node-p js3-beautify-current-scope)))
          (js3-beautify-report-error "msg.let.decl.not.in.block")
        (js3-beautify-define-new-symbol decl-type name node
			       js3-beautify-current-script-or-fn)))
     ((or (= decl-type js3-beautify-VAR)
          (= decl-type js3-beautify-CONST)
          (= decl-type js3-beautify-FUNCTION))
      (if symbol
          (if (and js3-beautify-strict-var-redeclaration-warning (= sdt js3-beautify-VAR))
              (js3-beautify-add-strict-warning "msg.var.redecl" name)
            (if (and js3-beautify-strict-var-hides-function-arg-warning (= sdt js3-beautify-LP))
                (js3-beautify-add-strict-warning "msg.var.hides.arg" name)))
        (js3-beautify-define-new-symbol decl-type name node)))
     ((= decl-type js3-beautify-LP)
      (if symbol
          ;; must be duplicate parameter. Second parameter hides the
          ;; first, so go ahead and add the second pararameter
          (js3-beautify-report-warning "msg.dup.parms" name))
      (js3-beautify-define-new-symbol decl-type name node))
     (t (js3-beautify-code-bug)))))

(defun js3-beautify-parse-expr ()
  (let* ((pn (js3-beautify-parse-assign-expr))
         (pos (js3-beautify-node-pos pn))
         left
         right
         op-pos)
    (while (js3-beautify-match-token js3-beautify-COMMA)
      (setq op-pos (- js3-beautify-token-beg pos))  ; relative
      (if (= (js3-beautify-peek-token) js3-beautify-YIELD)
          (js3-beautify-report-error "msg.yield.parenthesized"))
      (setq right (js3-beautify-parse-assign-expr)
            left pn
            pn (make-js3-beautify-infix-node :type js3-beautify-COMMA
                                    :pos pos
                                    :len (- js3-beautify-ts-cursor pos)
                                    :op-pos op-pos
                                    :left left
                                    :right right))
      (js3-beautify-node-add-children pn left right))
    pn))

(defun js3-beautify-parse-assign-expr ()
  (let ((tt (js3-beautify-peek-token))
        (pos js3-beautify-token-beg)
        pn
        left
        right
        op-pos)
    (if (= tt js3-beautify-YIELD)
        (js3-beautify-parse-return-or-yield tt t)
      ;; not yield - parse assignment expression
      (setq pn (js3-beautify-parse-cond-expr)
            tt (js3-beautify-peek-token))
      (when (and (<= js3-beautify-first-assign tt)
                 (<= tt js3-beautify-last-assign))
        (js3-beautify-consume-token)
        (setq op-pos (- js3-beautify-token-beg pos)  ; relative
              left pn
              right (js3-beautify-parse-assign-expr)
              pn (make-js3-beautify-assign-node :type tt
                                       :pos pos
                                       :len (- (js3-beautify-node-end right) pos)
                                       :op-pos op-pos
                                       :left left
                                       :right right))
        (js3-beautify-node-add-children pn left right))
      pn)))

(defun js3-beautify-parse-cond-expr ()
  (let ((pos js3-beautify-token-beg)
        (pn (js3-beautify-parse-or-expr))
        test-expr
        if-true
        if-false
        q-pos
        c-pos)
    (when (js3-beautify-match-token js3-beautify-HOOK)
      (setq q-pos (- js3-beautify-token-beg pos)
            if-true (js3-beautify-parse-assign-expr))
      (js3-beautify-must-match js3-beautify-COLON "msg.no.colon.cond")
      (setq c-pos (- js3-beautify-token-beg pos)
            if-false (js3-beautify-parse-assign-expr)
            test-expr pn
            pn (make-js3-beautify-cond-node :pos pos
                                   :len (- (js3-beautify-node-end if-false) pos)
                                   :test-expr test-expr
                                   :true-expr if-true
                                   :false-expr if-false
                                   :q-pos q-pos
                                   :c-pos c-pos))
      (js3-beautify-node-add-children pn test-expr if-true if-false))
    pn))

(defun js3-beautify-make-binary (type left parser)
  "Helper for constructing a binary-operator AST node.
LEFT is the left-side-expression, already parsed, and the
binary operator should have just been matched.
PARSER is a function to call to parse the right operand,
or a `js3-beautify-node' struct if it has already been parsed."
  (let* ((pos (js3-beautify-node-pos left))
         (op-pos (- js3-beautify-token-beg pos))
         (right (if (js3-beautify-node-p parser)
                    parser
                  (funcall parser)))
         (pn (make-js3-beautify-infix-node :type type
                                  :pos pos
                                  :len (- (js3-beautify-node-end right) pos)
                                  :op-pos op-pos
                                  :left left
                                  :right right)))
    (js3-beautify-node-add-children pn left right)
    pn))

(defun js3-beautify-parse-or-expr ()
  (let ((pn (js3-beautify-parse-and-expr)))
    (when (js3-beautify-match-token js3-beautify-OR)
      (setq pn (js3-beautify-make-binary js3-beautify-OR
                                pn
                                'js3-beautify-parse-or-expr)))
    pn))

(defun js3-beautify-parse-and-expr ()
  (let ((pn (js3-beautify-parse-bit-or-expr)))
    (when (js3-beautify-match-token js3-beautify-AND)
      (setq pn (js3-beautify-make-binary js3-beautify-AND
                                pn
                                'js3-beautify-parse-and-expr)))
    pn))

(defun js3-beautify-parse-bit-or-expr ()
  (let ((pn (js3-beautify-parse-bit-xor-expr)))
    (while (js3-beautify-match-token js3-beautify-BITOR)
      (setq pn (js3-beautify-make-binary js3-beautify-BITOR
                                pn
                                'js3-beautify-parse-bit-xor-expr)))
    pn))

(defun js3-beautify-parse-bit-xor-expr ()
  (let ((pn (js3-beautify-parse-bit-and-expr)))
    (while (js3-beautify-match-token js3-beautify-BITXOR)
      (setq pn (js3-beautify-make-binary js3-beautify-BITXOR
                                pn
                                'js3-beautify-parse-bit-and-expr)))
    pn))

(defun js3-beautify-parse-bit-and-expr ()
  (let ((pn (js3-beautify-parse-eq-expr)))
    (while (js3-beautify-match-token js3-beautify-BITAND)
      (setq pn (js3-beautify-make-binary js3-beautify-BITAND
                                pn
                                'js3-beautify-parse-eq-expr)))
    pn))

(defconst js3-beautify-parse-eq-ops
  (list js3-beautify-EQ js3-beautify-NE js3-beautify-SHEQ js3-beautify-SHNE))

(defun js3-beautify-parse-eq-expr ()
  (let ((pn (js3-beautify-parse-rel-expr))
        tt)
    (while (memq (setq tt (js3-beautify-peek-token)) js3-beautify-parse-eq-ops)
      (js3-beautify-consume-token)
      (setq pn (js3-beautify-make-binary tt
                                pn
                                'js3-beautify-parse-rel-expr)))
    pn))

(defconst js3-beautify-parse-rel-ops
  (list js3-beautify-IN js3-beautify-INSTANCEOF js3-beautify-LE js3-beautify-LT js3-beautify-GE js3-beautify-GT))

(defun js3-beautify-parse-rel-expr ()
  (let ((pn (js3-beautify-parse-shift-expr))
        (continue t)
        tt)
    (while continue
      (setq tt (js3-beautify-peek-token))
      (cond
       ((and js3-beautify-in-for-init (= tt js3-beautify-IN))
        (setq continue nil))
       ((memq tt js3-beautify-parse-rel-ops)
        (js3-beautify-consume-token)
        (setq pn (js3-beautify-make-binary tt pn 'js3-beautify-parse-shift-expr)))
       (t
        (setq continue nil))))
    pn))

(defconst js3-beautify-parse-shift-ops
  (list js3-beautify-LSH js3-beautify-URSH js3-beautify-RSH))

(defun js3-beautify-parse-shift-expr ()
  (let ((pn (js3-beautify-parse-add-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js3-beautify-peek-token))
      (if (memq tt js3-beautify-parse-shift-ops)
          (progn
            (js3-beautify-consume-token)
            (setq pn (js3-beautify-make-binary tt pn 'js3-beautify-parse-add-expr)))
        (setq continue nil)))
    pn))

(defun js3-beautify-parse-add-expr ()
  (let ((pn (js3-beautify-parse-mul-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js3-beautify-peek-token))
      (if (or (= tt js3-beautify-ADD) (= tt js3-beautify-SUB))
          (progn
            (js3-beautify-consume-token)
            (setq pn (js3-beautify-make-binary tt pn 'js3-beautify-parse-mul-expr)))
        (setq continue nil)))
    pn))

(defconst js3-beautify-parse-mul-ops
  (list js3-beautify-MUL js3-beautify-DIV js3-beautify-MOD))

(defun js3-beautify-parse-mul-expr ()
  (let ((pn (js3-beautify-parse-unary-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js3-beautify-peek-token))
      (if (memq tt js3-beautify-parse-mul-ops)
          (progn
            (js3-beautify-consume-token)
            (setq pn (js3-beautify-make-binary tt pn 'js3-beautify-parse-unary-expr)))
        (setq continue nil)))
    pn))

(defsubst js3-beautify-make-unary (type parser &rest args)
  "Make a unary node of type TYPE.
PARSER is either a node (for postfix operators) or a function to call
to parse the operand (for prefix operators)."
  (let* ((pos js3-beautify-token-beg)
         (postfix (js3-beautify-node-p parser))
         (expr (if postfix
                   parser
                 (apply parser args)))
         end
         pn)
    (if postfix  ; e.g. i++
        (setq pos (js3-beautify-node-pos expr)
              end js3-beautify-token-end)
      (setq end (js3-beautify-node-end expr)))
    (setq pn (make-js3-beautify-unary-node :type type
                                  :pos pos
                                  :len (- end pos)
                                  :operand expr))
    (js3-beautify-node-add-children pn expr)
    pn))

(defconst js3-beautify-incrementable-node-types
  (list js3-beautify-NAME js3-beautify-GETPROP js3-beautify-GETELEM js3-beautify-GET_REF js3-beautify-CALL)
  "Node types that can be the operand of a ++ or -- operator.")

(defsubst js3-beautify-check-bad-inc-dec (tt beg end unary)
  (unless (memq (js3-beautify-node-type (js3-beautify-unary-node-operand unary))
                js3-beautify-incrementable-node-types)
    (js3-beautify-report-error (if (= tt js3-beautify-INC)
                          "msg.bad.incr"
                        "msg.bad.decr")
                      nil beg (- end beg))))

(defun js3-beautify-parse-unary-expr ()
  (let ((tt (js3-beautify-peek-token))
        pn expr beg end)
    (cond
     ((or (= tt js3-beautify-VOID)
          (= tt js3-beautify-NOT)
          (= tt js3-beautify-BITNOT)
          (= tt js3-beautify-TYPEOF))
      (js3-beautify-consume-token)
      (js3-beautify-make-unary tt 'js3-beautify-parse-unary-expr))
     ((= tt js3-beautify-ADD)
      (js3-beautify-consume-token)
      ;; Convert to special POS token in decompiler and parse tree
      (js3-beautify-make-unary js3-beautify-POS 'js3-beautify-parse-unary-expr))
     ((= tt js3-beautify-SUB)
      (js3-beautify-consume-token)
      ;; Convert to special NEG token in decompiler and parse tree
      (js3-beautify-make-unary js3-beautify-NEG 'js3-beautify-parse-unary-expr))
     ((or (= tt js3-beautify-INC)
          (= tt js3-beautify-DEC))
      (js3-beautify-consume-token)
      (prog1
          (setq beg js3-beautify-token-beg
                end js3-beautify-token-end
                expr (js3-beautify-make-unary tt 'js3-beautify-parse-member-expr t))
        (js3-beautify-check-bad-inc-dec tt beg end expr)))
     ((= tt js3-beautify-DELPROP)
      (js3-beautify-consume-token)
      (js3-beautify-make-unary js3-beautify-DELPROP 'js3-beautify-parse-unary-expr))
     ((= tt js3-beautify-ERROR)
      (js3-beautify-consume-token)
      (make-js3-beautify-error-node))  ; try to continue
     (t
      (setq pn (js3-beautify-parse-member-expr t)
            ;; Don't look across a newline boundary for a postfix incop.
            tt (js3-beautify-peek-token-or-eol))
      (when (or (= tt js3-beautify-INC) (= tt js3-beautify-DEC))
        (js3-beautify-consume-token)
        (setf expr pn
              pn (js3-beautify-make-unary tt expr))
        (js3-beautify-node-set-prop pn 'postfix t)
        (js3-beautify-check-bad-inc-dec tt js3-beautify-token-beg js3-beautify-token-end pn))
      pn))))


(defun js3-beautify-parse-argument-list ()
  "Parse an argument list and return it as a lisp list of nodes.
Returns the list in reverse order.  Consumes the right-paren token."
  (let (result)
    (unless (js3-beautify-match-token js3-beautify-RP)
      (loop do
            (if (= (js3-beautify-peek-token) js3-beautify-YIELD)
                (js3-beautify-report-error "msg.yield.parenthesized"))
            (push (js3-beautify-parse-assign-expr) result)
            while
            (js3-beautify-match-token js3-beautify-COMMA))
      (js3-beautify-must-match js3-beautify-RP "msg.no.paren.arg")
      result)))

(defun js3-beautify-parse-member-expr (&optional allow-call-syntax)
  (let ((tt (js3-beautify-peek-token))
        pn
        pos
        target
        args
        beg
        end
        init
        tail)
    (if (/= tt js3-beautify-NEW)
        (setq pn (js3-beautify-parse-primary-expr))
      ;; parse a 'new' expression
      (js3-beautify-consume-token)
      (setq pos js3-beautify-token-beg
            beg pos
            target (js3-beautify-parse-member-expr)
            end (js3-beautify-node-end target)
            pn (make-js3-beautify-new-node :pos pos
                                  :target target
                                  :len (- end pos)))
      (js3-beautify-node-add-children pn target)
      (when (js3-beautify-match-token js3-beautify-LP)
        ;; Add the arguments to pn, if any are supplied.
        (setf beg pos  ; start of "new" keyword
              pos js3-beautify-token-beg
              args (nreverse (js3-beautify-parse-argument-list))
              (js3-beautify-new-node-args pn) args
              end js3-beautify-token-end
              (js3-beautify-new-node-lp pn) (- pos beg)
              (js3-beautify-new-node-rp pn) (- end 1 beg))
        (apply #'js3-beautify-node-add-children pn args))
      (when (and js3-beautify-allow-rhino-new-expr-initializer
                 (js3-beautify-match-token js3-beautify-LC))
        (setf init (js3-beautify-parse-object-literal)
              end (js3-beautify-node-end init)
              (js3-beautify-new-node-initializer pn) init)
        (js3-beautify-node-add-children pn init))
      (setf (js3-beautify-node-len pn) (- beg pos)))  ; end outer if
    (js3-beautify-parse-member-expr-tail allow-call-syntax pn)))

(defun js3-beautify-parse-member-expr-tail (allow-call-syntax pn)
  "Parse a chain of property/array accesses or function calls.
Includes parsing for E4X operators like `..' and `.@'.
If ALLOW-CALL-SYNTAX is nil, stops when we encounter a left-paren.
Returns an expression tree that includes PN, the parent node."
  (let ((beg (js3-beautify-node-pos pn))
        tt
        (continue t))
    (while continue
      (setq tt (js3-beautify-peek-token))
      (cond
       ((= tt js3-beautify-DOT)
        (setq pn (js3-beautify-parse-property-access tt pn)))
       ((= tt js3-beautify-LB)
        (setq pn (js3-beautify-parse-element-get pn)))
       ((= tt js3-beautify-LP)
        (if allow-call-syntax
            (setq pn (js3-beautify-parse-function-call pn))
          (setq continue nil)))
       (t
        (setq continue nil))))
    pn))

(defun js3-beautify-parse-element-get (pn)
  "Parse an element-get expression, e.g. foo[bar].
Last token parsed must be `js3-beautify-RB'."
  (let ((lb js3-beautify-token-beg)
        (pos (js3-beautify-node-pos pn))
        rb
        expr)
    (js3-beautify-consume-token)
    (setq expr (js3-beautify-parse-expr))
    (if (js3-beautify-must-match js3-beautify-RB "msg.no.bracket.index")
        (setq rb js3-beautify-token-beg))
    (setq pn (make-js3-beautify-elem-get-node :target pn
                                     :pos pos
                                     :element expr
                                     :lb (js3-beautify-relpos lb pos)
                                     :rb (js3-beautify-relpos rb pos)
                                     :len (- js3-beautify-token-end pos)))
    (js3-beautify-node-add-children pn
                           (js3-beautify-elem-get-node-target pn)
                           (js3-beautify-elem-get-node-element pn))
    pn))

(defun js3-beautify-parse-function-call (pn)
  (let (args
        (pos (js3-beautify-node-pos pn)))
    (js3-beautify-consume-token)
    (setq pn (make-js3-beautify-call-node :pos pos
                                 :target pn
                                 :lp (- js3-beautify-token-beg pos)))
    (js3-beautify-node-add-children pn (js3-beautify-call-node-target pn))
    ;; Add the arguments to pn, if any are supplied.
    (setf args (nreverse (js3-beautify-parse-argument-list))
          (js3-beautify-call-node-rp pn) (- js3-beautify-token-beg pos)
          (js3-beautify-call-node-args pn) args)
    (apply #'js3-beautify-node-add-children pn args)
    (setf (js3-beautify-node-len pn) (- js3-beautify-ts-cursor pos))
    pn))

(defun js3-beautify-parse-property-access (tt pn)
  "Parse a property access."
  (let (name
        ref  ; right side of . operator
        result)
    (js3-beautify-consume-token)
    (js3-beautify-must-match-prop-name "msg.no.name.after.dot")
    (setq name (js3-beautify-create-name-node t js3-beautify-GETPROP)
	  result (make-js3-beautify-prop-get-node :left pn
					 :pos js3-beautify-token-beg
					 :right name
					 :len (- js3-beautify-token-end
						 js3-beautify-token-beg)))
    (js3-beautify-node-add-children result pn name)
    result))


(defun js3-beautify-parse-primary-expr ()
  "Parses a literal (leaf) expression of some sort.
Includes complex literals such as functions, object-literals,
array-literals, array comprehensions and regular expressions."
  (let ((tt-flagged (js3-beautify-next-flagged-token))
        pn      ; parent node  (usually return value)
        tt
        px-pos  ; paren-expr pos
        len
        flags   ; regexp flags
        expr)
    (setq tt js3-beautify-current-token)
    (cond
     ((= tt js3-beautify-FUNCTION)
      (js3-beautify-parse-function 'FUNCTION_EXPRESSION))
     ((= tt js3-beautify-LB)
      (js3-beautify-parse-array-literal))
     ((= tt js3-beautify-LC)
      (js3-beautify-parse-object-literal))
     ((= tt js3-beautify-LET)
      (js3-beautify-parse-let js3-beautify-token-beg))
     ((= tt js3-beautify-LP)
      (setq px-pos js3-beautify-token-beg
            expr (js3-beautify-parse-expr))
      (js3-beautify-must-match js3-beautify-RP "msg.no.paren")
      (setq pn (make-js3-beautify-paren-node :pos px-pos
                                    :expr expr
                                    :len (- js3-beautify-token-end px-pos)))
      (js3-beautify-node-add-children pn (js3-beautify-paren-node-expr pn))
      pn)
     ((= tt js3-beautify-NAME)
      (js3-beautify-parse-name tt-flagged tt))
     ((= tt js3-beautify-NUMBER)
      (make-js3-beautify-number-node))
     ((= tt js3-beautify-STRING)
      (make-js3-beautify-string-node))
     ((or (= tt js3-beautify-DIV) (= tt js3-beautify-ASSIGN_DIV))
      ;; Got / or /= which in this context means a regexp literal
      (setq px-pos js3-beautify-token-beg)
      (js3-beautify-read-regexp tt)
      (setq flags js3-beautify-ts-regexp-flags
            js3-beautify-ts-regexp-flags nil)
      (prog1
	  (make-js3-beautify-regexp-node :pos px-pos
					 :len (- js3-beautify-ts-cursor px-pos)
					 :value js3-beautify-ts-string
					 :flags flags)
	(js3-beautify-record-text-property px-pos js3-beautify-ts-cursor 'syntax-table '(2))))
     ((or (= tt js3-beautify-NULL)
          (= tt js3-beautify-THIS)
          (= tt js3-beautify-FALSE)
          (= tt js3-beautify-TRUE))
      (make-js3-beautify-keyword-node :type tt))
     ((= tt js3-beautify-RESERVED)
      (js3-beautify-report-error "msg.reserved.id")
      (make-js3-beautify-name-node))
     ((= tt js3-beautify-ERROR)
      ;; the scanner or one of its subroutines reported the error.
      (make-js3-beautify-error-node))
     ((= tt js3-beautify-EOF)
      (setq px-pos (point-at-bol)
            len (- js3-beautify-ts-cursor px-pos))
      (js3-beautify-report-error "msg.unexpected.eof" nil px-pos len)
      (make-js3-beautify-error-node :pos px-pos :len len))
     (t
      (js3-beautify-report-error "msg.syntax")
      (make-js3-beautify-error-node)))))

(defun js3-beautify-parse-name (tt-flagged tt)
  (let ((name js3-beautify-ts-string)
        (name-pos js3-beautify-token-beg)
	node)
    (if (and (js3-beautify-flag-set-p tt-flagged js3-beautify-ti-check-label)
             (= (js3-beautify-peek-token) js3-beautify-COLON))
	(make-js3-beautify-label-node :pos name-pos
			     :len (- js3-beautify-token-end name-pos)
			     :name name)
      ;; Otherwise not a label, just a name.  Unfortunately peeking
      ;; the next token to check for a colon has biffed js3-beautify-token-beg
      ;; and js3-beautify-token-end.  We store the name's bounds in buffer vars
      ;; and `js3-beautify-create-name-node' uses them.
      (js3-beautify-save-name-token-data name-pos name)
      (setq node (js3-beautify-create-name-node 'check-activation))
      node)))

(defsubst js3-beautify-parse-warn-trailing-comma (msg pos elems comma-pos)
  (js3-beautify-add-strict-warning
   msg nil
   ;; back up from comma to beginning of line or array/objlit
   (max (if elems
            (js3-beautify-node-pos (car elems))
          pos)
        (save-excursion
          (goto-char comma-pos)
          (back-to-indentation)
          (point)))
   comma-pos))

(defun js3-beautify-parse-array-literal ()
  (let ((pos js3-beautify-token-beg)
        (end js3-beautify-token-end)
        (after-lb-or-comma t)
        after-comma
        tt
        elems
        pn
        (continue t))
    (while continue
      (setq tt (js3-beautify-peek-token))
      (cond
       ;; comma
       ((= tt js3-beautify-COMMA)
        (js3-beautify-consume-token)
        (setq after-comma js3-beautify-token-end)
        (if (not after-lb-or-comma)
            (setq after-lb-or-comma t)
          (push nil elems)))
       ;; end of array
       ((or (= tt js3-beautify-RB)
            (= tt js3-beautify-EOF))  ; prevent infinite loop
        (if (= tt js3-beautify-EOF)
            (js3-beautify-report-error "msg.no.bracket.arg" nil pos)
          (js3-beautify-consume-token))
        (setq continue nil
              end js3-beautify-token-end
              pn (make-js3-beautify-array-node :pos pos
                                      :len (- js3-beautify-ts-cursor pos)
                                      :elems (nreverse elems)))
        (apply #'js3-beautify-node-add-children pn (js3-beautify-array-node-elems pn))
        (when after-comma
          (js3-beautify-parse-warn-trailing-comma "msg.array.trailing.comma"
                                         pos elems after-comma)))
       ;; array comp
       ((and (>= js3-beautify-language-version 170)
             (= tt js3-beautify-FOR)          ; check for array comprehension
             (not after-lb-or-comma) ; "for" can't follow a comma
             elems                   ; must have at least 1 element
             (not (cdr elems)))      ; but no 2nd element
        (setf continue nil
              pn (js3-beautify-parse-array-comprehension (car elems) pos)))
       ;; another element
       (t
        (unless after-lb-or-comma
          (js3-beautify-report-error "msg.no.bracket.arg"))
        (push (js3-beautify-parse-assign-expr) elems)
        (setq after-lb-or-comma nil
              after-comma nil))))
    pn))

(defun js3-beautify-parse-array-comprehension (expr pos)
  "Parse a JavaScript 1.7 Array Comprehension.
EXPR is the first expression after the opening left-bracket.
POS is the beginning of the LB token preceding EXPR.
We should have just parsed the 'for' keyword before calling this function."
  (let (loops
        filter
        if-pos
        result)
    (while (= (js3-beautify-peek-token) js3-beautify-FOR)
      (push (js3-beautify-parse-array-comp-loop) loops))
    (when (= (js3-beautify-peek-token) js3-beautify-IF)
      (js3-beautify-consume-token)
      (setq if-pos (- js3-beautify-token-beg pos)  ; relative
            filter (js3-beautify-parse-condition)))
    (js3-beautify-must-match js3-beautify-RB "msg.no.bracket.arg" pos)
    (setq result (make-js3-beautify-array-comp-node :pos pos
                                           :len (- js3-beautify-ts-cursor pos)
                                           :result expr
                                           :loops (nreverse loops)
                                           :filter (car filter)
                                           :lp (js3-beautify-relpos (second filter) pos)
                                           :rp (js3-beautify-relpos (third filter) pos)
                                           :if-pos if-pos))
    (apply #'js3-beautify-node-add-children result expr (car filter)
           (js3-beautify-array-comp-node-loops result))
    result))

(defun js3-beautify-parse-array-comp-loop ()
  "Parse a 'for [each] (foo in bar)' expression in an Array comprehension.
Last token peeked should be the initial FOR."
  (let ((pos js3-beautify-token-beg)
        (pn (make-js3-beautify-array-comp-loop-node))
        tt
        iter
        obj
        foreach-p
        in-pos
        each-pos
        lp
        rp)
    (assert (= (js3-beautify-next-token) js3-beautify-FOR))  ; consumes token
    (js3-beautify-push-scope pn)
    (unwind-protect
        (progn
          (when (js3-beautify-match-token js3-beautify-NAME)
            (if (string= js3-beautify-ts-string "each")
		(setq foreach-p t
		      each-pos (- js3-beautify-token-beg pos)) ; relative
              (js3-beautify-report-error "msg.no.paren.for")))
          (if (js3-beautify-must-match js3-beautify-LP "msg.no.paren.for")
              (setq lp (- js3-beautify-token-beg pos)))
          (setq tt (js3-beautify-peek-token))
          (cond
           ((or (= tt js3-beautify-LB)
                (= tt js3-beautify-LC))
            ;; handle destructuring assignment
            (setq iter (js3-beautify-parse-primary-expr)))
           ((js3-beautify-valid-prop-name-token tt)
            (js3-beautify-consume-token)
            (setq iter (js3-beautify-create-name-node)))
           (t
            (js3-beautify-report-error "msg.bad.var")))
          ;; Define as a let since we want the scope of the variable to
          ;; be restricted to the array comprehension
          (if (js3-beautify-name-node-p iter)
              (js3-beautify-define-symbol js3-beautify-LET (js3-beautify-name-node-name iter) pn t))
          (if (js3-beautify-must-match js3-beautify-IN "msg.in.after.for.name")
              (setq in-pos (- js3-beautify-token-beg pos)))
          (setq obj (js3-beautify-parse-expr))
          (if (js3-beautify-must-match js3-beautify-RP "msg.no.paren.for.ctrl")
              (setq rp (- js3-beautify-token-beg pos)))
          (setf (js3-beautify-node-pos pn) pos
                (js3-beautify-node-len pn) (- js3-beautify-ts-cursor pos)
                (js3-beautify-array-comp-loop-node-iterator pn) iter
                (js3-beautify-array-comp-loop-node-object pn) obj
                (js3-beautify-array-comp-loop-node-in-pos pn) in-pos
                (js3-beautify-array-comp-loop-node-each-pos pn) each-pos
                (js3-beautify-array-comp-loop-node-foreach-p pn) foreach-p
                (js3-beautify-array-comp-loop-node-lp pn) lp
                (js3-beautify-array-comp-loop-node-rp pn) rp)
          (js3-beautify-node-add-children pn iter obj))
      (js3-beautify-pop-scope))
    pn))

(defun js3-beautify-parse-object-literal ()
  (let ((pos js3-beautify-token-beg)
        tt
        elems
        result
        after-comma
        (continue t))
    (while continue
      (setq tt (js3-beautify-peek-token))
      (cond
       ;; {foo: ...}, {'foo': ...}, {get foo() {...}}, or {set foo(x) {...}}
       ((or (js3-beautify-valid-prop-name-token tt)
            (= tt js3-beautify-STRING))
        (setq after-comma nil
              result (js3-beautify-parse-named-prop tt))
        (if (and (null result)
                 (not js3-beautify-recover-from-parse-errors))
            (setq continue nil)
          (push result elems)))
       ;; {12: x} or {10.7: x}
       ((= tt js3-beautify-NUMBER)
        (js3-beautify-consume-token)
        (setq after-comma nil)
        (push (js3-beautify-parse-plain-property (make-js3-beautify-number-node)) elems))
       ;; trailing comma
       ((= tt js3-beautify-RC)
        (setq continue nil)
        (if after-comma
            (js3-beautify-parse-warn-trailing-comma "msg.extra.trailing.comma"
                                           pos elems after-comma)))
       (t
        (js3-beautify-report-error "msg.bad.prop")
        (unless js3-beautify-recover-from-parse-errors
          (setq continue nil))))         ; end switch
      (if (js3-beautify-match-token js3-beautify-COMMA)
          (setq after-comma js3-beautify-token-end)
        (setq continue nil)))           ; end loop
    (js3-beautify-must-match js3-beautify-RC "msg.no.brace.prop")
    (setq result (make-js3-beautify-object-node :pos pos
                                       :len (- js3-beautify-ts-cursor pos)
                                       :elems (nreverse elems)))
    (apply #'js3-beautify-node-add-children result (js3-beautify-object-node-elems result))
    result))

(defun js3-beautify-parse-named-prop (tt)
  "Parse a name, string, or getter/setter object property."
  (js3-beautify-consume-token)
  (let ((string-prop (and (= tt js3-beautify-STRING)
                          (make-js3-beautify-string-node)))
        expr
        (ppos js3-beautify-token-beg)
        (pend js3-beautify-token-end)
        (name (js3-beautify-create-name-node))
        (prop js3-beautify-ts-string))
    (if (and (= tt js3-beautify-NAME)
             (= (js3-beautify-peek-token) js3-beautify-NAME)
             (or (string= prop "get")
                 (string= prop "set")))
        (progn
          ;; getter/setter prop
          (js3-beautify-consume-token)
          (setq name (js3-beautify-create-name-node)) ; discard get/set & use peeked name
          (js3-beautify-parse-getter-setter-prop ppos name (string= prop "get")))
      ;; regular prop
      (setq expr (js3-beautify-parse-plain-property (or string-prop name))))))

(defun js3-beautify-parse-plain-property (prop)
  "Parse a non-getter/setter property in an object literal.
PROP is the node representing the property:  a number, name or string."
  (js3-beautify-must-match js3-beautify-COLON "msg.no.colon.prop")
  (let* ((pos (js3-beautify-node-pos prop))
         (colon (- js3-beautify-token-beg pos))
         (expr (js3-beautify-parse-assign-expr))
         (result (make-js3-beautify-object-prop-node
                  :pos pos
                  ;; don't include last consumed token in length
                  :len (- (+ (js3-beautify-node-pos expr)
                             (js3-beautify-node-len expr))
                          pos)
                  :left prop
                  :right expr
                  :op-pos colon)))
    (js3-beautify-node-add-children result prop expr)
    result))

(defun js3-beautify-parse-getter-setter-prop (pos prop get-p)
  "Parse getter or setter property in an object literal.
JavaScript syntax is:

{ get foo() {...}, set foo(x) {...} }

POS is the start position of the `get' or `set' keyword.
PROP is the `js3-beautify-name-node' representing the property name.
GET-P is non-nil if the keyword was `get'."
  (let ((type (if get-p js3-beautify-GET js3-beautify-SET))
        result
        end
        (fn (js3-beautify-parse-function 'FUNCTION_EXPRESSION)))
    ;; it has to be an anonymous function, as we already parsed the name
    (if (/= (js3-beautify-node-type fn) js3-beautify-FUNCTION)
        (js3-beautify-report-error "msg.bad.prop")
      (if (plusp (length (js3-beautify-function-name fn)))
          (js3-beautify-report-error "msg.bad.prop")))
    (js3-beautify-node-set-prop fn 'GETTER_SETTER type)  ; for codegen
    (setq end (js3-beautify-node-end fn)
          result (make-js3-beautify-getter-setter-node :type type
                                              :pos pos
                                              :len (- end pos)
                                              :left prop
                                              :right fn))
    (js3-beautify-node-add-children result prop fn)
    result))

(defun js3-beautify-create-name-node (&optional check-activation-p token)
  "Create a name node using the token info from last scanned name.
In some cases we need to either synthesize a name node, or we lost
the name token information by peeking.  If the TOKEN parameter is
not `js3-beautify-NAME', then we use the token info saved in instance vars."
  (let ((beg js3-beautify-token-beg)
        (s js3-beautify-ts-string)
        name)
    (when (/= js3-beautify-current-token js3-beautify-NAME)
      (setq beg (or js3-beautify-prev-name-token-start js3-beautify-ts-cursor)
            s js3-beautify-prev-name-token-string
            js3-beautify-prev-name-token-start nil
            js3-beautify-prev-name-token-string nil))
    (setq name (make-js3-beautify-name-node :pos beg
                                   :name s
                                   :len (length s)))
    (if check-activation-p
        (js3-beautify-check-activation-name s (or token js3-beautify-NAME)))
    name))

(provide 'js3-beautify-parse)

;;; js3-beautify-parse.el ends here
