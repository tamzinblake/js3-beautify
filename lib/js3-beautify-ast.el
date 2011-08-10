;;; js3-beautify-ast.el --- JavaScript syntax tree node definitions

;;; Code:

(eval-and-compile
  (require 'cl))


(defsubst js3-beautify-relpos (pos anchor)
  "Convert POS to be relative to ANCHOR.
If POS is nil, returns nil."
  (and pos (- pos anchor)))

(defun js3-beautify-visit-ast (node callback)
  "Visit every node in ast NODE with visitor CALLBACK.

CALLBACK is a function that takes two arguments:  (NODE END-P).  It is
called twice:  once to visit the node, and again after all the node's
children have been processed.  The END-P argument is nil on the first
call and non-nil on the second call.  The return value of the callback
affects the traversal:  if non-nil, the children of NODE are processed.
If the callback returns nil, or if the node has no children, then the
callback is called immediately with a non-nil END-P argument.

The node traversal is approximately lexical-order, although there
are currently no guarantees around this."
  (when node
    (let ((vfunc (get (aref node 0) 'js3-beautify-visitor)))
      ;; visit the node
      (when  (funcall callback node nil)
	;; visit the kids
	(cond
	 ((eq vfunc 'js3-beautify-visit-none)
	  nil)                            ; don't even bother calling it
	 ;; Each AST node type has to define a `js3-beautify-visitor' function
	 ;; that takes a node and a callback, and calls `js3-beautify-visit-ast'
	 ;; on each child of the node.
	 (vfunc
	  (funcall vfunc node callback))
	 (t
	  (error "%s does not define a visitor-traversal function"
		 (aref node 0)))))
      ;; call the end-visit
      (funcall callback node t))))

(defstruct (js3-beautify-node
            (:constructor nil))  ; abstract
  "Base AST node type."
  (type -1)  ; token type
  (pos -1)   ; start position of this AST node in parsed input
  (abs -1)   ; absolute start of node, saved
  (len 1)    ; num characters spanned by the node
  props      ; optional node property list (an alist)
  parent)    ; link to parent node; null for root

(defsubst js3-beautify-node-get-prop (node prop &optional default)
  (or (cadr (assoc prop (js3-beautify-node-props node))) default))

(defsubst js3-beautify-node-set-prop (node prop value)
  (setf (js3-beautify-node-props node)
        (cons (list prop value) (js3-beautify-node-props node))))

(defsubst js3-beautify-fixup-starts (n nodes)
  "Adjust the start positions of NODES to be relative to N.
Any node in the list may be nil, for convenience."
  (dolist (node nodes)
    (when node
      (setf (js3-beautify-node-abs node) (js3-beautify-node-pos node))
      (setf (js3-beautify-node-pos node) (- (js3-beautify-node-pos node)
                                   (js3-beautify-node-pos n))))))

(defsubst js3-beautify-node-add-children (parent &rest nodes)
  "Set parent node of NODES to PARENT, and return PARENT.
Does nothing if we're not recording parent links.
If any given node in NODES is nil, doesn't record that link."
  (js3-beautify-fixup-starts parent nodes)
  (dolist (node nodes)
    (and node
         (setf (js3-beautify-node-parent node) parent))))

;; Non-recursive since it's called a frightening number of times.
(defsubst js3-beautify-node-abs-pos (n)
  (let ((pos (js3-beautify-node-pos n)))
    (while (setq n (js3-beautify-node-parent n))
      (setq pos (+ pos (js3-beautify-node-pos n))))
    pos))

(defsubst js3-beautify-node-abs-end (n)
  "Return absolute buffer position of end of N."
  (+ (js3-beautify-node-abs-pos n) (js3-beautify-node-len n)))

;; It's important to make sure block nodes have a lisp list for the
;; child nodes, to limit printing recursion depth in an AST that
;; otherwise consists of defstruct vectors.  Emacs will crash printing
;; a sufficiently large vector tree.

(defstruct (js3-beautify-block-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-block-node
			  (&key (type js3-beautify-BLOCK)
				(pos js3-beautify-token-beg)
				len
				props
				kids)))
  "A block of statements."
  kids)  ; a lisp list of the child statement nodes

(put 'cl-struct-js3-beautify-block-node 'js3-beautify-visitor 'js3-beautify-visit-block)
(put 'cl-struct-js3-beautify-block-node 'js3-beautify-printer 'js3-beautify-print-block)

(defsubst js3-beautify-visit-block (ast callback)
  "Visit the `js3-beautify-block-node' children of AST."
  (dolist (kid (js3-beautify-block-node-kids ast))
    (js3-beautify-visit-ast kid callback)))

(defun js3-beautify-print-block (n i)
  (js3-beautify-concat-curstr "{\n")
  (dolist (kid (js3-beautify-block-node-kids n))
    (js3-beautify-print-ast kid (1+ i)))
  (js3-beautify-concat-curstr "}\n"))

(defstruct (js3-beautify-scope
            (:include js3-beautify-block-node)
            (:constructor nil)
            (:constructor make-js3-beautify-scope
			  (&key (type js3-beautify-BLOCK)
				(pos js3-beautify-token-beg)
				len
				kids)))
  ;; The symbol-table is a LinkedHashMap<String,Symbol> in Rhino.
  ;; I don't have one of those handy, so I'll use an alist for now.
  ;; It's as fast as an emacs hashtable for up to about 50 elements,
  ;; and is much lighter-weight to construct (both CPU and mem).
  ;; The keys are interned strings (symbols) for faster lookup.
  ;; Should switch to hybrid alist/hashtable eventually.
  symbol-table  ; an alist of (symbol . js3-beautify-symbol)
  parent-scope  ; a `js3-beautify-scope'
  top)          ; top-level `js3-beautify-scope' (script/function)

(put 'cl-struct-js3-beautify-scope 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-scope 'js3-beautify-printer 'js3-beautify-print-none)

(defun js3-beautify-scope-set-parent-scope (scope parent)
  (setf (js3-beautify-scope-parent-scope scope) parent
        (js3-beautify-scope-top scope) (if (null parent)
                                  scope
                                (js3-beautify-scope-top parent))))

(defun js3-beautify-node-get-enclosing-scope (node)
  "Return the innermost `js3-beautify-scope' node surrounding NODE.
Returns nil if there is no enclosing scope node."
  (let ((parent (js3-beautify-node-parent node)))
    (while (not (js3-beautify-scope-p parent))
      (setq parent (js3-beautify-node-parent parent)))
    parent))

(defun js3-beautify-get-defining-scope (scope name)
  "Search up scope chain from SCOPE looking for NAME, a string or symbol.
Returns `js3-beautify-scope' in which NAME is defined, or nil if not found."
  (let ((sym (if (symbolp name)
                 name
               (intern name)))
        table
        result
        (continue t))
    (while (and scope continue)
      (if (and (setq table (js3-beautify-scope-symbol-table scope))
               (assq sym table))
          (setq continue nil
                result scope)
        (setq scope (js3-beautify-scope-parent-scope scope))))
    result))

(defsubst js3-beautify-scope-get-symbol (scope name)
  "Return symbol table entry for NAME in SCOPE.
NAME can be a string or symbol.   Returns a `js3-beautify-symbol' or nil if not found."
  (and (js3-beautify-scope-symbol-table scope)
       (cdr (assq (if (symbolp name)
                      name
                    (intern name))
                  (js3-beautify-scope-symbol-table scope)))))

(defsubst js3-beautify-scope-put-symbol (scope name symbol)
  "Enter SYMBOL into symbol-table for SCOPE under NAME.
NAME can be a lisp symbol or string.  SYMBOL is a `js3-beautify-symbol'."
  (let* ((table (js3-beautify-scope-symbol-table scope))
         (sym (if (symbolp name) name (intern name)))
         (entry (assq sym table)))
    (if entry
        (setcdr entry symbol)
      (push (cons sym symbol)
            (js3-beautify-scope-symbol-table scope)))))

(defstruct (js3-beautify-symbol
            (:constructor nil)
            (:constructor make-js3-beautify-symbol (decl-type name &optional ast-node)))
  "A symbol table entry."
  ;; One of js3-beautify-FUNCTION, js3-beautify-LP (for parameters), js3-beautify-VAR,
  ;; js3-beautify-LET, or js3-beautify-CONST
  decl-type
  name  ; string
  ast-node) ; a `js3-beautify-node'

(defstruct (js3-beautify-error-node
            (:include js3-beautify-node)
            (:constructor nil) ; silence emacs21 byte-compiler
            (:constructor make-js3-beautify-error-node (&key (type js3-beautify-ERROR)
                                                    (pos js3-beautify-token-beg)
                                                    len)))
  "AST node representing a parse error.")

(put 'cl-struct-js3-beautify-error-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-error-node 'js3-beautify-printer 'js3-beautify-print-none)

(defstruct (js3-beautify-script-node
            (:include js3-beautify-scope)
            (:constructor nil)
            (:constructor make-js3-beautify-script-node (&key (type js3-beautify-SCRIPT)
                                                     (pos js3-beautify-token-beg)
                                                     len
                                                     var-decls
                                                     fun-decls)))
  functions   ; lisp list of nested functions
  regexps     ; lisp list of (string . flags)
  symbols     ; alist (every symbol gets unique index)
  (param-count 0)
  var-names   ; vector of string names
  consts      ; bool-vector matching var-decls
  (temp-number 0))  ; for generating temp variables

(put 'cl-struct-js3-beautify-script-node 'js3-beautify-visitor 'js3-beautify-visit-block)
(put 'cl-struct-js3-beautify-script-node 'js3-beautify-printer 'js3-beautify-print-script)

(defun js3-beautify-print-script (node indent)
  (dolist (kid (js3-beautify-block-node-kids node))
    (js3-beautify-print-ast kid indent)))

(defstruct (js3-beautify-ast-root
            (:include js3-beautify-script-node)
            (:constructor nil)
            (:constructor make-js3-beautify-ast-root (&key (type js3-beautify-SCRIPT)
                                                  (pos js3-beautify-token-beg)
                                                  len
                                                  buffer)))
  "The root node of a js3 AST."
  buffer         ; the source buffer from which the code was parsed
  comments       ; a lisp list of comments, ordered by start position
  errors         ; a lisp list of errors found during parsing
  warnings       ; a lisp list of warnings found during parsing
  node-count)    ; number of nodes in the tree, including the root

(put 'cl-struct-js3-beautify-ast-root 'js3-beautify-visitor 'js3-beautify-visit-ast-root)
(put 'cl-struct-js3-beautify-ast-root 'js3-beautify-printer 'js3-beautify-print-script)

(defun js3-beautify-visit-ast-root (ast callback)
  (dolist (kid (js3-beautify-ast-root-kids ast))
    (js3-beautify-visit-ast kid callback))
  (dolist (comment (js3-beautify-ast-root-comments ast))
    (js3-beautify-visit-ast comment callback)))

(defstruct (js3-beautify-comment-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-comment-node (&key (type js3-beautify-COMMENT)
                                                      (pos js3-beautify-token-beg)
                                                      len
                                                      (format js3-beautify-ts-comment-type))))
  format)  ; 'line, 'block, 'jsdoc or 'html

(put 'cl-struct-js3-beautify-comment-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-comment-node 'js3-beautify-printer 'js3-beautify-print-comment)

(defun js3-beautify-print-comment (n i)
  ;; We really ought to link end-of-line comments to their nodes.
  ;; Or maybe we could add a new comment type, 'endline.
  (js3-beautify-concat-curstr (js3-beautify-node-string n)))

(defstruct (js3-beautify-expr-stmt-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-expr-stmt-node
			  (&key (type js3-beautify-EXPR_VOID)
				(pos js3-beautify-ts-cursor)
				len
				expr)))
  "An expression statement."
  expr)

(defsubst js3-beautify-expr-stmt-node-set-has-result (node)
  "Change the node type to `js3-beautify-EXPR_RESULT'.  Used for code generation."
  (setf (js3-beautify-node-type node) js3-beautify-EXPR_RESULT))

(put 'cl-struct-js3-beautify-expr-stmt-node 'js3-beautify-visitor 'js3-beautify-visit-expr-stmt-node)
(put 'cl-struct-js3-beautify-expr-stmt-node 'js3-beautify-printer 'js3-beautify-print-expr-stmt-node)

(defun js3-beautify-visit-expr-stmt-node (n v)
  (js3-beautify-visit-ast (js3-beautify-expr-stmt-node-expr n) v))

(defun js3-beautify-print-expr-stmt-node (n indent)
  (let ((expr (js3-beautify-expr-stmt-node-expr n)))
    (let ((type (js3-beautify-node-type expr)))
      (if (= js3-beautify-CALL type)
	  (let ((target (js3-beautify-call-node-target expr)))
	    (if (= js3-beautify-GETPROP (js3-beautify-node-type target))
		(let ((left (js3-beautify-prop-get-node-left target)))
		  (if (or (= js3-beautify-ARRAYLIT
			     (js3-beautify-node-type left))
			  (= js3-beautify-LP
			     (js3-beautify-node-type left)))
		      (js3-beautify-concat-curstr ";"))))))
      (if (or (= type js3-beautify-NOT)
	      (= type js3-beautify-BITNOT)
	      (= type js3-beautify-POS)
	      (= type js3-beautify-NEG)
	      (= type js3-beautify-INC)
	      (= type js3-beautify-DEC)
	      (= type js3-beautify-TYPEOF)
	      (= type js3-beautify-DELPROP))
	  (js3-beautify-concat-curstr ";"))))
  (js3-beautify-print-ast (js3-beautify-expr-stmt-node-expr n) indent)
  (when (/= js3-beautify-CASE
	    (js3-beautify-node-type (js3-beautify-node-parent n)))
    (js3-beautify-concat-curstr "\n")
    (if (= js3-beautify-VAR
	   (js3-beautify-node-type (js3-beautify-expr-stmt-node-expr n)))
	(js3-beautify-concat-curstr "\n"))))

(defstruct (js3-beautify-loop-node
            (:include js3-beautify-scope)
            (:constructor nil))
  "Abstract supertype of loop nodes."
  body      ; a `js3-beautify-block-node'
  lp        ; position of left-paren, nil if omitted
  rp)       ; position of right-paren, nil if omitted

(defstruct (js3-beautify-do-node
            (:include js3-beautify-loop-node)
            (:constructor nil)
            (:constructor make-js3-beautify-do-node (&key (type js3-beautify-DO)
                                                 (pos js3-beautify-token-beg)
                                                 len
                                                 body
                                                 condition
                                                 while-pos
                                                 lp
                                                 rp)))
  "AST node for do-loop."
  condition  ; while (expression)
  while-pos) ; buffer position of 'while' keyword

(put 'cl-struct-js3-beautify-do-node 'js3-beautify-visitor 'js3-beautify-visit-do-node)
(put 'cl-struct-js3-beautify-do-node 'js3-beautify-printer 'js3-beautify-print-do-node)

(defun js3-beautify-visit-do-node (n v)
  (js3-beautify-visit-ast (js3-beautify-do-node-body n) v)
  (js3-beautify-visit-ast (js3-beautify-do-node-condition n) v))

(defun js3-beautify-print-do-node (n i)
  (js3-beautify-concat-curstr "do {\n")
  (dolist (kid (js3-beautify-block-node-kids (js3-beautify-do-node-body n)))
    (js3-beautify-print-ast kid (1+ i)))
  (js3-beautify-concat-curstr "} while (")
  (js3-beautify-print-ast (js3-beautify-do-node-condition n) 0)
  (js3-beautify-concat-curstr ")"))

(defstruct (js3-beautify-while-node
            (:include js3-beautify-loop-node)
            (:constructor nil)
            (:constructor make-js3-beautify-while-node (&key (type js3-beautify-WHILE)
                                                    (pos js3-beautify-token-beg)
                                                    len
                                                    body
                                                    condition
                                                    lp
                                                    rp)))
  "AST node for while-loop."
  condition)    ; while-condition

(put 'cl-struct-js3-beautify-while-node 'js3-beautify-visitor 'js3-beautify-visit-while-node)
(put 'cl-struct-js3-beautify-while-node 'js3-beautify-printer 'js3-beautify-print-while-node)

(defun js3-beautify-visit-while-node (n v)
  (js3-beautify-visit-ast (js3-beautify-while-node-condition n) v)
  (js3-beautify-visit-ast (js3-beautify-while-node-body n) v))

(defun js3-beautify-print-while-node (n i)
  (js3-beautify-concat-curstr "while (")
  (js3-beautify-print-ast (js3-beautify-while-node-condition n) 0)
  (js3-beautify-concat-curstr ") {\n")
  (js3-beautify-print-body (js3-beautify-while-node-body n) (1+ i))
  (js3-beautify-concat-curstr "}\n"))

(defstruct (js3-beautify-for-node
            (:include js3-beautify-loop-node)
            (:constructor nil)
            (:constructor make-js3-beautify-for-node (&key (type js3-beautify-FOR)
                                                  (pos js3-beautify-ts-cursor)
                                                  len
                                                  body
                                                  init
                                                  condition
                                                  update
                                                  lp
                                                  rp)))
  "AST node for a C-style for-loop."
  init       ; initialization expression
  condition  ; loop condition
  update)    ; update clause

(put 'cl-struct-js3-beautify-for-node 'js3-beautify-visitor 'js3-beautify-visit-for-node)
(put 'cl-struct-js3-beautify-for-node 'js3-beautify-printer 'js3-beautify-print-for-node)

(defun js3-beautify-visit-for-node (n v)
  (js3-beautify-visit-ast (js3-beautify-for-node-init n) v)
  (js3-beautify-visit-ast (js3-beautify-for-node-condition n) v)
  (js3-beautify-visit-ast (js3-beautify-for-node-update n) v)
  (js3-beautify-visit-ast (js3-beautify-for-node-body n) v))

(defun js3-beautify-print-for-node (n i)
  (js3-beautify-concat-curstr "for (")
  (js3-beautify-print-ast (js3-beautify-for-node-init n) 0)
  (js3-beautify-concat-curstr "; ")
  (js3-beautify-print-ast (js3-beautify-for-node-condition n) 0)
  (js3-beautify-concat-curstr "; ")
  (js3-beautify-print-ast (js3-beautify-for-node-update n) 0)
  (js3-beautify-concat-curstr ") {\n")
  (js3-beautify-print-body (js3-beautify-for-node-body n) (1+ i))
  (js3-beautify-concat-curstr "}\n"))

(defstruct (js3-beautify-for-in-node
            (:include js3-beautify-loop-node)
            (:constructor nil)
            (:constructor make-js3-beautify-for-in-node
			  (&key (type js3-beautify-FOR)
				(pos js3-beautify-ts-cursor)
				len
				body
				iterator
				object
				in-pos
				each-pos
				foreach-p
				lp
				rp)))
  "AST node for a for..in loop."
  iterator  ; [var] foo in ...
  object    ; object over which we're iterating
  in-pos    ; buffer position of 'in' keyword
  each-pos  ; buffer position of 'each' keyword, if foreach-p
  foreach-p) ; t if it's a for-each loop

(put 'cl-struct-js3-beautify-for-in-node 'js3-beautify-visitor 'js3-beautify-visit-for-in-node)
(put 'cl-struct-js3-beautify-for-in-node 'js3-beautify-printer 'js3-beautify-print-for-in-node)

(defun js3-beautify-visit-for-in-node (n v)
  (js3-beautify-visit-ast (js3-beautify-for-in-node-iterator n) v)
  (js3-beautify-visit-ast (js3-beautify-for-in-node-object n) v)
  (js3-beautify-visit-ast (js3-beautify-for-in-node-body n) v))

(defun js3-beautify-print-for-in-node (n i)
  (js3-beautify-concat-curstr "for ")
  (if (js3-beautify-for-in-node-foreach-p n)
      (js3-beautify-concat-curstr "each "))
  (js3-beautify-concat-curstr "(")
  (js3-beautify-print-ast (js3-beautify-for-in-node-iterator n) 0)
  (js3-beautify-concat-curstr " in ")
  (js3-beautify-print-ast (js3-beautify-for-in-node-object n) 0)
  (js3-beautify-concat-curstr ") {\n")
  (js3-beautify-print-body (js3-beautify-for-in-node-body n) (1+ i))
  (js3-beautify-concat-curstr "}\n"))

(defstruct (js3-beautify-return-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-return-node
			  (&key (type js3-beautify-RETURN)
				(pos js3-beautify-ts-cursor)
				len
				retval)))
  "AST node for a return statement."
  retval)  ; expression to return, or 'undefined

(put 'cl-struct-js3-beautify-return-node 'js3-beautify-visitor 'js3-beautify-visit-return-node)
(put 'cl-struct-js3-beautify-return-node 'js3-beautify-printer 'js3-beautify-print-return-node)

(defun js3-beautify-visit-return-node (n v)
  (js3-beautify-visit-ast (js3-beautify-return-node-retval n) v))

(defun js3-beautify-print-return-node (n i)
  (js3-beautify-concat-curstr "return ")
  (if (js3-beautify-return-node-retval n)
      (js3-beautify-print-ast (js3-beautify-return-node-retval n) 0)
    (js3-beautify-concat-curstr ";"))
  (js3-beautify-concat-curstr "\n"))

(defstruct (js3-beautify-if-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-if-node (&key (type js3-beautify-IF)
                                                 (pos js3-beautify-ts-cursor)
                                                 len
                                                 condition
                                                 then-part
                                                 else-pos
                                                 else-part
                                                 lp
                                                 rp)))
  "AST node for an if-statement."
  condition   ; expression
  then-part   ; statement or block
  else-pos    ; optional buffer position of 'else' keyword
  else-part   ; optional statement or block
  lp          ; position of left-paren, nil if omitted
  rp)         ; position of right-paren, nil if omitted

(put 'cl-struct-js3-beautify-if-node 'js3-beautify-visitor 'js3-beautify-visit-if-node)
(put 'cl-struct-js3-beautify-if-node 'js3-beautify-printer 'js3-beautify-print-if-node)

(defun js3-beautify-visit-if-node (n v)
  (js3-beautify-visit-ast (js3-beautify-if-node-condition n) v)
  (js3-beautify-visit-ast (js3-beautify-if-node-then-part n) v)
  (js3-beautify-visit-ast (js3-beautify-if-node-else-part n) v))

(defun js3-beautify-print-if-node (n i)
  (js3-beautify-concat-curstr "if ( ")
  (js3-beautify-print-ast (js3-beautify-if-node-condition n) 0)
  (js3-beautify-concat-curstr " ) {\n")
  (js3-beautify-print-body (js3-beautify-if-node-then-part n) (1+ i))
  (js3-beautify-concat-curstr "}\n")
  (cond
   ((not (js3-beautify-if-node-else-part n))
    (js3-beautify-concat-curstr " "))
   ((js3-beautify-if-node-p (js3-beautify-if-node-else-part n))
    (js3-beautify-concat-curstr " else ")
    (js3-beautify-print-body (js3-beautify-if-node-else-part n) i))
   (t
    (js3-beautify-concat-curstr " else {\n")
    (js3-beautify-print-body (js3-beautify-if-node-else-part n) (1+ i))
    (js3-beautify-concat-curstr "}\n"))))

(defstruct (js3-beautify-try-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-try-node (&key (type js3-beautify-TRY)
                                                  (pos js3-beautify-ts-cursor)
                                                  len
                                                  try-block
                                                  catch-clauses
                                                  finally-block)))
  "AST node for a try-statement."
  try-block
  catch-clauses  ; a lisp list of `js3-beautify-catch-node'
  finally-block) ; a `js3-beautify-finally-node'

(put 'cl-struct-js3-beautify-try-node 'js3-beautify-visitor 'js3-beautify-visit-try-node)
(put 'cl-struct-js3-beautify-try-node 'js3-beautify-printer 'js3-beautify-print-try-node)

(defun js3-beautify-visit-try-node (n v)
  (js3-beautify-visit-ast (js3-beautify-try-node-try-block n) v)
  (dolist (clause (js3-beautify-try-node-catch-clauses n))
    (js3-beautify-visit-ast clause v))
  (js3-beautify-visit-ast (js3-beautify-try-node-finally-block n) v))

(defun js3-beautify-print-try-node (n i)
  (let ((catches (js3-beautify-try-node-catch-clauses n))
        (finally (js3-beautify-try-node-finally-block n)))
    (js3-beautify-concat-curstr "try {\n")
    (js3-beautify-print-body (js3-beautify-try-node-try-block n) (1+ i))
    (js3-beautify-concat-curstr "}\n")
    (when catches
      (dolist (catch catches)
        (js3-beautify-print-ast catch i)))
    (if finally
        (js3-beautify-print-ast finally i)
      (js3-beautify-concat-curstr ""))))

(defstruct (js3-beautify-catch-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-catch-node
			  (&key (type js3-beautify-CATCH)
				(pos js3-beautify-ts-cursor)
				len
				var-name
				guard-kwd
				guard-expr
				block
				lp
				rp)))
  "AST node for a catch clause."
  var-name    ; a `js3-beautify-name-node'
  guard-kwd   ; relative buffer position of "if" in "catch (x if ...)"
  guard-expr  ; catch condition, a `js3-beautify-node'
  block       ; statements, a `js3-beautify-block-node'
  lp          ; buffer position of left-paren, nil if omitted
  rp)         ; buffer position of right-paren, nil if omitted

(put 'cl-struct-js3-beautify-catch-node 'js3-beautify-visitor 'js3-beautify-visit-catch-node)
(put 'cl-struct-js3-beautify-catch-node 'js3-beautify-printer 'js3-beautify-print-catch-node)

(defun js3-beautify-visit-catch-node (n v)
  (js3-beautify-visit-ast (js3-beautify-catch-node-var-name n) v)
  (when (js3-beautify-catch-node-guard-kwd n)
    (js3-beautify-visit-ast (js3-beautify-catch-node-guard-expr n) v))
  (js3-beautify-visit-ast (js3-beautify-catch-node-block n) v))

(defun js3-beautify-print-catch-node (n i)
  (js3-beautify-concat-curstr " catch (")
  (js3-beautify-print-ast (js3-beautify-catch-node-var-name n) 0)
  (when (js3-beautify-catch-node-guard-kwd n)
    (js3-beautify-concat-curstr " if ")
    (js3-beautify-print-ast (js3-beautify-catch-node-guard-expr n) 0))
  (js3-beautify-concat-curstr ") {\n")
  (js3-beautify-print-body (js3-beautify-catch-node-block n) (1+ i))
  (js3-beautify-concat-curstr "}\n"))

(defstruct (js3-beautify-finally-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-finally-node
			  (&key (type js3-beautify-FINALLY)
				(pos js3-beautify-ts-cursor)
				len
				body)))
  "AST node for a finally clause."
  body)  ; a `js3-beautify-node', often but not always a block node

(put 'cl-struct-js3-beautify-finally-node 'js3-beautify-visitor 'js3-beautify-visit-finally-node)
(put 'cl-struct-js3-beautify-finally-node 'js3-beautify-printer 'js3-beautify-print-finally-node)

(defun js3-beautify-visit-finally-node (n v)
  (js3-beautify-visit-ast (js3-beautify-finally-node-body n) v))

(defun js3-beautify-print-finally-node (n i)
  (js3-beautify-concat-curstr " finally {\n")
  (js3-beautify-print-body (js3-beautify-finally-node-body n) (1+ i))
  (js3-beautify-concat-curstr "}\n"))

(defstruct (js3-beautify-switch-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-switch-node
			  (&key (type js3-beautify-SWITCH)
				(pos js3-beautify-ts-cursor)
				len
				discriminant
				cases
				lp
				rp)))
  "AST node for a switch statement."
  discriminant  ; a `js3-beautify-node' (switch expression)
  cases  ; a lisp list of `js3-beautify-case-node'
  lp     ; position of open-paren for discriminant, nil if omitted
  rp)    ; position of close-paren for discriminant, nil if omitted

(put 'cl-struct-js3-beautify-switch-node 'js3-beautify-visitor 'js3-beautify-visit-switch-node)
(put 'cl-struct-js3-beautify-switch-node 'js3-beautify-printer 'js3-beautify-print-switch-node)

(defun js3-beautify-visit-switch-node (n v)
  (js3-beautify-visit-ast (js3-beautify-switch-node-discriminant n) v)
  (dolist (c (js3-beautify-switch-node-cases n))
    (js3-beautify-visit-ast c v)))

(defun js3-beautify-print-switch-node (n i)
  (js3-beautify-concat-curstr "switch (")
  (js3-beautify-print-ast (js3-beautify-switch-node-discriminant n) 0)
  (js3-beautify-concat-curstr ") {")
  (dolist (case (js3-beautify-switch-node-cases n))
    (js3-beautify-print-ast case i))
  (js3-beautify-concat-curstr "\n}\n"))

(defstruct (js3-beautify-case-node
            (:include js3-beautify-block-node)
            (:constructor nil)
            (:constructor make-js3-beautify-case-node
			  (&key (type js3-beautify-CASE)
				(pos js3-beautify-ts-cursor)
				len
				kids
				expr)))
  "AST node for a case clause of a switch statement."
  expr)   ; the case expression (nil for default)

(put 'cl-struct-js3-beautify-case-node 'js3-beautify-visitor 'js3-beautify-visit-case-node)
(put 'cl-struct-js3-beautify-case-node 'js3-beautify-printer 'js3-beautify-print-case-node)

(defun js3-beautify-visit-case-node (n v)
  (js3-beautify-visit-ast (js3-beautify-case-node-expr n) v)
  (js3-beautify-visit-block n v))

(defun js3-beautify-print-case-node (n i)
  (if (null (js3-beautify-case-node-expr n))
      (js3-beautify-concat-curstr "\ndefault: ")
    (js3-beautify-concat-curstr "\ncase ")
    (js3-beautify-print-ast (js3-beautify-case-node-expr n) 0)
    (js3-beautify-concat-curstr ": "))
  (dolist (kid (js3-beautify-case-node-kids n))
    (js3-beautify-print-ast kid (1+ i))))

(defstruct (js3-beautify-throw-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-throw-node
			  (&key (type js3-beautify-THROW)
				(pos js3-beautify-ts-cursor)
				len
				expr)))
  "AST node for a throw statement."
  expr)   ; the expression to throw

(put 'cl-struct-js3-beautify-throw-node 'js3-beautify-visitor 'js3-beautify-visit-throw-node)
(put 'cl-struct-js3-beautify-throw-node 'js3-beautify-printer 'js3-beautify-print-throw-node)

(defun js3-beautify-visit-throw-node (n v)
  (js3-beautify-visit-ast (js3-beautify-throw-node-expr n) v))

(defun js3-beautify-print-throw-node (n i)
  (js3-beautify-concat-curstr "throw ")
  (js3-beautify-print-ast (js3-beautify-throw-node-expr n) 0)
  (js3-beautify-concat-curstr " "))

(defstruct (js3-beautify-with-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-with-node (&key (type js3-beautify-WITH)
                                                   (pos js3-beautify-ts-cursor)
                                                   len
                                                   object
                                                   body
                                                   lp
                                                   rp)))
  "AST node for a with-statement."
  object
  body
  lp    ; buffer position of left-paren around object, nil if omitted
  rp)   ; buffer position of right-paren around object, nil if omitted

(put 'cl-struct-js3-beautify-with-node 'js3-beautify-visitor 'js3-beautify-visit-with-node)
(put 'cl-struct-js3-beautify-with-node 'js3-beautify-printer 'js3-beautify-print-with-node)

(defun js3-beautify-visit-with-node (n v)
  (js3-beautify-visit-ast (js3-beautify-with-node-object n) v)
  (js3-beautify-visit-ast (js3-beautify-with-node-body n) v))

(defun js3-beautify-print-with-node (n i)
  (js3-beautify-concat-curstr "with (")
  (js3-beautify-print-ast (js3-beautify-with-node-object n) 0)
  (js3-beautify-concat-curstr ") {\n")
  (js3-beautify-print-body (js3-beautify-with-node-body n) (1+ i))
  (js3-beautify-concat-curstr "}\n"))

(defstruct (js3-beautify-label-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-label-node (&key (type js3-beautify-LABEL)
                                                    (pos js3-beautify-ts-cursor)
                                                    len
                                                    name)))
  "AST node for a statement label or case label."
  name   ; a string
  loop)  ; for validating and code-generating continue-to-label

(put 'cl-struct-js3-beautify-label-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-label-node 'js3-beautify-printer 'js3-beautify-print-label)

(defun js3-beautify-print-label (n i)
  (js3-beautify-concat-curstr (concat (js3-beautify-label-node-name n) ":")))

(defstruct (js3-beautify-labeled-stmt-node
            (:include js3-beautify-node)
            (:constructor nil)
            ;; type needs to be in `js3-beautify-side-effecting-tokens' to avoid spurious
            ;; no-side-effects warnings, hence js3-beautify-EXPR_RESULT.
            (:constructor make-js3-beautify-labeled-stmt-node (&key (type js3-beautify-EXPR_RESULT)
                                                           (pos js3-beautify-ts-cursor)
                                                           len
                                                           labels
                                                           stmt)))
  "AST node for a statement with one or more labels.
Multiple labels for a statement are collapsed into the labels field."
  labels  ; lisp list of `js3-beautify-label-node'
  stmt)   ; the statement these labels are for

(put 'cl-struct-js3-beautify-labeled-stmt-node 'js3-beautify-visitor 'js3-beautify-visit-labeled-stmt)
(put 'cl-struct-js3-beautify-labeled-stmt-node 'js3-beautify-printer 'js3-beautify-print-labeled-stmt)

(defun js3-beautify-get-label-by-name (lbl-stmt name)
  "Return a `js3-beautify-label-node' by NAME from LBL-STMT's labels list.
Returns nil if no such label is in the list."
  (let ((label-list (js3-beautify-labeled-stmt-node-labels lbl-stmt))
        result)
    (while (and label-list (not result))
      (if (string= (js3-beautify-label-node-name (car label-list)) name)
          (setq result (car label-list))
        (setq label-list (cdr label-list))))
    result))

(defun js3-beautify-visit-labeled-stmt (n v)
  (dolist (label (js3-beautify-labeled-stmt-node-labels n))
    (js3-beautify-visit-ast label v))
  (js3-beautify-visit-ast (js3-beautify-labeled-stmt-node-stmt n) v))

(defun js3-beautify-print-labeled-stmt (n i)
  (dolist (label (js3-beautify-labeled-stmt-node-labels n))
    (js3-beautify-print-ast label i))
  (js3-beautify-print-ast (js3-beautify-labeled-stmt-node-stmt n) (1+ i)))

(defun js3-beautify-labeled-stmt-node-contains (node label)
  "Return t if NODE contains LABEL in its label set.
NODE is a `js3-beautify-labels-node'.  LABEL is an identifier."
  (loop for nl in (js3-beautify-labeled-stmt-node-labels node)
        if (string= label (js3-beautify-label-node-name nl))
        return t
        finally return nil))

(defsubst js3-beautify-labeled-stmt-node-add-label (node label)
  "Add a `js3-beautify-label-node' to the label set for this statement."
  (setf (js3-beautify-labeled-stmt-node-labels node)
        (nconc (js3-beautify-labeled-stmt-node-labels node) (list label))))

(defstruct (js3-beautify-jump-node
            (:include js3-beautify-node)
            (:constructor nil))
  "Abstract supertype of break and continue nodes."
  label   ; `js3-beautify-name-node' for location of label identifier, if present
  target) ; target js3-beautify-labels-node or loop/switch statement

(defun js3-beautify-visit-jump-node (n v)
  (js3-beautify-visit-ast (js3-beautify-jump-node-label n) v))

(defstruct (js3-beautify-break-node
            (:include js3-beautify-jump-node)
            (:constructor nil)
            (:constructor make-js3-beautify-break-node (&key (type js3-beautify-BREAK)
                                                    (pos js3-beautify-ts-cursor)
                                                    len
                                                    label
                                                    target)))
  "AST node for a break statement.
The label field is a `js3-beautify-name-node', possibly nil, for the named label
if provided.  E.g. in 'break foo', it represents 'foo'.  The target field
is the target of the break - a label node or enclosing loop/switch statement.")

(put 'cl-struct-js3-beautify-break-node 'js3-beautify-visitor 'js3-beautify-visit-jump-node)
(put 'cl-struct-js3-beautify-break-node 'js3-beautify-printer 'js3-beautify-print-break-node)

(defun js3-beautify-print-break-node (n i)
  (js3-beautify-concat-curstr "; break")
  (when (js3-beautify-break-node-label n)
    (js3-beautify-concat-curstr " ")
    (js3-beautify-print-ast (js3-beautify-break-node-label n) 0)))

(defstruct (js3-beautify-continue-node
            (:include js3-beautify-jump-node)
            (:constructor nil)
            (:constructor make-js3-beautify-continue-node
			  (&key (type js3-beautify-CONTINUE)
				(pos js3-beautify-ts-cursor)
				len
				label
				target)))
  "AST node for a continue statement.
The label field is the user-supplied enclosing label name, a `js3-beautify-name-node'.
It is nil if continue specifies no label.  The target field is the jump target:
a `js3-beautify-label-node' or the innermost enclosing loop.")

(put 'cl-struct-js3-beautify-continue-node 'js3-beautify-visitor 'js3-beautify-visit-jump-node)
(put 'cl-struct-js3-beautify-continue-node 'js3-beautify-printer 'js3-beautify-print-continue-node)

(defun js3-beautify-print-continue-node (n i)
  (js3-beautify-concat-curstr "; continue")
  (when (js3-beautify-continue-node-label n)
    (js3-beautify-concat-curstr " ")
    (js3-beautify-print-ast (js3-beautify-continue-node-label n) 0)))

(defstruct (js3-beautify-function-node
            (:include js3-beautify-script-node)
            (:constructor nil)
            (:constructor make-js3-beautify-function-node
			  (&key (type js3-beautify-FUNCTION)
				(pos js3-beautify-ts-cursor)
				len
				(ftype 'FUNCTION)
				(form 'FUNCTION_STATEMENT)
				(name "")
				params
				body
				lp
				rp)))
  "AST node for a function declaration.
The `params' field is a lisp list of nodes.  Each node is either a simple
`js3-beautify-name-node', or if it's a destructuring-assignment parameter, a
`js3-beautify-array-node' or `js3-beautify-object-node'."
  ftype            ; FUNCTION, GETTER or SETTER
  form             ; FUNCTION_{STATEMENT|EXPRESSION|EXPRESSION_STATEMENT}
  name             ; function name (a `js3-beautify-name-node', or nil if anonymous)
  params           ; a lisp list of destructuring forms or simple name nodes
  body             ; a `js3-beautify-block-node' or expression node (1.8 only)
  lp               ; position of arg-list open-paren, or nil if omitted
  rp               ; position of arg-list close-paren, or nil if omitted
  ignore-dynamic   ; ignore value of the dynamic-scope flag (interpreter only)
  needs-activation ; t if we need an activation object for this frame
  is-generator     ; t if this function contains a yield
  member-expr)     ; nonstandard Ecma extension from Rhino

(put 'cl-struct-js3-beautify-function-node 'js3-beautify-visitor 'js3-beautify-visit-function-node)
(put 'cl-struct-js3-beautify-function-node 'js3-beautify-printer 'js3-beautify-print-function-node)

(defun js3-beautify-visit-function-node (n v)
  (js3-beautify-visit-ast (js3-beautify-function-node-name n) v)
  (dolist (p (js3-beautify-function-node-params n))
    (js3-beautify-visit-ast p v))
  (js3-beautify-visit-ast (js3-beautify-function-node-body n) v))

(defun js3-beautify-print-function-node (n i)
  (let ((getter (js3-beautify-node-get-prop n 'GETTER_SETTER))
        (name (js3-beautify-function-node-name n))
        (params (js3-beautify-function-node-params n))
        (body (js3-beautify-function-node-body n))
        (expr (eq (js3-beautify-function-node-form n) 'FUNCTION_EXPRESSION)))
    (unless expr
      (js3-beautify-concat-curstr "\n"))
    (unless getter
      (js3-beautify-concat-curstr "function"))
    (when name
      (js3-beautify-concat-curstr " ")
      (js3-beautify-print-ast name 0))
    (js3-beautify-concat-curstr " (")
    (loop with len = (length params)
          for param in params
          for count from 1
          do
          (js3-beautify-print-ast param 0)
          (if (< count len)
              (js3-beautify-concat-curstr ", ")))
    (js3-beautify-concat-curstr ") {\n")
    (js3-beautify-print-body body (1+ i))
    (js3-beautify-concat-curstr "}")
    (unless expr
      (js3-beautify-concat-curstr "\n"))))

(defsubst js3-beautify-function-name (node)
  "Return function name for NODE, a `js3-beautify-function-node', or nil if anonymous."
  (and (js3-beautify-function-node-name node)
       (js3-beautify-name-node-name (js3-beautify-function-node-name node))))

;; Having this be an expression node makes it more flexible.
;; There are IDE contexts, such as indentation in a for-loop initializer,
;; that work better if you assume it's an expression.  Whenever we have
;; a standalone var/const declaration, we just wrap with an expr stmt.
;; Eclipse apparently screwed this up and now has two versions, expr and stmt.
(defstruct (js3-beautify-var-decl-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-var-decl-node
			  (&key (type js3-beautify-VAR)
				(pos js3-beautify-token-beg)
				len
				kids
				decl-type)))
  "AST node for a variable declaration list (VAR, CONST or LET).
The node bounds differ depending on the declaration type.  For VAR or
CONST declarations, the bounds include the var/const keyword.  For LET
declarations, the node begins at the position of the first child."
  kids        ; a lisp list of `js3-beautify-var-init-node' structs.
  decl-type)  ; js3-beautify-VAR, js3-beautify-CONST or js3-beautify-LET

(put 'cl-struct-js3-beautify-var-decl-node 'js3-beautify-visitor 'js3-beautify-visit-var-decl)
(put 'cl-struct-js3-beautify-var-decl-node 'js3-beautify-printer 'js3-beautify-print-var-decl)

(defun js3-beautify-visit-var-decl (n v)
  (dolist (kid (js3-beautify-var-decl-node-kids n))
    (js3-beautify-visit-ast kid v)))

(defun js3-beautify-print-var-decl (n i)
  (let ((tt (js3-beautify-var-decl-node-decl-type n)))
    (js3-beautify-concat-curstr
     (cond
      ((= tt js3-beautify-VAR) "var ")
      ((= tt js3-beautify-LET) "")  ; handled by parent let-{expr/stmt}
      ((= tt js3-beautify-CONST) "const ")
      (t
       (error "malformed var-decl node"))))
    (loop with kids = (js3-beautify-var-decl-node-kids n)
          with len = (length kids)
          for kid in kids
          for count from 1
          do
          (js3-beautify-print-ast kid 0)
          (if (< count len)
              (js3-beautify-concat-curstr "\n, ")))))

(defstruct (js3-beautify-var-init-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-var-init-node
			  (&key (type js3-beautify-VAR)
				(pos js3-beautify-ts-cursor)
				len
				target
				initializer)))
  "AST node for a variable declaration.
The type field will be js3-beautify-CONST for a const decl."
  target        ; `js3-beautify-name-node', `js3-beautify-object-node', or `js3-beautify-array-node'
  initializer)  ; initializer expression, a `js3-beautify-node'

(put 'cl-struct-js3-beautify-var-init-node 'js3-beautify-visitor 'js3-beautify-visit-var-init-node)
(put 'cl-struct-js3-beautify-var-init-node 'js3-beautify-printer 'js3-beautify-print-var-init-node)

(defun js3-beautify-visit-var-init-node (n v)
  (js3-beautify-visit-ast (js3-beautify-var-init-node-target n) v)
  (js3-beautify-visit-ast (js3-beautify-var-init-node-initializer n) v))

(defun js3-beautify-print-var-init-node (n i)
  (js3-beautify-print-ast (js3-beautify-var-init-node-target n) 0)
  (when (js3-beautify-var-init-node-initializer n)
    (js3-beautify-concat-curstr " = ")
    (js3-beautify-print-ast (js3-beautify-var-init-node-initializer n) 0)))

(defstruct (js3-beautify-cond-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-cond-node
			  (&key (type js3-beautify-HOOK)
				(pos js3-beautify-ts-cursor)
				len
				test-expr
				true-expr
				false-expr
				q-pos
				c-pos)))
  "AST node for the ternary operator"
  test-expr
  true-expr
  false-expr
  q-pos   ; buffer position of ?
  c-pos)  ; buffer position of :

(put 'cl-struct-js3-beautify-cond-node 'js3-beautify-visitor 'js3-beautify-visit-cond-node)
(put 'cl-struct-js3-beautify-cond-node 'js3-beautify-printer 'js3-beautify-print-cond-node)

(defun js3-beautify-visit-cond-node (n v)
  (js3-beautify-visit-ast (js3-beautify-cond-node-test-expr n) v)
  (js3-beautify-visit-ast (js3-beautify-cond-node-true-expr n) v)
  (js3-beautify-visit-ast (js3-beautify-cond-node-false-expr n) v))

(defun js3-beautify-print-cond-node (n i)
  (js3-beautify-print-ast (js3-beautify-cond-node-test-expr n) 0)
  (js3-beautify-concat-curstr " ? ")
  (js3-beautify-print-ast (js3-beautify-cond-node-true-expr n) 0)
  (js3-beautify-concat-curstr " : ")
  (js3-beautify-print-ast (js3-beautify-cond-node-false-expr n) 0))

(defstruct (js3-beautify-infix-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-infix-node (&key type
                                                    (pos js3-beautify-ts-cursor)
                                                    len
                                                    op-pos
                                                    left
                                                    right)))
  "Represents infix expressions.
Includes assignment ops like `|=', and the comma operator.
The type field inherited from `js3-beautify-node' holds the operator."
  op-pos    ; buffer position where operator begins
  left      ; any `js3-beautify-node'
  right)    ; any `js3-beautify-node'

(put 'cl-struct-js3-beautify-infix-node 'js3-beautify-visitor 'js3-beautify-visit-infix-node)
(put 'cl-struct-js3-beautify-infix-node 'js3-beautify-printer 'js3-beautify-print-infix-node)

(defun js3-beautify-visit-infix-node (n v)
  (js3-beautify-visit-ast (js3-beautify-infix-node-left n) v)
  (js3-beautify-visit-ast (js3-beautify-infix-node-right n) v))

(defconst js3-beautify-operator-tokens
  (let ((table (make-hash-table :test 'eq))
        (tokens
         (list (cons js3-beautify-IN "in")
               (cons js3-beautify-TYPEOF "typeof")
               (cons js3-beautify-INSTANCEOF "instanceof")
               (cons js3-beautify-DELPROP "delete")
               (cons js3-beautify-COMMA ",")
               (cons js3-beautify-COLON ":")
               (cons js3-beautify-OR "||")
               (cons js3-beautify-AND "&&")
               (cons js3-beautify-INC "++")
               (cons js3-beautify-DEC "--")
               (cons js3-beautify-BITOR "|")
               (cons js3-beautify-BITXOR "^")
               (cons js3-beautify-BITAND "&")
               (cons js3-beautify-EQ "==")
               (cons js3-beautify-NE "!=")
               (cons js3-beautify-LT "<")
               (cons js3-beautify-LE "<=")
               (cons js3-beautify-GT ">")
               (cons js3-beautify-GE ">=")
               (cons js3-beautify-LSH "<<")
               (cons js3-beautify-RSH ">>")
               (cons js3-beautify-URSH ">>>")
               (cons js3-beautify-ADD "+")       ; infix plus
               (cons js3-beautify-SUB "-")       ; infix minus
               (cons js3-beautify-MUL "*")
               (cons js3-beautify-DIV "/")
               (cons js3-beautify-MOD "%")
               (cons js3-beautify-NOT "!")
               (cons js3-beautify-BITNOT "~")
               (cons js3-beautify-POS "+")       ; unary plus
               (cons js3-beautify-NEG "-")       ; unary minus
               (cons js3-beautify-SHEQ "===")    ; shallow equality
               (cons js3-beautify-SHNE "!==")    ; shallow inequality
               (cons js3-beautify-ASSIGN "=")
               (cons js3-beautify-ASSIGN_BITOR "|=")
               (cons js3-beautify-ASSIGN_BITXOR "^=")
               (cons js3-beautify-ASSIGN_BITAND "&=")
               (cons js3-beautify-ASSIGN_LSH "<<=")
               (cons js3-beautify-ASSIGN_RSH ">>=")
               (cons js3-beautify-ASSIGN_URSH ">>>=")
               (cons js3-beautify-ASSIGN_ADD "+=")
               (cons js3-beautify-ASSIGN_SUB "-=")
               (cons js3-beautify-ASSIGN_MUL "*=")
               (cons js3-beautify-ASSIGN_DIV "/=")
               (cons js3-beautify-ASSIGN_MOD "%="))))
    (loop for (k . v) in tokens do
          (puthash k v table))
    table))

(defun js3-beautify-print-infix-node (n i)
  (let* ((tt (js3-beautify-node-type n))
         (op (gethash tt js3-beautify-operator-tokens)))
    (unless op
      (error "unrecognized infix operator %s" (js3-beautify-node-type n)))
    (js3-beautify-print-ast (js3-beautify-infix-node-left n) 0)
    (unless (= tt js3-beautify-COMMA)
      (js3-beautify-concat-curstr " "))
    (js3-beautify-concat-curstr op)
    (js3-beautify-concat-curstr " ")
    (js3-beautify-print-ast (js3-beautify-infix-node-right n) 0)))

(defstruct (js3-beautify-assign-node
            (:include js3-beautify-infix-node)
            (:constructor nil)
            (:constructor make-js3-beautify-assign-node (&key type
                                                     (pos js3-beautify-ts-cursor)
                                                     len
                                                     op-pos
                                                     left
                                                     right)))
  "Represents any assignment.
The type field holds the actual assignment operator.")

(put 'cl-struct-js3-beautify-assign-node 'js3-beautify-visitor 'js3-beautify-visit-infix-node)
(put 'cl-struct-js3-beautify-assign-node 'js3-beautify-printer 'js3-beautify-print-infix-node)

(defstruct (js3-beautify-unary-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-unary-node (&key type ; required
                                                    (pos js3-beautify-ts-cursor)
                                                    len
                                                    operand)))
  "AST node type for unary operator nodes.
The type field can be NOT, BITNOT, POS, NEG, INC, DEC,
TYPEOF, or DELPROP.  For INC or DEC, a 'postfix node
property is added if the operator follows the operand."
  operand)  ; a `js3-beautify-node' expression

(put 'cl-struct-js3-beautify-unary-node 'js3-beautify-visitor 'js3-beautify-visit-unary-node)
(put 'cl-struct-js3-beautify-unary-node 'js3-beautify-printer 'js3-beautify-print-unary-node)

(defun js3-beautify-visit-unary-node (n v)
  (js3-beautify-visit-ast (js3-beautify-unary-node-operand n) v))

(defun js3-beautify-print-unary-node (n i)
  (let* ((tt (js3-beautify-node-type n))
         (op (gethash tt js3-beautify-operator-tokens))
         (postfix (js3-beautify-node-get-prop n 'postfix)))
    (unless op
      (error "unrecognized unary operator %s" tt))
    (unless postfix
      (js3-beautify-concat-curstr op))
    (if (or (= tt js3-beautify-TYPEOF)
            (= tt js3-beautify-DELPROP))
        (js3-beautify-concat-curstr " "))
    (js3-beautify-print-ast (js3-beautify-unary-node-operand n) 0)
    (when postfix
      (js3-beautify-concat-curstr op))))

(defstruct (js3-beautify-let-node
            (:include js3-beautify-scope)
            (:constructor nil)
            (:constructor make-js3-beautify-let-node (&key (type js3-beautify-LETEXPR)
                                                  (pos js3-beautify-token-beg)
                                                  len
                                                  vars
                                                  body
                                                  lp
                                                  rp)))
  "AST node for a let expression or a let statement.
Note that a let declaration such as let x=6, y=7 is a `js3-beautify-var-decl-node'."
  vars   ; a `js3-beautify-var-decl-node'
  body   ; a `js3-beautify-node' representing the expression or body block
  lp
  rp)

(put 'cl-struct-js3-beautify-let-node 'js3-beautify-visitor 'js3-beautify-visit-let-node)
(put 'cl-struct-js3-beautify-let-node 'js3-beautify-printer 'js3-beautify-print-let-node)

(defun js3-beautify-visit-let-node (n v)
  (js3-beautify-visit-ast (js3-beautify-let-node-vars n) v)
  (js3-beautify-visit-ast (js3-beautify-let-node-body n) v))

(defun js3-beautify-print-let-node (n i)
  (js3-beautify-concat-curstr "let (")
  (js3-beautify-print-ast (js3-beautify-let-node-vars n) 0)
  (js3-beautify-concat-curstr ") ")
  (js3-beautify-print-ast (js3-beautify-let-node-body n) i))

(defstruct (js3-beautify-keyword-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-keyword-node (&key type
                                                      (pos js3-beautify-token-beg)
                                                      (len (- js3-beautify-ts-cursor pos)))))
  "AST node representing a literal keyword such as `null'.
Used for `null', `this', `true', `false' and `debugger'.
The node type is set to js3-beautify-NULL, js3-beautify-THIS, etc.")

(put 'cl-struct-js3-beautify-keyword-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-keyword-node 'js3-beautify-printer 'js3-beautify-print-keyword-node)

(defun js3-beautify-print-keyword-node (n i)
  (js3-beautify-concat-curstr
   (let ((tt (js3-beautify-node-type n)))
     (cond
      ((= tt 'js3-beautify-THIS) "this")
      ((= tt 'js3-beautify-NULL) "null")
      ((= tt 'js3-beautify-TRUE) "true")
      ((= tt 'js3-beautify-FALSE) "false")
      ((= tt 'js3-beautify-DEBUGGER) "debugger")
      (t (error "Invalid keyword literal type: %d" tt))))))

(defsubst js3-beautify-this-node-p (node)
  "Return t if this node is a `js3-beautify-literal-node' of type js3-beautify-THIS."
  (eq (js3-beautify-node-type node) js3-beautify-THIS))

(defstruct (js3-beautify-new-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-new-node (&key (type js3-beautify-NEW)
                                                  (pos js3-beautify-token-beg)
                                                  len
                                                  target
                                                  args
                                                  initializer
                                                  lp
                                                  rp)))
  "AST node for new-expression such as new Foo()."
  target  ; an identifier or reference
  args    ; a lisp list of argument nodes
  lp      ; position of left-paren, nil if omitted
  rp      ; position of right-paren, nil if omitted
  initializer) ; experimental Rhino syntax:  optional `js3-beautify-object-node'

(put 'cl-struct-js3-beautify-new-node 'js3-beautify-visitor 'js3-beautify-visit-new-node)
(put 'cl-struct-js3-beautify-new-node 'js3-beautify-printer 'js3-beautify-print-new-node)

(defun js3-beautify-visit-new-node (n v)
  (js3-beautify-visit-ast (js3-beautify-new-node-target n) v)
  (dolist (arg (js3-beautify-new-node-args n))
    (js3-beautify-visit-ast arg v))
  (js3-beautify-visit-ast (js3-beautify-new-node-initializer n) v))

(defun js3-beautify-print-new-node (n i)
  (js3-beautify-concat-curstr "new ")
  (js3-beautify-print-ast (js3-beautify-new-node-target n))
  (js3-beautify-concat-curstr "(")
  (js3-beautify-print-list (js3-beautify-new-node-args n))
  (js3-beautify-concat-curstr ")")
  (when (js3-beautify-new-node-initializer n)
    (js3-beautify-concat-curstr " ")
    (js3-beautify-print-ast (js3-beautify-new-node-initializer n))))

(defstruct (js3-beautify-name-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-name-node
			  (&key (type js3-beautify-NAME)
				(pos js3-beautify-token-beg)
				(len (- js3-beautify-ts-cursor
					js3-beautify-token-beg))
				(name js3-beautify-ts-string))))
  "AST node for a JavaScript identifier"
  name   ; a string
  scope) ; a `js3-beautify-scope' (optional, used for codegen)

(put 'cl-struct-js3-beautify-name-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-name-node 'js3-beautify-printer 'js3-beautify-print-name-node)

(defun js3-beautify-print-name-node (n i)
  (js3-beautify-concat-curstr (js3-beautify-name-node-name n)))

(defsubst js3-beautify-name-node-length (node)
  "Return identifier length of NODE, a `js3-beautify-name-node'.
Returns 0 if NODE is nil or its identifier field is nil."
  (if node
      (length (js3-beautify-name-node-name node))
    0))

(defstruct (js3-beautify-number-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-number-node
			  (&key (type js3-beautify-NUMBER)
				(pos js3-beautify-token-beg)
				(len (- js3-beautify-ts-cursor
					js3-beautify-token-beg))
				(value js3-beautify-ts-string)
				(num-value js3-beautify-ts-number))))
  "AST node for a number literal."
  value      ; the original string, e.g. "6.02e23"
  num-value) ; the parsed number value

(put 'cl-struct-js3-beautify-number-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-number-node 'js3-beautify-printer 'js3-beautify-print-number-node)

(defun js3-beautify-print-number-node (n i)
  (js3-beautify-concat-curstr
   (number-to-string (js3-beautify-number-node-num-value n))))

(defstruct (js3-beautify-regexp-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-regexp-node
			  (&key (type js3-beautify-REGEXP)
				(pos js3-beautify-token-beg)
				(len (- js3-beautify-ts-cursor
					js3-beautify-token-beg))
				value
				flags)))
  "AST node for a regular expression literal."
  value  ; the regexp string, without // delimiters
  flags) ; a string of flags, e.g. `mi'.

(put 'cl-struct-js3-beautify-regexp-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-regexp-node 'js3-beautify-printer 'js3-beautify-print-regexp)

(defun js3-beautify-print-regexp (n i)
  (js3-beautify-concat-curstr
   (concat
    "/"
    (js3-beautify-regexp-node-value n)
    "/"))
  (if (js3-beautify-regexp-node-flags n)
      (js3-beautify-concat-curstr (js3-beautify-regexp-node-flags n))))

(defstruct (js3-beautify-string-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-string-node
			  (&key (type js3-beautify-STRING)
				(pos js3-beautify-token-beg)
				(len (- js3-beautify-ts-cursor
					js3-beautify-token-beg))
				(value js3-beautify-ts-string))))
  "String literal.
Escape characters are not evaluated; e.g. \n is 2 chars in value field.
You can tell the quote type by looking at the first character."
  value) ; the characters of the string, including the quotes

(put 'cl-struct-js3-beautify-string-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-string-node 'js3-beautify-printer 'js3-beautify-print-string-node)

(defun js3-beautify-print-string-node (n i)
  (js3-beautify-concat-curstr (js3-beautify-node-string n)))

(defstruct (js3-beautify-array-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-array-node
			  (&key (type js3-beautify-ARRAYLIT)
				(pos js3-beautify-ts-cursor)
				len
				elems)))
  "AST node for an array literal."
  elems)  ; list of expressions.  [foo,,bar] yields a nil middle element.

(put 'cl-struct-js3-beautify-array-node 'js3-beautify-visitor 'js3-beautify-visit-array-node)
(put 'cl-struct-js3-beautify-array-node 'js3-beautify-printer 'js3-beautify-print-array-node)

(defun js3-beautify-visit-array-node (n v)
  (dolist (e (js3-beautify-array-node-elems n))
    (js3-beautify-visit-ast e v)))

(defun js3-beautify-print-array-node (n i)
  (js3-beautify-concat-curstr "[")
  (js3-beautify-print-list (js3-beautify-array-node-elems n))
  (js3-beautify-concat-curstr "]"))

(defstruct (js3-beautify-object-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-object-node
			  (&key (type js3-beautify-OBJECTLIT)
				(pos js3-beautify-ts-cursor)
				len
				elems)))
  "AST node for an object literal expression."
  elems)  ; a lisp list of `js3-beautify-object-prop-node'

(put 'cl-struct-js3-beautify-object-node 'js3-beautify-visitor 'js3-beautify-visit-object-node)
(put 'cl-struct-js3-beautify-object-node 'js3-beautify-printer 'js3-beautify-print-object-node)

(defun js3-beautify-visit-object-node (n v)
  (dolist (e (js3-beautify-object-node-elems n))
    (js3-beautify-visit-ast e v)))

(defun js3-beautify-print-object-node (n i)
  (js3-beautify-concat-curstr "{")
  (js3-beautify-print-list (js3-beautify-object-node-elems n))
  (js3-beautify-concat-curstr "}"))

(defstruct (js3-beautify-object-prop-node
            (:include js3-beautify-infix-node)
            (:constructor nil)
            (:constructor make-js3-beautify-object-prop-node (&key (type js3-beautify-COLON)
                                                          (pos js3-beautify-ts-cursor)
                                                          len
                                                          left
                                                          right
                                                          op-pos)))
  "AST node for an object literal prop:value entry.
The `left' field is the property:  a name node, string node or number node.
The `right' field is a `js3-beautify-node' representing the initializer value.")

(put 'cl-struct-js3-beautify-object-prop-node 'js3-beautify-visitor 'js3-beautify-visit-infix-node)
(put 'cl-struct-js3-beautify-object-prop-node 'js3-beautify-printer 'js3-beautify-print-object-prop-node)

(defun js3-beautify-print-object-prop-node (n i)
  (js3-beautify-print-ast (js3-beautify-object-prop-node-left n) 0)
  (js3-beautify-concat-curstr ": ")
  (js3-beautify-print-ast (js3-beautify-object-prop-node-right n) 0))

(defstruct (js3-beautify-getter-setter-node
            (:include js3-beautify-infix-node)
            (:constructor nil)
            (:constructor make-js3-beautify-getter-setter-node (&key type ; GET or SET
                                                            (pos js3-beautify-ts-cursor)
                                                            len
                                                            left
                                                            right)))
  "AST node for a getter/setter property in an object literal.
The `left' field is the `js3-beautify-name-node' naming the getter/setter prop.
The `right' field is always an anonymous `js3-beautify-function-node' with a node
property `GETTER_SETTER' set to js3-beautify-GET or js3-beautify-SET. ")

(put 'cl-struct-js3-beautify-getter-setter-node 'js3-beautify-visitor 'js3-beautify-visit-infix-node)
(put 'cl-struct-js3-beautify-getter-setter-node 'js3-beautify-printer 'js3-beautify-print-getter-setter)

(defun js3-beautify-print-getter-setter (n i)
  (js3-beautify-concat-curstr (if (= (js3-beautify-node-type n) js3-beautify-GET) "get " "set "))
  (js3-beautify-print-ast (js3-beautify-getter-setter-node-left n) 0)
  (js3-beautify-print-ast (js3-beautify-getter-setter-node-right n) 0))

(defstruct (js3-beautify-prop-get-node
            (:include js3-beautify-infix-node)
            (:constructor nil)
            (:constructor make-js3-beautify-prop-get-node
			  (&key (type js3-beautify-GETPROP)
				(pos js3-beautify-ts-cursor)
				len
				left
				right)))
  "AST node for a dotted property reference, e.g. foo.bar or foo().bar")

(put 'cl-struct-js3-beautify-prop-get-node 'js3-beautify-visitor 'js3-beautify-visit-prop-get-node)
(put 'cl-struct-js3-beautify-prop-get-node 'js3-beautify-printer 'js3-beautify-print-prop-get-node)

(defun js3-beautify-visit-prop-get-node (n v)
  (js3-beautify-visit-ast (js3-beautify-prop-get-node-left n) v)
  (js3-beautify-visit-ast (js3-beautify-prop-get-node-right n) v))

(defun js3-beautify-print-prop-get-node (n i)
  (js3-beautify-print-ast (js3-beautify-prop-get-node-left n) 0)
  (js3-beautify-concat-curstr ".")
  (js3-beautify-print-ast (js3-beautify-prop-get-node-right n) 0))

(defstruct (js3-beautify-elem-get-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-elem-get-node (&key (type js3-beautify-GETELEM)
                                                       (pos js3-beautify-ts-cursor)
                                                       len
                                                       target
                                                       element
                                                       lb
                                                       rb)))
  "AST node for an array index expression such as foo[bar]."
  target  ; a `js3-beautify-node' - the expression preceding the "."
  element ; a `js3-beautify-node' - the expression in brackets
  lb      ; position of left-bracket, nil if omitted
  rb)     ; position of right-bracket, nil if omitted

(put 'cl-struct-js3-beautify-elem-get-node 'js3-beautify-visitor 'js3-beautify-visit-elem-get-node)
(put 'cl-struct-js3-beautify-elem-get-node 'js3-beautify-printer 'js3-beautify-print-elem-get-node)

(defun js3-beautify-visit-elem-get-node (n v)
  (when (js3-beautify-elem-get-node-target n)
    (js3-beautify-visit-ast (js3-beautify-elem-get-node-target n) v))
  (when (js3-beautify-elem-get-node-element n)
    (js3-beautify-visit-ast (js3-beautify-elem-get-node-element n) v)))

(defun js3-beautify-print-elem-get-node (n i)
  (js3-beautify-print-ast (js3-beautify-elem-get-node-target n) 0)
  (js3-beautify-concat-curstr "[")
  (js3-beautify-print-ast (js3-beautify-elem-get-node-element n) 0)
  (js3-beautify-concat-curstr "]"))

(defstruct (js3-beautify-call-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-call-node
			  (&key (type js3-beautify-CALL)
				(pos js3-beautify-ts-cursor)
				len
				target
				args
				lp
				rp)))
  "AST node for a JavaScript function call."
  target  ; a `js3-beautify-node' evaluating to the function to call
  args  ; a lisp list of `js3-beautify-node' arguments
  lp    ; position of open-paren, or nil if missing
  rp)   ; position of close-paren, or nil if missing

(put 'cl-struct-js3-beautify-call-node 'js3-beautify-visitor 'js3-beautify-visit-call-node)
(put 'cl-struct-js3-beautify-call-node 'js3-beautify-printer 'js3-beautify-print-call-node)

(defun js3-beautify-visit-call-node (n v)
  (js3-beautify-visit-ast (js3-beautify-call-node-target n) v)
  (dolist (arg (js3-beautify-call-node-args n))
    (js3-beautify-visit-ast arg v)))

(defun js3-beautify-print-call-node (n i)
  (js3-beautify-print-ast (js3-beautify-call-node-target n) 0)
  (js3-beautify-concat-curstr "(")
  (js3-beautify-print-list (js3-beautify-call-node-args n))
  (js3-beautify-concat-curstr ")"))

(defstruct (js3-beautify-yield-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-yield-node (&key (type js3-beautify-YIELD)
                                                    (pos js3-beautify-ts-cursor)
                                                    len
                                                    value)))
  "AST node for yield statement or expression."
  value) ; optional:  value to be yielded

(put 'cl-struct-js3-beautify-yield-node 'js3-beautify-visitor 'js3-beautify-visit-yield-node)
(put 'cl-struct-js3-beautify-yield-node 'js3-beautify-printer 'js3-beautify-print-yield-node)

(defun js3-beautify-visit-yield-node (n v)
  (js3-beautify-visit-ast (js3-beautify-yield-node-value n) v))

(defun js3-beautify-print-yield-node (n i)
  (js3-beautify-concat-curstr "yield")
  (when (js3-beautify-yield-node-value n)
    (js3-beautify-concat-curstr " ")
    (js3-beautify-print-ast (js3-beautify-yield-node-value n) 0)))

(defstruct (js3-beautify-paren-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-paren-node (&key (type js3-beautify-LP)
                                                    (pos js3-beautify-ts-cursor)
                                                    len
                                                    expr)))
  "AST node for a parenthesized expression.
In particular, used when the parens are syntactically optional,
as opposed to required parens such as those enclosing an if-conditional."
  expr)   ; `js3-beautify-node'

(put 'cl-struct-js3-beautify-paren-node 'js3-beautify-visitor 'js3-beautify-visit-paren-node)
(put 'cl-struct-js3-beautify-paren-node 'js3-beautify-printer 'js3-beautify-print-paren-node)

(defun js3-beautify-visit-paren-node (n v)
  (js3-beautify-visit-ast (js3-beautify-paren-node-expr n) v))

(defun js3-beautify-print-paren-node (n i)
  (js3-beautify-concat-curstr "( ")
  (js3-beautify-print-ast (js3-beautify-paren-node-expr n) 0)
  (js3-beautify-concat-curstr " )"))

(defstruct (js3-beautify-array-comp-node
            (:include js3-beautify-scope)
            (:constructor nil)
            (:constructor make-js3-beautify-array-comp-node
			  (&key (type js3-beautify-ARRAYCOMP)
				(pos js3-beautify-ts-cursor)
				len
				result
				loops
				filter
				if-pos
				lp
				rp)))
  "AST node for an Array comprehension such as [[x,y] for (x in foo) for (y in bar)]."
  result  ; result expression (just after left-bracket)
  loops   ; a lisp list of `js3-beautify-array-comp-loop-node'
  filter  ; guard/filter expression
  if-pos  ; buffer pos of 'if' keyword, if present, else nil
  lp      ; buffer position of if-guard left-paren, or nil if not present
  rp)     ; buffer position of if-guard right-paren, or nil if not present

(put 'cl-struct-js3-beautify-array-comp-node 'js3-beautify-visitor 'js3-beautify-visit-array-comp-node)
(put 'cl-struct-js3-beautify-array-comp-node 'js3-beautify-printer 'js3-beautify-print-array-comp-node)

(defun js3-beautify-visit-array-comp-node (n v)
  (js3-beautify-visit-ast (js3-beautify-array-comp-node-result n) v)
  (dolist (l (js3-beautify-array-comp-node-loops n))
    (js3-beautify-visit-ast l v))
  (js3-beautify-visit-ast (js3-beautify-array-comp-node-filter n) v))

(defun js3-beautify-print-array-comp-node (n i)
  (js3-beautify-concat-curstr "[")
  (js3-beautify-print-ast (js3-beautify-array-comp-node-result n) 0)
  (dolist (l (js3-beautify-array-comp-node-loops n))
    (js3-beautify-concat-curstr " ")
    (js3-beautify-print-ast l 0))
  (when (js3-beautify-array-comp-node-filter n)
    (js3-beautify-concat-curstr " if (")
    (js3-beautify-print-ast (js3-beautify-array-comp-node-filter n) 0))
  (js3-beautify-concat-curstr ")]"))

(defstruct (js3-beautify-array-comp-loop-node
            (:include js3-beautify-for-in-node)
            (:constructor nil)
            (:constructor make-js3-beautify-array-comp-loop-node
			  (&key (type js3-beautify-FOR)
				(pos js3-beautify-ts-cursor)
				len
				iterator
				object
				in-pos
				foreach-p
				each-pos
				lp
				rp)))
  "AST subtree for each 'for (foo in bar)' loop in an array comprehension.")

(put 'cl-struct-js3-beautify-array-comp-loop-node 'js3-beautify-visitor 'js3-beautify-visit-array-comp-loop)
(put 'cl-struct-js3-beautify-array-comp-loop-node 'js3-beautify-printer 'js3-beautify-print-array-comp-loop)

(defun js3-beautify-visit-array-comp-loop (n v)
  (js3-beautify-visit-ast (js3-beautify-array-comp-loop-node-iterator n) v)
  (js3-beautify-visit-ast (js3-beautify-array-comp-loop-node-object n) v))

(defun js3-beautify-print-array-comp-loop (n i)
  (js3-beautify-concat-curstr "for (")
  (js3-beautify-print-ast (js3-beautify-array-comp-loop-node-iterator n) 0)
  (js3-beautify-concat-curstr " in ")
  (js3-beautify-print-ast (js3-beautify-array-comp-loop-node-object n) 0)
  (js3-beautify-concat-curstr ")"))

(defstruct (js3-beautify-empty-expr-node
            (:include js3-beautify-node)
            (:constructor nil)
            (:constructor make-js3-beautify-empty-expr-node (&key (type js3-beautify-EMPTY)
                                                         (pos js3-beautify-token-beg)
                                                         len)))
  "AST node for an empty expression.")

(put 'cl-struct-js3-beautify-empty-expr-node 'js3-beautify-visitor 'js3-beautify-visit-none)
(put 'cl-struct-js3-beautify-empty-expr-node 'js3-beautify-printer 'js3-beautify-print-none)

;;; Node utilities

(defsubst js3-beautify-node-line (n)
  "Fetch the source line number at the start of node N.
This is O(n) in the length of the source buffer; use prudently."
  (1+ (count-lines (point-min) (js3-beautify-node-abs-pos n))))

(defsubst js3-beautify-block-node-kid (n i)
  "Return child I of node N, or nil if there aren't that many."
  (nth i (js3-beautify-block-node-kids n)))

(defsubst js3-beautify-block-node-first (n)
  "Return first child of block node N, or nil if there is none."
  (first (js3-beautify-block-node-kids n)))

(defun js3-beautify-node-root (n)
  "Return the root of the AST containing N.
If N has no parent pointer, returns N."
  (let ((parent (js3-beautify-node-parent n)))
    (if parent
        (js3-beautify-node-root parent)
      n)))

(defun js3-beautify-node-position-in-parent (node &optional parent)
  "Return the position of NODE in parent's block-kids list.
PARENT can be supplied if known.  Positioned returned is zero-indexed.
Returns 0 if NODE is not a child of a block statement, or if NODE
is not a statement node."
  (let ((p (or parent (js3-beautify-node-parent node)))
        (i 0))
    (if (not (js3-beautify-block-node-p p))
        i
      (or (js3-beautify-position node (js3-beautify-block-node-kids p))
          0))))

(defsubst js3-beautify-node-short-name (n)
  "Return the short name of node N as a string, e.g. `js3-beautify-if-node'."
  (substring (symbol-name (aref n 0))
             (length "cl-struct-")))

(defsubst js3-beautify-node-child-list (node)
  "Return the child list for NODE, a lisp list of nodes.
Works for block nodes, array nodes, obj literals, funarg lists,
var decls and try nodes (for catch clauses).  Note that you should call
`js3-beautify-block-node-kids' on the function body for the body statements.
Returns nil for zero-length child lists or unsupported nodes."
  (cond
   ((js3-beautify-function-node-p node)
    (js3-beautify-function-node-params node))
   ((js3-beautify-block-node-p node)
    (js3-beautify-block-node-kids node))
   ((js3-beautify-try-node-p node)
    (js3-beautify-try-node-catch-clauses node))
   ((js3-beautify-array-node-p node)
    (js3-beautify-array-node-elems node))
   ((js3-beautify-object-node-p node)
    (js3-beautify-object-node-elems node))
   ((js3-beautify-call-node-p node)
    (js3-beautify-call-node-args node))
   ((js3-beautify-new-node-p node)
    (js3-beautify-new-node-args node))
   ((js3-beautify-var-decl-node-p node)
    (js3-beautify-var-decl-node-kids node))
   (t
    nil)))

(defsubst js3-beautify-node-set-child-list (node kids)
  "Set the child list for NODE to KIDS."
  (cond
   ((js3-beautify-function-node-p node)
    (setf (js3-beautify-function-node-params node) kids))
   ((js3-beautify-block-node-p node)
    (setf (js3-beautify-block-node-kids node) kids))
   ((js3-beautify-try-node-p node)
    (setf (js3-beautify-try-node-catch-clauses node) kids))
   ((js3-beautify-array-node-p node)
    (setf (js3-beautify-array-node-elems node) kids))
   ((js3-beautify-object-node-p node)
    (setf (js3-beautify-object-node-elems node) kids))
   ((js3-beautify-call-node-p node)
    (setf (js3-beautify-call-node-args node) kids))
   ((js3-beautify-new-node-p node)
    (setf (js3-beautify-new-node-args node) kids))
   ((js3-beautify-var-decl-node-p node)
    (setf (js3-beautify-var-decl-node-kids node) kids))
   (t
    (error "Unsupported node type: %s" (js3-beautify-node-short-name node))))
  kids)

;; All because Common Lisp doesn't support multiple inheritance for defstructs.
(defconst js3-beautify-paren-expr-nodes
  '(cl-struct-js3-beautify-array-comp-loop-node
    cl-struct-js3-beautify-array-comp-node
    cl-struct-js3-beautify-call-node
    cl-struct-js3-beautify-catch-node
    cl-struct-js3-beautify-do-node
    cl-struct-js3-beautify-elem-get-node
    cl-struct-js3-beautify-for-in-node
    cl-struct-js3-beautify-for-node
    cl-struct-js3-beautify-function-node
    cl-struct-js3-beautify-if-node
    cl-struct-js3-beautify-let-node
    cl-struct-js3-beautify-new-node
    cl-struct-js3-beautify-paren-node
    cl-struct-js3-beautify-switch-node
    cl-struct-js3-beautify-while-node
    cl-struct-js3-beautify-with-node)
  "Node types that can have a parenthesized child expression.
In particular, nodes that respond to `js3-beautify-node-lp' and `js3-beautify-node-rp'.")

(defsubst js3-beautify-paren-expr-node-p (node)
  "Return t for nodes that typically have a parenthesized child expression.
Useful for computing the indentation anchors for arg-lists and conditions.
Note that it may return a false positive, for instance when NODE is
a `js3-beautify-new-node' and there are no arguments or parentheses."
  (memq (aref node 0) js3-beautify-paren-expr-nodes))

;; Fake polymorphism... yech.
(defsubst js3-beautify-node-lp (node)
  "Return relative left-paren position for NODE, if applicable.
For `js3-beautify-elem-get-node' structs, returns left-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js3-beautify-elem-get-node-p node)
    (js3-beautify-elem-get-node-lb node))
   ((js3-beautify-loop-node-p node)
    (js3-beautify-loop-node-lp node))
   ((js3-beautify-function-node-p node)
    (js3-beautify-function-node-lp node))
   ((js3-beautify-if-node-p node)
    (js3-beautify-if-node-lp node))
   ((js3-beautify-new-node-p node)
    (js3-beautify-new-node-lp node))
   ((js3-beautify-call-node-p node)
    (js3-beautify-call-node-lp node))
   ((js3-beautify-paren-node-p node)
    (js3-beautify-node-pos node))
   ((js3-beautify-switch-node-p node)
    (js3-beautify-switch-node-lp node))
   ((js3-beautify-catch-node-p node)
    (js3-beautify-catch-node-lp node))
   ((js3-beautify-let-node-p node)
    (js3-beautify-let-node-lp node))
   ((js3-beautify-array-comp-node-p node)
    (js3-beautify-array-comp-node-lp node))
   ((js3-beautify-with-node-p node)
    (js3-beautify-with-node-lp node))
   (t
    (error "Unsupported node type: %s" (js3-beautify-node-short-name node)))))

;; Fake polymorphism... blech.
(defsubst js3-beautify-node-rp (node)
  "Return relative right-paren position for NODE, if applicable.
For `js3-beautify-elem-get-node' structs, returns right-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js3-beautify-elem-get-node-p node)
    (js3-beautify-elem-get-node-lb node))
   ((js3-beautify-loop-node-p node)
    (js3-beautify-loop-node-rp node))
   ((js3-beautify-function-node-p node)
    (js3-beautify-function-node-rp node))
   ((js3-beautify-if-node-p node)
    (js3-beautify-if-node-rp node))
   ((js3-beautify-new-node-p node)
    (js3-beautify-new-node-rp node))
   ((js3-beautify-call-node-p node)
    (js3-beautify-call-node-rp node))
   ((js3-beautify-paren-node-p node)
    (+ (js3-beautify-node-pos node) (js3-beautify-node-len node)))
   ((js3-beautify-switch-node-p node)
    (js3-beautify-switch-node-rp node))
   ((js3-beautify-catch-node-p node)
    (js3-beautify-catch-node-rp node))
   ((js3-beautify-let-node-p node)
    (js3-beautify-let-node-rp node))
   ((js3-beautify-array-comp-node-p node)
    (js3-beautify-array-comp-node-rp node))
   ((js3-beautify-with-node-p node)
    (js3-beautify-with-node-rp node))
   (t
    (error "Unsupported node type: %s" (js3-beautify-node-short-name node)))))

(defsubst js3-beautify-node-first-child (node)
  "Returns the first element of `js3-beautify-node-child-list' for NODE."
  (car (js3-beautify-node-child-list node)))

(defsubst js3-beautify-node-last-child (node)
  "Returns the last element of `js3-beautify-node-last-child' for NODE."
  (car (last (js3-beautify-node-child-list node))))

(defun js3-beautify-node-prev-sibling (node)
  "Return the previous statement in parent.
Works for parents supported by `js3-beautify-node-child-list'.
Returns nil if NODE is not in the parent, or PARENT is
not a supported node, or if NODE is the first child."
  (let* ((p (js3-beautify-node-parent node))
         (kids (js3-beautify-node-child-list p))
         (sib (car kids)))
    (while (and kids
                (neq node (cadr kids)))
      (setq kids (cdr kids)
            sib (car kids)))
    sib))

(defun js3-beautify-node-next-sibling (node)
  "Return the next statement in parent block.
Returns nil if NODE is not in the block, or PARENT is not
a block node, or if NODE is the last statement."
  (let* ((p (js3-beautify-node-parent node))
         (kids (js3-beautify-node-child-list p)))
    (while (and kids
                (neq node (car kids)))
      (setq kids (cdr kids)))
    (cadr kids)))

(defun js3-beautify-node-find-child-before (pos parent &optional after)
  "Find the last child that starts before POS in parent.
If AFTER is non-nil, returns first child starting after POS.
POS is an absolute buffer position.  PARENT is any node
supported by `js3-beautify-node-child-list'.
Returns nil if no applicable child is found."
  (let ((kids (if (js3-beautify-function-node-p parent)
                  (js3-beautify-block-node-kids (js3-beautify-function-node-body parent))
                (js3-beautify-node-child-list parent)))
        (beg (if (js3-beautify-function-node-p parent)
                 (js3-beautify-node-abs-pos (js3-beautify-function-node-body parent))
               (js3-beautify-node-abs-pos parent)))
        kid
        result
        fn
        (continue t))
    (setq fn (if after '> '<))
    (while (and kids continue)
      (setq kid (car kids))
      (if (funcall fn (+ beg (js3-beautify-node-pos kid)) pos)
          (setq result kid
                continue (if after nil t))
        (setq continue (if after t nil)))
      (setq kids (cdr kids)))
    result))

(defun js3-beautify-node-find-child-after (pos parent)
  "Find first child that starts after POS in parent.
POS is an absolute buffer position.  PARENT is any node
supported by `js3-beautify-node-child-list'.
Returns nil if no applicable child is found."
  (js3-beautify-node-find-child-before pos parent 'after))

(defun js3-beautify-node-replace-child (pos parent new-node)
  "Replace node at index POS in PARENT with NEW-NODE.
Only works for parents supported by `js3-beautify-node-child-list'."
  (let ((kids (js3-beautify-node-child-list parent))
        (i 0))
    (while (< i pos)
      (setq kids (cdr kids)
            i (1+ i)))
    (setcar kids new-node)
    (js3-beautify-node-add-children parent new-node)))

(defun js3-beautify-node-buffer (n)
  "Return the buffer associated with AST N.
Returns nil if the buffer is not set as a property on the root
node, or if parent links were not recorded during parsing."
  (let ((root (js3-beautify-node-root n)))
    (and root
         (js3-beautify-ast-root-p root)
         (js3-beautify-ast-root-buffer root))))

(defsubst js3-beautify-block-node-push (n kid)
  "Push js3-beautify-node KID onto the end of js3-beautify-block-node N's child list.
KID is always added to the -end- of the kids list.
Function also calls `js3-beautify-node-add-children' to add the parent link."
  (let ((kids (js3-beautify-node-child-list n)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (js3-beautify-node-set-child-list n (list kid)))
    (js3-beautify-node-add-children n kid)))

(defun js3-beautify-node-string (node)
  (let ((buf (js3-beautify-node-buffer node))
        pos)
    (unless buf
      (error "No buffer available for node %s" node))
    (save-excursion
      (set-buffer buf)
      (buffer-substring-no-properties (setq pos (js3-beautify-node-abs-pos node))
                                      (+ pos (js3-beautify-node-len node))))))

;; Container for storing the node we're looking for in a traversal.
(defvar js3-beautify-discovered-node nil)
(make-variable-buffer-local 'js3-beautify-discovered-node)

;; Keep track of absolute node position during traversals.
(defvar js3-beautify-visitor-offset nil)
(make-variable-buffer-local 'js3-beautify-visitor-offset)

(defvar js3-beautify-node-search-point nil)
(make-variable-buffer-local 'js3-beautify-node-search-point)

(defun js3-beautify-find-node-at-point ()
  (interactive)
  (let ((node (js3-beautify-node-at-point)))
    (message "%s" (or node "No node found at point"))))

(defun js3-beautify-node-name-at-point ()
  (interactive)
  (let ((node (js3-beautify-node-at-point)))
    (message "%s" (if node
		      (js3-beautify-node-short-name node)
		    "No node found at point."))))

(defun js3-beautify-node-at-point (&optional pos skip-comments)
  "Return AST node at POS, a buffer position, defaulting to current point.
The `js3-beautify-ast' variable must be set to the current parse tree.
Signals an error if the AST (`js3-beautify-ast') is nil.
Always returns a node - if it can't find one, it returns the root.
If SKIP-COMMENTS is non-nil, comment nodes are ignored."
  (let ((ast js3-beautify-ast)
        result)
    (unless ast
      (error "No JavaScript AST available"))
    ;; Look through comments first, since they may be inside nodes that
    ;; would otherwise report a match.
    (setq pos (or pos (point))
          result (if (> pos (js3-beautify-node-abs-end ast))
                     ast
                   (if (not skip-comments)
                       (js3-beautify-comment-at-point pos))))
    (unless result
      (setq js3-beautify-discovered-node nil
            js3-beautify-visitor-offset 0
            js3-beautify-node-search-point pos)
      (unwind-protect
          (catch 'js3-beautify-visit-done
            (js3-beautify-visit-ast ast #'js3-beautify-node-at-point-visitor))
        (setq js3-beautify-visitor-offset nil
              js3-beautify-node-search-point nil))
      (setq result js3-beautify-discovered-node))
    ;; may have found a comment beyond end of last child node,
    ;; since visiting the ast-root looks at the comment-list last.
    (if (and skip-comments
             (js3-beautify-comment-node-p result))
        (setq result nil))
    (or result js3-beautify-ast)))

(defun js3-beautify-node-at-point-visitor (node end-p)
  (let ((rel-pos (js3-beautify-node-pos node))
        abs-pos
        abs-end
        (point js3-beautify-node-search-point))
    (cond
     (end-p
      ;; this evaluates to a non-nil return value, even if it's zero
      (decf js3-beautify-visitor-offset rel-pos))
     ;; we already looked for comments before visiting, and don't want them now
     ((js3-beautify-comment-node-p node)
      nil)
     (t
      (setq abs-pos (incf js3-beautify-visitor-offset rel-pos)
            ;; we only want to use the node if the point is before
            ;; the last character position in the node, so we decrement
            ;; the absolute end by 1.
            abs-end (+ abs-pos (js3-beautify-node-len node) -1))
      (cond
       ;; If this node starts after search-point, stop the search.
       ((> abs-pos point)
        (throw 'js3-beautify-visit-done nil))
       ;; If this node ends before the search-point, don't check kids.
       ((> point abs-end)
        nil)
       (t
        ;; Otherwise point is within this node, possibly in a child.
        (setq js3-beautify-discovered-node node)
        t))))))  ; keep processing kids to look for more specific match

(defsubst js3-beautify-block-comment-p (node)
  "Return non-nil if NODE is a comment node of format `jsdoc' or `block'."
  (and (js3-beautify-comment-node-p node)
       (memq (js3-beautify-comment-node-format node) '(jsdoc block))))

;; TODO:  put the comments in a vector and binary-search them instead
(defun js3-beautify-comment-at-point (&optional pos)
  "Look through scanned comment nodes for one containing POS.
POS is a buffer position that defaults to current point.
Function returns nil if POS was not in any comment node."
  (let ((ast js3-beautify-ast)
        (x (or pos (point)))
        beg
        end)
    (unless ast
      (error "No JavaScript AST available"))
    (catch 'done
      ;; Comments are stored in lexical order.
      (dolist (comment (js3-beautify-ast-root-comments ast) nil)
        (setq beg (js3-beautify-node-abs-pos comment)
              end (+ beg (js3-beautify-node-len comment)))
        (if (and (>= x beg)
                 (<= x end))
            (throw 'done comment))))))

(defun js3-beautify-find-parent-fn (node)
  "Find function enclosing NODE.
Returns nil if NODE is not inside a function."
  (setq node (js3-beautify-node-parent node))
  (while (and node (not (js3-beautify-function-node-p node)))
    (setq node (js3-beautify-node-parent node)))
  (and (js3-beautify-function-node-p node) node))

(defun js3-beautify-find-enclosing-fn (node)
  "Find function or root enclosing NODE."
  (if (js3-beautify-ast-root-p node)
      node
    (setq node (js3-beautify-node-parent node))
    (while (not (or (js3-beautify-ast-root-p node)
                    (js3-beautify-function-node-p node)))
      (setq node (js3-beautify-node-parent node)))
    node))

(defun js3-beautify-find-enclosing-node (beg end)
  "Find script or function fully enclosing BEG and END."
  (let ((node (js3-beautify-node-at-point beg))
        pos
        (continue t))
    (while continue
      (if (or (js3-beautify-ast-root-p node)
              (and (js3-beautify-function-node-p node)
                   (<= (setq pos (js3-beautify-node-abs-pos node)) beg)
                   (>= (+ pos (js3-beautify-node-len node)) end)))
          (setq continue nil)
        (setq node (js3-beautify-node-parent node))))
    node))

(defun js3-beautify-node-parent-script-or-fn (node)
  "Find script or function immediately enclosing NODE.
If NODE is the ast-root, returns nil."
  (if (js3-beautify-ast-root-p node)
      nil
    (setq node (js3-beautify-node-parent node))
    (while (and node (not (or (js3-beautify-function-node-p node)
                              (js3-beautify-script-node-p node))))
      (setq node (js3-beautify-node-parent node)))
    node))

(defsubst js3-beautify-nested-function-p (node)
  "Return t if NODE is a nested function, or is inside a nested function."
  (unless (js3-beautify-ast-root-p node)
    (js3-beautify-function-node-p (if (js3-beautify-function-node-p node)
                             (js3-beautify-node-parent-script-or-fn node)
                           (js3-beautify-node-parent-script-or-fn
                            (js3-beautify-node-parent-script-or-fn node))))))

(defsubst js3-beautify-shift-kids (kids start offset)
  (dolist (kid kids)
    (if (> (js3-beautify-node-pos kid) start)
        (incf (js3-beautify-node-pos kid) offset))))

(defsubst js3-beautify-shift-children (parent start offset)
  "Update start-positions of all children of PARENT beyond START."
  (let ((root (js3-beautify-node-root parent)))
    (js3-beautify-shift-kids (js3-beautify-node-child-list parent) start offset)
    (js3-beautify-shift-kids (js3-beautify-ast-root-comments root) start offset)))

(defsubst js3-beautify-node-is-descendant (node ancestor)
  "Return t if NODE is a descendant of ANCESTOR."
  (while (and node
              (neq node ancestor))
    (setq node (js3-beautify-node-parent node)))
  node)

;;; visitor infrastructure

(defun js3-beautify-visit-none (node callback)
  "Visitor for AST node that have no node children."
  nil)

(defun js3-beautify-print-none (node indent)
  "Visitor for AST node with no printed representation.")

(defun js3-beautify-print-body (node indent)
  "Print a statement, or a block without braces."
  (if (js3-beautify-block-node-p node)
      (dolist (kid (js3-beautify-block-node-kids node))
        (js3-beautify-print-ast kid indent))
    (js3-beautify-print-ast node indent)))

(defun js3-beautify-print-list (args &optional delimiter)
  (let ((oldstr js3-beautify-curstr))
    (js3-beautify-print-list-compact args delimiter)
    (when (and (not (string= js3-beautify-curstr oldstr))
	       (or (> (length js3-beautify-curln) js3-beautify-max-columns)
		   (let ((c (compare-strings js3-beautify-curstr 0 nil
					     oldstr 0 nil))
			 (diffstr))
		     (setq diffstr (substring js3-beautify-curstr c))
		     (string-match "\n" diffstr))))
      (setq js3-beautify-curstr oldstr)
      (js3-beautify-concat-curstr "")
      (js3-beautify-print-list-long args delimiter))))

(defun js3-beautify-print-list-long (args &optional delimiter)
  (loop with len = (length args)
        for arg in args
        for count from 1
        do
        (if (and (= count 1) (> len 1))
	    (js3-beautify-concat-curstr " "))
        (js3-beautify-print-ast arg 0)
        (if (< count len)
            (js3-beautify-concat-curstr (or delimiter "\n, "))
	  (when (> len 1)
	    (js3-beautify-concat-curstr "\n")))))

(defun js3-beautify-print-list-compact (args &optional delimiter)
  (loop with len = (length args)
        for arg in args
        for count from 1
        do
        (if (and (= count 1) (> len 1))
	    (js3-beautify-concat-curstr " "))
        (js3-beautify-print-ast arg 0)
        (if (< count len)
            (js3-beautify-concat-curstr (or delimiter ", "))
	  (when (> len 1)
	    (js3-beautify-concat-curstr " ")))))

(defun js3-beautify-print-tree (ast)
  "Prints an AST to js3-beautify-curstr.
Makes `js3-beautify-ast-parent-nodes' available to the printer functions."
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth 1500)))
    (js3-beautify-print-ast ast)
    (js3-beautify-concat-curstr "\n")))

(defun js3-beautify-print-ast (node &optional indent)
  "Helper function for printing AST nodes.
Requires `js3-beautify-ast-parent-nodes' to be non-nil.
You should use `js3-beautify-print-tree' instead of this function."
  (let ((printer (get (aref node 0) 'js3-beautify-printer))
        (i (or indent 0))
        (pos (js3-beautify-node-abs-pos node)))
    ;; TODO:  wedge comments in here somewhere
    (if printer
        (funcall printer node i))))

(defconst js3-beautify-side-effecting-tokens
  (let ((tokens (make-bool-vector js3-beautify-num-tokens nil)))
    (dolist (tt (list js3-beautify-ASSIGN
                      js3-beautify-ASSIGN_ADD
                      js3-beautify-ASSIGN_BITAND
                      js3-beautify-ASSIGN_BITOR
                      js3-beautify-ASSIGN_BITXOR
                      js3-beautify-ASSIGN_DIV
                      js3-beautify-ASSIGN_LSH
                      js3-beautify-ASSIGN_MOD
                      js3-beautify-ASSIGN_MUL
                      js3-beautify-ASSIGN_RSH
                      js3-beautify-ASSIGN_SUB
                      js3-beautify-ASSIGN_URSH
                      js3-beautify-BLOCK
                      js3-beautify-BREAK
                      js3-beautify-CALL
                      js3-beautify-CATCH
                      js3-beautify-CATCH_SCOPE
                      js3-beautify-CONST
                      js3-beautify-CONTINUE
                      js3-beautify-DEBUGGER
                      js3-beautify-DEC
                      js3-beautify-DELPROP
                      js3-beautify-DEL_REF
                      js3-beautify-DO
                      js3-beautify-ELSE
                      js3-beautify-EMPTY
                      js3-beautify-ENTERWITH
                      js3-beautify-EXPORT
                      js3-beautify-EXPR_RESULT
                      js3-beautify-FINALLY
                      js3-beautify-FOR
                      js3-beautify-FUNCTION
                      js3-beautify-GOTO
                      js3-beautify-IF
                      js3-beautify-IFEQ
                      js3-beautify-IFNE
                      js3-beautify-IMPORT
                      js3-beautify-INC
                      js3-beautify-JSR
                      js3-beautify-LABEL
                      js3-beautify-LEAVEWITH
                      js3-beautify-LET
                      js3-beautify-LETEXPR
                      js3-beautify-LOCAL_BLOCK
                      js3-beautify-LOOP
                      js3-beautify-NEW
                      js3-beautify-REF_CALL
                      js3-beautify-RETHROW
                      js3-beautify-RETURN
                      js3-beautify-RETURN_RESULT
                      js3-beautify-SEMI
                      js3-beautify-SETELEM
                      js3-beautify-SETELEM_OP
                      js3-beautify-SETNAME
                      js3-beautify-SETPROP
                      js3-beautify-SETPROP_OP
                      js3-beautify-SETVAR
                      js3-beautify-SET_REF
                      js3-beautify-SET_REF_OP
                      js3-beautify-SWITCH
                      js3-beautify-TARGET
                      js3-beautify-THROW
                      js3-beautify-TRY
                      js3-beautify-VAR
                      js3-beautify-WHILE
                      js3-beautify-WITH
                      js3-beautify-WITHEXPR
                      js3-beautify-YIELD))
      (aset tokens tt t))
    (if js3-beautify-instanceof-has-side-effects
        (aset tokens js3-beautify-INSTANCEOF t))
    tokens))

(defun js3-beautify-node-has-side-effects (node)
  "Return t if NODE has side effects."
  (when node  ; makes it easier to handle malformed expressions
    (let ((tt (js3-beautify-node-type node)))
      (cond
       ;; This doubtless needs some work, since EXPR_VOID is used
       ;; in several ways in Rhino, and I may not have caught them all.
       ;; I'll wait for people to notice incorrect warnings.
       ((and (= tt js3-beautify-EXPR_VOID)
             (js3-beautify-expr-stmt-node-p node)) ; but not if EXPR_RESULT
        (js3-beautify-node-has-side-effects (js3-beautify-expr-stmt-node-expr node)))
       ((= tt js3-beautify-COMMA)
        (js3-beautify-node-has-side-effects (js3-beautify-infix-node-right node)))
       ((or (= tt js3-beautify-AND)
            (= tt js3-beautify-OR))
        (or (js3-beautify-node-has-side-effects (js3-beautify-infix-node-right node))
            (js3-beautify-node-has-side-effects (js3-beautify-infix-node-left node))))
       ((= tt js3-beautify-HOOK)
        (and (js3-beautify-node-has-side-effects (js3-beautify-cond-node-true-expr node))
             (js3-beautify-node-has-side-effects (js3-beautify-cond-node-false-expr node))))
       ((js3-beautify-paren-node-p node)
        (js3-beautify-node-has-side-effects (js3-beautify-paren-node-expr node)))
       ((= tt js3-beautify-ERROR) ; avoid cascaded error messages
        nil)
       (t
        (aref js3-beautify-side-effecting-tokens tt))))))

(defun js3-beautify-member-expr-leftmost-name (node)
  "For an expr such as foo.bar.baz, return leftmost node foo.
NODE is any `js3-beautify-node' object.  If it represents a member
expression, which is any sequence of property gets, element-gets,
or function calls, then we look at the lexically leftmost (first)
node in the chain.  If it is a name-node we return it.  Note that
NODE can be a raw name-node and it will be returned as well.  If
NODE is not a name-node or member expression, or if it is a
member expression whose leftmost target is not a name node,
returns nil."
  (let ((continue t)
        result)
    (while (and continue (not result))
      (cond
       ((js3-beautify-name-node-p node)
        (setq result node))
       ((js3-beautify-prop-get-node-p node)
        (setq node (js3-beautify-prop-get-node-left node)))
       (t
        (setq continue nil))))
    result))

(defconst js3-beautify-stmt-node-types
  (list js3-beautify-BLOCK
        js3-beautify-BREAK
        js3-beautify-CONTINUE
        js3-beautify-DO
        js3-beautify-EXPR_RESULT
        js3-beautify-EXPR_VOID
        js3-beautify-FOR
        js3-beautify-IF
        js3-beautify-RETURN
        js3-beautify-SWITCH
        js3-beautify-THROW
        js3-beautify-TRY
        js3-beautify-WHILE
        js3-beautify-WITH)
  "Node types that only appear in statement contexts.
The list does not include nodes that always appear as the child
of another specific statement type, such as switch-cases,
catch and finally blocks, and else-clauses.  The list also excludes
nodes like yield, let and var, which may appear in either expression
or statement context, and in the latter context always have a
`js3-beautify-expr-stmt-node' parent.  Finally, the list does not include
functions or scripts, which are treated separately from statements
by the JavaScript parser and runtime.")

(defun js3-beautify-stmt-node-p (node)
  "Heuristic for figuring out if NODE is a statement.
Some node types can appear in either an expression context or a
statement context, e.g. let-nodes, yield-nodes, and var-decl nodes.
For these node types in a statement context, the parent will be a
`js3-beautify-expr-stmt-node'.
Functions aren't included in the check."
  (memq (js3-beautify-node-type node) js3-beautify-stmt-node-types))

(defsubst js3-beautify-find-first-stmt (node)
  "Search upward starting from NODE looking for a statement.
For purposes of this function, a `js3-beautify-function-node' counts."
  (while (not (or (js3-beautify-stmt-node-p node)
                  (js3-beautify-function-node-p node)))
    (setq node (js3-beautify-node-parent node)))
  node)

(defun js3-beautify-node-parent-stmt (node)
  "Return the node's first ancestor that is a statement.
Returns nil if NODE is a `js3-beautify-ast-root'.  Note that any expression
appearing in a statement context will have a parent that is a
`js3-beautify-expr-stmt-node' that will be returned by this function."
  (let ((parent (js3-beautify-node-parent node)))
    (if (or (null parent)
            (js3-beautify-stmt-node-p parent)
            (and (js3-beautify-function-node-p parent)
                 (neq (js3-beautify-function-node-form parent) 'FUNCTION_EXPRESSION)))
        parent
      (js3-beautify-node-parent-stmt parent))))

;; Roshan James writes:
;;  Does consistent-return analysis on the function body when strict mode is
;;  enabled.
;;
;;    function (x) { return (x+1) }
;;
;;  is ok, but
;;
;;    function (x) { if (x < 0) return (x+1); }
;;
;;  is not because the function can potentially return a value when the
;;  condition is satisfied and if not, the function does not explicitly
;;  return a value.
;;
;;  This extends to checking mismatches such as "return" and "return <value>"
;;  used in the same function. Warnings are not emitted if inconsistent
;;  returns exist in code that can be statically shown to be unreachable.
;;  Ex.
;;    function (x) { while (true) { ... if (..) { return value } ... } }
;;
;;  emits no warning. However if the loop had a break statement, then a
;;  warning would be emitted.
;;
;;  The consistency analysis looks at control structures such as loops, ifs,
;;  switch, try-catch-finally blocks, examines the reachable code paths and
;;  warns the user about an inconsistent set of termination possibilities.
;;
;;  These flags enumerate the possible ways a statement/function can
;;  terminate. These flags are used by endCheck() and by the Parser to
;;  detect inconsistent return usage.
;;
;;  END_UNREACHED is reserved for code paths that are assumed to always be
;;  able to execute (example: throw, continue)
;;
;;  END_DROPS_OFF indicates if the statement can transfer control to the
;;  next one. Statement such as return dont. A compound statement may have
;;  some branch that drops off control to the next statement.
;;
;;  END_RETURNS indicates that the statement can return with no value.
;;  END_RETURNS_VALUE indicates that the statement can return a value.
;;
;;  A compound statement such as
;;  if (condition) {
;;    return value;
;;  }
;;  Will be detected as (END_DROPS_OFF | END_RETURN_VALUE) by endCheck()

(defconst js3-beautify-END_UNREACHED 0)
(defconst js3-beautify-END_DROPS_OFF 1)
(defconst js3-beautify-END_RETURNS 2)
(defconst js3-beautify-END_RETURNS_VALUE 4)
(defconst js3-beautify-END_YIELDS 8)

(defun js3-beautify-has-consistent-return-usage (node)
  "Check that every return usage in a function body is consistent.
Returns t if the function satisfies strict mode requirement."
  (let ((n (js3-beautify-end-check node)))
    ;; either it doesn't return a value in any branch...
    (or (js3-beautify-flag-not-set-p n js3-beautify-END_RETURNS_VALUE)
        ;; or it returns a value (or is unreached) at every branch
        (js3-beautify-flag-not-set-p n (logior js3-beautify-END_DROPS_OFF
                                      js3-beautify-END_RETURNS
                                      js3-beautify-END_YIELDS)))))

(defun js3-beautify-end-check-if (node)
  "Returns in the then and else blocks must be consistent with each other.
If there is no else block, then the return statement can fall through.
Returns logical OR of END_* flags"
  (let ((th (js3-beautify-if-node-then-part node))
        (el (js3-beautify-if-node-else-part node)))
    (if (null th)
        js3-beautify-END_UNREACHED
      (logior (js3-beautify-end-check th) (if el
                                     (js3-beautify-end-check el)
                                   js3-beautify-END_DROPS_OFF)))))

(defun js3-beautify-end-check-switch (node)
  "Consistency of return statements is checked between the case statements.
If there is no default, then the switch can fall through. If there is a
default, we check to see if all code paths in the default return or if
there is a code path that can fall through.
Returns logical OR of END_* flags."
  (let ((rv js3-beautify-END_UNREACHED)
        default-case)
    ;; examine the cases
    (catch 'break
      (dolist (c (js3-beautify-switch-node-cases node))
        (if (js3-beautify-case-node-expr c)
            (js3-beautify-set-flag rv (js3-beautify-end-check-block c))
          (setq default-case c)
          (throw 'break nil))))
    ;; we don't care how the cases drop into each other
    (js3-beautify-clear-flag rv js3-beautify-END_DROPS_OFF)
    ;; examine the default
    (js3-beautify-set-flag rv (if default-case
                         (js3-beautify-end-check default-case)
                       js3-beautify-END_DROPS_OFF))
    rv))

(defun js3-beautify-end-check-try (node)
  "If the block has a finally, return consistency is checked in the
finally block. If all code paths in the finally return, then the
returns in the try-catch blocks don't matter. If there is a code path
that does not return or if there is no finally block, the returns
of the try and catch blocks are checked for mismatch.
Returns logical OR of END_* flags."
  (let ((finally (js3-beautify-try-node-finally-block node))
        rv)
    ;; check the finally if it exists
    (setq rv (if finally
                 (js3-beautify-end-check (js3-beautify-finally-node-body finally))
               js3-beautify-END_DROPS_OFF))
    ;; If the finally block always returns, then none of the returns
    ;; in the try or catch blocks matter.
    (when (js3-beautify-flag-set-p rv js3-beautify-END_DROPS_OFF)
      (js3-beautify-clear-flag rv js3-beautify-END_DROPS_OFF)
      ;; examine the try block
      (js3-beautify-set-flag rv (js3-beautify-end-check (js3-beautify-try-node-try-block node)))
      ;; check each catch block
      (dolist (cb (js3-beautify-try-node-catch-clauses node))
        (js3-beautify-set-flag rv (js3-beautify-end-check (js3-beautify-catch-node-block cb)))))
    rv))

(defun js3-beautify-end-check-loop (node)
  "Return statement in the loop body must be consistent. The default
assumption for any kind of a loop is that it will eventually terminate.
The only exception is a loop with a constant true condition. Code that
follows such a loop is examined only if one can statically determine
that there is a break out of the loop.

for(... ; ... ; ...) {}
for(... in ... ) {}
while(...) { }
do { } while(...)

Returns logical OR of END_* flags."
  (let ((rv (js3-beautify-end-check (js3-beautify-loop-node-body node)))
        (condition (cond
                    ((js3-beautify-while-node-p node)
                     (js3-beautify-while-node-condition node))
                    ((js3-beautify-do-node-p node)
                     (js3-beautify-do-node-condition node))
                    ((js3-beautify-for-node-p node)
                     (js3-beautify-for-node-condition node)))))

    ;; check to see if the loop condition is always true
    (if (and condition
             (eq (js3-beautify-always-defined-boolean-p condition) 'ALWAYS_TRUE))
        (js3-beautify-clear-flag rv js3-beautify-END_DROPS_OFF))

    ;; look for effect of breaks
    (js3-beautify-set-flag rv (js3-beautify-node-get-prop node
                                        'CONTROL_BLOCK_PROP
                                        js3-beautify-END_UNREACHED))
    rv))

(defun js3-beautify-end-check-block (node)
  "A general block of code is examined statement by statement.
If any statement (even a compound one) returns in all branches, then
subsequent statements are not examined.
Returns logical OR of END_* flags."
  (let* ((rv js3-beautify-END_DROPS_OFF)
         (kids (js3-beautify-block-node-kids node))
         (n (car kids)))
    ;; Check each statment.  If the statement can continue onto the next
    ;; one (i.e. END_DROPS_OFF is set), then check the next statement.
    (while (and n (js3-beautify-flag-set-p rv js3-beautify-END_DROPS_OFF))
      (js3-beautify-clear-flag rv js3-beautify-END_DROPS_OFF)
      (js3-beautify-set-flag rv (js3-beautify-end-check n))
      (setq kids (cdr kids)
            n (car kids)))
    rv))

(defun js3-beautify-end-check-label (node)
  "A labeled statement implies that there may be a break to the label.
The function processes the labeled statement and then checks the
CONTROL_BLOCK_PROP property to see if there is ever a break to the
particular label.
Returns logical OR of END_* flags."
  (let ((rv (js3-beautify-end-check (js3-beautify-labeled-stmt-node-stmt node))))
    (logior rv (js3-beautify-node-get-prop node
                                  'CONTROL_BLOCK_PROP
                                  js3-beautify-END_UNREACHED))))

(defun js3-beautify-end-check-break (node)
  "When a break is encountered annotate the statement being broken
out of by setting its CONTROL_BLOCK_PROP property.
Returns logical OR of END_* flags."
  (and (js3-beautify-break-node-target node)
       (js3-beautify-node-set-prop (js3-beautify-break-node-target node)
                          'CONTROL_BLOCK_PROP
                          js3-beautify-END_DROPS_OFF))
  js3-beautify-END_UNREACHED)

(defun js3-beautify-end-check (node)
  "Examine the body of a function, doing a basic reachability analysis.
Returns a combination of flags END_* flags that indicate
how the function execution can terminate. These constitute only the
pessimistic set of termination conditions. It is possible that at
runtime certain code paths will never be actually taken. Hence this
analysis will flag errors in cases where there may not be errors.
Returns logical OR of END_* flags"
  (let (kid)
    (cond
     ((js3-beautify-break-node-p node)
      (js3-beautify-end-check-break node))
     ((js3-beautify-expr-stmt-node-p node)
      (if (setq kid (js3-beautify-expr-stmt-node-expr node))
          (js3-beautify-end-check kid)
        js3-beautify-END_DROPS_OFF))
     ((or (js3-beautify-continue-node-p node)
          (js3-beautify-throw-node-p node))
      js3-beautify-END_UNREACHED)
     ((js3-beautify-return-node-p node)
      (if (setq kid (js3-beautify-return-node-retval node))
          js3-beautify-END_RETURNS_VALUE
        js3-beautify-END_RETURNS))
     ((js3-beautify-loop-node-p node)
      (js3-beautify-end-check-loop node))
     ((js3-beautify-switch-node-p node)
      (js3-beautify-end-check-switch node))
     ((js3-beautify-labeled-stmt-node-p node)
      (js3-beautify-end-check-label node))
     ((js3-beautify-if-node-p node)
      (js3-beautify-end-check-if node))
     ((js3-beautify-try-node-p node)
      (js3-beautify-end-check-try node))
     ((js3-beautify-block-node-p node)
      (if (null (js3-beautify-block-node-kids node))
          js3-beautify-END_DROPS_OFF
        (js3-beautify-end-check-block node)))
     ((js3-beautify-yield-node-p node)
      js3-beautify-END_YIELDS)
     (t
      js3-beautify-END_DROPS_OFF))))

(defun js3-beautify-always-defined-boolean-p (node)
  "Check if NODE always evaluates to true or false in boolean context.
Returns 'ALWAYS_TRUE, 'ALWAYS_FALSE, or nil if it's neither always true
nor always false."
  (let ((tt (js3-beautify-node-type node))
        num)
    (cond
     ((or (= tt js3-beautify-FALSE) (= tt js3-beautify-NULL))
      'ALWAYS_FALSE)
     ((= tt js3-beautify-TRUE)
      'ALWAYS_TRUE)
     ((= tt js3-beautify-NUMBER)
      (setq num (js3-beautify-number-node-num-value node))
      (if (and (not (eq num 0.0e+NaN))
               (not (zerop num)))
          'ALWAYS_TRUE
        'ALWAYS_FALSE))
     (t
      nil))))

(defun js3-beautify-concat-curstr (str)
  "Update curstr with the value of str."
  (setq js3-beautify-curstr (concat js3-beautify-curstr str))
  (if (string-match "\n\\(.*\\)\\'" js3-beautify-curstr)
      (setq js3-beautify-curln (match-string 1 js3-beautify-curstr))
    (setq js3-beautify-curln js3-beautify-curstr)))

(provide 'js3-beautify-ast)

;;; js3-beautify-ast.el ends here
