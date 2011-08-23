;;; js3-bfy-ast.el --- JavaScript syntax tree node definitions

;;; Code:

(eval-and-compile
  (require 'cl))


(defsubst js3-bfy-relpos (pos anchor)
  "Convert POS to be relative to ANCHOR.
If POS is nil, returns nil."
  (and pos (- pos anchor)))

(defun js3-bfy-visit-ast (node callback)
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
    (let ((vfunc (get (aref node 0) 'js3-bfy-visitor)))
      ;; visit the node
      (when  (funcall callback node nil)
	;; visit the kids
	(cond
	 ((eq vfunc 'js3-bfy-visit-none)
	  nil)                            ; don't even bother calling it
	 ;; Each AST node type has to define a `js3-bfy-visitor' function
	 ;; that takes a node and a callback, and calls `js3-bfy-visit-ast'
	 ;; on each child of the node.
	 (vfunc
	  (funcall vfunc node callback))
	 (t
	  (error "%s does not define a visitor-traversal function"
		 (aref node 0)))))
      ;; call the end-visit
      (funcall callback node t))))

(defstruct (js3-bfy-node
            (:constructor nil))  ; abstract
  "Base AST node type."
  (type -1)  ; token type
  (pos -1)   ; start position of this AST node in parsed input
  (abs -1)   ; absolute start of node, saved
  (len 1)    ; num characters spanned by the node
  props      ; optional node property list (an alist)
  parent)    ; link to parent node; null for root

(defsubst js3-bfy-node-get-prop (node prop &optional default)
  (or (cadr (assoc prop (js3-bfy-node-props node))) default))

(defsubst js3-bfy-node-set-prop (node prop value)
  (setf (js3-bfy-node-props node)
        (cons (list prop value) (js3-bfy-node-props node))))

(defsubst js3-bfy-fixup-starts (n nodes)
  "Adjust the start positions of NODES to be relative to N.
Any node in the list may be nil, for convenience."
  (dolist (node nodes)
    (when node
      (setf (js3-bfy-node-abs node) (js3-bfy-node-pos node))
      (setf (js3-bfy-node-pos node) (- (js3-bfy-node-pos node)
				       (js3-bfy-node-pos n))))))

(defsubst js3-bfy-node-add-children (parent &rest nodes)
  "Set parent node of NODES to PARENT, and return PARENT.
Does nothing if we're not recording parent links.
If any given node in NODES is nil, doesn't record that link."
  (js3-bfy-fixup-starts parent nodes)
  (dolist (node nodes)
    (and node
         (setf (js3-bfy-node-parent node) parent))))

;; Non-recursive since it's called a frightening number of times.
(defsubst js3-bfy-node-abs-pos (n)
  (let ((pos (js3-bfy-node-pos n)))
    (while (setq n (js3-bfy-node-parent n))
      (setq pos (+ pos (js3-bfy-node-pos n))))
    pos))

(defsubst js3-bfy-node-abs-end (n)
  "Return absolute buffer position of end of N."
  (+ (js3-bfy-node-abs-pos n) (js3-bfy-node-len n)))

;; It's important to make sure block nodes have a lisp list for the
;; child nodes, to limit printing recursion depth in an AST that
;; otherwise consists of defstruct vectors.  Emacs will crash printing
;; a sufficiently large vector tree.

(defstruct (js3-bfy-block-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-block-node
			  (&key (type js3-bfy-BLOCK)
				(pos js3-bfy-token-beg)
				len
				props
				kids)))
  "A block of statements."
  kids)  ; a lisp list of the child statement nodes

(put 'cl-struct-js3-bfy-block-node 'js3-bfy-visitor 'js3-bfy-visit-block)
(put 'cl-struct-js3-bfy-block-node 'js3-bfy-printer 'js3-bfy-print-block)

(defsubst js3-bfy-visit-block (ast callback)
  "Visit the `js3-bfy-block-node' children of AST."
  (dolist (kid (js3-bfy-block-node-kids ast))
    (js3-bfy-visit-ast kid callback)))

(defun js3-bfy-print-block (n i)
  (js3-bfy-print "{\n")
  (dolist (kid (js3-bfy-block-node-kids n))
    (js3-bfy-print-ast kid (1+ i)))
  (js3-bfy-print "}\n"))

(defstruct (js3-bfy-scope
            (:include js3-bfy-block-node)
            (:constructor nil)
            (:constructor make-js3-bfy-scope
			  (&key (type js3-bfy-BLOCK)
				(pos js3-bfy-token-beg)
				len
				kids)))
  ;; The symbol-table is a LinkedHashMap<String,Symbol> in Rhino.
  ;; I don't have one of those handy, so I'll use an alist for now.
  ;; It's as fast as an emacs hashtable for up to about 50 elements,
  ;; and is much lighter-weight to construct (both CPU and mem).
  ;; The keys are interned strings (symbols) for faster lookup.
  ;; Should switch to hybrid alist/hashtable eventually.
  symbol-table  ; an alist of (symbol . js3-bfy-symbol)
  parent-scope  ; a `js3-bfy-scope'
  top)          ; top-level `js3-bfy-scope' (script/function)

(put 'cl-struct-js3-bfy-scope 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-scope 'js3-bfy-printer 'js3-bfy-print-none)

(defun js3-bfy-scope-set-parent-scope (scope parent)
  (setf (js3-bfy-scope-parent-scope scope) parent
        (js3-bfy-scope-top scope) (if (null parent)
				      scope
				    (js3-bfy-scope-top parent))))

(defun js3-bfy-node-get-enclosing-scope (node)
  "Return the innermost `js3-bfy-scope' node surrounding NODE.
Returns nil if there is no enclosing scope node."
  (let ((parent (js3-bfy-node-parent node)))
    (while (not (js3-bfy-scope-p parent))
      (setq parent (js3-bfy-node-parent parent)))
    parent))

(defun js3-bfy-get-defining-scope (scope name)
  "Search up scope chain from SCOPE looking for NAME, a string or symbol.
Returns `js3-bfy-scope' in which NAME is defined, or nil if not found."
  (let ((sym (if (symbolp name)
                 name
               (intern name)))
        table
        result
        (continue t))
    (while (and scope continue)
      (if (and (setq table (js3-bfy-scope-symbol-table scope))
               (assq sym table))
          (setq continue nil
                result scope)
        (setq scope (js3-bfy-scope-parent-scope scope))))
    result))

(defsubst js3-bfy-scope-get-symbol (scope name)
  "Return symbol table entry for NAME in SCOPE.
NAME can be a string or symbol.   Returns a `js3-bfy-symbol' or nil if not found."
  (and (js3-bfy-scope-symbol-table scope)
       (cdr (assq (if (symbolp name)
                      name
                    (intern name))
                  (js3-bfy-scope-symbol-table scope)))))

(defsubst js3-bfy-scope-put-symbol (scope name symbol)
  "Enter SYMBOL into symbol-table for SCOPE under NAME.
NAME can be a lisp symbol or string.  SYMBOL is a `js3-bfy-symbol'."
  (let* ((table (js3-bfy-scope-symbol-table scope))
         (sym (if (symbolp name) name (intern name)))
         (entry (assq sym table)))
    (if entry
        (setcdr entry symbol)
      (push (cons sym symbol)
            (js3-bfy-scope-symbol-table scope)))))

(defstruct (js3-bfy-symbol
            (:constructor nil)
            (:constructor make-js3-bfy-symbol (decl-type name &optional ast-node)))
  "A symbol table entry."
  ;; One of js3-bfy-FUNCTION, js3-bfy-LP (for parameters), js3-bfy-VAR,
  ;; js3-bfy-LET, or js3-bfy-CONST
  decl-type
  name  ; string
  ast-node) ; a `js3-bfy-node'

(defstruct (js3-bfy-error-node
            (:include js3-bfy-node)
            (:constructor nil) ; silence emacs21 byte-compiler
            (:constructor make-js3-bfy-error-node
			  (&key (type js3-bfy-ERROR)
				(pos js3-bfy-token-beg)
				len)))
  "AST node representing a parse error.")

(put 'cl-struct-js3-bfy-error-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-error-node 'js3-bfy-printer 'js3-bfy-print-none)

(defstruct (js3-bfy-script-node
            (:include js3-bfy-scope)
            (:constructor nil)
            (:constructor make-js3-bfy-script-node
			  (&key (type js3-bfy-SCRIPT)
				(pos js3-bfy-token-beg)
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

(put 'cl-struct-js3-bfy-script-node 'js3-bfy-visitor 'js3-bfy-visit-block)
(put 'cl-struct-js3-bfy-script-node 'js3-bfy-printer 'js3-bfy-print-script)

(defun js3-bfy-print-script (node indent)
  (dolist (kid (js3-bfy-block-node-kids node))
    (js3-bfy-print-ast kid indent)))

(defstruct (js3-bfy-ast-root
            (:include js3-bfy-script-node)
            (:constructor nil)
            (:constructor make-js3-bfy-ast-root
			  (&key (type js3-bfy-SCRIPT)
				(pos js3-bfy-token-beg)
				len
				buffer)))
  "The root node of a js3 AST."
  buffer         ; the source buffer from which the code was parsed
  comments       ; a lisp list of comments, ordered by start position
  errors         ; a lisp list of errors found during parsing
  warnings       ; a lisp list of warnings found during parsing
  node-count)    ; number of nodes in the tree, including the root

(put 'cl-struct-js3-bfy-ast-root 'js3-bfy-visitor 'js3-bfy-visit-ast-root)
(put 'cl-struct-js3-bfy-ast-root 'js3-bfy-printer 'js3-bfy-print-script)

(defun js3-bfy-visit-ast-root (ast callback)
  (dolist (kid (js3-bfy-ast-root-kids ast))
    (js3-bfy-visit-ast kid callback))
  (dolist (comment (js3-bfy-ast-root-comments ast))
    (js3-bfy-visit-ast comment callback)))

(defstruct (js3-bfy-comment-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-comment-node
			  (&key (type js3-bfy-COMMENT)
				(pos js3-bfy-token-beg)
				len
				(format js3-bfy-ts-comment-type))))
  format)  ; 'line, 'block, 'jsdoc or 'html

(put 'cl-struct-js3-bfy-comment-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-comment-node 'js3-bfy-printer 'js3-bfy-print-comment)

(defun js3-bfy-print-comment (n i)
  ;; We really ought to link end-of-line comments to their nodes.
  ;; Or maybe we could add a new comment type, 'endline.
  (js3-bfy-print (js3-bfy-node-string n)))

(defstruct (js3-bfy-expr-stmt-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-expr-stmt-node
			  (&key (type js3-bfy-EXPR_VOID)
				(pos js3-bfy-ts-cursor)
				len
				expr)))
  "An expression statement."
  expr)

(defsubst js3-bfy-expr-stmt-node-set-has-result (node)
  "Change the node type to `js3-bfy-EXPR_RESULT'.  Used for code generation."
  (setf (js3-bfy-node-type node) js3-bfy-EXPR_RESULT))

(put 'cl-struct-js3-bfy-expr-stmt-node 'js3-bfy-visitor 'js3-bfy-visit-expr-stmt-node)
(put 'cl-struct-js3-bfy-expr-stmt-node 'js3-bfy-printer 'js3-bfy-print-expr-stmt-node)

(defun js3-bfy-visit-expr-stmt-node (n v)
  (js3-bfy-visit-ast (js3-bfy-expr-stmt-node-expr n) v))

(defun js3-bfy-print-expr-stmt-node (n indent)
  (let ((expr (js3-bfy-expr-stmt-node-expr n)))
    (let ((type (js3-bfy-node-type expr)))
      (if (= js3-bfy-CALL type)
	  (let ((target (js3-bfy-call-node-target expr)))
	    (if (= js3-bfy-GETPROP (js3-bfy-node-type target))
		(let ((left (js3-bfy-prop-get-node-left target)))
		  (if (or (= js3-bfy-ARRAYLIT
			     (js3-bfy-node-type left))
			  (= js3-bfy-LP
			     (js3-bfy-node-type left)))
		      (js3-bfy-print ";"))))))
      (if (or (= type js3-bfy-POS)
	      (= type js3-bfy-NEG))
	  (js3-bfy-print ";"))))
  (js3-bfy-print-ast (js3-bfy-expr-stmt-node-expr n) indent)
  (when (/= js3-bfy-CASE
	    (js3-bfy-node-type (js3-bfy-node-parent n)))
    (js3-bfy-print "\n")
    (if (= js3-bfy-VAR
	   (js3-bfy-node-type (js3-bfy-expr-stmt-node-expr n)))
	(js3-bfy-print "\n"))))

(defstruct (js3-bfy-loop-node
            (:include js3-bfy-scope)
            (:constructor nil))
  "Abstract supertype of loop nodes."
  body      ; a `js3-bfy-block-node'
  lp        ; position of left-paren, nil if omitted
  rp)       ; position of right-paren, nil if omitted

(defstruct (js3-bfy-do-node
            (:include js3-bfy-loop-node)
            (:constructor nil)
            (:constructor make-js3-bfy-do-node
			  (&key (type js3-bfy-DO)
				(pos js3-bfy-token-beg)
				len
				body
				condition
				while-pos
				lp
				rp)))
  "AST node for do-loop."
  condition  ; while (expression)
  while-pos) ; buffer position of 'while' keyword

(put 'cl-struct-js3-bfy-do-node 'js3-bfy-visitor 'js3-bfy-visit-do-node)
(put 'cl-struct-js3-bfy-do-node 'js3-bfy-printer 'js3-bfy-print-do-node)

(defun js3-bfy-visit-do-node (n v)
  (js3-bfy-visit-ast (js3-bfy-do-node-body n) v)
  (js3-bfy-visit-ast (js3-bfy-do-node-condition n) v))

(defun js3-bfy-print-do-node (n i)
  (js3-bfy-print "do {\n")
  (dolist (kid (js3-bfy-block-node-kids (js3-bfy-do-node-body n)))
    (js3-bfy-print-ast kid (1+ i)))
  (js3-bfy-print "} while (")
  (js3-bfy-print-ast (js3-bfy-do-node-condition n) 0)
  (js3-bfy-print ")"))

(defstruct (js3-bfy-while-node
            (:include js3-bfy-loop-node)
            (:constructor nil)
            (:constructor make-js3-bfy-while-node
			  (&key (type js3-bfy-WHILE)
				(pos js3-bfy-token-beg)
				len
				body
				condition
				lp
				rp)))
  "AST node for while-loop."
  condition)    ; while-condition

(put 'cl-struct-js3-bfy-while-node 'js3-bfy-visitor 'js3-bfy-visit-while-node)
(put 'cl-struct-js3-bfy-while-node 'js3-bfy-printer 'js3-bfy-print-while-node)

(defun js3-bfy-visit-while-node (n v)
  (js3-bfy-visit-ast (js3-bfy-while-node-condition n) v)
  (js3-bfy-visit-ast (js3-bfy-while-node-body n) v))

(defun js3-bfy-print-while-node (n i)
  (if (or (not (or js3-bfy-compact js3-bfy-compact-while))
	  (and (js3-bfy-block-node-p (js3-bfy-while-node-body n))
	   (> (length (js3-bfy-block-node-kids
		       (js3-bfy-while-node-body n)))
	      1)))
      (js3-bfy-print-while-node-long n i)
    (let ((oldstr js3-bfy-curstr))
      (js3-bfy-print-while-node-compact n i)
      (when (and (not (string= js3-bfy-curstr oldstr))
		 (or (> (length js3-bfy-curln) js3-bfy-max-columns)
		     (let ((c (compare-strings js3-bfy-curstr 0 nil
					       oldstr 0 nil))
			   (diffstr))
		       (setq diffstr (substring js3-bfy-curstr c))
		       (string-match "\n\\(.\\|\n\\)" diffstr))))
	(setq js3-bfy-curstr oldstr)
	(js3-bfy-print "")
	(js3-bfy-print-while-node-long n i)))))

(defun js3-bfy-print-while-node-long (n i)
  (js3-bfy-print "while (")
  (js3-bfy-print-ast (js3-bfy-while-node-condition n) 0)
  (js3-bfy-print ") {\n")
  (js3-bfy-print-body (js3-bfy-while-node-body n) (1+ i))
  (js3-bfy-print "}\n"))

(defun js3-bfy-print-while-node-compact (n i)
  (js3-bfy-print "while (")
  (js3-bfy-print-ast (js3-bfy-while-node-condition n) 0)
  (js3-bfy-print ") ")
  (js3-bfy-print-body (js3-bfy-while-node-body n) (1+ i)))

(defstruct (js3-bfy-for-node
            (:include js3-bfy-loop-node)
            (:constructor nil)
            (:constructor make-js3-bfy-for-node
			  (&key (type js3-bfy-FOR)
				(pos js3-bfy-ts-cursor)
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

(put 'cl-struct-js3-bfy-for-node 'js3-bfy-visitor 'js3-bfy-visit-for-node)
(put 'cl-struct-js3-bfy-for-node 'js3-bfy-printer 'js3-bfy-print-for-node)

(defun js3-bfy-visit-for-node (n v)
  (js3-bfy-visit-ast (js3-bfy-for-node-init n) v)
  (js3-bfy-visit-ast (js3-bfy-for-node-condition n) v)
  (js3-bfy-visit-ast (js3-bfy-for-node-update n) v)
  (js3-bfy-visit-ast (js3-bfy-for-node-body n) v))

(defun js3-bfy-print-for-node (n i)
  (if (or (not (or js3-bfy-compact js3-bfy-compact-for))
	  (and (js3-bfy-block-node-p (js3-bfy-for-node-body n))
	       (> (length (js3-bfy-block-node-kids
			   (js3-bfy-for-node-body n)))
		  1)))
      (js3-bfy-print-for-node-long n i)
    (let ((oldstr js3-bfy-curstr))
      (js3-bfy-print-for-node-compact n i)
      (when (and (not (string= js3-bfy-curstr oldstr))
		 (or (> (length js3-bfy-curln) js3-bfy-max-columns)
		     (let ((c (compare-strings js3-bfy-curstr 0 nil
					       oldstr 0 nil))
			   (diffstr))
		       (setq diffstr (substring js3-bfy-curstr c))
		       (string-match "\n\\(.\\|\n\\)" diffstr))))
	(setq js3-bfy-curstr oldstr)
	(js3-bfy-print "")
	(js3-bfy-print-for-node-long n i)))))

(defun js3-bfy-print-for-node-long (n i)
  (js3-bfy-print "for (")
  (js3-bfy-print-ast (js3-bfy-for-node-init n) 0)
  (js3-bfy-print "; ")
  (js3-bfy-print-ast (js3-bfy-for-node-condition n) 0)
  (js3-bfy-print "; ")
  (js3-bfy-print-ast (js3-bfy-for-node-update n) 0)
  (js3-bfy-print ") {\n")
  (js3-bfy-print-body (js3-bfy-for-node-body n) (1+ i))
  (js3-bfy-print "}\n"))

(defun js3-bfy-print-for-node-compact (n i)
  (js3-bfy-print "for (")
  (js3-bfy-print-ast (js3-bfy-for-node-init n) 0)
  (js3-bfy-print "; ")
  (js3-bfy-print-ast (js3-bfy-for-node-condition n) 0)
  (js3-bfy-print "; ")
  (js3-bfy-print-ast (js3-bfy-for-node-update n) 0)
  (js3-bfy-print ") ")
  (js3-bfy-print-body (js3-bfy-for-node-body n) (1+ i)))

(defstruct (js3-bfy-for-in-node
            (:include js3-bfy-loop-node)
            (:constructor nil)
            (:constructor make-js3-bfy-for-in-node
			  (&key (type js3-bfy-FOR)
				(pos js3-bfy-ts-cursor)
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

(put 'cl-struct-js3-bfy-for-in-node 'js3-bfy-visitor 'js3-bfy-visit-for-in-node)
(put 'cl-struct-js3-bfy-for-in-node 'js3-bfy-printer 'js3-bfy-print-for-in-node)

(defun js3-bfy-visit-for-in-node (n v)
  (js3-bfy-visit-ast (js3-bfy-for-in-node-iterator n) v)
  (js3-bfy-visit-ast (js3-bfy-for-in-node-object n) v)
  (js3-bfy-visit-ast (js3-bfy-for-in-node-body n) v))

(defun js3-bfy-print-for-in-node (n i)
  (js3-bfy-print "for ")
  (if (js3-bfy-for-in-node-foreach-p n)
      (js3-bfy-print "each "))
  (js3-bfy-print "(")
  (js3-bfy-print-ast (js3-bfy-for-in-node-iterator n) 0)
  (js3-bfy-print " in ")
  (js3-bfy-print-ast (js3-bfy-for-in-node-object n) 0)
  (js3-bfy-print ") {\n")
  (js3-bfy-print-body (js3-bfy-for-in-node-body n) (1+ i))
  (js3-bfy-print "}\n"))

(defstruct (js3-bfy-return-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-return-node
			  (&key (type js3-bfy-RETURN)
				(pos js3-bfy-ts-cursor)
				len
				retval)))
  "AST node for a return statement."
  retval)  ; expression to return, or 'undefined

(put 'cl-struct-js3-bfy-return-node 'js3-bfy-visitor 'js3-bfy-visit-return-node)
(put 'cl-struct-js3-bfy-return-node 'js3-bfy-printer 'js3-bfy-print-return-node)

(defun js3-bfy-visit-return-node (n v)
  (js3-bfy-visit-ast (js3-bfy-return-node-retval n) v))

(defun js3-bfy-print-return-node (n i)
  (js3-bfy-print "return ")
  (if (js3-bfy-return-node-retval n)
      (js3-bfy-print-ast (js3-bfy-return-node-retval n) 0))
  (js3-bfy-print "\n"))

(defstruct (js3-bfy-if-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-if-node
			  (&key (type js3-bfy-IF)
				(pos js3-bfy-ts-cursor)
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

(put 'cl-struct-js3-bfy-if-node 'js3-bfy-visitor 'js3-bfy-visit-if-node)
(put 'cl-struct-js3-bfy-if-node 'js3-bfy-printer 'js3-bfy-print-if-node)

(defun js3-bfy-visit-if-node (n v)
  (js3-bfy-visit-ast (js3-bfy-if-node-condition n) v)
  (js3-bfy-visit-ast (js3-bfy-if-node-then-part n) v)
  (js3-bfy-visit-ast (js3-bfy-if-node-else-part n) v))

(defun js3-bfy-print-if-node (n i)
  (if (or (not (or js3-bfy-compact js3-bfy-compact-if))
	  (js3-bfy-if-node-else-part n)
	  (and (js3-bfy-block-node-p (js3-bfy-if-node-then-part n))
	       (> (length (js3-bfy-block-node-kids
			   (js3-bfy-if-node-then-part n)))
		  1)))
      (js3-bfy-print-if-node-long n i)
    (let ((oldstr js3-bfy-curstr))
      (js3-bfy-print-if-node-compact n i)
      (when (and (not (string= js3-bfy-curstr oldstr))
		 (or (> (length js3-bfy-curln) js3-bfy-max-columns)
		     (let ((c (compare-strings js3-bfy-curstr 0 nil
					       oldstr 0 nil))
			   (diffstr))
		       (setq diffstr (substring js3-bfy-curstr c))
		       (string-match "\n\\(.\\|\n\\)" diffstr))))
	(setq js3-bfy-curstr oldstr)
	(js3-bfy-print "")
	(js3-bfy-print-if-node-long n i)))))

(defun js3-bfy-print-if-node-long (n i)
  (js3-bfy-print "if (")
  (js3-bfy-print-expr (js3-bfy-if-node-condition n) 0)
  (js3-bfy-print ") {\n")
  (js3-bfy-print-body (js3-bfy-if-node-then-part n) (1+ i))
  (js3-bfy-print "}\n")
  (cond
   ((not (js3-bfy-if-node-else-part n))
    (js3-bfy-print " "))
   ((js3-bfy-if-node-p (js3-bfy-if-node-else-part n))
    (js3-bfy-print " else ")
    (js3-bfy-print-body (js3-bfy-if-node-else-part n) i))
   (t
    (js3-bfy-print " else {\n")
    (js3-bfy-print-body (js3-bfy-if-node-else-part n) (1+ i))
    (js3-bfy-print "}\n"))))

(defun js3-bfy-print-if-node-compact (n i)
  (js3-bfy-print "if (")
  (js3-bfy-print-expr (js3-bfy-if-node-condition n) 0)
  (js3-bfy-print ") ")
  (js3-bfy-print-body (js3-bfy-if-node-then-part n) (1+ i)))

(defstruct (js3-bfy-try-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-try-node
			  (&key (type js3-bfy-TRY)
				(pos js3-bfy-ts-cursor)
				len
				try-block
				catch-clauses
				finally-block)))
  "AST node for a try-statement."
  try-block
  catch-clauses  ; a lisp list of `js3-bfy-catch-node'
  finally-block) ; a `js3-bfy-finally-node'

(put 'cl-struct-js3-bfy-try-node 'js3-bfy-visitor 'js3-bfy-visit-try-node)
(put 'cl-struct-js3-bfy-try-node 'js3-bfy-printer 'js3-bfy-print-try-node)

(defun js3-bfy-visit-try-node (n v)
  (js3-bfy-visit-ast (js3-bfy-try-node-try-block n) v)
  (dolist (clause (js3-bfy-try-node-catch-clauses n))
    (js3-bfy-visit-ast clause v))
  (js3-bfy-visit-ast (js3-bfy-try-node-finally-block n) v))

(defun js3-bfy-print-try-node (n i)
  (let ((catches (js3-bfy-try-node-catch-clauses n))
        (finally (js3-bfy-try-node-finally-block n)))
    (js3-bfy-print "try {\n")
    (js3-bfy-print-body (js3-bfy-try-node-try-block n) (1+ i))
    (js3-bfy-print "}\n")
    (when catches
      (dolist (catch catches)
        (js3-bfy-print-ast catch i)))
    (if finally
        (js3-bfy-print-ast finally i)
      (js3-bfy-print ""))))

(defstruct (js3-bfy-catch-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-catch-node
			  (&key (type js3-bfy-CATCH)
				(pos js3-bfy-ts-cursor)
				len
				var-name
				guard-kwd
				guard-expr
				block
				lp
				rp)))
  "AST node for a catch clause."
  var-name    ; a `js3-bfy-name-node'
  guard-kwd   ; relative buffer position of "if" in "catch (x if ...)"
  guard-expr  ; catch condition, a `js3-bfy-node'
  block       ; statements, a `js3-bfy-block-node'
  lp          ; buffer position of left-paren, nil if omitted
  rp)         ; buffer position of right-paren, nil if omitted

(put 'cl-struct-js3-bfy-catch-node 'js3-bfy-visitor 'js3-bfy-visit-catch-node)
(put 'cl-struct-js3-bfy-catch-node 'js3-bfy-printer 'js3-bfy-print-catch-node)

(defun js3-bfy-visit-catch-node (n v)
  (js3-bfy-visit-ast (js3-bfy-catch-node-var-name n) v)
  (when (js3-bfy-catch-node-guard-kwd n)
    (js3-bfy-visit-ast (js3-bfy-catch-node-guard-expr n) v))
  (js3-bfy-visit-ast (js3-bfy-catch-node-block n) v))

(defun js3-bfy-print-catch-node (n i)
  (js3-bfy-print " catch (")
  (js3-bfy-print-ast (js3-bfy-catch-node-var-name n) 0)
  (when (js3-bfy-catch-node-guard-kwd n)
    (js3-bfy-print " if ")
    (js3-bfy-print-ast (js3-bfy-catch-node-guard-expr n) 0))
  (js3-bfy-print ") {\n")
  (js3-bfy-print-body (js3-bfy-catch-node-block n) (1+ i))
  (js3-bfy-print "}\n"))

(defstruct (js3-bfy-finally-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-finally-node
			  (&key (type js3-bfy-FINALLY)
				(pos js3-bfy-ts-cursor)
				len
				body)))
  "AST node for a finally clause."
  body)  ; a `js3-bfy-node', often but not always a block node

(put 'cl-struct-js3-bfy-finally-node 'js3-bfy-visitor 'js3-bfy-visit-finally-node)
(put 'cl-struct-js3-bfy-finally-node 'js3-bfy-printer 'js3-bfy-print-finally-node)

(defun js3-bfy-visit-finally-node (n v)
  (js3-bfy-visit-ast (js3-bfy-finally-node-body n) v))

(defun js3-bfy-print-finally-node (n i)
  (js3-bfy-print " finally {\n")
  (js3-bfy-print-body (js3-bfy-finally-node-body n) (1+ i))
  (js3-bfy-print "}\n"))

(defstruct (js3-bfy-switch-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-switch-node
			  (&key (type js3-bfy-SWITCH)
				(pos js3-bfy-ts-cursor)
				len
				discriminant
				cases
				lp
				rp)))
  "AST node for a switch statement."
  discriminant  ; a `js3-bfy-node' (switch expression)
  cases  ; a lisp list of `js3-bfy-case-node'
  lp     ; position of open-paren for discriminant, nil if omitted
  rp)    ; position of close-paren for discriminant, nil if omitted

(put 'cl-struct-js3-bfy-switch-node 'js3-bfy-visitor 'js3-bfy-visit-switch-node)
(put 'cl-struct-js3-bfy-switch-node 'js3-bfy-printer 'js3-bfy-print-switch-node)

(defun js3-bfy-visit-switch-node (n v)
  (js3-bfy-visit-ast (js3-bfy-switch-node-discriminant n) v)
  (dolist (c (js3-bfy-switch-node-cases n))
    (js3-bfy-visit-ast c v)))

(defun js3-bfy-print-switch-node (n i)
  (js3-bfy-print "switch (")
  (js3-bfy-print-ast (js3-bfy-switch-node-discriminant n) 0)
  (js3-bfy-print ") {")
  (dolist (case (js3-bfy-switch-node-cases n))
    (js3-bfy-print-ast case i))
  (js3-bfy-print "\n}\n"))

(defstruct (js3-bfy-case-node
            (:include js3-bfy-block-node)
            (:constructor nil)
            (:constructor make-js3-bfy-case-node
			  (&key (type js3-bfy-CASE)
				(pos js3-bfy-ts-cursor)
				len
				kids
				expr)))
  "AST node for a case clause of a switch statement."
  expr)   ; the case expression (nil for default)

(put 'cl-struct-js3-bfy-case-node 'js3-bfy-visitor 'js3-bfy-visit-case-node)
(put 'cl-struct-js3-bfy-case-node 'js3-bfy-printer 'js3-bfy-print-case-node)

(defun js3-bfy-visit-case-node (n v)
  (js3-bfy-visit-ast (js3-bfy-case-node-expr n) v)
  (js3-bfy-visit-block n v))

(defun js3-bfy-print-case-node (n i)
  (if (null (js3-bfy-case-node-expr n))
      (js3-bfy-print "\ndefault: ")
    (js3-bfy-print "\ncase ")
    (js3-bfy-print-ast (js3-bfy-case-node-expr n) 0)
    (js3-bfy-print ": "))
  (dolist (kid (js3-bfy-case-node-kids n))
    (js3-bfy-print-ast kid (1+ i))))

(defstruct (js3-bfy-throw-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-throw-node
			  (&key (type js3-bfy-THROW)
				(pos js3-bfy-ts-cursor)
				len
				expr)))
  "AST node for a throw statement."
  expr)   ; the expression to throw

(put 'cl-struct-js3-bfy-throw-node 'js3-bfy-visitor 'js3-bfy-visit-throw-node)
(put 'cl-struct-js3-bfy-throw-node 'js3-bfy-printer 'js3-bfy-print-throw-node)

(defun js3-bfy-visit-throw-node (n v)
  (js3-bfy-visit-ast (js3-bfy-throw-node-expr n) v))

(defun js3-bfy-print-throw-node (n i)
  (js3-bfy-print "throw ")
  (js3-bfy-print-ast (js3-bfy-throw-node-expr n) 0)
  (js3-bfy-print " "))

(defstruct (js3-bfy-with-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-with-node
			  (&key (type js3-bfy-WITH)
				(pos js3-bfy-ts-cursor)
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

(put 'cl-struct-js3-bfy-with-node 'js3-bfy-visitor 'js3-bfy-visit-with-node)
(put 'cl-struct-js3-bfy-with-node 'js3-bfy-printer 'js3-bfy-print-with-node)

(defun js3-bfy-visit-with-node (n v)
  (js3-bfy-visit-ast (js3-bfy-with-node-object n) v)
  (js3-bfy-visit-ast (js3-bfy-with-node-body n) v))

(defun js3-bfy-print-with-node (n i)
  (js3-bfy-print "with (")
  (js3-bfy-print-ast (js3-bfy-with-node-object n) 0)
  (js3-bfy-print ") {\n")
  (js3-bfy-print-body (js3-bfy-with-node-body n) (1+ i))
  (js3-bfy-print "}\n"))

(defstruct (js3-bfy-label-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-label-node
			  (&key (type js3-bfy-LABEL)
				(pos js3-bfy-ts-cursor)
				len
				name)))
  "AST node for a statement label or case label."
  name   ; a string
  loop)  ; for validating and code-generating continue-to-label

(put 'cl-struct-js3-bfy-label-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-label-node 'js3-bfy-printer 'js3-bfy-print-label)

(defun js3-bfy-print-label (n i)
  (js3-bfy-print (concat (js3-bfy-label-node-name n) ":")))

(defstruct (js3-bfy-labeled-stmt-node
            (:include js3-bfy-node)
            (:constructor nil)
            ;; type needs to be in `js3-bfy-side-effecting-tokens' to avoid spurious
            ;; no-side-effects warnings, hence js3-bfy-EXPR_RESULT.
            (:constructor make-js3-bfy-labeled-stmt-node
			  (&key (type js3-bfy-EXPR_RESULT)
				(pos js3-bfy-ts-cursor)
				len
				labels
				stmt)))
  "AST node for a statement with one or more labels.
Multiple labels for a statement are collapsed into the labels field."
  labels  ; lisp list of `js3-bfy-label-node'
  stmt)   ; the statement these labels are for

(put 'cl-struct-js3-bfy-labeled-stmt-node 'js3-bfy-visitor 'js3-bfy-visit-labeled-stmt)
(put 'cl-struct-js3-bfy-labeled-stmt-node 'js3-bfy-printer 'js3-bfy-print-labeled-stmt)

(defun js3-bfy-get-label-by-name (lbl-stmt name)
  "Return a `js3-bfy-label-node' by NAME from LBL-STMT's labels list.
Returns nil if no such label is in the list."
  (let ((label-list (js3-bfy-labeled-stmt-node-labels lbl-stmt))
        result)
    (while (and label-list (not result))
      (if (string= (js3-bfy-label-node-name (car label-list)) name)
          (setq result (car label-list))
        (setq label-list (cdr label-list))))
    result))

(defun js3-bfy-visit-labeled-stmt (n v)
  (dolist (label (js3-bfy-labeled-stmt-node-labels n))
    (js3-bfy-visit-ast label v))
  (js3-bfy-visit-ast (js3-bfy-labeled-stmt-node-stmt n) v))

(defun js3-bfy-print-labeled-stmt (n i)
  (dolist (label (js3-bfy-labeled-stmt-node-labels n))
    (js3-bfy-print-ast label i))
  (js3-bfy-print-ast (js3-bfy-labeled-stmt-node-stmt n) (1+ i)))

(defun js3-bfy-labeled-stmt-node-contains (node label)
  "Return t if NODE contains LABEL in its label set.
NODE is a `js3-bfy-labels-node'.  LABEL is an identifier."
  (loop for nl in (js3-bfy-labeled-stmt-node-labels node)
        if (string= label (js3-bfy-label-node-name nl))
        return t
        finally return nil))

(defsubst js3-bfy-labeled-stmt-node-add-label (node label)
  "Add a `js3-bfy-label-node' to the label set for this statement."
  (setf (js3-bfy-labeled-stmt-node-labels node)
        (nconc (js3-bfy-labeled-stmt-node-labels node) (list label))))

(defstruct (js3-bfy-jump-node
            (:include js3-bfy-node)
            (:constructor nil))
  "Abstract supertype of break and continue nodes."
  label   ; `js3-bfy-name-node' for location of label identifier, if present
  target) ; target js3-bfy-labels-node or loop/switch statement

(defun js3-bfy-visit-jump-node (n v)
  (js3-bfy-visit-ast (js3-bfy-jump-node-label n) v))

(defstruct (js3-bfy-break-node
            (:include js3-bfy-jump-node)
            (:constructor nil)
            (:constructor make-js3-bfy-break-node
			  (&key (type js3-bfy-BREAK)
				(pos js3-bfy-ts-cursor)
				len
				label
				target)))
  "AST node for a break statement.
The label field is a `js3-bfy-name-node', possibly nil, for the named label
if provided.  E.g. in 'break foo', it represents 'foo'.  The target field
is the target of the break - a label node or enclosing loop/switch statement.")

(put 'cl-struct-js3-bfy-break-node 'js3-bfy-visitor 'js3-bfy-visit-jump-node)
(put 'cl-struct-js3-bfy-break-node 'js3-bfy-printer 'js3-bfy-print-break-node)

(defun js3-bfy-print-break-node (n i)
  (js3-bfy-print "; break")
  (when (js3-bfy-break-node-label n)
    (js3-bfy-print " ")
    (js3-bfy-print-ast (js3-bfy-break-node-label n) 0)))

(defstruct (js3-bfy-continue-node
            (:include js3-bfy-jump-node)
            (:constructor nil)
            (:constructor make-js3-bfy-continue-node
			  (&key (type js3-bfy-CONTINUE)
				(pos js3-bfy-ts-cursor)
				len
				label
				target)))
  "AST node for a continue statement.
The label field is the user-supplied enclosing label name, a `js3-bfy-name-node'.
It is nil if continue specifies no label.  The target field is the jump target:
a `js3-bfy-label-node' or the innermost enclosing loop.")

(put 'cl-struct-js3-bfy-continue-node 'js3-bfy-visitor 'js3-bfy-visit-jump-node)
(put 'cl-struct-js3-bfy-continue-node 'js3-bfy-printer 'js3-bfy-print-continue-node)

(defun js3-bfy-print-continue-node (n i)
  (js3-bfy-print "; continue")
  (when (js3-bfy-continue-node-label n)
    (js3-bfy-print " ")
    (js3-bfy-print-ast (js3-bfy-continue-node-label n) 0)))

(defstruct (js3-bfy-function-node
            (:include js3-bfy-script-node)
            (:constructor nil)
            (:constructor make-js3-bfy-function-node
			  (&key (type js3-bfy-FUNCTION)
				(pos js3-bfy-ts-cursor)
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
`js3-bfy-name-node', or if it's a destructuring-assignment parameter, a
`js3-bfy-array-node' or `js3-bfy-object-node'."
  ftype            ; FUNCTION, GETTER or SETTER
  form             ; FUNCTION_{STATEMENT|EXPRESSION|EXPRESSION_STATEMENT}
  name             ; function name (a `js3-bfy-name-node', or nil if anonymous)
  params           ; a lisp list of destructuring forms or simple name nodes
  body             ; a `js3-bfy-block-node' or expression node (1.8 only)
  lp               ; position of arg-list open-paren, or nil if omitted
  rp               ; position of arg-list close-paren, or nil if omitted
  ignore-dynamic   ; ignore value of the dynamic-scope flag (interpreter only)
  needs-activation ; t if we need an activation object for this frame
  is-generator     ; t if this function contains a yield
  member-expr)     ; nonstandard Ecma extension from Rhino

(put 'cl-struct-js3-bfy-function-node 'js3-bfy-visitor 'js3-bfy-visit-function-node)
(put 'cl-struct-js3-bfy-function-node 'js3-bfy-printer 'js3-bfy-print-function-node)

(defun js3-bfy-visit-function-node (n v)
  (js3-bfy-visit-ast (js3-bfy-function-node-name n) v)
  (dolist (p (js3-bfy-function-node-params n))
    (js3-bfy-visit-ast p v))
  (js3-bfy-visit-ast (js3-bfy-function-node-body n) v))

(defun js3-bfy-print-function-node (n i)
  (let ((getter (js3-bfy-node-get-prop n 'GETTER_SETTER))
        (name (js3-bfy-function-node-name n))
        (params (js3-bfy-function-node-params n))
        (body (js3-bfy-function-node-body n))
        (expr (eq (js3-bfy-function-node-form n) 'FUNCTION_EXPRESSION)))
    (unless expr
      (js3-bfy-print "\n"))
    (unless getter
      (js3-bfy-print "function"))
    (when name
      (js3-bfy-print " ")
      (js3-bfy-print-ast name 0))
    (js3-bfy-print " (")
    (loop with len = (length params)
          for param in params
          for count from 1
          do
          (js3-bfy-print-ast param 0)
          (if (< count len)
              (js3-bfy-print ", ")))
    (js3-bfy-print ") {\n")
    (js3-bfy-print-body body (1+ i))
    (js3-bfy-print "}")
    (unless expr
      (js3-bfy-print "\n"))))

(defsubst js3-bfy-function-name (node)
  "Return function name for NODE, a `js3-bfy-function-node', or nil if anonymous."
  (and (js3-bfy-function-node-name node)
       (js3-bfy-name-node-name (js3-bfy-function-node-name node))))

;; Having this be an expression node makes it more flexible.
;; There are IDE contexts, such as indentation in a for-loop initializer,
;; that work better if you assume it's an expression.  Whenever we have
;; a standalone var/const declaration, we just wrap with an expr stmt.
;; Eclipse apparently screwed this up and now has two versions, expr and stmt.
(defstruct (js3-bfy-var-decl-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-var-decl-node
			  (&key (type js3-bfy-VAR)
				(pos js3-bfy-token-beg)
				len
				kids
				decl-type)))
  "AST node for a variable declaration list (VAR, CONST or LET).
The node bounds differ depending on the declaration type.  For VAR or
CONST declarations, the bounds include the var/const keyword.  For LET
declarations, the node begins at the position of the first child."
  kids        ; a lisp list of `js3-bfy-var-init-node' structs.
  decl-type)  ; js3-bfy-VAR, js3-bfy-CONST or js3-bfy-LET

(put 'cl-struct-js3-bfy-var-decl-node 'js3-bfy-visitor 'js3-bfy-visit-var-decl)
(put 'cl-struct-js3-bfy-var-decl-node 'js3-bfy-printer 'js3-bfy-print-var-decl)

(defun js3-bfy-visit-var-decl (n v)
  (dolist (kid (js3-bfy-var-decl-node-kids n))
    (js3-bfy-visit-ast kid v)))

(defun js3-bfy-print-var-decl (n i)
  (let ((tt (js3-bfy-var-decl-node-decl-type n)))
    (js3-bfy-print
     (cond
      ((= tt js3-bfy-VAR) "var ")
      ((= tt js3-bfy-LET) "")  ; handled by parent let-{expr/stmt}
      ((= tt js3-bfy-CONST) "const ")
      (t
       (error "malformed var-decl node"))))
    (loop with kids = (js3-bfy-var-decl-node-kids n)
          with len = (length kids)
          for kid in kids
          for count from 1
          do
          (js3-bfy-print-ast kid 0)
          (if (< count len)
              (js3-bfy-print "\n, ")))))

(defstruct (js3-bfy-var-init-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-var-init-node
			  (&key (type js3-bfy-VAR)
				(pos js3-bfy-ts-cursor)
				len
				target
				initializer)))
  "AST node for a variable declaration.
The type field will be js3-bfy-CONST for a const decl."
  target        ; `js3-bfy-name-node', `js3-bfy-object-node', or `js3-bfy-array-node'
  initializer)  ; initializer expression, a `js3-bfy-node'

(put 'cl-struct-js3-bfy-var-init-node 'js3-bfy-visitor 'js3-bfy-visit-var-init-node)
(put 'cl-struct-js3-bfy-var-init-node 'js3-bfy-printer 'js3-bfy-print-var-init-node)

(defun js3-bfy-visit-var-init-node (n v)
  (js3-bfy-visit-ast (js3-bfy-var-init-node-target n) v)
  (js3-bfy-visit-ast (js3-bfy-var-init-node-initializer n) v))

(defun js3-bfy-print-var-init-node (n i)
  (js3-bfy-print-ast (js3-bfy-var-init-node-target n) 0)
  (when (js3-bfy-var-init-node-initializer n)
    (js3-bfy-print " = ")
    (js3-bfy-print-ast (js3-bfy-var-init-node-initializer n) 0)))

(defstruct (js3-bfy-cond-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-cond-node
			  (&key (type js3-bfy-HOOK)
				(pos js3-bfy-ts-cursor)
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

(put 'cl-struct-js3-bfy-cond-node 'js3-bfy-visitor 'js3-bfy-visit-cond-node)
(put 'cl-struct-js3-bfy-cond-node 'js3-bfy-printer 'js3-bfy-print-cond-node)

(defun js3-bfy-visit-cond-node (n v)
  (js3-bfy-visit-ast (js3-bfy-cond-node-test-expr n) v)
  (js3-bfy-visit-ast (js3-bfy-cond-node-true-expr n) v)
  (js3-bfy-visit-ast (js3-bfy-cond-node-false-expr n) v))

(defun js3-bfy-print-cond-node (n i)
  (js3-bfy-print-ast (js3-bfy-cond-node-test-expr n) 0)
  (js3-bfy-print " ? ")
  (js3-bfy-print-ast (js3-bfy-cond-node-true-expr n) 0)
  (js3-bfy-print " : ")
  (js3-bfy-print-ast (js3-bfy-cond-node-false-expr n) 0))

(defstruct (js3-bfy-infix-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-infix-node
			  (&key type
				(pos js3-bfy-ts-cursor)
				len
				op-pos
				left
				right)))
  "Represents infix expressions.
Includes assignment ops like `|=', and the comma operator.
The type field inherited from `js3-bfy-node' holds the operator."
  op-pos    ; buffer position where operator begins
  left      ; any `js3-bfy-node'
  right)    ; any `js3-bfy-node'

(put 'cl-struct-js3-bfy-infix-node 'js3-bfy-visitor 'js3-bfy-visit-infix-node)
(put 'cl-struct-js3-bfy-infix-node 'js3-bfy-printer 'js3-bfy-print-infix-node)

(defun js3-bfy-visit-infix-node (n v)
  (js3-bfy-visit-ast (js3-bfy-infix-node-left n) v)
  (js3-bfy-visit-ast (js3-bfy-infix-node-right n) v))

(defconst js3-bfy-operator-tokens
  (let ((table (make-hash-table :test 'eq))
        (tokens
         (list (cons js3-bfy-IN "in")
               (cons js3-bfy-TYPEOF "typeof")
               (cons js3-bfy-INSTANCEOF "instanceof")
               (cons js3-bfy-DELPROP "delete")
               (cons js3-bfy-COMMA ",")
               (cons js3-bfy-COLON ":")
               (cons js3-bfy-OR "||")
               (cons js3-bfy-AND "&&")
               (cons js3-bfy-INC "++")
               (cons js3-bfy-DEC "--")
               (cons js3-bfy-BITOR "|")
               (cons js3-bfy-BITXOR "^")
               (cons js3-bfy-BITAND "&")
               (cons js3-bfy-EQ "==")
               (cons js3-bfy-NE "!=")
               (cons js3-bfy-LT "<")
               (cons js3-bfy-LE "<=")
               (cons js3-bfy-GT ">")
               (cons js3-bfy-GE ">=")
               (cons js3-bfy-LSH "<<")
               (cons js3-bfy-RSH ">>")
               (cons js3-bfy-URSH ">>>")
               (cons js3-bfy-ADD "+")       ; infix plus
               (cons js3-bfy-SUB "-")       ; infix minus
               (cons js3-bfy-MUL "*")
               (cons js3-bfy-DIV "/")
               (cons js3-bfy-MOD "%")
               (cons js3-bfy-NOT "!")
               (cons js3-bfy-BITNOT "~")
               (cons js3-bfy-POS "+")       ; unary plus
               (cons js3-bfy-NEG "-")       ; unary minus
               (cons js3-bfy-SHEQ "===")    ; shallow equality
               (cons js3-bfy-SHNE "!==")    ; shallow inequality
               (cons js3-bfy-ASSIGN "=")
               (cons js3-bfy-ASSIGN_BITOR "|=")
               (cons js3-bfy-ASSIGN_BITXOR "^=")
               (cons js3-bfy-ASSIGN_BITAND "&=")
               (cons js3-bfy-ASSIGN_LSH "<<=")
               (cons js3-bfy-ASSIGN_RSH ">>=")
               (cons js3-bfy-ASSIGN_URSH ">>>=")
               (cons js3-bfy-ASSIGN_ADD "+=")
               (cons js3-bfy-ASSIGN_SUB "-=")
               (cons js3-bfy-ASSIGN_MUL "*=")
               (cons js3-bfy-ASSIGN_DIV "/=")
               (cons js3-bfy-ASSIGN_MOD "%="))))
    (loop for (k . v) in tokens do
          (puthash k v table))
    table))

(defun js3-bfy-print-infix-node (args &optional delimiter)
  (if (or (not (or js3-bfy-compact js3-bfy-compact-infix))
	  js3-bfy-multiln)
      (js3-bfy-print-infix-node-long args delimiter)
    (let ((oldstr js3-bfy-curstr))
      (js3-bfy-print-infix-node-compact args delimiter)
      (when (and (not (string= js3-bfy-curstr oldstr))
		 (or (> (length js3-bfy-curln) js3-bfy-max-columns)
		     (let ((c (compare-strings js3-bfy-curstr 0 nil
					       oldstr 0 nil))
			   (diffstr))
		       (setq diffstr (substring js3-bfy-curstr c))
		       (string-match "\n" diffstr))))
	(setq js3-bfy-curstr oldstr)
	(js3-bfy-print "")
	(setq js3-bfy-multiln t)
	(js3-bfy-print-infix-node-long args delimiter)
	(setq js3-bfy-multiln nil)))))

(defun js3-bfy-print-infix-node-long (n i)
  (let* ((tt (js3-bfy-node-type n))
         (op (gethash tt js3-bfy-operator-tokens)))
    (unless op
      (error "unrecognized infix operator %s" (js3-bfy-node-type n)))
    (js3-bfy-print-ast (js3-bfy-infix-node-left n) 0)
    (if (and (/= tt js3-bfy-ASSIGN)
	     (/= tt js3-bfy-ASSIGN_BITOR)
	     (/= tt js3-bfy-ASSIGN_BITXOR)
	     (/= tt js3-bfy-ASSIGN_BITAND)
	     (/= tt js3-bfy-ASSIGN_LSH)
	     (/= tt js3-bfy-ASSIGN_RSH)
	     (/= tt js3-bfy-ASSIGN_URSH)
	     (/= tt js3-bfy-ASSIGN_ADD)
	     (/= tt js3-bfy-ASSIGN_SUB)
	     (/= tt js3-bfy-ASSIGN_MUL)
	     (/= tt js3-bfy-ASSIGN_DIV)
	     (/= tt js3-bfy-ASSIGN_MOD))
	(js3-bfy-print "\n"))
    (js3-bfy-print op)
    (js3-bfy-print " ")
    (js3-bfy-print-ast (js3-bfy-infix-node-right n) 0)))

(defun js3-bfy-print-infix-node-compact (n i)
  (let* ((tt (js3-bfy-node-type n))
         (op (gethash tt js3-bfy-operator-tokens)))
    (unless op
      (error "unrecognized infix operator %s" (js3-bfy-node-type n)))
    (js3-bfy-print-ast (js3-bfy-infix-node-left n) 0)
    (unless (= tt js3-bfy-COMMA)
      (js3-bfy-print " "))
    (js3-bfy-print op)
    (js3-bfy-print " ")
    (js3-bfy-print-ast (js3-bfy-infix-node-right n) 0)))

(defstruct (js3-bfy-assign-node
            (:include js3-bfy-infix-node)
            (:constructor nil)
            (:constructor make-js3-bfy-assign-node
			  (&key type
				(pos js3-bfy-ts-cursor)
				len
				op-pos
				left
				right)))
  "Represents any assignment.
The type field holds the actual assignment operator.")

(put 'cl-struct-js3-bfy-assign-node 'js3-bfy-visitor 'js3-bfy-visit-infix-node)
(put 'cl-struct-js3-bfy-assign-node 'js3-bfy-printer 'js3-bfy-print-infix-node)

(defstruct (js3-bfy-unary-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-unary-node
			  (&key type ; required
				(pos js3-bfy-ts-cursor)
				len
				operand)))
  "AST node type for unary operator nodes.
The type field can be NOT, BITNOT, POS, NEG, INC, DEC,
TYPEOF, or DELPROP.  For INC or DEC, a 'postfix node
property is added if the operator follows the operand."
  operand)  ; a `js3-bfy-node' expression

(put 'cl-struct-js3-bfy-unary-node 'js3-bfy-visitor 'js3-bfy-visit-unary-node)
(put 'cl-struct-js3-bfy-unary-node 'js3-bfy-printer 'js3-bfy-print-unary-node)

(defun js3-bfy-visit-unary-node (n v)
  (js3-bfy-visit-ast (js3-bfy-unary-node-operand n) v))

(defun js3-bfy-print-unary-node (n i)
  (let* ((tt (js3-bfy-node-type n))
         (op (gethash tt js3-bfy-operator-tokens))
         (postfix (js3-bfy-node-get-prop n 'postfix)))
    (unless op
      (error "unrecognized unary operator %s" tt))
    (unless postfix
      (js3-bfy-print op))
    (if (or (= tt js3-bfy-TYPEOF)
            (= tt js3-bfy-DELPROP))
        (js3-bfy-print " "))
    (js3-bfy-print-ast (js3-bfy-unary-node-operand n) 0)
    (when postfix
      (js3-bfy-print op))))

(defstruct (js3-bfy-let-node
            (:include js3-bfy-scope)
            (:constructor nil)
            (:constructor make-js3-bfy-let-node
			  (&key (type js3-bfy-LETEXPR)
				(pos js3-bfy-token-beg)
				len
				vars
				body
				lp
				rp)))
  "AST node for a let expression or a let statement.
Note that a let declaration such as let x=6, y=7 is a `js3-bfy-var-decl-node'."
  vars   ; a `js3-bfy-var-decl-node'
  body   ; a `js3-bfy-node' representing the expression or body block
  lp
  rp)

(put 'cl-struct-js3-bfy-let-node 'js3-bfy-visitor 'js3-bfy-visit-let-node)
(put 'cl-struct-js3-bfy-let-node 'js3-bfy-printer 'js3-bfy-print-let-node)

(defun js3-bfy-visit-let-node (n v)
  (js3-bfy-visit-ast (js3-bfy-let-node-vars n) v)
  (js3-bfy-visit-ast (js3-bfy-let-node-body n) v))

(defun js3-bfy-print-let-node (n i)
  (js3-bfy-print "let (")
  (js3-bfy-print-ast (js3-bfy-let-node-vars n) 0)
  (js3-bfy-print ") ")
  (js3-bfy-print-ast (js3-bfy-let-node-body n) i))

(defstruct (js3-bfy-keyword-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-keyword-node
			  (&key type
				(pos js3-bfy-token-beg)
				(len (- js3-bfy-ts-cursor pos)))))
  "AST node representing a literal keyword such as `null'.
Used for `null', `this', `true', `false' and `debugger'.
The node type is set to js3-bfy-NULL, js3-bfy-THIS, etc.")

(put 'cl-struct-js3-bfy-keyword-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-keyword-node 'js3-bfy-printer 'js3-bfy-print-keyword-node)

(defun js3-bfy-print-keyword-node (n i)
  (js3-bfy-print
   (let ((tt (js3-bfy-node-type n)))
     (cond
      ((= tt js3-bfy-THIS) "this")
      ((= tt js3-bfy-NULL) "null")
      ((= tt js3-bfy-TRUE) "true")
      ((= tt js3-bfy-FALSE) "false")
      ((= tt js3-bfy-DEBUGGER) "debugger")
      (t (error "Invalid keyword literal type: %d" tt))))))

(defsubst js3-bfy-this-node-p (node)
  "Return t if this node is a `js3-bfy-literal-node' of type js3-bfy-THIS."
  (eq (js3-bfy-node-type node) js3-bfy-THIS))

(defstruct (js3-bfy-new-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-new-node
			  (&key (type js3-bfy-NEW)
				(pos js3-bfy-token-beg)
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
  initializer) ; experimental Rhino syntax:  optional `js3-bfy-object-node'

(put 'cl-struct-js3-bfy-new-node 'js3-bfy-visitor 'js3-bfy-visit-new-node)
(put 'cl-struct-js3-bfy-new-node 'js3-bfy-printer 'js3-bfy-print-new-node)

(defun js3-bfy-visit-new-node (n v)
  (js3-bfy-visit-ast (js3-bfy-new-node-target n) v)
  (dolist (arg (js3-bfy-new-node-args n))
    (js3-bfy-visit-ast arg v))
  (js3-bfy-visit-ast (js3-bfy-new-node-initializer n) v))

(defun js3-bfy-print-new-node (n i)
  (js3-bfy-print "new ")
  (js3-bfy-print-ast (js3-bfy-new-node-target n))
  (js3-bfy-print "(")
  (js3-bfy-print-list (js3-bfy-new-node-args n))
  (js3-bfy-print ")")
  (when (js3-bfy-new-node-initializer n)
    (js3-bfy-print " ")
    (js3-bfy-print-ast (js3-bfy-new-node-initializer n))))

(defstruct (js3-bfy-name-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-name-node
			  (&key (type js3-bfy-NAME)
				(pos js3-bfy-token-beg)
				(len (- js3-bfy-ts-cursor
					js3-bfy-token-beg))
				(name js3-bfy-ts-string))))
  "AST node for a JavaScript identifier"
  name   ; a string
  scope) ; a `js3-bfy-scope' (optional, used for codegen)

(put 'cl-struct-js3-bfy-name-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-name-node 'js3-bfy-printer 'js3-bfy-print-name-node)

(defun js3-bfy-print-name-node (n i)
  (js3-bfy-print (js3-bfy-name-node-name n)))

(defsubst js3-bfy-name-node-length (node)
  "Return identifier length of NODE, a `js3-bfy-name-node'.
Returns 0 if NODE is nil or its identifier field is nil."
  (if node
      (length (js3-bfy-name-node-name node))
    0))

(defstruct (js3-bfy-number-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-number-node
			  (&key (type js3-bfy-NUMBER)
				(pos js3-bfy-token-beg)
				(len (- js3-bfy-ts-cursor
					js3-bfy-token-beg))
				(value js3-bfy-ts-string)
				(num-value js3-bfy-ts-number))))
  "AST node for a number literal."
  value      ; the original string, e.g. "6.02e23"
  num-value) ; the parsed number value

(put 'cl-struct-js3-bfy-number-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-number-node 'js3-bfy-printer 'js3-bfy-print-number-node)

(defun js3-bfy-print-number-node (n i)
  (js3-bfy-print
   (number-to-string (js3-bfy-number-node-num-value n))))

(defstruct (js3-bfy-regexp-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-regexp-node
			  (&key (type js3-bfy-REGEXP)
				(pos js3-bfy-token-beg)
				(len (- js3-bfy-ts-cursor
					js3-bfy-token-beg))
				value
				flags)))
  "AST node for a regular expression literal."
  value  ; the regexp string, without // delimiters
  flags) ; a string of flags, e.g. `mi'.

(put 'cl-struct-js3-bfy-regexp-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-regexp-node 'js3-bfy-printer 'js3-bfy-print-regexp)

(defun js3-bfy-print-regexp (n i)
  (js3-bfy-print
   (concat
    "/"
    (js3-bfy-regexp-node-value n)
    "/"))
  (if (js3-bfy-regexp-node-flags n)
      (js3-bfy-print (js3-bfy-regexp-node-flags n))))

(defstruct (js3-bfy-string-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-string-node
			  (&key (type js3-bfy-STRING)
				(pos js3-bfy-token-beg)
				(len (- js3-bfy-ts-cursor
					js3-bfy-token-beg))
				(value js3-bfy-ts-string))))
  "String literal.
Escape characters are not evaluated; e.g. \n is 2 chars in value field.
You can tell the quote type by looking at the first character."
  value) ; the characters of the string, including the quotes

(put 'cl-struct-js3-bfy-string-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-string-node 'js3-bfy-printer 'js3-bfy-print-string-node)

(defun js3-bfy-print-string-node (n i)
  (js3-bfy-print (js3-bfy-node-string n)))

(defstruct (js3-bfy-array-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-array-node
			  (&key (type js3-bfy-ARRAYLIT)
				(pos js3-bfy-ts-cursor)
				len
				elems)))
  "AST node for an array literal."
  elems)  ; list of expressions.  [foo,,bar] yields a nil middle element.

(put 'cl-struct-js3-bfy-array-node 'js3-bfy-visitor 'js3-bfy-visit-array-node)
(put 'cl-struct-js3-bfy-array-node 'js3-bfy-printer 'js3-bfy-print-array-node)

(defun js3-bfy-visit-array-node (n v)
  (dolist (e (js3-bfy-array-node-elems n))
    (js3-bfy-visit-ast e v)))

(defun js3-bfy-print-array-node (n i)
  (js3-bfy-print "[")
  (js3-bfy-print-list (js3-bfy-array-node-elems n))
  (js3-bfy-print "]"))

(defstruct (js3-bfy-object-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-object-node
			  (&key (type js3-bfy-OBJECTLIT)
				(pos js3-bfy-ts-cursor)
				len
				elems)))
  "AST node for an object literal expression."
  elems)  ; a lisp list of `js3-bfy-object-prop-node'

(put 'cl-struct-js3-bfy-object-node 'js3-bfy-visitor 'js3-bfy-visit-object-node)
(put 'cl-struct-js3-bfy-object-node 'js3-bfy-printer 'js3-bfy-print-object-node)

(defun js3-bfy-visit-object-node (n v)
  (dolist (e (js3-bfy-object-node-elems n))
    (js3-bfy-visit-ast e v)))

(defun js3-bfy-print-object-node (n i)
  (js3-bfy-print "{")
  (js3-bfy-print-list (js3-bfy-object-node-elems n))
  (js3-bfy-print "}"))

(defstruct (js3-bfy-object-prop-node
            (:include js3-bfy-infix-node)
            (:constructor nil)
            (:constructor make-js3-bfy-object-prop-node
			  (&key (type js3-bfy-COLON)
				(pos js3-bfy-ts-cursor)
				len
				left
				right
				op-pos)))
  "AST node for an object literal prop:value entry.
The `left' field is the property:  a name node, string node or number node.
The `right' field is a `js3-bfy-node' representing the initializer value.")

(put 'cl-struct-js3-bfy-object-prop-node 'js3-bfy-visitor 'js3-bfy-visit-infix-node)
(put 'cl-struct-js3-bfy-object-prop-node 'js3-bfy-printer 'js3-bfy-print-object-prop-node)

(defun js3-bfy-print-object-prop-node (n i)
  (js3-bfy-print-ast (js3-bfy-object-prop-node-left n) 0)
  (js3-bfy-print ": ")
  (js3-bfy-print-ast (js3-bfy-object-prop-node-right n) 0))

(defstruct (js3-bfy-getter-setter-node
            (:include js3-bfy-infix-node)
            (:constructor nil)
            (:constructor make-js3-bfy-getter-setter-node
			  (&key type ; GET or SET
				(pos js3-bfy-ts-cursor)
				len
				left
				right)))
  "AST node for a getter/setter property in an object literal.
The `left' field is the `js3-bfy-name-node' naming the getter/setter prop.
The `right' field is always an anonymous `js3-bfy-function-node' with a node
property `GETTER_SETTER' set to js3-bfy-GET or js3-bfy-SET. ")

(put 'cl-struct-js3-bfy-getter-setter-node 'js3-bfy-visitor 'js3-bfy-visit-infix-node)
(put 'cl-struct-js3-bfy-getter-setter-node 'js3-bfy-printer 'js3-bfy-print-getter-setter)

(defun js3-bfy-print-getter-setter (n i)
  (js3-bfy-print (if (= (js3-bfy-node-type n) js3-bfy-GET) "get " "set "))
  (js3-bfy-print-ast (js3-bfy-getter-setter-node-left n) 0)
  (js3-bfy-print-ast (js3-bfy-getter-setter-node-right n) 0))

(defstruct (js3-bfy-prop-get-node
            (:include js3-bfy-infix-node)
            (:constructor nil)
            (:constructor make-js3-bfy-prop-get-node
			  (&key (type js3-bfy-GETPROP)
				(pos js3-bfy-ts-cursor)
				len
				left
				right)))
  "AST node for a dotted property reference, e.g. foo.bar or foo().bar")

(put 'cl-struct-js3-bfy-prop-get-node 'js3-bfy-visitor 'js3-bfy-visit-prop-get-node)
(put 'cl-struct-js3-bfy-prop-get-node 'js3-bfy-printer 'js3-bfy-print-prop-get-node)

(defun js3-bfy-visit-prop-get-node (n v)
  (js3-bfy-visit-ast (js3-bfy-prop-get-node-left n) v)
  (js3-bfy-visit-ast (js3-bfy-prop-get-node-right n) v))

(defun js3-bfy-print-prop-get-node (n i)
  (js3-bfy-print-ast (js3-bfy-prop-get-node-left n) 0)
  (js3-bfy-print ".")
  (js3-bfy-print-ast (js3-bfy-prop-get-node-right n) 0))

(defstruct (js3-bfy-elem-get-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-elem-get-node
			  (&key (type js3-bfy-GETELEM)
				(pos js3-bfy-ts-cursor)
				len
				target
				element
				lb
				rb)))
  "AST node for an array index expression such as foo[bar]."
  target  ; a `js3-bfy-node' - the expression preceding the "."
  element ; a `js3-bfy-node' - the expression in brackets
  lb      ; position of left-bracket, nil if omitted
  rb)     ; position of right-bracket, nil if omitted

(put 'cl-struct-js3-bfy-elem-get-node 'js3-bfy-visitor 'js3-bfy-visit-elem-get-node)
(put 'cl-struct-js3-bfy-elem-get-node 'js3-bfy-printer 'js3-bfy-print-elem-get-node)

(defun js3-bfy-visit-elem-get-node (n v)
  (when (js3-bfy-elem-get-node-target n)
    (js3-bfy-visit-ast (js3-bfy-elem-get-node-target n) v))
  (when (js3-bfy-elem-get-node-element n)
    (js3-bfy-visit-ast (js3-bfy-elem-get-node-element n) v)))

(defun js3-bfy-print-elem-get-node (n i)
  (js3-bfy-print-ast (js3-bfy-elem-get-node-target n) 0)
  (js3-bfy-print "[")
  (js3-bfy-print-ast (js3-bfy-elem-get-node-element n) 0)
  (js3-bfy-print "]"))

(defstruct (js3-bfy-call-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-call-node
			  (&key (type js3-bfy-CALL)
				(pos js3-bfy-ts-cursor)
				len
				target
				args
				lp
				rp)))
  "AST node for a JavaScript function call."
  target  ; a `js3-bfy-node' evaluating to the function to call
  args  ; a lisp list of `js3-bfy-node' arguments
  lp    ; position of open-paren, or nil if missing
  rp)   ; position of close-paren, or nil if missing

(put 'cl-struct-js3-bfy-call-node 'js3-bfy-visitor 'js3-bfy-visit-call-node)
(put 'cl-struct-js3-bfy-call-node 'js3-bfy-printer 'js3-bfy-print-call-node)

(defun js3-bfy-visit-call-node (n v)
  (js3-bfy-visit-ast (js3-bfy-call-node-target n) v)
  (dolist (arg (js3-bfy-call-node-args n))
    (js3-bfy-visit-ast arg v)))

(defun js3-bfy-print-call-node (n i)
  (js3-bfy-print-ast (js3-bfy-call-node-target n) 0)
  (js3-bfy-print "(")
  (js3-bfy-print-list (js3-bfy-call-node-args n))
  (js3-bfy-print ")"))

(defstruct (js3-bfy-yield-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-yield-node
			  (&key (type js3-bfy-YIELD)
				(pos js3-bfy-ts-cursor)
				len
				value)))
  "AST node for yield statement or expression."
  value) ; optional:  value to be yielded

(put 'cl-struct-js3-bfy-yield-node 'js3-bfy-visitor 'js3-bfy-visit-yield-node)
(put 'cl-struct-js3-bfy-yield-node 'js3-bfy-printer 'js3-bfy-print-yield-node)

(defun js3-bfy-visit-yield-node (n v)
  (js3-bfy-visit-ast (js3-bfy-yield-node-value n) v))

(defun js3-bfy-print-yield-node (n i)
  (js3-bfy-print "yield")
  (when (js3-bfy-yield-node-value n)
    (js3-bfy-print " ")
    (js3-bfy-print-ast (js3-bfy-yield-node-value n) 0)))

(defstruct (js3-bfy-paren-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-paren-node
			  (&key (type js3-bfy-LP)
				(pos js3-bfy-ts-cursor)
				len
				expr)))
  "AST node for a parenthesized expression.
In particular, used when the parens are syntactically optional,
as opposed to required parens such as those enclosing an if-conditional."
  expr)   ; `js3-bfy-node'

(put 'cl-struct-js3-bfy-paren-node 'js3-bfy-visitor 'js3-bfy-visit-paren-node)
(put 'cl-struct-js3-bfy-paren-node 'js3-bfy-printer 'js3-bfy-print-paren-node)

(defun js3-bfy-visit-paren-node (n v)
  (js3-bfy-visit-ast (js3-bfy-paren-node-expr n) v))

(defun js3-bfy-print-paren-node (n i)
  (js3-bfy-print "(")
  (js3-bfy-print-expr (js3-bfy-paren-node-expr n) 0)
  (js3-bfy-print ")"))

(defun js3-bfy-print-expr (n i)
  (if (not (or js3-bfy-compact js3-bfy-compact-expr))
      (js3-bfy-print-ast n i)
    (let ((oldstr js3-bfy-curstr))
      (js3-bfy-print-expr-compact n i)
      (when (and (not (string= js3-bfy-curstr oldstr))
		 (or (> (length js3-bfy-curln) js3-bfy-max-columns)
		     (let ((c (compare-strings js3-bfy-curstr 0 nil
					       oldstr 0 nil))
			   (diffstr))
		       (setq diffstr (substring js3-bfy-curstr c))
		       (string-match "\n" diffstr))))
	(setq js3-bfy-curstr oldstr)
	(js3-bfy-print " ")
	(js3-bfy-print-ast n i)
	(js3-bfy-print "\n")))))

(defun js3-bfy-print-expr-compact (n i)
  (js3-bfy-print-ast n i))

(defstruct (js3-bfy-array-comp-node
            (:include js3-bfy-scope)
            (:constructor nil)
            (:constructor make-js3-bfy-array-comp-node
			  (&key (type js3-bfy-ARRAYCOMP)
				(pos js3-bfy-ts-cursor)
				len
				result
				loops
				filter
				if-pos
				lp
				rp)))
  "AST node for an Array comprehension such as [[x,y] for (x in foo) for (y in bar)]."
  result  ; result expression (just after left-bracket)
  loops   ; a lisp list of `js3-bfy-array-comp-loop-node'
  filter  ; guard/filter expression
  if-pos  ; buffer pos of 'if' keyword, if present, else nil
  lp      ; buffer position of if-guard left-paren, or nil if not present
  rp)     ; buffer position of if-guard right-paren, or nil if not present

(put 'cl-struct-js3-bfy-array-comp-node 'js3-bfy-visitor 'js3-bfy-visit-array-comp-node)
(put 'cl-struct-js3-bfy-array-comp-node 'js3-bfy-printer 'js3-bfy-print-array-comp-node)

(defun js3-bfy-visit-array-comp-node (n v)
  (js3-bfy-visit-ast (js3-bfy-array-comp-node-result n) v)
  (dolist (l (js3-bfy-array-comp-node-loops n))
    (js3-bfy-visit-ast l v))
  (js3-bfy-visit-ast (js3-bfy-array-comp-node-filter n) v))

(defun js3-bfy-print-array-comp-node (n i)
  (js3-bfy-print "[")
  (js3-bfy-print-ast (js3-bfy-array-comp-node-result n) 0)
  (dolist (l (js3-bfy-array-comp-node-loops n))
    (js3-bfy-print " ")
    (js3-bfy-print-ast l 0))
  (when (js3-bfy-array-comp-node-filter n)
    (js3-bfy-print " if (")
    (js3-bfy-print-ast (js3-bfy-array-comp-node-filter n) 0))
  (js3-bfy-print ")]"))

(defstruct (js3-bfy-array-comp-loop-node
            (:include js3-bfy-for-in-node)
            (:constructor nil)
            (:constructor make-js3-bfy-array-comp-loop-node
			  (&key (type js3-bfy-FOR)
				(pos js3-bfy-ts-cursor)
				len
				iterator
				object
				in-pos
				foreach-p
				each-pos
				lp
				rp)))
  "AST subtree for each 'for (foo in bar)' loop in an array comprehension.")

(put 'cl-struct-js3-bfy-array-comp-loop-node 'js3-bfy-visitor 'js3-bfy-visit-array-comp-loop)
(put 'cl-struct-js3-bfy-array-comp-loop-node 'js3-bfy-printer 'js3-bfy-print-array-comp-loop)

(defun js3-bfy-visit-array-comp-loop (n v)
  (js3-bfy-visit-ast (js3-bfy-array-comp-loop-node-iterator n) v)
  (js3-bfy-visit-ast (js3-bfy-array-comp-loop-node-object n) v))

(defun js3-bfy-print-array-comp-loop (n i)
  (js3-bfy-print "for (")
  (js3-bfy-print-ast (js3-bfy-array-comp-loop-node-iterator n) 0)
  (js3-bfy-print " in ")
  (js3-bfy-print-ast (js3-bfy-array-comp-loop-node-object n) 0)
  (js3-bfy-print ")"))

(defstruct (js3-bfy-empty-expr-node
            (:include js3-bfy-node)
            (:constructor nil)
            (:constructor make-js3-bfy-empty-expr-node
			  (&key (type js3-bfy-EMPTY)
				(pos js3-bfy-token-beg)
				len)))
  "AST node for an empty expression.")

(put 'cl-struct-js3-bfy-empty-expr-node 'js3-bfy-visitor 'js3-bfy-visit-none)
(put 'cl-struct-js3-bfy-empty-expr-node 'js3-bfy-printer 'js3-bfy-print-none)

;;; Node utilities

(defsubst js3-bfy-node-line (n)
  "Fetch the source line number at the start of node N.
This is O(n) in the length of the source buffer; use prudently."
  (1+ (count-lines (point-min) (js3-bfy-node-abs-pos n))))

(defsubst js3-bfy-block-node-kid (n i)
  "Return child I of node N, or nil if there aren't that many."
  (nth i (js3-bfy-block-node-kids n)))

(defsubst js3-bfy-block-node-first (n)
  "Return first child of block node N, or nil if there is none."
  (first (js3-bfy-block-node-kids n)))

(defun js3-bfy-node-root (n)
  "Return the root of the AST containing N.
If N has no parent pointer, returns N."
  (let ((parent (js3-bfy-node-parent n)))
    (if parent
        (js3-bfy-node-root parent)
      n)))

(defun js3-bfy-node-position-in-parent (node &optional parent)
  "Return the position of NODE in parent's block-kids list.
PARENT can be supplied if known.  Positioned returned is zero-indexed.
Returns 0 if NODE is not a child of a block statement, or if NODE
is not a statement node."
  (let ((p (or parent (js3-bfy-node-parent node)))
        (i 0))
    (if (not (js3-bfy-block-node-p p))
        i
      (or (js3-bfy-position node (js3-bfy-block-node-kids p))
          0))))

(defsubst js3-bfy-node-short-name (n)
  "Return the short name of node N as a string, e.g. `js3-bfy-if-node'."
  (substring (symbol-name (aref n 0))
             (length "cl-struct-")))

(defsubst js3-bfy-node-child-list (node)
  "Return the child list for NODE, a lisp list of nodes.
Works for block nodes, array nodes, obj literals, funarg lists,
var decls and try nodes (for catch clauses).  Note that you should call
`js3-bfy-block-node-kids' on the function body for the body statements.
Returns nil for zero-length child lists or unsupported nodes."
  (cond
   ((js3-bfy-function-node-p node)
    (js3-bfy-function-node-params node))
   ((js3-bfy-block-node-p node)
    (js3-bfy-block-node-kids node))
   ((js3-bfy-try-node-p node)
    (js3-bfy-try-node-catch-clauses node))
   ((js3-bfy-array-node-p node)
    (js3-bfy-array-node-elems node))
   ((js3-bfy-object-node-p node)
    (js3-bfy-object-node-elems node))
   ((js3-bfy-call-node-p node)
    (js3-bfy-call-node-args node))
   ((js3-bfy-new-node-p node)
    (js3-bfy-new-node-args node))
   ((js3-bfy-var-decl-node-p node)
    (js3-bfy-var-decl-node-kids node))
   (t
    nil)))

(defsubst js3-bfy-node-set-child-list (node kids)
  "Set the child list for NODE to KIDS."
  (cond
   ((js3-bfy-function-node-p node)
    (setf (js3-bfy-function-node-params node) kids))
   ((js3-bfy-block-node-p node)
    (setf (js3-bfy-block-node-kids node) kids))
   ((js3-bfy-try-node-p node)
    (setf (js3-bfy-try-node-catch-clauses node) kids))
   ((js3-bfy-array-node-p node)
    (setf (js3-bfy-array-node-elems node) kids))
   ((js3-bfy-object-node-p node)
    (setf (js3-bfy-object-node-elems node) kids))
   ((js3-bfy-call-node-p node)
    (setf (js3-bfy-call-node-args node) kids))
   ((js3-bfy-new-node-p node)
    (setf (js3-bfy-new-node-args node) kids))
   ((js3-bfy-var-decl-node-p node)
    (setf (js3-bfy-var-decl-node-kids node) kids))
   (t
    (error "Unsupported node type: %s" (js3-bfy-node-short-name node))))
  kids)

;; All because Common Lisp doesn't support multiple inheritance for defstructs.
(defconst js3-bfy-paren-expr-nodes
  '(cl-struct-js3-bfy-array-comp-loop-node
    cl-struct-js3-bfy-array-comp-node
    cl-struct-js3-bfy-call-node
    cl-struct-js3-bfy-catch-node
    cl-struct-js3-bfy-do-node
    cl-struct-js3-bfy-elem-get-node
    cl-struct-js3-bfy-for-in-node
    cl-struct-js3-bfy-for-node
    cl-struct-js3-bfy-function-node
    cl-struct-js3-bfy-if-node
    cl-struct-js3-bfy-let-node
    cl-struct-js3-bfy-new-node
    cl-struct-js3-bfy-paren-node
    cl-struct-js3-bfy-switch-node
    cl-struct-js3-bfy-while-node
    cl-struct-js3-bfy-with-node)
  "Node types that can have a parenthesized child expression.
In particular, nodes that respond to `js3-bfy-node-lp' and `js3-bfy-node-rp'.")

(defsubst js3-bfy-paren-expr-node-p (node)
  "Return t for nodes that typically have a parenthesized child expression.
Useful for computing the indentation anchors for arg-lists and conditions.
Note that it may return a false positive, for instance when NODE is
a `js3-bfy-new-node' and there are no arguments or parentheses."
  (memq (aref node 0) js3-bfy-paren-expr-nodes))

;; Fake polymorphism... yech.
(defsubst js3-bfy-node-lp (node)
  "Return relative left-paren position for NODE, if applicable.
For `js3-bfy-elem-get-node' structs, returns left-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js3-bfy-elem-get-node-p node)
    (js3-bfy-elem-get-node-lb node))
   ((js3-bfy-loop-node-p node)
    (js3-bfy-loop-node-lp node))
   ((js3-bfy-function-node-p node)
    (js3-bfy-function-node-lp node))
   ((js3-bfy-if-node-p node)
    (js3-bfy-if-node-lp node))
   ((js3-bfy-new-node-p node)
    (js3-bfy-new-node-lp node))
   ((js3-bfy-call-node-p node)
    (js3-bfy-call-node-lp node))
   ((js3-bfy-paren-node-p node)
    (js3-bfy-node-pos node))
   ((js3-bfy-switch-node-p node)
    (js3-bfy-switch-node-lp node))
   ((js3-bfy-catch-node-p node)
    (js3-bfy-catch-node-lp node))
   ((js3-bfy-let-node-p node)
    (js3-bfy-let-node-lp node))
   ((js3-bfy-array-comp-node-p node)
    (js3-bfy-array-comp-node-lp node))
   ((js3-bfy-with-node-p node)
    (js3-bfy-with-node-lp node))
   (t
    (error "Unsupported node type: %s" (js3-bfy-node-short-name node)))))

;; Fake polymorphism... blech.
(defsubst js3-bfy-node-rp (node)
  "Return relative right-paren position for NODE, if applicable.
For `js3-bfy-elem-get-node' structs, returns right-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js3-bfy-elem-get-node-p node)
    (js3-bfy-elem-get-node-lb node))
   ((js3-bfy-loop-node-p node)
    (js3-bfy-loop-node-rp node))
   ((js3-bfy-function-node-p node)
    (js3-bfy-function-node-rp node))
   ((js3-bfy-if-node-p node)
    (js3-bfy-if-node-rp node))
   ((js3-bfy-new-node-p node)
    (js3-bfy-new-node-rp node))
   ((js3-bfy-call-node-p node)
    (js3-bfy-call-node-rp node))
   ((js3-bfy-paren-node-p node)
    (+ (js3-bfy-node-pos node) (js3-bfy-node-len node)))
   ((js3-bfy-switch-node-p node)
    (js3-bfy-switch-node-rp node))
   ((js3-bfy-catch-node-p node)
    (js3-bfy-catch-node-rp node))
   ((js3-bfy-let-node-p node)
    (js3-bfy-let-node-rp node))
   ((js3-bfy-array-comp-node-p node)
    (js3-bfy-array-comp-node-rp node))
   ((js3-bfy-with-node-p node)
    (js3-bfy-with-node-rp node))
   (t
    (error "Unsupported node type: %s" (js3-bfy-node-short-name node)))))

(defsubst js3-bfy-node-first-child (node)
  "Returns the first element of `js3-bfy-node-child-list' for NODE."
  (car (js3-bfy-node-child-list node)))

(defsubst js3-bfy-node-last-child (node)
  "Returns the last element of `js3-bfy-node-last-child' for NODE."
  (car (last (js3-bfy-node-child-list node))))

(defun js3-bfy-node-prev-sibling (node)
  "Return the previous statement in parent.
Works for parents supported by `js3-bfy-node-child-list'.
Returns nil if NODE is not in the parent, or PARENT is
not a supported node, or if NODE is the first child."
  (let* ((p (js3-bfy-node-parent node))
         (kids (js3-bfy-node-child-list p))
         (sib (car kids)))
    (while (and kids
                (neq node (cadr kids)))
      (setq kids (cdr kids)
            sib (car kids)))
    sib))

(defun js3-bfy-node-next-sibling (node)
  "Return the next statement in parent block.
Returns nil if NODE is not in the block, or PARENT is not
a block node, or if NODE is the last statement."
  (let* ((p (js3-bfy-node-parent node))
         (kids (js3-bfy-node-child-list p)))
    (while (and kids
                (neq node (car kids)))
      (setq kids (cdr kids)))
    (cadr kids)))

(defun js3-bfy-node-find-child-before (pos parent &optional after)
  "Find the last child that starts before POS in parent.
If AFTER is non-nil, returns first child starting after POS.
POS is an absolute buffer position.  PARENT is any node
supported by `js3-bfy-node-child-list'.
Returns nil if no applicable child is found."
  (let ((kids (if (js3-bfy-function-node-p parent)
                  (js3-bfy-block-node-kids (js3-bfy-function-node-body parent))
                (js3-bfy-node-child-list parent)))
        (beg (if (js3-bfy-function-node-p parent)
                 (js3-bfy-node-abs-pos (js3-bfy-function-node-body parent))
               (js3-bfy-node-abs-pos parent)))
        kid
        result
        fn
        (continue t))
    (setq fn (if after '> '<))
    (while (and kids continue)
      (setq kid (car kids))
      (if (funcall fn (+ beg (js3-bfy-node-pos kid)) pos)
          (setq result kid
                continue (if after nil t))
        (setq continue (if after t nil)))
      (setq kids (cdr kids)))
    result))

(defun js3-bfy-node-find-child-after (pos parent)
  "Find first child that starts after POS in parent.
POS is an absolute buffer position.  PARENT is any node
supported by `js3-bfy-node-child-list'.
Returns nil if no applicable child is found."
  (js3-bfy-node-find-child-before pos parent 'after))

(defun js3-bfy-node-replace-child (pos parent new-node)
  "Replace node at index POS in PARENT with NEW-NODE.
Only works for parents supported by `js3-bfy-node-child-list'."
  (let ((kids (js3-bfy-node-child-list parent))
        (i 0))
    (while (< i pos)
      (setq kids (cdr kids)
            i (1+ i)))
    (setcar kids new-node)
    (js3-bfy-node-add-children parent new-node)))

(defun js3-bfy-node-buffer (n)
  "Return the buffer associated with AST N.
Returns nil if the buffer is not set as a property on the root
node, or if parent links were not recorded during parsing."
  (let ((root (js3-bfy-node-root n)))
    (and root
         (js3-bfy-ast-root-p root)
         (js3-bfy-ast-root-buffer root))))

(defsubst js3-bfy-block-node-push (n kid)
  "Push js3-bfy-node KID onto the end of js3-bfy-block-node N's child list.
KID is always added to the -end- of the kids list.
Function also calls `js3-bfy-node-add-children' to add the parent link."
  (let ((kids (js3-bfy-node-child-list n)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (js3-bfy-node-set-child-list n (list kid)))
    (js3-bfy-node-add-children n kid)))

(defun js3-bfy-node-string (node)
  (let ((buf (js3-bfy-node-buffer node))
        pos)
    (unless buf
      (error "No buffer available for node %s" node))
    (save-excursion
      (set-buffer buf)
      (buffer-substring-no-properties (setq pos (js3-bfy-node-abs-pos node))
                                      (+ pos (js3-bfy-node-len node))))))

;; Container for storing the node we're looking for in a traversal.
(defvar js3-bfy-discovered-node nil)
(make-variable-buffer-local 'js3-bfy-discovered-node)

;; Keep track of absolute node position during traversals.
(defvar js3-bfy-visitor-offset nil)
(make-variable-buffer-local 'js3-bfy-visitor-offset)

(defvar js3-bfy-node-search-point nil)
(make-variable-buffer-local 'js3-bfy-node-search-point)

(defun js3-bfy-find-node-at-point ()
  (interactive)
  (let ((node (js3-bfy-node-at-point)))
    (message "%s" (or node "No node found at point"))))

(defun js3-bfy-node-name-at-point ()
  (interactive)
  (let ((node (js3-bfy-node-at-point)))
    (message "%s" (if node
		      (js3-bfy-node-short-name node)
		    "No node found at point."))))

(defun js3-bfy-node-at-point (&optional pos skip-comments)
  "Return AST node at POS, a buffer position, defaulting to current point.
The `js3-bfy-ast' variable must be set to the current parse tree.
Signals an error if the AST (`js3-bfy-ast') is nil.
Always returns a node - if it can't find one, it returns the root.
If SKIP-COMMENTS is non-nil, comment nodes are ignored."
  (let ((ast js3-bfy-ast)
        result)
    (unless ast
      (error "No JavaScript AST available"))
    ;; Look through comments first, since they may be inside nodes that
    ;; would otherwise report a match.
    (setq pos (or pos (point))
          result (if (> pos (js3-bfy-node-abs-end ast))
                     ast
                   (if (not skip-comments)
                       (js3-bfy-comment-at-point pos))))
    (unless result
      (setq js3-bfy-discovered-node nil
            js3-bfy-visitor-offset 0
            js3-bfy-node-search-point pos)
      (unwind-protect
          (catch 'js3-bfy-visit-done
            (js3-bfy-visit-ast ast #'js3-bfy-node-at-point-visitor))
        (setq js3-bfy-visitor-offset nil
              js3-bfy-node-search-point nil))
      (setq result js3-bfy-discovered-node))
    ;; may have found a comment beyond end of last child node,
    ;; since visiting the ast-root looks at the comment-list last.
    (if (and skip-comments
             (js3-bfy-comment-node-p result))
        (setq result nil))
    (or result js3-bfy-ast)))

(defun js3-bfy-node-at-point-visitor (node end-p)
  (let ((rel-pos (js3-bfy-node-pos node))
        abs-pos
        abs-end
        (point js3-bfy-node-search-point))
    (cond
     (end-p
      ;; this evaluates to a non-nil return value, even if it's zero
      (decf js3-bfy-visitor-offset rel-pos))
     ;; we already looked for comments before visiting, and don't want them now
     ((js3-bfy-comment-node-p node)
      nil)
     (t
      (setq abs-pos (incf js3-bfy-visitor-offset rel-pos)
            ;; we only want to use the node if the point is before
            ;; the last character position in the node, so we decrement
            ;; the absolute end by 1.
            abs-end (+ abs-pos (js3-bfy-node-len node) -1))
      (cond
       ;; If this node starts after search-point, stop the search.
       ((> abs-pos point)
        (throw 'js3-bfy-visit-done nil))
       ;; If this node ends before the search-point, don't check kids.
       ((> point abs-end)
        nil)
       (t
        ;; Otherwise point is within this node, possibly in a child.
        (setq js3-bfy-discovered-node node)
        t))))))  ; keep processing kids to look for more specific match

(defsubst js3-bfy-block-comment-p (node)
  "Return non-nil if NODE is a comment node of format `jsdoc' or `block'."
  (and (js3-bfy-comment-node-p node)
       (memq (js3-bfy-comment-node-format node) '(jsdoc block))))

;; TODO:  put the comments in a vector and binary-search them instead
(defun js3-bfy-comment-at-point (&optional pos)
  "Look through scanned comment nodes for one containing POS.
POS is a buffer position that defaults to current point.
Function returns nil if POS was not in any comment node."
  (let ((ast js3-bfy-ast)
        (x (or pos (point)))
        beg
        end)
    (unless ast
      (error "No JavaScript AST available"))
    (catch 'done
      ;; Comments are stored in lexical order.
      (dolist (comment (js3-bfy-ast-root-comments ast) nil)
        (setq beg (js3-bfy-node-abs-pos comment)
              end (+ beg (js3-bfy-node-len comment)))
        (if (and (>= x beg)
                 (<= x end))
            (throw 'done comment))))))

(defun js3-bfy-find-parent-fn (node)
  "Find function enclosing NODE.
Returns nil if NODE is not inside a function."
  (setq node (js3-bfy-node-parent node))
  (while (and node (not (js3-bfy-function-node-p node)))
    (setq node (js3-bfy-node-parent node)))
  (and (js3-bfy-function-node-p node) node))

(defun js3-bfy-find-enclosing-fn (node)
  "Find function or root enclosing NODE."
  (if (js3-bfy-ast-root-p node)
      node
    (setq node (js3-bfy-node-parent node))
    (while (not (or (js3-bfy-ast-root-p node)
                    (js3-bfy-function-node-p node)))
      (setq node (js3-bfy-node-parent node)))
    node))

(defun js3-bfy-find-enclosing-node (beg end)
  "Find script or function fully enclosing BEG and END."
  (let ((node (js3-bfy-node-at-point beg))
        pos
        (continue t))
    (while continue
      (if (or (js3-bfy-ast-root-p node)
              (and (js3-bfy-function-node-p node)
                   (<= (setq pos (js3-bfy-node-abs-pos node)) beg)
                   (>= (+ pos (js3-bfy-node-len node)) end)))
          (setq continue nil)
        (setq node (js3-bfy-node-parent node))))
    node))

(defun js3-bfy-node-parent-script-or-fn (node)
  "Find script or function immediately enclosing NODE.
If NODE is the ast-root, returns nil."
  (if (js3-bfy-ast-root-p node)
      nil
    (setq node (js3-bfy-node-parent node))
    (while (and node (not (or (js3-bfy-function-node-p node)
                              (js3-bfy-script-node-p node))))
      (setq node (js3-bfy-node-parent node)))
    node))

(defsubst js3-bfy-nested-function-p (node)
  "Return t if NODE is a nested function, or is inside a nested function."
  (unless (js3-bfy-ast-root-p node)
    (js3-bfy-function-node-p (if (js3-bfy-function-node-p node)
				 (js3-bfy-node-parent-script-or-fn node)
			       (js3-bfy-node-parent-script-or-fn
				(js3-bfy-node-parent-script-or-fn node))))))

(defsubst js3-bfy-shift-kids (kids start offset)
  (dolist (kid kids)
    (if (> (js3-bfy-node-pos kid) start)
        (incf (js3-bfy-node-pos kid) offset))))

(defsubst js3-bfy-shift-children (parent start offset)
  "Update start-positions of all children of PARENT beyond START."
  (let ((root (js3-bfy-node-root parent)))
    (js3-bfy-shift-kids (js3-bfy-node-child-list parent) start offset)
    (js3-bfy-shift-kids (js3-bfy-ast-root-comments root) start offset)))

(defsubst js3-bfy-node-is-descendant (node ancestor)
  "Return t if NODE is a descendant of ANCESTOR."
  (while (and node
              (neq node ancestor))
    (setq node (js3-bfy-node-parent node)))
  node)

;;; visitor infrastructure

(defun js3-bfy-visit-none (node callback)
  "Visitor for AST node that have no node children."
  nil)

(defun js3-bfy-print-none (node indent)
  "Visitor for AST node with no printed representation.")

(defun js3-bfy-print-body (node indent)
  "Print a statement, or a block without braces."
  (if (js3-bfy-block-node-p node)
      (dolist (kid (js3-bfy-block-node-kids node))
        (js3-bfy-print-ast kid indent))
    (js3-bfy-print-ast node indent)))

(defun js3-bfy-print-list (args &optional delimiter)
  (if (not (or js3-bfy-compact js3-bfy-compact-list))
      (js3-bfy-print-list-long args delimiter)
    (let ((oldstr js3-bfy-curstr))
      (js3-bfy-print-list-compact args delimiter)
      (when (and (not (string= js3-bfy-curstr oldstr))
		 (or (> (length js3-bfy-curln) js3-bfy-max-columns)
		     (let ((c (compare-strings js3-bfy-curstr 0 nil
					       oldstr 0 nil))
			   (diffstr))
		       (setq diffstr (substring js3-bfy-curstr c))
		       (string-match "\n" diffstr))))
	(setq js3-bfy-curstr oldstr)
	(js3-bfy-print "")
	(js3-bfy-print-list-long args delimiter)))))

(defun js3-bfy-print-list-long (args &optional delimiter)
  (loop with len = (length args)
        for arg in args
        for count from 1
        do
        (if (and (= count 1) (> len 1))
	    (js3-bfy-print " "))
        (js3-bfy-print-ast arg 0)
        (if (< count len)
            (js3-bfy-print (or delimiter "\n, "))
	  (when (> len 1)
	    (js3-bfy-print "\n")))))

(defun js3-bfy-print-list-compact (args &optional delimiter)
  (loop with len = (length args)
        for arg in args
        for count from 1
        do
        (if (and (= count 1) (> len 1))
	    (js3-bfy-print ""))
        (js3-bfy-print-ast arg 0)
        (if (< count len)
            (js3-bfy-print (or delimiter ", "))
	  (when (> len 1)
	    (js3-bfy-print "")))))

(defun js3-bfy-print-tree (ast)
  "Prints an AST to js3-bfy-curstr.
Makes `js3-bfy-ast-parent-nodes' available to the printer functions."
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth 1500)))
    (set-buffer (get-buffer-create js3-bfy-temp-buffer))
    (erase-buffer)
    (set-buffer js3-bfy-current-buffer)
    (js3-bfy-print-ast ast)
    (js3-bfy-print "\n")))

(defun js3-bfy-print-ast (node &optional indent)
  "Helper function for printing AST nodes.
Requires `js3-bfy-ast-parent-nodes' to be non-nil.
You should use `js3-bfy-print-tree' instead of this function."
  (let ((printer (get (aref node 0) 'js3-bfy-printer))
        (i (or indent 0))
        (pos (js3-bfy-node-abs-pos node)))
    ;; TODO:  wedge comments in here somewhere
    (if printer
        (funcall printer node i))))

(defconst js3-bfy-side-effecting-tokens
  (let ((tokens (make-bool-vector js3-bfy-num-tokens nil)))
    (dolist (tt (list js3-bfy-ASSIGN
                      js3-bfy-ASSIGN_ADD
                      js3-bfy-ASSIGN_BITAND
                      js3-bfy-ASSIGN_BITOR
                      js3-bfy-ASSIGN_BITXOR
                      js3-bfy-ASSIGN_DIV
                      js3-bfy-ASSIGN_LSH
                      js3-bfy-ASSIGN_MOD
                      js3-bfy-ASSIGN_MUL
                      js3-bfy-ASSIGN_RSH
                      js3-bfy-ASSIGN_SUB
                      js3-bfy-ASSIGN_URSH
                      js3-bfy-BLOCK
                      js3-bfy-BREAK
                      js3-bfy-CALL
                      js3-bfy-CATCH
                      js3-bfy-CATCH_SCOPE
                      js3-bfy-CONST
                      js3-bfy-CONTINUE
                      js3-bfy-DEBUGGER
                      js3-bfy-DEC
                      js3-bfy-DELPROP
                      js3-bfy-DEL_REF
                      js3-bfy-DO
                      js3-bfy-ELSE
                      js3-bfy-EMPTY
                      js3-bfy-ENTERWITH
                      js3-bfy-EXPORT
                      js3-bfy-EXPR_RESULT
                      js3-bfy-FINALLY
                      js3-bfy-FOR
                      js3-bfy-FUNCTION
                      js3-bfy-GOTO
                      js3-bfy-IF
                      js3-bfy-IFEQ
                      js3-bfy-IFNE
                      js3-bfy-IMPORT
                      js3-bfy-INC
                      js3-bfy-JSR
                      js3-bfy-LABEL
                      js3-bfy-LEAVEWITH
                      js3-bfy-LET
                      js3-bfy-LETEXPR
                      js3-bfy-LOCAL_BLOCK
                      js3-bfy-LOOP
                      js3-bfy-NEW
                      js3-bfy-REF_CALL
                      js3-bfy-RETHROW
                      js3-bfy-RETURN
                      js3-bfy-RETURN_RESULT
                      js3-bfy-SEMI
                      js3-bfy-SETELEM
                      js3-bfy-SETELEM_OP
                      js3-bfy-SETNAME
                      js3-bfy-SETPROP
                      js3-bfy-SETPROP_OP
                      js3-bfy-SETVAR
                      js3-bfy-SET_REF
                      js3-bfy-SET_REF_OP
                      js3-bfy-SWITCH
                      js3-bfy-TARGET
                      js3-bfy-THROW
                      js3-bfy-TRY
                      js3-bfy-VAR
                      js3-bfy-WHILE
                      js3-bfy-WITH
                      js3-bfy-WITHEXPR
                      js3-bfy-YIELD))
      (aset tokens tt t))
    (if js3-bfy-instanceof-has-side-effects
        (aset tokens js3-bfy-INSTANCEOF t))
    tokens))

(defun js3-bfy-node-has-side-effects (node)
  "Return t if NODE has side effects."
  (when node  ; makes it easier to handle malformed expressions
    (let ((tt (js3-bfy-node-type node)))
      (cond
       ;; This doubtless needs some work, since EXPR_VOID is used
       ;; in several ways in Rhino, and I may not have caught them all.
       ;; I'll wait for people to notice incorrect warnings.
       ((and (= tt js3-bfy-EXPR_VOID)
             (js3-bfy-expr-stmt-node-p node)) ; but not if EXPR_RESULT
        (js3-bfy-node-has-side-effects (js3-bfy-expr-stmt-node-expr node)))
       ((= tt js3-bfy-COMMA)
        (js3-bfy-node-has-side-effects (js3-bfy-infix-node-right node)))
       ((or (= tt js3-bfy-AND)
            (= tt js3-bfy-OR))
        (or (js3-bfy-node-has-side-effects (js3-bfy-infix-node-right node))
            (js3-bfy-node-has-side-effects (js3-bfy-infix-node-left node))))
       ((= tt js3-bfy-HOOK)
        (and (js3-bfy-node-has-side-effects (js3-bfy-cond-node-true-expr node))
             (js3-bfy-node-has-side-effects (js3-bfy-cond-node-false-expr node))))
       ((js3-bfy-paren-node-p node)
        (js3-bfy-node-has-side-effects (js3-bfy-paren-node-expr node)))
       ((= tt js3-bfy-ERROR) ; avoid cascaded error messages
        nil)
       (t
        (aref js3-bfy-side-effecting-tokens tt))))))

(defun js3-bfy-member-expr-leftmost-name (node)
  "For an expr such as foo.bar.baz, return leftmost node foo.
NODE is any `js3-bfy-node' object.  If it represents a member
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
       ((js3-bfy-name-node-p node)
        (setq result node))
       ((js3-bfy-prop-get-node-p node)
        (setq node (js3-bfy-prop-get-node-left node)))
       (t
        (setq continue nil))))
    result))

(defconst js3-bfy-stmt-node-types
  (list js3-bfy-BLOCK
        js3-bfy-BREAK
        js3-bfy-CONTINUE
        js3-bfy-DO
        js3-bfy-EXPR_RESULT
        js3-bfy-EXPR_VOID
        js3-bfy-FOR
        js3-bfy-IF
        js3-bfy-RETURN
        js3-bfy-SWITCH
        js3-bfy-THROW
        js3-bfy-TRY
        js3-bfy-WHILE
        js3-bfy-WITH)
  "Node types that only appear in statement contexts.
The list does not include nodes that always appear as the child
of another specific statement type, such as switch-cases,
catch and finally blocks, and else-clauses.  The list also excludes
nodes like yield, let and var, which may appear in either expression
or statement context, and in the latter context always have a
`js3-bfy-expr-stmt-node' parent.  Finally, the list does not include
functions or scripts, which are treated separately from statements
by the JavaScript parser and runtime.")

(defun js3-bfy-stmt-node-p (node)
  "Heuristic for figuring out if NODE is a statement.
Some node types can appear in either an expression context or a
statement context, e.g. let-nodes, yield-nodes, and var-decl nodes.
For these node types in a statement context, the parent will be a
`js3-bfy-expr-stmt-node'.
Functions aren't included in the check."
  (memq (js3-bfy-node-type node) js3-bfy-stmt-node-types))

(defsubst js3-bfy-find-first-stmt (node)
  "Search upward starting from NODE looking for a statement.
For purposes of this function, a `js3-bfy-function-node' counts."
  (while (not (or (js3-bfy-stmt-node-p node)
                  (js3-bfy-function-node-p node)))
    (setq node (js3-bfy-node-parent node)))
  node)

(defun js3-bfy-node-parent-stmt (node)
  "Return the node's first ancestor that is a statement.
Returns nil if NODE is a `js3-bfy-ast-root'.  Note that any expression
appearing in a statement context will have a parent that is a
`js3-bfy-expr-stmt-node' that will be returned by this function."
  (let ((parent (js3-bfy-node-parent node)))
    (if (or (null parent)
            (js3-bfy-stmt-node-p parent)
            (and (js3-bfy-function-node-p parent)
                 (neq (js3-bfy-function-node-form parent) 'FUNCTION_EXPRESSION)))
        parent
      (js3-bfy-node-parent-stmt parent))))

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

(defconst js3-bfy-END_UNREACHED 0)
(defconst js3-bfy-END_DROPS_OFF 1)
(defconst js3-bfy-END_RETURNS 2)
(defconst js3-bfy-END_RETURNS_VALUE 4)
(defconst js3-bfy-END_YIELDS 8)

(defun js3-bfy-has-consistent-return-usage (node)
  "Check that every return usage in a function body is consistent.
Returns t if the function satisfies strict mode requirement."
  (let ((n (js3-bfy-end-check node)))
    ;; either it doesn't return a value in any branch...
    (or (js3-bfy-flag-not-set-p n js3-bfy-END_RETURNS_VALUE)
        ;; or it returns a value (or is unreached) at every branch
        (js3-bfy-flag-not-set-p n (logior js3-bfy-END_DROPS_OFF
					  js3-bfy-END_RETURNS
					  js3-bfy-END_YIELDS)))))

(defun js3-bfy-end-check-if (node)
  "Returns in the then and else blocks must be consistent with each other.
If there is no else block, then the return statement can fall through.
Returns logical OR of END_* flags"
  (let ((th (js3-bfy-if-node-then-part node))
        (el (js3-bfy-if-node-else-part node)))
    (if (null th)
        js3-bfy-END_UNREACHED
      (logior (js3-bfy-end-check th) (if el
					 (js3-bfy-end-check el)
				       js3-bfy-END_DROPS_OFF)))))

(defun js3-bfy-end-check-switch (node)
  "Consistency of return statements is checked between the case statements.
If there is no default, then the switch can fall through. If there is a
default, we check to see if all code paths in the default return or if
there is a code path that can fall through.
Returns logical OR of END_* flags."
  (let ((rv js3-bfy-END_UNREACHED)
        default-case)
    ;; examine the cases
    (catch 'break
      (dolist (c (js3-bfy-switch-node-cases node))
        (if (js3-bfy-case-node-expr c)
            (js3-bfy-set-flag rv (js3-bfy-end-check-block c))
          (setq default-case c)
          (throw 'break nil))))
    ;; we don't care how the cases drop into each other
    (js3-bfy-clear-flag rv js3-bfy-END_DROPS_OFF)
    ;; examine the default
    (js3-bfy-set-flag rv (if default-case
			     (js3-bfy-end-check default-case)
			   js3-bfy-END_DROPS_OFF))
    rv))

(defun js3-bfy-end-check-try (node)
  "If the block has a finally, return consistency is checked in the
finally block. If all code paths in the finally return, then the
returns in the try-catch blocks don't matter. If there is a code path
that does not return or if there is no finally block, the returns
of the try and catch blocks are checked for mismatch.
Returns logical OR of END_* flags."
  (let ((finally (js3-bfy-try-node-finally-block node))
        rv)
    ;; check the finally if it exists
    (setq rv (if finally
                 (js3-bfy-end-check (js3-bfy-finally-node-body finally))
               js3-bfy-END_DROPS_OFF))
    ;; If the finally block always returns, then none of the returns
    ;; in the try or catch blocks matter.
    (when (js3-bfy-flag-set-p rv js3-bfy-END_DROPS_OFF)
      (js3-bfy-clear-flag rv js3-bfy-END_DROPS_OFF)
      ;; examine the try block
      (js3-bfy-set-flag rv (js3-bfy-end-check (js3-bfy-try-node-try-block node)))
      ;; check each catch block
      (dolist (cb (js3-bfy-try-node-catch-clauses node))
        (js3-bfy-set-flag rv (js3-bfy-end-check (js3-bfy-catch-node-block cb)))))
    rv))

(defun js3-bfy-end-check-loop (node)
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
  (let ((rv (js3-bfy-end-check (js3-bfy-loop-node-body node)))
        (condition (cond
                    ((js3-bfy-while-node-p node)
                     (js3-bfy-while-node-condition node))
                    ((js3-bfy-do-node-p node)
                     (js3-bfy-do-node-condition node))
                    ((js3-bfy-for-node-p node)
                     (js3-bfy-for-node-condition node)))))

    ;; check to see if the loop condition is always true
    (if (and condition
             (eq (js3-bfy-always-defined-boolean-p condition) 'ALWAYS_TRUE))
        (js3-bfy-clear-flag rv js3-bfy-END_DROPS_OFF))

    ;; look for effect of breaks
    (js3-bfy-set-flag rv (js3-bfy-node-get-prop node
						'CONTROL_BLOCK_PROP
						js3-bfy-END_UNREACHED))
    rv))

(defun js3-bfy-end-check-block (node)
  "A general block of code is examined statement by statement.
If any statement (even a compound one) returns in all branches, then
subsequent statements are not examined.
Returns logical OR of END_* flags."
  (let* ((rv js3-bfy-END_DROPS_OFF)
         (kids (js3-bfy-block-node-kids node))
         (n (car kids)))
    ;; Check each statment.  If the statement can continue onto the next
    ;; one (i.e. END_DROPS_OFF is set), then check the next statement.
    (while (and n (js3-bfy-flag-set-p rv js3-bfy-END_DROPS_OFF))
      (js3-bfy-clear-flag rv js3-bfy-END_DROPS_OFF)
      (js3-bfy-set-flag rv (js3-bfy-end-check n))
      (setq kids (cdr kids)
            n (car kids)))
    rv))

(defun js3-bfy-end-check-label (node)
  "A labeled statement implies that there may be a break to the label.
The function processes the labeled statement and then checks the
CONTROL_BLOCK_PROP property to see if there is ever a break to the
particular label.
Returns logical OR of END_* flags."
  (let ((rv (js3-bfy-end-check (js3-bfy-labeled-stmt-node-stmt node))))
    (logior rv (js3-bfy-node-get-prop node
				      'CONTROL_BLOCK_PROP
				      js3-bfy-END_UNREACHED))))

(defun js3-bfy-end-check-break (node)
  "When a break is encountered annotate the statement being broken
out of by setting its CONTROL_BLOCK_PROP property.
Returns logical OR of END_* flags."
  (and (js3-bfy-break-node-target node)
       (js3-bfy-node-set-prop (js3-bfy-break-node-target node)
			      'CONTROL_BLOCK_PROP
			      js3-bfy-END_DROPS_OFF))
  js3-bfy-END_UNREACHED)

(defun js3-bfy-end-check (node)
  "Examine the body of a function, doing a basic reachability analysis.
Returns a combination of flags END_* flags that indicate
how the function execution can terminate. These constitute only the
pessimistic set of termination conditions. It is possible that at
runtime certain code paths will never be actually taken. Hence this
analysis will flag errors in cases where there may not be errors.
Returns logical OR of END_* flags"
  (let (kid)
    (cond
     ((js3-bfy-break-node-p node)
      (js3-bfy-end-check-break node))
     ((js3-bfy-expr-stmt-node-p node)
      (if (setq kid (js3-bfy-expr-stmt-node-expr node))
          (js3-bfy-end-check kid)
        js3-bfy-END_DROPS_OFF))
     ((or (js3-bfy-continue-node-p node)
          (js3-bfy-throw-node-p node))
      js3-bfy-END_UNREACHED)
     ((js3-bfy-return-node-p node)
      (if (setq kid (js3-bfy-return-node-retval node))
          js3-bfy-END_RETURNS_VALUE
        js3-bfy-END_RETURNS))
     ((js3-bfy-loop-node-p node)
      (js3-bfy-end-check-loop node))
     ((js3-bfy-switch-node-p node)
      (js3-bfy-end-check-switch node))
     ((js3-bfy-labeled-stmt-node-p node)
      (js3-bfy-end-check-label node))
     ((js3-bfy-if-node-p node)
      (js3-bfy-end-check-if node))
     ((js3-bfy-try-node-p node)
      (js3-bfy-end-check-try node))
     ((js3-bfy-block-node-p node)
      (if (null (js3-bfy-block-node-kids node))
          js3-bfy-END_DROPS_OFF
        (js3-bfy-end-check-block node)))
     ((js3-bfy-yield-node-p node)
      js3-bfy-END_YIELDS)
     (t
      js3-bfy-END_DROPS_OFF))))

(defun js3-bfy-always-defined-boolean-p (node)
  "Check if NODE always evaluates to true or false in boolean context.
Returns 'ALWAYS_TRUE, 'ALWAYS_FALSE, or nil if it's neither always true
nor always false."
  (let ((tt (js3-bfy-node-type node))
        num)
    (cond
     ((or (= tt js3-bfy-FALSE) (= tt js3-bfy-NULL))
      'ALWAYS_FALSE)
     ((= tt js3-bfy-TRUE)
      'ALWAYS_TRUE)
     ((= tt js3-bfy-NUMBER)
      (setq num (js3-bfy-number-node-num-value node))
      (if (and (not (eq num 0.0e+NaN))
               (not (zerop num)))
          'ALWAYS_TRUE
        'ALWAYS_FALSE))
     (t
      nil))))

(defun js3-bfy-print (str)
  "Update curstr with the value of str."
  (setq js3-bfy-curstr (concat js3-bfy-curstr str))
  (if (string-match "\n\\(.*\\)\\'" js3-bfy-curstr)
      (setq js3-bfy-curln (match-string 1 js3-bfy-curstr))
    (setq js3-bfy-curln js3-bfy-curstr))
  (set-buffer (get-buffer-create js3-bfy-temp-buffer))
  (insert str)
  (set-buffer js3-bfy-current-buffer))

(provide 'js3-bfy-ast)

;;; js3-bfy-ast.el ends here
