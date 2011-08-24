;;; js3-bfy-vars.el -- byte-compiler support for js3-bfy

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'thingatpt)                    ; forward-symbol etc

(eval-and-compile
  (require 'cc-mode)     ; (only) for `c-populate-syntax-table'
  (require 'cc-langs)    ; it's here in Emacs 21...
  (require 'cc-engine))  ; for `c-paragraph-start' et. al.

(defvar js3-bfy-emacs22 (>= emacs-major-version 22))

(defun js3-bfy-mark-safe-local (name pred)
  "Make the variable NAME buffer-local and mark it as safe file-local
variable with predicate PRED."
  (make-variable-buffer-local name)
  (put name 'safe-local-variable pred))

(defgroup js3-bfy nil
  "A Javascript pretty-printer based on js3-mode."
  :group 'languages)

(defcustom js3-bfy-compact t
  "If set to t, try to shorten as much as possible onto one line.
Overrides other compact settings."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-compact-while nil
  "If set to t, try to shorten while statements onto one line."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-compact-for nil
  "If set to t, try to shorten for statements onto one line."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-compact-if nil
  "If set to t, try to shorten if statements onto one line."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-compact-infix nil
  "If set to t, try to shorten infix expressions onto one line."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-compact-expr nil
  "If set to t, try to shorten expressions onto one line."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-compact-list nil
  "If set to t, try to shorten lists onto one line."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-max-columns 70
  "Max number of columns per line."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-indent-tabs-mode nil
  "Default setting for indent-tabs-mode for js3-bfy."
  :group 'js3-bfy
  :type 'boolean)
(js3-bfy-mark-safe-local 'js3-bfy-indent-tabs-mode 'booleanp)

(defcustom js3-bfy-cleanup-whitespace t
  "Non-nil to invoke `delete-trailing-whitespace' before saves."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-move-point-on-right-click t
  "Non-nil to move insertion point when you right-click.
This makes right-click context menu behavior a bit more intuitive,
since menu operations generally apply to the point.  The exception
is if there is a region selection, in which case the point does -not-
move, so cut/copy/paste etc. can work properly.

Note that IntelliJ moves the point, and Eclipse leaves it alone,
so this behavior is customizable."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-auto-indent-p nil
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :group 'js3-bfy
  :type 'boolean)
(js3-bfy-mark-safe-local 'js3-bfy-auto-indent-p 'booleanp)

(defcustom js3-bfy-consistent-level-indent-inner-bracket nil
  "Non-nil to make indentation level inner bracket consistent,
regardless of the beginning bracket position."
  :group 'js3-bfy
  :type 'boolean)
(js3-bfy-mark-safe-local 'js3-bfy-consistent-level-indent-inner-bracket 'booleanp)

(defcustom js3-bfy-enter-indents-newline nil
  "Non-nil to have Enter/Return key indent the newly-inserted line.
This is unusual for Emacs modes but common in IDEs like Eclipse."
  :type 'boolean
  :group 'js3-bfy)
(js3-bfy-mark-safe-local 'js3-bfy-enter-indents-newline 'booleanp)

(defcustom js3-bfy-rebind-eol-bol-keys nil
  "Non-nil to rebind beginning-of-line and end-of-line keys.
If non-nil, bounce between bol/eol and first/last non-whitespace char."
  :group 'js3-bfy
  :type 'boolean)

(defcustom js3-bfy-electric-keys '("{" "}" "(" ")" "[" "]" ":" ";" "," "*")
  "Keys that auto-indent when `js3-bfy-auto-indent-p' is non-nil.
Each value in the list is passed to `define-key'."
  :type 'list
  :group 'js3-bfy)

(defcustom js3-bfy-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `js3-bfy-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'js3-bfy)
(make-variable-buffer-local 'js3-bfy-idle-timer-delay)

(defcustom js3-bfy-dynamic-idle-timer-adjust 0
  "Positive to adjust `js3-bfy-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `js3-bfy-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so js3-bfy-idle-timer-delay is multiplied by 2.
If `js3-bfy-dynamic-idle-timer-adjust' is 0 or negative,
`js3-bfy-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'js3-bfy)

(defcustom js3-bfy-escape-quotes t
  "Non-nil to disable automatic quote-escaping inside strings."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-squeeze-spaces t
  "Non-nil to normalize whitespace when filling in comments.
Multiple runs of spaces are converted to a single space."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-show-strict-warnings t
  "Non-nil to emit Ecma strict-mode warnings.
Some of the warnings can be individually disabled by other flags,
even if this flag is non-nil."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-strict-trailing-comma-warning t
  "Non-nil to warn about trailing commas in array literals.
Ecma-262 forbids them, but many browsers permit them.  IE is the
big exception, and can produce bugs if you have trailing commas."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-strict-missing-semi-warning nil
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-missing-semi-one-line-override nil
  "Non-nil to permit missing semicolons in one-line functions.
In one-liner functions such as `function identity(x) {return x}'
people often omit the semicolon for a cleaner look.  If you are
such a person, you can suppress the missing-semicolon warning
by setting this variable to t."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-strict-inconsistent-return-warning t
  "Non-nil to warn about mixing returns with value-returns.
It's perfectly legal to have a `return' and a `return foo' in the
same function, but it's often an indicator of a bug, and it also
interferes with type inference (in systems that support it.)"
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-strict-var-redeclaration-warning t
  "Non-nil to warn about redeclaring variables in a script or function."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-strict-var-hides-function-arg-warning t
  "Non-nil to warn about a var decl hiding a function argument."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-skip-preprocessor-directives nil
  "Non-nil to treat lines beginning with # as comments.
Useful for viewing Mozilla JavaScript source code."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-language-version 180
  "Configures what JavaScript language version to recognize.
Currently versions 150, 160, 170 and 180 are supported, corresponding
to JavaScript 1.5, 1.6, 1.7 and 1.8, respectively.  In a nutshell,
1.6 adds E4X support, 1.7 adds let, yield, and Array comprehensions,
and 1.8 adds function closures."
  :type 'integer
  :group 'js3-bfy)

(defcustom js3-bfy-allow-keywords-as-property-names t
  "If non-nil, you can use JavaScript keywords as object property names.
Examples:

var foo = {int: 5, while: 6, continue: 7};
foo.return = 8;

Ecma-262 forbids this syntax, but many browsers support it."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-instanceof-has-side-effects nil
  "If non-nil, treats the instanceof operator as having side effects.
This is useful for xulrunner apps."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-allow-rhino-new-expr-initializer nil
  "Non-nil to support a Rhino's experimental syntactic construct.

Rhino supports the ability to follow a `new' expression with an object
literal, which is used to set additional properties on the new object
after calling its constructor.  Syntax:

new <expr> [ ( arglist ) ] [initializer]

Hence, this expression:

new Object {a: 1, b: 2}

results in an Object with properties a=1 and b=2.  This syntax is
apparently not configurable in Rhino - it's currently always enabled,
as of Rhino version 1.7R2."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-allow-member-expr-as-function-name nil
  "Non-nil to support experimental Rhino syntax for function names.

Rhino supports an experimental syntax configured via the Rhino Context
setting `allowMemberExprAsFunctionName'.  The experimental syntax is:

function <member-expr> ( [ arg-list ] ) { <body> }

Where member-expr is a non-parenthesized 'member expression', which
is anything at the grammar level of a new-expression or lower, meaning
any expression that does not involve infix or unary operators.

When <member-expr> is not a simple identifier, then it is syntactic
sugar for assigning the anonymous function to the <member-expr>.  Hence,
this code:

function a.b().c[2] (x, y) { ... }

is rewritten as:

a.b().c[2] = function(x, y) {...}

which doesn't seem particularly useful, but Rhino permits it."
  :type 'boolean
  :group 'js3-bfy)

(defvar js3-bfy-version 20110809
  "Release number for `js3-bfy'.")

;; scanner variables

(defmacro deflocal (name value &optional comment)
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

;; We record the start and end position of each token.
(deflocal js3-bfy-token-beg 1)
(deflocal js3-bfy-token-end -1)

(defvar js3-bfy-EOF_CHAR -1
  "Represents end of stream.  Distinct from js3-bfy-EOF token type.")

;; I originally used symbols to represent tokens, but Rhino uses
;; ints and then sets various flag bits in them, so ints it is.
;; The upshot is that we need a `js3-bfy-' prefix in front of each name.
(defvar js3-bfy-ERROR -1)
(defvar js3-bfy-EOF 0)
(defvar js3-bfy-EOL 1)
(defvar js3-bfy-ENTERWITH 2)       ; begin interpreter bytecodes
(defvar js3-bfy-LEAVEWITH 3)
(defvar js3-bfy-RETURN 4)
(defvar js3-bfy-GOTO 5)
(defvar js3-bfy-IFEQ 6)
(defvar js3-bfy-IFNE 7)
(defvar js3-bfy-SETNAME 8)
(defvar js3-bfy-BITOR 9)
(defvar js3-bfy-BITXOR 10)
(defvar js3-bfy-BITAND 11)
(defvar js3-bfy-ADD 12)            ; infix plus
(defvar js3-bfy-SUB 13)            ; infix minus
(defvar js3-bfy-MUL 14)
(defvar js3-bfy-DIV 15)
(defvar js3-bfy-MOD 16)
(defvar js3-bfy-LT 17)
(defvar js3-bfy-GT 18)
(defvar js3-bfy-EQ 19)
(defvar js3-bfy-NE 20)
(defvar js3-bfy-LE 21)
(defvar js3-bfy-GE 22)
(defvar js3-bfy-LSH 23)
(defvar js3-bfy-RSH 24)
(defvar js3-bfy-URSH 25)
(defvar js3-bfy-NOT 26)
(defvar js3-bfy-BITNOT 27)
(defvar js3-bfy-POS 28)            ; unary plus
(defvar js3-bfy-NEG 29)            ; unary minus
(defvar js3-bfy-NEW 30)
(defvar js3-bfy-DELPROP 31)
(defvar js3-bfy-TYPEOF 32)
(defvar js3-bfy-GETPROP 33)
(defvar js3-bfy-GETPROPNOWARN 34)
(defvar js3-bfy-SETPROP 35)
(defvar js3-bfy-GETELEM 36)
(defvar js3-bfy-SETELEM 37)
(defvar js3-bfy-CALL 38)
(defvar js3-bfy-NAME 39)           ; an identifier
(defvar js3-bfy-NUMBER 40)
(defvar js3-bfy-STRING 41)
(defvar js3-bfy-NULL 42)
(defvar js3-bfy-THIS 43)
(defvar js3-bfy-FALSE 44)
(defvar js3-bfy-TRUE 45)
(defvar js3-bfy-SHEQ 46)           ; shallow equality (===)
(defvar js3-bfy-SHNE 47)           ; shallow inequality (!==)
(defvar js3-bfy-REGEXP 48)
(defvar js3-bfy-BINDNAME 49)
(defvar js3-bfy-THROW 50)
(defvar js3-bfy-RETHROW 51)        ; rethrow caught exception: catch (e if ) uses it
(defvar js3-bfy-IN 52)
(defvar js3-bfy-INSTANCEOF 53)
(defvar js3-bfy-LOCAL_LOAD 54)
(defvar js3-bfy-GETVAR 55)
(defvar js3-bfy-SETVAR 56)
(defvar js3-bfy-CATCH_SCOPE 57)
(defvar js3-bfy-ENUM_INIT_KEYS 58)
(defvar js3-bfy-ENUM_INIT_VALUES 59)
(defvar js3-bfy-ENUM_INIT_ARRAY 60)
(defvar js3-bfy-ENUM_NEXT 61)
(defvar js3-bfy-ENUM_ID 62)
(defvar js3-bfy-THISFN 63)
(defvar js3-bfy-RETURN_RESULT 64)  ; to return previously stored return result
(defvar js3-bfy-ARRAYLIT 65)       ; array literal
(defvar js3-bfy-OBJECTLIT 66)      ; object literal
(defvar js3-bfy-GET_REF 67)        ; *reference
(defvar js3-bfy-SET_REF 68)        ; *reference = something
(defvar js3-bfy-DEL_REF 69)        ; delete reference
(defvar js3-bfy-REF_CALL 70)       ; f(args) = something or f(args)++
(defvar js3-bfy-REF_SPECIAL 71)    ; reference for special properties like __proto
(defvar js3-bfy-YIELD 72)          ; JS 1.7 yield pseudo keyword

;; deprecated
(defvar js3-bfy-DEPRECATED-A 73)
(defvar js3-bfy-DEPRECATED-B 74)
(defvar js3-bfy-DEPRECATED-C 75)
(defvar js3-bfy-DEPRECATED-D 76)
(defvar js3-bfy-DEPRECATED-E 77)
(defvar js3-bfy-DEPRECATED-F 78)
(defvar js3-bfy-DEPRECATED-G 79)

(defvar js3-bfy-TRY 80)
(defvar js3-bfy-SEMI 81)           ; semicolon
(defvar js3-bfy-LB 82)             ; left and right brackets
(defvar js3-bfy-RB 83)
(defvar js3-bfy-LC 84)             ; left and right curly-braces
(defvar js3-bfy-RC 85)
(defvar js3-bfy-LP 86)             ; left and right parens
(defvar js3-bfy-RP 87)
(defvar js3-bfy-COMMA 88)          ; comma operator

(defvar js3-bfy-ASSIGN 89)         ; simple assignment (=)
(defvar js3-bfy-ASSIGN_BITOR 90)   ; |=
(defvar js3-bfy-ASSIGN_BITXOR 91)  ; ^=
(defvar js3-bfy-ASSIGN_BITAND 92)  ; &=
(defvar js3-bfy-ASSIGN_ADD 93)     ; +=
(defvar js3-bfy-ASSIGN_SUB 94)     ; -=
(defvar js3-bfy-ASSIGN_MUL 95)     ; *=
(defvar js3-bfy-ASSIGN_DIV 96)     ; /=
(defvar js3-bfy-ASSIGN_MOD 97)     ; %=
(defvar js3-bfy-ASSIGN_LSH 98)     ; <<=
(defvar js3-bfy-ASSIGN_RSH 99)     ; >>=
(defvar js3-bfy-ASSIGN_URSH 100)   ; >>>=

(defvar js3-bfy-first-assign js3-bfy-ASSIGN)
(defvar js3-bfy-last-assign js3-bfy-ASSIGN_MOD)

(defvar js3-bfy-HOOK 101)          ; conditional (?:)
(defvar js3-bfy-COLON 102)
(defvar js3-bfy-OR 103)            ; logical or (||)
(defvar js3-bfy-AND 104)           ; logical and (&&)
(defvar js3-bfy-INC 105)           ; increment/decrement (++ --)
(defvar js3-bfy-DEC 106)
(defvar js3-bfy-DOT 107)           ; member operator (.)
(defvar js3-bfy-FUNCTION 108)      ; function keyword
(defvar js3-bfy-EXPORT 109)        ; export keyword
(defvar js3-bfy-IMPORT 110)        ; import keyword
(defvar js3-bfy-IF 111)            ; if keyword
(defvar js3-bfy-ELSE 112)          ; else keyword
(defvar js3-bfy-SWITCH 113)        ; switch keyword
(defvar js3-bfy-CASE 114)          ; case keyword
(defvar js3-bfy-DEFAULT 115)       ; default keyword
(defvar js3-bfy-WHILE 116)         ; while keyword
(defvar js3-bfy-DO 117)            ; do keyword
(defvar js3-bfy-FOR 118)           ; for keyword
(defvar js3-bfy-BREAK 119)         ; break keyword
(defvar js3-bfy-CONTINUE 120)      ; continue keyword
(defvar js3-bfy-VAR 121)           ; var keyword
(defvar js3-bfy-WITH 122)          ; with keyword
(defvar js3-bfy-CATCH 123)         ; catch keyword
(defvar js3-bfy-FINALLY 124)       ; finally keyword
(defvar js3-bfy-VOID 125)          ; void keyword
(defvar js3-bfy-RESERVED 126)      ; reserved keywords

(defvar js3-bfy-EMPTY 127)

;; Types used for the parse tree - never returned by scanner.

(defvar js3-bfy-BLOCK 128)         ; statement block
(defvar js3-bfy-LABEL 129)         ; label
(defvar js3-bfy-TARGET 130)
(defvar js3-bfy-LOOP 131)
(defvar js3-bfy-EXPR_VOID 132)     ; expression statement in functions
(defvar js3-bfy-EXPR_RESULT 133)   ; expression statement in scripts
(defvar js3-bfy-JSR 134)
(defvar js3-bfy-SCRIPT 135)        ; top-level node for entire script
(defvar js3-bfy-TYPEOFNAME 136)    ; for typeof(simple-name)
(defvar js3-bfy-USE_STACK 137)
(defvar js3-bfy-SETPROP_OP 138)    ; x.y op= something
(defvar js3-bfy-SETELEM_OP 139)    ; x[y] op= something
(defvar js3-bfy-LOCAL_BLOCK 140)
(defvar js3-bfy-SET_REF_OP 141)    ; *reference op= something

;; deprecated
(defvar js3-bfy-DEPRECATED-H 142)
(defvar js3-bfy-DEPRECATED-I 143)
(defvar js3-bfy-DEPRECATED-J 144)
(defvar js3-bfy-DEPRECATED-K 145)
(defvar js3-bfy-DEPRECATED-L 146)
(defvar js3-bfy-DEPRECATED-M 147)

;; Optimizer-only tokens
(defvar js3-bfy-TO_OBJECT 148)
(defvar js3-bfy-TO_DOUBLE 149)

(defvar js3-bfy-GET 150)           ; JS 1.5 get pseudo keyword
(defvar js3-bfy-SET 151)           ; JS 1.5 set pseudo keyword
(defvar js3-bfy-LET 152)           ; JS 1.7 let pseudo keyword
(defvar js3-bfy-CONST 153)
(defvar js3-bfy-SETCONST 154)
(defvar js3-bfy-SETCONSTVAR 155)
(defvar js3-bfy-ARRAYCOMP 156)
(defvar js3-bfy-LETEXPR 157)
(defvar js3-bfy-WITHEXPR 158)
(defvar js3-bfy-DEBUGGER 159)

(defvar js3-bfy-COMMENT 160)
(defvar js3-bfy-ENUM 161)  ; for "enum" reserved word

(defconst js3-bfy-num-tokens (1+ js3-bfy-ENUM))

;; Rhino accepts any string or stream as input.
;; Emacs character processing works best in buffers, so we'll
;; assume the input is a buffer.  JavaScript strings can be
;; copied into temp buffers before scanning them.

;; Buffer-local variables yield much cleaner code than using `defstruct'.
;; They're the Emacs equivalent of instance variables, more or less.

(deflocal js3-bfy-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(deflocal js3-bfy-ts-regexp-flags nil
  "Token stream buffer-local variable.")

(deflocal js3-bfy-ts-string ""
  "Token stream buffer-local variable.
Last string scanned.")

(deflocal js3-bfy-ts-number nil
  "Token stream buffer-local variable.
Last literal number scanned.")

(deflocal js3-bfy-ts-hit-eof nil
  "Token stream buffer-local variable.")

(deflocal js3-bfy-ts-line-start 0
  "Token stream buffer-local variable.")

(deflocal js3-bfy-ts-lineno 1
  "Token stream buffer-local variable.")

(deflocal js3-bfy-ts-line-end-char -1
  "Token stream buffer-local variable.")

(deflocal js3-bfy-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

(deflocal js3-bfy-ts-string-buffer nil
  "Token stream buffer-local variable.
List of chars built up while scanning various tokens.")

(deflocal js3-bfy-ts-comment-type nil
  "Token stream buffer-local variable.")

;;; Parser variables

(deflocal js3-bfy-parsed-errors nil
  "List of errors produced during scanning/parsing.")

(deflocal js3-bfy-parsed-warnings nil
  "List of warnings produced during scanning/parsing.")

(deflocal js3-bfy-recover-from-parse-errors t
  "Non-nil to continue parsing after a syntax error.

In recovery mode, the AST will be built in full, and any error
nodes will be flagged with appropriate error information.  If
this flag is nil, a syntax error will result in an error being
signaled.

The variable is automatically buffer-local, because different
modes that use the parser will need different settings.")

(deflocal js3-bfy-parse-hook nil
  "List of callbacks for receiving parsing progress.")

(defvar js3-bfy-parse-finished-hook nil
  "List of callbacks to notify when parsing finishes.
Not called if parsing was interrupted.")

(deflocal js3-bfy-is-eval-code nil
  "True if we're evaluating code in a string.
If non-nil, the tokenizer will record the token text, and the AST nodes
will record their source text.  Off by default for IDE modes, since the
text is available in the buffer.")

(defvar js3-bfy-parse-ide-mode t
  "Non-nil if the parser is being used for `js3-bfy'.
If non-nil, the parser will set text properties for fontification
and the syntax-table.  The value should be nil when using the
parser as a frontend to an interpreter or byte compiler.")

;;; Parser instance variables (buffer-local vars for js3-bfy-parse)

(defconst js3-bfy-clear-ti-mask #xFFFF
  "Mask to clear token information bits.")

(defconst js3-bfy-ti-after-eol (lsh 1 16)
  "Flag:  first token of the source line.")

(defconst js3-bfy-ti-check-label (lsh 1 17)
  "Flag:  indicates to check for label.")

;; Inline Rhino's CompilerEnvirons vars as buffer-locals.

(deflocal js3-bfy-compiler-generate-debug-info t)
(deflocal js3-bfy-compiler-use-dynamic-scope nil)
(deflocal js3-bfy-compiler-reserved-keywords-as-identifier nil)
(deflocal js3-bfy-compiler-optimization-level 0)
(deflocal js3-bfy-compiler-generating-source t)
(deflocal js3-bfy-compiler-strict-mode nil)
(deflocal js3-bfy-compiler-report-warning-as-error nil)
(deflocal js3-bfy-compiler-generate-observer-count nil)
(deflocal js3-bfy-compiler-activation-names nil)

;; SKIP:  sourceURI

;; There's a compileFunction method in Context.java - may need it.
(deflocal js3-bfy-called-by-compile-function nil
  "True if `js3-bfy-parse' was called by `js3-bfy-compile-function'.
Will only be used when we finish implementing the interpreter.")

;; SKIP:  ts  (we just call `js3-bfy-init-scanner' and use its vars)

(deflocal js3-bfy-current-flagged-token js3-bfy-EOF)
(deflocal js3-bfy-current-token js3-bfy-EOF)

;; SKIP:  node factory - we're going to just call functions directly,
;; and eventually go to a unified AST format.

(deflocal js3-bfy-nesting-of-function 0)

(deflocal js3-bfy-recorded-identifiers nil
  "Tracks identifiers found during parsing.")

;;; The following variables are per-function and should be saved/restored
;;; during function parsing...

(deflocal js3-bfy-current-script-or-fn nil)
(deflocal js3-bfy-current-scope nil)
(deflocal js3-bfy-nesting-of-with 0)
(deflocal js3-bfy-label-set nil
  "An alist mapping label names to nodes.")

(deflocal js3-bfy-loop-set nil)
(deflocal js3-bfy-loop-and-switch-set nil)
(deflocal js3-bfy-has-return-value nil)
(deflocal js3-bfy-end-flags 0)

;;; ...end of per function variables

;; Without 2-token lookahead, labels are a problem.
;; These vars store the token info of the last matched name,
;; iff it wasn't the last matched token.  Only valid in some contexts.
(defvar js3-bfy-prev-name-token-start nil)
(defvar js3-bfy-prev-name-token-string nil)

(defsubst js3-bfy-save-name-token-data (pos name)
  (setq js3-bfy-prev-name-token-start pos
        js3-bfy-prev-name-token-string name))

;; These flags enumerate the possible ways a statement/function can
;; terminate. These flags are used by endCheck() and by the Parser to
;; detect inconsistent return usage.
;;
;; END_UNREACHED is reserved for code paths that are assumed to always be
;; able to execute (example: throw, continue)
;;
;; END_DROPS_OFF indicates if the statement can transfer control to the
;; next one. Statement such as return dont. A compound statement may have
;; some branch that drops off control to the next statement.
;;
;; END_RETURNS indicates that the statement can return (without arguments)
;; END_RETURNS_VALUE indicates that the statement can return a value.
;;
;; A compound statement such as
;; if (condition) {
;;   return value;
;; }
;; Will be detected as (END_DROPS_OFF | END_RETURN_VALUE) by endCheck()

(defconst js3-bfy-end-unreached     #x0)
(defconst js3-bfy-end-drops-off     #x1)
(defconst js3-bfy-end-returns       #x2)
(defconst js3-bfy-end-returns-value #x4)
(defconst js3-bfy-end-yields        #x8)

;; Rhino awkwardly passes a statementLabel parameter to the
;; statementHelper() function, the main statement parser, which
;; is then used by quite a few of the sub-parsers.  We just make
;; it a buffer-local variable and make sure it's cleaned up properly.
(deflocal js3-bfy-labeled-stmt nil)  ; type `js3-bfy-labeled-stmt-node'

;; Similarly, Rhino passes an inForInit boolean through about half
;; the expression parsers.  We use a dynamically-scoped variable,
;; which makes it easier to funcall the parsers individually without
;; worrying about whether they take the parameter or not.
(deflocal js3-bfy-in-for-init nil)
(deflocal js3-bfy-temp-name-counter 0)
(deflocal js3-bfy-parse-stmt-count 0)

(defsubst js3-bfy-get-next-temp-name ()
  (format "$%d" (incf js3-bfy-temp-name-counter)))

(defvar js3-bfy-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.
This will mostly be useful for interpreters.")

(deflocal js3-bfy-record-comments t
  "Instructs the scanner to record comments in `js3-bfy-scanned-comments'.")

(deflocal js3-bfy-scanned-comments nil
  "List of all comments from the current parse.")

(defcustom js3-bfy-auto-insert-catch-block t
  "Non-nil to insert matching catch block on open-curly after `try'."
  :type 'boolean
  :group 'js3-bfy)

(defcustom js3-bfy-indent-level 2
  "Number of spaces for each indentation step in `js3-bfy'."
  :type 'integer
  :group 'js3-bfy)
(js3-bfy-mark-safe-local 'js3-bfy-indent-level 'integerp)

(defcustom js3-bfy-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `js3-bfy-indent-level'."
  :type 'integer
  :group 'js-mode)
(js3-bfy-mark-safe-local 'js3-bfy-expr-indent-offset 'integerp)

(defcustom js3-bfy-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `js3-bfy-indent-level'."
  :type 'integer
  :group 'js3-bfy
  :version "24.1")
(js3-bfy-mark-safe-local 'js3-bfy-paren-indent-offset 'integerp)

(defcustom js3-bfy-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `js3-bfy-indent-level'."
  :type 'integer
  :group 'js3-bfy
  :version "24.1")
(js3-bfy-mark-safe-local 'js3-bfy-square-indent-offset 'integerp)

(defcustom js3-bfy-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `js3-bfy-indent-level'."
  :type 'integer
  :group 'js3-bfy
  :version "24.1")
(js3-bfy-mark-safe-local 'js3-bfy-curly-indent-offset 'integerp)

(defcustom js3-bfy-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `js3-bfy'."
  :type 'function
  :group 'js3-bfy)

(defconst js3-bfy-identifier-re "[a-zA-Z_$][a-zA-Z0-9_$]*")

(defvar js3-bfy-//-comment-re "^\\(\\s-*\\)//.+"
  "Matches a //-comment line.  Must be first non-whitespace on line.
First match-group is the leading whitespace.")

(defvar js3-bfy-hook nil)

(deflocal js3-bfy-ast nil "Private variable.")
(deflocal js3-bfy-parse-timer nil "Private variable.")
(deflocal js3-bfy-buffer-dirty-p nil "Private variable.")
(deflocal js3-bfy-parsing nil "Private variable.")
(deflocal js3-bfy-node-overlay nil)

(defvar js3-bfy-paragraph-start
  "\\(@[a-zA-Z]+\\>\\|$\\)")

;; Note that we also set a 'c-in-sws text property in html comments,
;; so that `c-forward-sws' and `c-backward-sws' work properly.
(defvar js3-bfy-syntactic-ws-start
  "\\s \\|/[*/]\\|[\n\r]\\|\\\\[\n\r]\\|\\s!\\|<!--\\|^\\s-*-->")

(defvar js3-bfy-syntactic-ws-end
  "\\s \\|[\n\r/]\\|\\s!")

(defvar js3-bfy-syntactic-eol
  (concat "\\s *\\(/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*"
          "\\*+/\\s *\\)*"
          "\\(//\\|/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*$"
          "\\|\\\\$\\|$\\)")
  "Copied from java-mode.  Needed for some cc-engine functions.")

(defvar js3-bfy-comment-prefix-regexp
  "//+\\|\\**")

(defvar js3-bfy-comment-start-skip
  "\\(//+\\|/\\*+\\)\\s *")

(defvar js3-bfy-verbose-parse-p nil
  "Non-nil to emit status messages during parsing.")

(defvar js3-bfy-functions-hidden nil "private variable")
(defvar js3-bfy-comments-hidden nil "private variable")

(defvar js3-bfy-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in js3-bfy buffers.")

(defvar js3-bfy-abbrev-table nil
  "Abbrev table in use in `js3-bfy' buffers.")
(define-abbrev-table 'js3-bfy-abbrev-table ())

(defvar js3-bfy-must-byte-compile t
  "Non-nil to have `js3-bfy' signal an error if not byte-compiled.")

(defvar js3-bfy-pending-parse-callbacks nil
  "List of functions waiting to be notified that parse is finished.")

(defvar js3-bfy-last-indented-line -1)

(defvar js3-bfy-looking-at-parent-for-update nil)
(defvar js3-bfy-node-found-for-update nil)
(defvar js3-bfy-node-for-update nil)
(defvar js3-bfy-pos-for-update 0)
(defvar js3-bfy-multiln nil)
(defvar js3-bfy-current-buffer nil)
(defvar js3-bfy-temp-buffer "js3-bfy-temp")

(eval-when-compile
  (defvar c-paragraph-start nil)
  (defvar c-paragraph-separate nil)
  (defvar c-syntactic-ws-start nil)
  (defvar c-syntactic-ws-end nil)
  (defvar c-syntactic-eol nil)
  (defvar running-xemacs nil)
  (defvar font-lock-mode nil)
  (defvar font-lock-keywords nil))

;; Workaround for buggy Emacs 21 behavior.
(eval-when-compile
  (if (< emacs-major-version 22)
      (defun c-setup-paragraph-variables () nil)))

(provide 'js3-bfy-vars)

;;; js3-bfy-vars.el ends here
