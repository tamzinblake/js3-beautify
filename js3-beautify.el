;;; js3-beautify.el -- a JavaScript beautifier based on js3-mode
;;;

;;; js3-beautify-head.el

;; Author:  Thom Blake (webmaster@thomblake.com)
;;   See js3-mode for additional credits
;;
;; Keywords:  javascript languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Please submit bug reports to github at
;;   https://github.com/thomblake/js3-beautify

;;; Code:

;;; js3-beautify-head.el ends here
;;; js3-beautify-vars.el -- byte-compiler support for js3-beautify

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'thingatpt)                    ; forward-symbol etc

(eval-and-compile
  (require 'cc-mode)     ; (only) for `c-populate-syntax-table'
  (require 'cc-langs)    ; it's here in Emacs 21...
  (require 'cc-engine))  ; for `c-paragraph-start' et. al.

(defvar js3-beautify-emacs22 (>= emacs-major-version 22))

(defun js3-beautify-mark-safe-local (name pred)
  "Make the variable NAME buffer-local and mark it as safe file-local
variable with predicate PRED."
  (make-variable-buffer-local name)
  (put name 'safe-local-variable pred))

(defgroup js3-beautify nil
  "A Javascript pretty-printer based on js3-mode."
  :group 'languages)

(defcustom js3-beautify-max-columns 80
  "Max number of columns per line"
  :group 'js3-beautify
  :type 'boolean)

(defcustom js3-beautify-indent-tabs-mode nil
  "Default setting for indent-tabs-mode for js3-beautify."
  :group 'js3-beautify
  :type 'boolean)
(js3-beautify-mark-safe-local 'js3-beautify-indent-tabs-mode 'booleanp)

(defcustom js3-beautify-pretty-vars t
  "Non-nil to try to indent comma-last continued var statements in a pretty way.
Does not affect comma-first continued var statements.

Note that this forces a reparse so should be turned off if not being used"
  :group 'js3-beautify
  :type 'boolean)
(js3-beautify-mark-safe-local 'js3-beautify-pretty-vars 'booleanp)

(defcustom js3-beautify-cleanup-whitespace t
  "Non-nil to invoke `delete-trailing-whitespace' before saves."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-move-point-on-right-click t
  "Non-nil to move insertion point when you right-click.
This makes right-click context menu behavior a bit more intuitive,
since menu operations generally apply to the point.  The exception
is if there is a region selection, in which case the point does -not-
move, so cut/copy/paste etc. can work properly.

Note that IntelliJ moves the point, and Eclipse leaves it alone,
so this behavior is customizable."
  :group 'js3-beautify
  :type 'boolean)

(defcustom js3-beautify-auto-indent-p nil
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :group 'js3-beautify
  :type 'boolean)
(js3-beautify-mark-safe-local 'js3-beautify-auto-indent-p 'booleanp)

(defcustom js3-beautify-consistent-level-indent-inner-bracket nil
  "Non-nil to make indentation level inner bracket consistent,
regardless of the beginning bracket position."
  :group 'js3-beautify
  :type 'boolean)
(js3-beautify-mark-safe-local 'js3-beautify-consistent-level-indent-inner-bracket 'booleanp)

(defcustom js3-beautify-enter-indents-newline nil
  "Non-nil to have Enter/Return key indent the newly-inserted line.
This is unusual for Emacs modes but common in IDEs like Eclipse."
  :type 'boolean
  :group 'js3-beautify)
(js3-beautify-mark-safe-local 'js3-beautify-enter-indents-newline 'booleanp)

(defcustom js3-beautify-rebind-eol-bol-keys nil
  "Non-nil to rebind beginning-of-line and end-of-line keys.
If non-nil, bounce between bol/eol and first/last non-whitespace char."
  :group 'js3-beautify
  :type 'boolean)

(defcustom js3-beautify-electric-keys '("{" "}" "(" ")" "[" "]" ":" ";" "," "*")
  "Keys that auto-indent when `js3-beautify-auto-indent-p' is non-nil.
Each value in the list is passed to `define-key'."
  :type 'list
  :group 'js3-beautify)

(defcustom js3-beautify-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `js3-beautify-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'js3-beautify)
(make-variable-buffer-local 'js3-beautify-idle-timer-delay)

(defcustom js3-beautify-dynamic-idle-timer-adjust 0
  "Positive to adjust `js3-beautify-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `js3-beautify-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so js3-beautify-idle-timer-delay is multiplied by 2.
If `js3-beautify-dynamic-idle-timer-adjust' is 0 or negative,
`js3-beautify-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'js3-beautify)

(defcustom js3-beautify-escape-quotes t
  "Non-nil to disable automatic quote-escaping inside strings."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-squeeze-spaces t
  "Non-nil to normalize whitespace when filling in comments.
Multiple runs of spaces are converted to a single space."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-show-strict-warnings t
  "Non-nil to emit Ecma strict-mode warnings.
Some of the warnings can be individually disabled by other flags,
even if this flag is non-nil."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-strict-trailing-comma-warning t
  "Non-nil to warn about trailing commas in array literals.
Ecma-262 forbids them, but many browsers permit them.  IE is the
big exception, and can produce bugs if you have trailing commas."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-strict-missing-semi-warning nil
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-missing-semi-one-line-override nil
  "Non-nil to permit missing semicolons in one-line functions.
In one-liner functions such as `function identity(x) {return x}'
people often omit the semicolon for a cleaner look.  If you are
such a person, you can suppress the missing-semicolon warning
by setting this variable to t."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-strict-inconsistent-return-warning t
  "Non-nil to warn about mixing returns with value-returns.
It's perfectly legal to have a `return' and a `return foo' in the
same function, but it's often an indicator of a bug, and it also
interferes with type inference (in systems that support it.)"
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-strict-var-redeclaration-warning t
  "Non-nil to warn about redeclaring variables in a script or function."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-strict-var-hides-function-arg-warning t
  "Non-nil to warn about a var decl hiding a function argument."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-skip-preprocessor-directives nil
  "Non-nil to treat lines beginning with # as comments.
Useful for viewing Mozilla JavaScript source code."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-language-version 180
  "Configures what JavaScript language version to recognize.
Currently versions 150, 160, 170 and 180 are supported, corresponding
to JavaScript 1.5, 1.6, 1.7 and 1.8, respectively.  In a nutshell,
1.6 adds E4X support, 1.7 adds let, yield, and Array comprehensions,
and 1.8 adds function closures."
  :type 'integer
  :group 'js3-beautify)

(defcustom js3-beautify-allow-keywords-as-property-names t
  "If non-nil, you can use JavaScript keywords as object property names.
Examples:

var foo = {int: 5, while: 6, continue: 7};
foo.return = 8;

Ecma-262 forbids this syntax, but many browsers support it."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-instanceof-has-side-effects nil
  "If non-nil, treats the instanceof operator as having side effects.
This is useful for xulrunner apps."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-allow-rhino-new-expr-initializer nil
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
  :group 'js3-beautify)

(defcustom js3-beautify-allow-member-expr-as-function-name nil
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
  :group 'js3-beautify)

(defvar js3-beautify-version 20110809
  "Release number for `js3-beautify'.")

;; scanner variables

(defmacro deflocal (name value &optional comment)
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

;; We record the start and end position of each token.
(deflocal js3-beautify-token-beg 1)
(deflocal js3-beautify-token-end -1)

(defvar js3-beautify-EOF_CHAR -1
  "Represents end of stream.  Distinct from js3-beautify-EOF token type.")

;; I originally used symbols to represent tokens, but Rhino uses
;; ints and then sets various flag bits in them, so ints it is.
;; The upshot is that we need a `js3-beautify-' prefix in front of each name.
(defvar js3-beautify-ERROR -1)
(defvar js3-beautify-EOF 0)
(defvar js3-beautify-EOL 1)
(defvar js3-beautify-ENTERWITH 2)       ; begin interpreter bytecodes
(defvar js3-beautify-LEAVEWITH 3)
(defvar js3-beautify-RETURN 4)
(defvar js3-beautify-GOTO 5)
(defvar js3-beautify-IFEQ 6)
(defvar js3-beautify-IFNE 7)
(defvar js3-beautify-SETNAME 8)
(defvar js3-beautify-BITOR 9)
(defvar js3-beautify-BITXOR 10)
(defvar js3-beautify-BITAND 11)
(defvar js3-beautify-ADD 12)            ; infix plus
(defvar js3-beautify-SUB 13)            ; infix minus
(defvar js3-beautify-MUL 14)
(defvar js3-beautify-DIV 15)
(defvar js3-beautify-MOD 16)
(defvar js3-beautify-LT 17)
(defvar js3-beautify-GT 18)
(defvar js3-beautify-EQ 19)
(defvar js3-beautify-NE 20)
(defvar js3-beautify-LE 21)
(defvar js3-beautify-GE 22)
(defvar js3-beautify-LSH 23)
(defvar js3-beautify-RSH 24)
(defvar js3-beautify-URSH 25)
(defvar js3-beautify-NOT 26)
(defvar js3-beautify-BITNOT 27)
(defvar js3-beautify-POS 28)            ; unary plus
(defvar js3-beautify-NEG 29)            ; unary minus
(defvar js3-beautify-NEW 30)
(defvar js3-beautify-DELPROP 31)
(defvar js3-beautify-TYPEOF 32)
(defvar js3-beautify-GETPROP 33)
(defvar js3-beautify-GETPROPNOWARN 34)
(defvar js3-beautify-SETPROP 35)
(defvar js3-beautify-GETELEM 36)
(defvar js3-beautify-SETELEM 37)
(defvar js3-beautify-CALL 38)
(defvar js3-beautify-NAME 39)           ; an identifier
(defvar js3-beautify-NUMBER 40)
(defvar js3-beautify-STRING 41)
(defvar js3-beautify-NULL 42)
(defvar js3-beautify-THIS 43)
(defvar js3-beautify-FALSE 44)
(defvar js3-beautify-TRUE 45)
(defvar js3-beautify-SHEQ 46)           ; shallow equality (===)
(defvar js3-beautify-SHNE 47)           ; shallow inequality (!==)
(defvar js3-beautify-REGEXP 48)
(defvar js3-beautify-BINDNAME 49)
(defvar js3-beautify-THROW 50)
(defvar js3-beautify-RETHROW 51)        ; rethrow caught exception: catch (e if ) uses it
(defvar js3-beautify-IN 52)
(defvar js3-beautify-INSTANCEOF 53)
(defvar js3-beautify-LOCAL_LOAD 54)
(defvar js3-beautify-GETVAR 55)
(defvar js3-beautify-SETVAR 56)
(defvar js3-beautify-CATCH_SCOPE 57)
(defvar js3-beautify-ENUM_INIT_KEYS 58)
(defvar js3-beautify-ENUM_INIT_VALUES 59)
(defvar js3-beautify-ENUM_INIT_ARRAY 60)
(defvar js3-beautify-ENUM_NEXT 61)
(defvar js3-beautify-ENUM_ID 62)
(defvar js3-beautify-THISFN 63)
(defvar js3-beautify-RETURN_RESULT 64)  ; to return previously stored return result
(defvar js3-beautify-ARRAYLIT 65)       ; array literal
(defvar js3-beautify-OBJECTLIT 66)      ; object literal
(defvar js3-beautify-GET_REF 67)        ; *reference
(defvar js3-beautify-SET_REF 68)        ; *reference = something
(defvar js3-beautify-DEL_REF 69)        ; delete reference
(defvar js3-beautify-REF_CALL 70)       ; f(args) = something or f(args)++
(defvar js3-beautify-REF_SPECIAL 71)    ; reference for special properties like __proto
(defvar js3-beautify-YIELD 72)          ; JS 1.7 yield pseudo keyword

;; deprecated
(defvar js3-beautify-DEPRECATED-A 73)
(defvar js3-beautify-DEPRECATED-B 74)
(defvar js3-beautify-DEPRECATED-C 75)
(defvar js3-beautify-DEPRECATED-D 76)
(defvar js3-beautify-DEPRECATED-E 77)
(defvar js3-beautify-DEPRECATED-F 78)
(defvar js3-beautify-DEPRECATED-G 79)

(defvar js3-beautify-TRY 80)
(defvar js3-beautify-SEMI 81)           ; semicolon
(defvar js3-beautify-LB 82)             ; left and right brackets
(defvar js3-beautify-RB 83)
(defvar js3-beautify-LC 84)             ; left and right curly-braces
(defvar js3-beautify-RC 85)
(defvar js3-beautify-LP 86)             ; left and right parens
(defvar js3-beautify-RP 87)
(defvar js3-beautify-COMMA 88)          ; comma operator

(defvar js3-beautify-ASSIGN 89)         ; simple assignment (=)
(defvar js3-beautify-ASSIGN_BITOR 90)   ; |=
(defvar js3-beautify-ASSIGN_BITXOR 91)  ; ^=
(defvar js3-beautify-ASSIGN_BITAND 92)  ; &=
(defvar js3-beautify-ASSIGN_ADD 93)     ; +=
(defvar js3-beautify-ASSIGN_SUB 94)     ; -=
(defvar js3-beautify-ASSIGN_MUL 95)     ; *=
(defvar js3-beautify-ASSIGN_DIV 96)     ; /=
(defvar js3-beautify-ASSIGN_MOD 97)     ; %=
(defvar js3-beautify-ASSIGN_LSH 98)     ; <<=
(defvar js3-beautify-ASSIGN_RSH 99)     ; >>=
(defvar js3-beautify-ASSIGN_URSH 100)   ; >>>=

(defvar js3-beautify-first-assign js3-beautify-ASSIGN)
(defvar js3-beautify-last-assign js3-beautify-ASSIGN_MOD)

(defvar js3-beautify-HOOK 101)          ; conditional (?:)
(defvar js3-beautify-COLON 102)
(defvar js3-beautify-OR 103)            ; logical or (||)
(defvar js3-beautify-AND 104)           ; logical and (&&)
(defvar js3-beautify-INC 105)           ; increment/decrement (++ --)
(defvar js3-beautify-DEC 106)
(defvar js3-beautify-DOT 107)           ; member operator (.)
(defvar js3-beautify-FUNCTION 108)      ; function keyword
(defvar js3-beautify-EXPORT 109)        ; export keyword
(defvar js3-beautify-IMPORT 110)        ; import keyword
(defvar js3-beautify-IF 111)            ; if keyword
(defvar js3-beautify-ELSE 112)          ; else keyword
(defvar js3-beautify-SWITCH 113)        ; switch keyword
(defvar js3-beautify-CASE 114)          ; case keyword
(defvar js3-beautify-DEFAULT 115)       ; default keyword
(defvar js3-beautify-WHILE 116)         ; while keyword
(defvar js3-beautify-DO 117)            ; do keyword
(defvar js3-beautify-FOR 118)           ; for keyword
(defvar js3-beautify-BREAK 119)         ; break keyword
(defvar js3-beautify-CONTINUE 120)      ; continue keyword
(defvar js3-beautify-VAR 121)           ; var keyword
(defvar js3-beautify-WITH 122)          ; with keyword
(defvar js3-beautify-CATCH 123)         ; catch keyword
(defvar js3-beautify-FINALLY 124)       ; finally keyword
(defvar js3-beautify-VOID 125)          ; void keyword
(defvar js3-beautify-RESERVED 126)      ; reserved keywords

(defvar js3-beautify-EMPTY 127)

;; Types used for the parse tree - never returned by scanner.

(defvar js3-beautify-BLOCK 128)         ; statement block
(defvar js3-beautify-LABEL 129)         ; label
(defvar js3-beautify-TARGET 130)
(defvar js3-beautify-LOOP 131)
(defvar js3-beautify-EXPR_VOID 132)     ; expression statement in functions
(defvar js3-beautify-EXPR_RESULT 133)   ; expression statement in scripts
(defvar js3-beautify-JSR 134)
(defvar js3-beautify-SCRIPT 135)        ; top-level node for entire script
(defvar js3-beautify-TYPEOFNAME 136)    ; for typeof(simple-name)
(defvar js3-beautify-USE_STACK 137)
(defvar js3-beautify-SETPROP_OP 138)    ; x.y op= something
(defvar js3-beautify-SETELEM_OP 139)    ; x[y] op= something
(defvar js3-beautify-LOCAL_BLOCK 140)
(defvar js3-beautify-SET_REF_OP 141)    ; *reference op= something

;; deprecated
(defvar js3-beautify-DEPRECATED-H 142)
(defvar js3-beautify-DEPRECATED-I 143)
(defvar js3-beautify-DEPRECATED-J 144)
(defvar js3-beautify-DEPRECATED-K 145)
(defvar js3-beautify-DEPRECATED-L 146)
(defvar js3-beautify-DEPRECATED-M 147)

;; Optimizer-only tokens
(defvar js3-beautify-TO_OBJECT 148)
(defvar js3-beautify-TO_DOUBLE 149)

(defvar js3-beautify-GET 150)           ; JS 1.5 get pseudo keyword
(defvar js3-beautify-SET 151)           ; JS 1.5 set pseudo keyword
(defvar js3-beautify-LET 152)           ; JS 1.7 let pseudo keyword
(defvar js3-beautify-CONST 153)
(defvar js3-beautify-SETCONST 154)
(defvar js3-beautify-SETCONSTVAR 155)
(defvar js3-beautify-ARRAYCOMP 156)
(defvar js3-beautify-LETEXPR 157)
(defvar js3-beautify-WITHEXPR 158)
(defvar js3-beautify-DEBUGGER 159)

(defvar js3-beautify-COMMENT 160)
(defvar js3-beautify-ENUM 161)  ; for "enum" reserved word

(defconst js3-beautify-num-tokens (1+ js3-beautify-ENUM))

;; Rhino accepts any string or stream as input.
;; Emacs character processing works best in buffers, so we'll
;; assume the input is a buffer.  JavaScript strings can be
;; copied into temp buffers before scanning them.

;; Buffer-local variables yield much cleaner code than using `defstruct'.
;; They're the Emacs equivalent of instance variables, more or less.

(deflocal js3-beautify-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(deflocal js3-beautify-ts-regexp-flags nil
  "Token stream buffer-local variable.")

(deflocal js3-beautify-ts-string ""
  "Token stream buffer-local variable.
Last string scanned.")

(deflocal js3-beautify-ts-number nil
  "Token stream buffer-local variable.
Last literal number scanned.")

(deflocal js3-beautify-ts-hit-eof nil
  "Token stream buffer-local variable.")

(deflocal js3-beautify-ts-line-start 0
  "Token stream buffer-local variable.")

(deflocal js3-beautify-ts-lineno 1
  "Token stream buffer-local variable.")

(deflocal js3-beautify-ts-line-end-char -1
  "Token stream buffer-local variable.")

(deflocal js3-beautify-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

(deflocal js3-beautify-ts-string-buffer nil
  "Token stream buffer-local variable.
List of chars built up while scanning various tokens.")

(deflocal js3-beautify-ts-comment-type nil
  "Token stream buffer-local variable.")

;;; Parser variables

(deflocal js3-beautify-parsed-errors nil
  "List of errors produced during scanning/parsing.")

(deflocal js3-beautify-parsed-warnings nil
  "List of warnings produced during scanning/parsing.")

(deflocal js3-beautify-recover-from-parse-errors t
  "Non-nil to continue parsing after a syntax error.

In recovery mode, the AST will be built in full, and any error
nodes will be flagged with appropriate error information.  If
this flag is nil, a syntax error will result in an error being
signaled.

The variable is automatically buffer-local, because different
modes that use the parser will need different settings.")

(deflocal js3-beautify-parse-hook nil
  "List of callbacks for receiving parsing progress.")

(defvar js3-beautify-parse-finished-hook nil
  "List of callbacks to notify when parsing finishes.
Not called if parsing was interrupted.")

(deflocal js3-beautify-is-eval-code nil
  "True if we're evaluating code in a string.
If non-nil, the tokenizer will record the token text, and the AST nodes
will record their source text.  Off by default for IDE modes, since the
text is available in the buffer.")

(defvar js3-beautify-parse-ide-mode t
  "Non-nil if the parser is being used for `js3-beautify'.
If non-nil, the parser will set text properties for fontification
and the syntax-table.  The value should be nil when using the
parser as a frontend to an interpreter or byte compiler.")

;;; Parser instance variables (buffer-local vars for js3-beautify-parse)

(defconst js3-beautify-clear-ti-mask #xFFFF
  "Mask to clear token information bits.")

(defconst js3-beautify-ti-after-eol (lsh 1 16)
  "Flag:  first token of the source line.")

(defconst js3-beautify-ti-check-label (lsh 1 17)
  "Flag:  indicates to check for label.")

;; Inline Rhino's CompilerEnvirons vars as buffer-locals.

(deflocal js3-beautify-compiler-generate-debug-info t)
(deflocal js3-beautify-compiler-use-dynamic-scope nil)
(deflocal js3-beautify-compiler-reserved-keywords-as-identifier nil)
(deflocal js3-beautify-compiler-optimization-level 0)
(deflocal js3-beautify-compiler-generating-source t)
(deflocal js3-beautify-compiler-strict-mode nil)
(deflocal js3-beautify-compiler-report-warning-as-error nil)
(deflocal js3-beautify-compiler-generate-observer-count nil)
(deflocal js3-beautify-compiler-activation-names nil)

;; SKIP:  sourceURI

;; There's a compileFunction method in Context.java - may need it.
(deflocal js3-beautify-called-by-compile-function nil
  "True if `js3-beautify-parse' was called by `js3-beautify-compile-function'.
Will only be used when we finish implementing the interpreter.")

;; SKIP:  ts  (we just call `js3-beautify-init-scanner' and use its vars)

(deflocal js3-beautify-current-flagged-token js3-beautify-EOF)
(deflocal js3-beautify-current-token js3-beautify-EOF)

;; SKIP:  node factory - we're going to just call functions directly,
;; and eventually go to a unified AST format.

(deflocal js3-beautify-nesting-of-function 0)

(deflocal js3-beautify-recorded-identifiers nil
  "Tracks identifiers found during parsing.")

;;; The following variables are per-function and should be saved/restored
;;; during function parsing...

(deflocal js3-beautify-current-script-or-fn nil)
(deflocal js3-beautify-current-scope nil)
(deflocal js3-beautify-nesting-of-with 0)
(deflocal js3-beautify-label-set nil
  "An alist mapping label names to nodes.")

(deflocal js3-beautify-loop-set nil)
(deflocal js3-beautify-loop-and-switch-set nil)
(deflocal js3-beautify-has-return-value nil)
(deflocal js3-beautify-end-flags 0)

;;; ...end of per function variables

;; Without 2-token lookahead, labels are a problem.
;; These vars store the token info of the last matched name,
;; iff it wasn't the last matched token.  Only valid in some contexts.
(defvar js3-beautify-prev-name-token-start nil)
(defvar js3-beautify-prev-name-token-string nil)

(defsubst js3-beautify-save-name-token-data (pos name)
  (setq js3-beautify-prev-name-token-start pos
        js3-beautify-prev-name-token-string name))

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

(defconst js3-beautify-end-unreached     #x0)
(defconst js3-beautify-end-drops-off     #x1)
(defconst js3-beautify-end-returns       #x2)
(defconst js3-beautify-end-returns-value #x4)
(defconst js3-beautify-end-yields        #x8)

;; Rhino awkwardly passes a statementLabel parameter to the
;; statementHelper() function, the main statement parser, which
;; is then used by quite a few of the sub-parsers.  We just make
;; it a buffer-local variable and make sure it's cleaned up properly.
(deflocal js3-beautify-labeled-stmt nil)  ; type `js3-beautify-labeled-stmt-node'

;; Similarly, Rhino passes an inForInit boolean through about half
;; the expression parsers.  We use a dynamically-scoped variable,
;; which makes it easier to funcall the parsers individually without
;; worrying about whether they take the parameter or not.
(deflocal js3-beautify-in-for-init nil)
(deflocal js3-beautify-temp-name-counter 0)
(deflocal js3-beautify-parse-stmt-count 0)

(defsubst js3-beautify-get-next-temp-name ()
  (format "$%d" (incf js3-beautify-temp-name-counter)))

(defvar js3-beautify-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.
This will mostly be useful for interpreters.")

(deflocal js3-beautify-record-comments t
  "Instructs the scanner to record comments in `js3-beautify-scanned-comments'.")

(deflocal js3-beautify-scanned-comments nil
  "List of all comments from the current parse.")

(defcustom js3-beautify-auto-insert-catch-block t
  "Non-nil to insert matching catch block on open-curly after `try'."
  :type 'boolean
  :group 'js3-beautify)

(defcustom js3-beautify-indent-level 2
  "Number of spaces for each indentation step in `js3-beautify'."
  :type 'integer
  :group 'js3-beautify)
(js3-beautify-mark-safe-local 'js3-beautify-indent-level 'integerp)

(defcustom js3-beautify-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `js3-beautify-indent-level'."
  :type 'integer
  :group 'js-mode)
(js3-beautify-mark-safe-local 'js3-beautify-expr-indent-offset 'integerp)

(defcustom js3-beautify-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `js3-beautify-indent-level'."
  :type 'integer
  :group 'js3-beautify
  :version "24.1")
(js3-beautify-mark-safe-local 'js3-beautify-paren-indent-offset 'integerp)

(defcustom js3-beautify-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `js3-beautify-indent-level'."
  :type 'integer
  :group 'js3-beautify
  :version "24.1")
(js3-beautify-mark-safe-local 'js3-beautify-square-indent-offset 'integerp)

(defcustom js3-beautify-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `js3-beautify-indent-level'."
  :type 'integer
  :group 'js3-beautify
  :version "24.1")
(js3-beautify-mark-safe-local 'js3-beautify-curly-indent-offset 'integerp)

(defcustom js3-beautify-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `js3-beautify'."
  :type 'function
  :group 'js3-beautify)

(defcustom js3-beautify-reparse-on-indent t
  "Whether `js3-beautify' should perform a reparse before indenting.
Might be slow, but important for comma-first and operator-first style,
as well as pretty var statements."
  :type 'boolean
  :group 'js3-beautify)
(js3-beautify-mark-safe-local 'js3-beautify-lazy-commas 'booleanp)

(defcustom js3-beautify-lazy-commas nil
  "Whether `js3-beautify' should line up commas to the indent-minus-2,
rather than trying to line up to braces."
  :type 'boolean
  :group 'js3-beautify)
(js3-beautify-mark-safe-local 'js3-beautify-lazy-commas 'booleanp)

(defcustom js3-beautify-lazy-operators nil
  "Whether `js3-beautify' should line up operators to the indent-minus-2,
rather than trying to line up to braces."
  :type 'boolean
  :group 'js3-beautify)
(js3-beautify-mark-safe-local 'js3-beautify-lazy-operators 'booleanp)

(defcustom js3-beautify-lazy-dots nil
  "Whether `js3-beautify' should line up dots to the next indent level,
rather than trying to line up to dots."
  :type 'boolean
  :group 'js3-beautify)
(js3-beautify-mark-safe-local 'js3-beautify-lazy-dots 'booleanp)

(defconst js3-beautify-identifier-re "[a-zA-Z_$][a-zA-Z0-9_$]*")

(defvar js3-beautify-//-comment-re "^\\(\\s-*\\)//.+"
  "Matches a //-comment line.  Must be first non-whitespace on line.
First match-group is the leading whitespace.")

(defvar js3-beautify-hook nil)

(deflocal js3-beautify-ast nil "Private variable.")
(deflocal js3-beautify-parse-timer nil "Private variable.")
(deflocal js3-beautify-buffer-dirty-p nil "Private variable.")
(deflocal js3-beautify-parsing nil "Private variable.")
(deflocal js3-beautify-node-overlay nil)

(defvar js3-beautify-paragraph-start
  "\\(@[a-zA-Z]+\\>\\|$\\)")

;; Note that we also set a 'c-in-sws text property in html comments,
;; so that `c-forward-sws' and `c-backward-sws' work properly.
(defvar js3-beautify-syntactic-ws-start
  "\\s \\|/[*/]\\|[\n\r]\\|\\\\[\n\r]\\|\\s!\\|<!--\\|^\\s-*-->")

(defvar js3-beautify-syntactic-ws-end
  "\\s \\|[\n\r/]\\|\\s!")

(defvar js3-beautify-syntactic-eol
  (concat "\\s *\\(/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*"
          "\\*+/\\s *\\)*"
          "\\(//\\|/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*$"
          "\\|\\\\$\\|$\\)")
  "Copied from java-mode.  Needed for some cc-engine functions.")

(defvar js3-beautify-comment-prefix-regexp
  "//+\\|\\**")

(defvar js3-beautify-comment-start-skip
  "\\(//+\\|/\\*+\\)\\s *")

(defvar js3-beautify-verbose-parse-p nil
  "Non-nil to emit status messages during parsing.")

(defvar js3-beautify-functions-hidden nil "private variable")
(defvar js3-beautify-comments-hidden nil "private variable")

(defvar js3-beautify-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in js3-beautify buffers.")

(defvar js3-beautify-abbrev-table nil
  "Abbrev table in use in `js3-beautify' buffers.")
(define-abbrev-table 'js3-beautify-abbrev-table ())

(defvar js3-beautify-must-byte-compile t
  "Non-nil to have `js3-beautify' signal an error if not byte-compiled.")

(defvar js3-beautify-pending-parse-callbacks nil
  "List of functions waiting to be notified that parse is finished.")

(defvar js3-beautify-last-indented-line -1)

(defvar js3-beautify-curstr "")
(defvar js3-beautify-curln "")

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

(provide 'js3-beautify-vars)

;;; js3-beautify-vars.el ends here
;;; js3-beautify-util.el -- JavaScript utilities

;;; Code

(eval-when-compile
  (require 'cl))


;; Emacs21 compatibility, plus some stuff to avoid runtime dependency on CL

(unless (fboundp #'looking-back)
  (defun looking-back (regexp &optional limit greedy)
    "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
    (let ((start (point))
          (pos
           (save-excursion
             (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                  (point)))))
      (if (and greedy pos)
          (save-restriction
            (narrow-to-region (point-min) start)
            (while (and (> pos (point-min))
                        (save-excursion
                          (goto-char pos)
                          (backward-char 1)
                          (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
              (setq pos (1- pos)))
            (save-excursion
              (goto-char pos)
              (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
      (not (null pos)))))

(unless (fboundp #'copy-overlay)
  (defun copy-overlay (o)
    "Return a copy of overlay O."
    (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
                            ;; FIXME: there's no easy way to find the
                            ;; insertion-type of the two markers.
                            (overlay-buffer o)))
          (props (overlay-properties o)))
      (while props
        (overlay-put o1 (pop props) (pop props)))
      o1)))

(unless (fboundp #'remove-overlays)
  (defun remove-overlays (&optional beg end name val)
    "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
    (unless beg (setq beg (point-min)))
    (unless end (setq end (point-max)))
    (if (< end beg)
        (setq beg (prog1 end (setq end beg))))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (eq (overlay-get o name) val)
          ;; Either push this overlay outside beg...end
          ;; or split it to exclude beg...end
          ;; or delete it entirely (if it is contained in beg...end).
          (if (< (overlay-start o) beg)
              (if (> (overlay-end o) end)
                  (progn
                    (move-overlay (copy-overlay o)
                                  (overlay-start o) beg)
                    (move-overlay o end (overlay-end o)))
                (move-overlay o (overlay-start o) beg))
            (if (> (overlay-end o) end)
                (move-overlay o end (overlay-end o))
              (delete-overlay o))))))))

;; we don't want a runtime dependency on the CL package, so define
;; our own versions of these functions.

(defun js3-beautify-delete-if (predicate list)
  "Remove all items satisfying PREDICATE in LIST."
  (loop for item in list
        if (not (funcall predicate item))
        collect item))

(defun js3-beautify-position (element list)
  "Find 0-indexed position of ELEMENT in LIST comparing with `eq'.
Returns nil if element is not found in the list."
  (let ((count 0)
        found)
    (while (and list (not found))
      (if (eq element (car list))
          (setq found t)
        (setq count (1+ count)
              list (cdr list))))
    (if found count)))

(defun js3-beautify-find-if (predicate list)
  "Find first item satisfying PREDICATE in LIST."
  (let (result)
    (while (and list (not result))
      (if (funcall predicate (car list))
          (setq result (car list)))
      (setq list (cdr list)))
    result))

;;; end Emacs 21 compat

(defmacro js3-beautify-time (form)
  "Evaluate FORM, discard result, and return elapsed time in sec"
  (let ((beg (make-symbol "--js3-beautify-time-beg--"))
        (delta (make-symbol "--js3-beautify-time-end--")))
    `(let ((,beg (current-time))
           ,delta)
       ,form
       (/ (truncate (* (- (float-time (current-time))
                          (float-time ,beg))
		       10000))
          10000.0))))

(def-edebug-spec js3-beautify-time t)

(defsubst neq (expr1 expr2)
  "Return (not (eq expr1 expr2))."
  (not (eq expr1 expr2)))

(defsubst js3-beautify-same-line (pos)
  "Return t if POS is on the same line as current point."
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defsubst js3-beautify-same-line-2 (p1 p2)
  "Return t if p1 is on the same line as p2."
  (save-excursion
    (goto-char p1)
    (js3-beautify-same-line p2)))

(defun js3-beautify-code-bug ()
  "Signal an error when we encounter an unexpected code path."
  (error "failed assertion"))

(defsubst js3-beautify-record-text-property (beg end prop value)
  "Set a text property."
  (apply #'put-text-property beg end prop value))

;; I'd like to associate errors with nodes, but for now the
;; easiest thing to do is get the context info from the last token.
(defsubst js3-beautify-record-parse-error (msg &optional arg pos len)
  (push (list (list msg arg)
              (or pos js3-beautify-token-beg)
              (or len (- js3-beautify-token-end js3-beautify-token-beg)))
        js3-beautify-parsed-errors))

(defsubst js3-beautify-report-error (msg &optional msg-arg pos len)
  "Signal a syntax error or record a parse error."
  (if js3-beautify-recover-from-parse-errors
      (js3-beautify-record-parse-error msg msg-arg pos len)
    (signal 'js3-beautify-syntax-error
            (list msg
                  js3-beautify-ts-lineno
                  (save-excursion
                    (goto-char js3-beautify-ts-cursor)
                    (current-column))
                  js3-beautify-ts-hit-eof))))

(defsubst js3-beautify-report-warning (msg &optional msg-arg pos len)
  (if js3-beautify-compiler-report-warning-as-error
      (js3-beautify-report-error msg msg-arg pos len)
    (push (list (list msg msg-arg)
                (or pos js3-beautify-token-beg)
                (or len (- js3-beautify-token-end js3-beautify-token-beg)))
          js3-beautify-parsed-warnings)))

(defsubst js3-beautify-add-strict-warning (msg-id &optional msg-arg beg end)
  (if js3-beautify-compiler-strict-mode
      (js3-beautify-report-warning msg-id msg-arg beg
                          (and beg end (- end beg)))))

(put 'js3-beautify-syntax-error 'error-conditions
     '(error syntax-error js3-beautify-syntax-error))
(put 'js3-beautify-syntax-error 'error-message "Syntax error")

(put 'js3-beautify-parse-error 'error-conditions
     '(error parse-error js3-beautify-parse-error))
(put 'js3-beautify-parse-error 'error-message "Parse error")

(defmacro js3-beautify-clear-flag (flags flag)
  `(setq ,flags (logand ,flags (lognot ,flag))))

(defmacro js3-beautify-set-flag (flags flag)
  "Logical-or FLAG into FLAGS."
  `(setq ,flags (logior ,flags ,flag)))

(defsubst js3-beautify-flag-set-p (flags flag)
  (/= 0 (logand flags flag)))

(defsubst js3-beautify-flag-not-set-p (flags flag)
  (zerop (logand flags flag)))

;; Stolen shamelessly from James Clark's nxml-mode.
(defmacro js3-beautify-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without any text property changes modifying the buffer.
Any text properties changes happen as usual but the changes are not treated as
modifications to the buffer."
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
           (inhibit-read-only t)
           (inhibit-modification-hooks t)
           (buffer-undo-list t)
           (deactivate-mark nil)
           ;; Apparently these avoid file locking problems.
           (buffer-file-name nil)
           (buffer-file-truename nil))
       (unwind-protect
           (progn ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(put 'js3-beautify-with-unmodifying-text-property-changes 'lisp-indent-function 0)
(def-edebug-spec js3-beautify-with-unmodifying-text-property-changes t)

(defmacro js3-beautify-with-underscore-as-word-syntax (&rest body)
  "Evaluate BODY with the _ character set to be word-syntax."
  (let ((old-syntax (make-symbol "old-syntax")))
    `(let ((,old-syntax (string (char-syntax ?_))))
       (unwind-protect
           (progn
             (modify-syntax-entry ?_ "w" js3-beautify-syntax-table)
             ,@body)
         (modify-syntax-entry ?_ ,old-syntax js3-beautify-syntax-table)))))

(put 'js3-beautify-with-underscore-as-word-syntax 'lisp-indent-function 0)
(def-edebug-spec js3-beautify-with-underscore-as-word-syntax t)

(defmacro with-buffer (buf form)
  "Executes FORM in buffer BUF.
BUF can be a buffer name or a buffer object.
If the buffer doesn't exist, it's created."
  `(let ((buffer (gentemp)))
     (setq buffer
           (if (stringp ,buf)
               (get-buffer-create ,buf)
             ,buf))
     (save-excursion
       (set-buffer buffer)
       ,form)))

(defsubst char-is-uppercase (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (downcase c)))

(defsubst char-is-lowercase (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (upcase c)))

(put 'with-buffer 'lisp-indent-function 1)
(def-edebug-spec with-buffer t)

(provide 'js3-beautify-util)

;;; js3-beautify-util.el ends here
;;; js3-beautify-scan.el --- JavaScript scanner

;;; Commentary:

;; A port of Mozilla Rhino's scanner.
;; Corresponds to Rhino files Token.java and TokenStream.java.

;;; Code:


(eval-when-compile
  (require 'cl))

(defvar js3-beautify-tokens nil
  "List of all defined token names.")  ; initialized in `js3-beautify-token-names'

(defconst js3-beautify-token-names
  (let* ((names (make-vector js3-beautify-num-tokens -1))
         (case-fold-search nil)  ; only match js3-beautify-UPPER_CASE
         (syms (apropos-internal "^js3-beautify-\\(?:[A-Z_]+\\)")))
    (loop for sym in syms
          for i from 0
          do
          (unless (or (memq sym '(js3-beautify-EOF_CHAR js3-beautify-ERROR))
                      (not (boundp sym)))
            (aset names (symbol-value sym)         ; code, e.g. 152
                  (substring (symbol-name sym) 13)) ; name, e.g. "LET"
            (push sym js3-beautify-tokens)))
    names)
  "Vector mapping int values to token string names, sans `js3-beautify-' prefix.")

(defun js3-beautify-token-name (tok)
  "Return a string name for TOK, a token symbol or code.
Signals an error if it's not a recognized token."
  (let ((code tok))
    (if (symbolp tok)
        (setq code (symbol-value tok)))
    (if (eq code -1)
        "ERROR"
      (if (and (numberp code)
               (not (minusp code))
               (< code js3-beautify-num-tokens))
          (aref js3-beautify-token-names code)
        (error "Invalid token: %s" code)))))

(defsubst js3-beautify-token-sym (tok)
  "Return symbol for TOK given its code, e.g. 'js3-beautify-LP for code 86."
  (intern (js3-beautify-token-name tok)))

(defconst js3-beautify-token-codes
  (let ((table (make-hash-table :test 'eq :size 256)))
    (loop for name across js3-beautify-token-names
          for sym = (intern (concat "js3-beautify-" name))
          do
          (puthash sym (symbol-value sym) table))
    ;; clean up a few that are "wrong" in Rhino's token codes
    (puthash 'js3-beautify-DELETE js3-beautify-DELPROP table)
    table)
  "Hashtable mapping token symbols to their bytecodes.")

(defsubst js3-beautify-token-code (sym)
  "Return code for token symbol SYM, e.g. 86 for 'js3-beautify-LP."
  (or (gethash sym js3-beautify-token-codes)
      (error "Invalid token symbol: %s " sym)))  ; signal code bug

(defsubst js3-beautify-report-scan-error (msg &optional no-throw beg len)
  (setq js3-beautify-token-end js3-beautify-ts-cursor)
  (js3-beautify-report-error msg nil
                    (or beg js3-beautify-token-beg)
                    (or len (- js3-beautify-token-end js3-beautify-token-beg)))
  (unless no-throw
    (throw 'return js3-beautify-ERROR)))

(defsubst js3-beautify-get-string-from-buffer ()
  "Reverse the char accumulator and return it as a string."
  (setq js3-beautify-token-end js3-beautify-ts-cursor)
  (if js3-beautify-ts-string-buffer
      (apply #'string (nreverse js3-beautify-ts-string-buffer))
    ""))

;; TODO:  could potentially avoid a lot of consing by allocating a
;; char buffer the way Rhino does.
(defsubst js3-beautify-add-to-string (c)
  (push c js3-beautify-ts-string-buffer))

;; Note that when we "read" the end-of-file, we advance js3-beautify-ts-cursor
;; to (1+ (point-max)), which lets the scanner treat end-of-file like
;; any other character:  when it's not part of the current token, we
;; unget it, allowing it to be read again by the following call.
(defsubst js3-beautify-unget-char ()
  (decf js3-beautify-ts-cursor))

;; Rhino distinguishes \r and \n line endings.  We don't need to
;; because we only scan from Emacs buffers, which always use \n.
(defsubst js3-beautify-get-char ()
  "Read and return the next character from the input buffer.
Increments `js3-beautify-ts-lineno' if the return value is a newline char.
Updates `js3-beautify-ts-cursor' to the point after the returned char.
Returns `js3-beautify-EOF_CHAR' if we hit the end of the buffer.
Also updates `js3-beautify-ts-hit-eof' and `js3-beautify-ts-line-start' as needed."
  (let (c)
    ;; check for end of buffer
    (if (>= js3-beautify-ts-cursor (point-max))
        (setq js3-beautify-ts-hit-eof t
              js3-beautify-ts-cursor (1+ js3-beautify-ts-cursor)
              c js3-beautify-EOF_CHAR)  ; return value
      ;; otherwise read next char
      (setq c (char-before (incf js3-beautify-ts-cursor)))
      ;; if we read a newline, update counters
      (if (= c ?\n)
          (setq js3-beautify-ts-line-start js3-beautify-ts-cursor
                js3-beautify-ts-lineno (1+ js3-beautify-ts-lineno)))
      ;; TODO:  skip over format characters
      c)))

(defsubst js3-beautify-read-unicode-escape ()
  "Read a \\uNNNN sequence from the input.
Assumes the ?\ and ?u have already been read.
Returns the unicode character, or nil if it wasn't a valid character.
Doesn't change the values of any scanner variables."
  ;; I really wish I knew a better way to do this, but I can't
  ;; find the Emacs function that takes a 16-bit int and converts
  ;; it to a Unicode/utf-8 character.  So I basically eval it with (read).
  ;; Have to first check that it's 4 hex characters or it may stop
  ;; the read early.
  (ignore-errors
   (let ((s (buffer-substring-no-properties js3-beautify-ts-cursor
                                            (+ 4 js3-beautify-ts-cursor))))
     (if (string-match "[a-zA-Z0-9]\\{4\\}" s)
         (read (concat "?\\u" s))))))

(defsubst js3-beautify-match-char (test)
  "Consume and return next character if it matches TEST, a character.
Returns nil and consumes nothing if TEST is not the next character."
  (let ((c (js3-beautify-get-char)))
    (if (eq c test)
        t
      (js3-beautify-unget-char)
      nil)))

(defsubst js3-beautify-peek-char ()
  (prog1
      (js3-beautify-get-char)
    (js3-beautify-unget-char)))

(defsubst js3-beautify-java-identifier-start-p (c)
  (or
   (memq c '(?$ ?_))
   (char-is-uppercase c)
   (char-is-lowercase c)))

(defsubst js3-beautify-java-identifier-part-p (c)
  "Implementation of java.lang.Character.isJavaIdentifierPart()"
  ;; TODO:  make me Unicode-friendly.  See comments above.
  (or
   (memq c '(?$ ?_))
   (char-is-uppercase c)
   (char-is-lowercase c)
   (and (>= c ?0) (<= c ?9))))

(defsubst js3-beautify-alpha-p (c)
  (cond ((and (<= ?A c) (<= c ?Z)) t)
        ((and (<= ?a c) (<= c ?z)) t)
        (t nil)))

(defsubst js3-beautify-digit-p (c)
  (and (<= ?0 c) (<= c ?9)))

(defsubst js3-beautify-js-space-p (c)
  (if (<= c 127)
      (memq c '(#x20 #x9 #xB #xC #xD))
    (or
     (eq c #xA0)
     ;; TODO:  change this nil to check for Unicode space character
     nil)))

(defconst js3-beautify-eol-chars (list js3-beautify-EOF_CHAR ?\n ?\r))

(defsubst js3-beautify-skip-line ()
  "Skip to end of line"
  (let (c)
    (while (not (memq (setq c (js3-beautify-get-char)) js3-beautify-eol-chars)))
    (js3-beautify-unget-char)
    (setq js3-beautify-token-end js3-beautify-ts-cursor)))

(defun js3-beautify-init-scanner (&optional buf line)
  "Create token stream for BUF starting on LINE.
BUF defaults to current-buffer and line defaults to 1.

A buffer can only have one scanner active at a time, which yields
dramatically simpler code than using a defstruct.  If you need to
have simultaneous scanners in a buffer, copy the regions to scan
into temp buffers."
  (save-excursion
    (when buf
      (set-buffer buf))
    (setq js3-beautify-ts-dirty-line nil
          js3-beautify-ts-regexp-flags nil
          js3-beautify-ts-string ""
          js3-beautify-ts-number nil
          js3-beautify-ts-hit-eof nil
          js3-beautify-ts-line-start 0
          js3-beautify-ts-lineno (or line 1)
          js3-beautify-ts-line-end-char -1
          js3-beautify-ts-cursor (point-min)
          js3-beautify-ts-string-buffer nil)))

(defconst js3-beautify-keywords
  '(break
    case catch const continue
    debugger default delete do
    else enum
    false finally for function
    if in instanceof import
    let
    new null
    return
    switch
    this throw true try typeof
    var void
    while with
    yield))

;; Token names aren't exactly the same as the keywords, unfortunately.
;; E.g. enum isn't in the tokens, and delete is js3-beautify-DELPROP.
(defconst js3-beautify-kwd-tokens
  (let ((table (make-vector js3-beautify-num-tokens nil))
        (tokens
         (list js3-beautify-BREAK
               js3-beautify-CASE js3-beautify-CATCH js3-beautify-CONST js3-beautify-CONTINUE
               js3-beautify-DEBUGGER js3-beautify-DEFAULT js3-beautify-DELPROP js3-beautify-DO
               js3-beautify-ELSE
               js3-beautify-FALSE js3-beautify-FINALLY js3-beautify-FOR js3-beautify-FUNCTION
               js3-beautify-IF js3-beautify-IN js3-beautify-INSTANCEOF js3-beautify-IMPORT
               js3-beautify-LET
               js3-beautify-NEW js3-beautify-NULL
               js3-beautify-RETURN
               js3-beautify-SWITCH
               js3-beautify-THIS js3-beautify-THROW js3-beautify-TRUE js3-beautify-TRY js3-beautify-TYPEOF
               js3-beautify-VAR
               js3-beautify-WHILE js3-beautify-WITH
               js3-beautify-YIELD)))
    (dolist (i tokens)
      (aset table i t))
    (aset table js3-beautify-STRING t)
    (aset table js3-beautify-REGEXP t)
    (aset table js3-beautify-COMMENT t)
    (aset table js3-beautify-THIS t)
    (aset table js3-beautify-VOID t)
    (aset table js3-beautify-NULL t)
    (aset table js3-beautify-TRUE t)
    (aset table js3-beautify-FALSE t)
    table)
  "Vector whose values are non-nil for tokens that are keywords.")

(defconst js3-beautify-reserved-words
  '(abstract
    boolean byte
    char class
    double
    enum export extends
    final float
    goto
    implements import int interface
    long
    native
    package private protected public
    short static super synchronized
    throws transient
    volatile))

(defconst js3-beautify-keyword-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js3-beautify-keywords
          do (puthash
              (symbol-name k)                            ; instanceof
              (intern (concat "js3-beautify-"
                              (upcase (symbol-name k)))) ; js3-beautify-INSTANCEOF
              table))
    table)
  "JavaScript keywords by name, mapped to their symbols.")

(defconst js3-beautify-reserved-word-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js3-beautify-reserved-words
          do
          (puthash (symbol-name k) 'js3-beautify-RESERVED table))
    table)
  "JavaScript reserved words by name, mapped to 'js3-beautify-RESERVED.")

(defsubst js3-beautify-collect-string (buf)
  "Convert BUF, a list of chars, to a string.
Reverses BUF before converting."
  (cond
   ((stringp buf)
    buf)
   ((null buf)  ; for emacs21 compat
    "")
   (t
    (if buf
        (apply #'string (nreverse buf))
      ""))))

(defun js3-beautify-string-to-keyword (s)
  "Return token for S, a string, if S is a keyword or reserved word.
Returns a symbol such as 'js3-beautify-BREAK, or nil if not keyword/reserved."
  (or (gethash s js3-beautify-keyword-names)
      (gethash s js3-beautify-reserved-word-names)))

(defsubst js3-beautify-ts-set-char-token-bounds ()
  "Used when next token is one character."
  (setq js3-beautify-token-beg (1- js3-beautify-ts-cursor)
        js3-beautify-token-end js3-beautify-ts-cursor))

(defsubst js3-beautify-ts-return (token)
  "Return an N-character TOKEN from `js3-beautify-get-token'.
Updates `js3-beautify-token-end' accordingly."
  (setq js3-beautify-token-end js3-beautify-ts-cursor)
  (throw 'return token))

(defsubst js3-beautify-x-digit-to-int (c accumulator)
  "Build up a hex number.
If C is a hexadecimal digit, return ACCUMULATOR * 16 plus
corresponding number.  Otherwise return -1."
  (catch 'return
    (catch 'check
      ;; Use 0..9 < A..Z < a..z
      (cond
       ((<= c ?9)
        (decf c ?0)
        (if (<= 0 c)
            (throw 'check nil)))
       ((<= c ?F)
        (when (<= ?A c)
          (decf c (- ?A 10))
          (throw 'check nil)))
       ((<= c ?f)
        (when (<= ?a c)
          (decf c (- ?a 10))
          (throw 'check nil))))
      (throw 'return -1))
    (logior c (lsh accumulator 4))))

(defun js3-beautify-get-token ()
  "Return next JavaScript token, an int such as js3-beautify-RETURN."
  (let (c
        c1
        identifier-start
        is-unicode-escape-start
        contains-escape
        escape-val
        escape-start
        str
        result
        base
        is-integer
        quote-char
        val
        look-for-slash
        continue)
    (catch 'return
      (while t
        ;; Eat whitespace, possibly sensitive to newlines.
        (setq continue t)
        (while continue
          (setq c (js3-beautify-get-char))
          (cond
           ((eq c js3-beautify-EOF_CHAR)
            (js3-beautify-ts-set-char-token-bounds)
            (throw 'return js3-beautify-EOF))
           ((eq c ?\n)
            (js3-beautify-ts-set-char-token-bounds)
            (setq js3-beautify-ts-dirty-line nil)
            (throw 'return js3-beautify-EOL))
           ((not (js3-beautify-js-space-p c))
            (if (/= c ?-)               ; in case end of HTML comment
                (setq js3-beautify-ts-dirty-line t))
            (setq continue nil))))
        ;; Assume the token will be 1 char - fixed up below.
        (js3-beautify-ts-set-char-token-bounds)
        ;; identifier/keyword/instanceof?
        ;; watch out for starting with a <backslash>
        (cond
         ((eq c ?\\)
          (setq c (js3-beautify-get-char))
          (if (eq c ?u)
              (setq identifier-start t
                    is-unicode-escape-start t
                    js3-beautify-ts-string-buffer nil)
            (setq identifier-start nil)
            (js3-beautify-unget-char)
            (setq c ?\\)))
         (t
          (when (setq identifier-start (js3-beautify-java-identifier-start-p c))
            (setq js3-beautify-ts-string-buffer nil)
            (js3-beautify-add-to-string c))))
        (when identifier-start
          (setq contains-escape is-unicode-escape-start)
          (catch 'break
            (while t
              (if is-unicode-escape-start
                  ;; strictly speaking we should probably push-back
                  ;; all the bad characters if the <backslash>uXXXX
                  ;; sequence is malformed. But since there isn't a
                  ;; correct context(is there?) for a bad Unicode
                  ;; escape sequence in an identifier, we can report
                  ;; an error here.
                  (progn
                    (setq escape-val 0)
                    (dotimes (i 4)
                      (setq c (js3-beautify-get-char)
                            escape-val (js3-beautify-x-digit-to-int c escape-val))
                      ;; Next check takes care of c < 0 and bad escape
                      (if (minusp escape-val)
                          (throw 'break nil)))
                    (if (minusp escape-val)
                        (js3-beautify-report-scan-error "msg.invalid.escape" t))
                    (js3-beautify-add-to-string escape-val)
                    (setq is-unicode-escape-start nil))
                (setq c (js3-beautify-get-char))
                (cond
                 ((eq c ?\\)
                  (setq c (js3-beautify-get-char))
                  (if (eq c ?u)
                      (setq is-unicode-escape-start t
                            contains-escape t)
                    (js3-beautify-report-scan-error "msg.illegal.character" t)))
                 (t
                  (if (or (eq c js3-beautify-EOF_CHAR)
                          (not (js3-beautify-java-identifier-part-p c)))
                      (throw 'break nil))
                  (js3-beautify-add-to-string c))))))
          (js3-beautify-unget-char)
          (setq str (js3-beautify-get-string-from-buffer))
          (unless contains-escape
            ;; OPT we shouldn't have to make a string (object!) to
            ;; check if it's a keyword.
            ;; Return the corresponding token if it's a keyword
            (when (setq result (js3-beautify-string-to-keyword str))
              (if (and (< js3-beautify-language-version 170)
                       (memq result '(js3-beautify-LET js3-beautify-YIELD)))
                  ;; LET and YIELD are tokens only in 1.7 and later
                  (setq result 'js3-beautify-NAME))
              (if (neq result 'js3-beautify-RESERVED)
                  (throw 'return (js3-beautify-token-code result)))
              (js3-beautify-report-warning "msg.reserved.keyword" str)))
          ;; If we want to intern these as Rhino does, just use (intern str)
          (setq js3-beautify-ts-string str)
          (throw 'return js3-beautify-NAME))     ; end identifier/kwd check
        ;; is it a number?
        (when (or (js3-beautify-digit-p c)
                  (and (eq c ?.) (js3-beautify-digit-p (js3-beautify-peek-char))))
          (setq js3-beautify-ts-string-buffer nil
                base 10)
          (when (eq c ?0)
            (setq c (js3-beautify-get-char))
            (cond
             ((or (eq c ?x) (eq c ?X))
              (setq base 16)
              (setq c (js3-beautify-get-char)))
             ((js3-beautify-digit-p c)
              (setq base 8))
             (t
              (js3-beautify-add-to-string ?0))))
          (if (eq base 16)
              (while (<= 0 (js3-beautify-x-digit-to-int c 0))
                (js3-beautify-add-to-string c)
                (setq c (js3-beautify-get-char)))
            (while (and (<= ?0 c) (<= c ?9))
              ;; We permit 08 and 09 as decimal numbers, which
              ;; makes our behavior a superset of the ECMA
              ;; numeric grammar.  We might not always be so
              ;; permissive, so we warn about it.
              (when (and (eq base 8) (>= c ?8))
                (js3-beautify-report-warning "msg.bad.octal.literal"
                                    (if (eq c ?8) "8" "9"))
                (setq base 10))
              (js3-beautify-add-to-string c)
              (setq c (js3-beautify-get-char))))
          (setq is-integer t)
          (when (and (eq base 10) (memq c '(?. ?e ?E)))
            (setq is-integer nil)
            (when (eq c ?.)
              (loop do
                    (js3-beautify-add-to-string c)
                    (setq c (js3-beautify-get-char))
                    while (js3-beautify-digit-p c)))
            (when (memq c '(?e ?E))
              (js3-beautify-add-to-string c)
              (setq c (js3-beautify-get-char))
              (when (memq c '(?+ ?-))
                (js3-beautify-add-to-string c)
                (setq c (js3-beautify-get-char)))
              (unless (js3-beautify-digit-p c)
                (js3-beautify-report-scan-error "msg.missing.exponent" t))
              (loop do
                    (js3-beautify-add-to-string c)
                    (setq c (js3-beautify-get-char))
                    while (js3-beautify-digit-p c))))
          (js3-beautify-unget-char)
          (setq js3-beautify-ts-string (js3-beautify-get-string-from-buffer)
                js3-beautify-ts-number
                (if (and (eq base 10) (not is-integer))
                    (string-to-number js3-beautify-ts-string)
                  ;; TODO:  call runtime number-parser.  Some of it is in
                  ;; js3-beautify-util.el, but I need to port ScriptRuntime.stringToNumber.
                  (string-to-number js3-beautify-ts-string)))
          (throw 'return js3-beautify-NUMBER))
        ;; is it a string?
        (when (memq c '(?\" ?\'))
          ;; We attempt to accumulate a string the fast way, by
          ;; building it directly out of the reader.  But if there
          ;; are any escaped characters in the string, we revert to
          ;; building it out of a string buffer.
          (setq quote-char c
                js3-beautify-ts-string-buffer nil
                c (js3-beautify-get-char))
          (catch 'break
            (while (/= c quote-char)
              (catch 'continue
                (when (or (eq c ?\n) (eq c js3-beautify-EOF_CHAR))
                  (js3-beautify-unget-char)
                  (setq js3-beautify-token-end js3-beautify-ts-cursor)
                  (js3-beautify-report-error "msg.unterminated.string.lit")
                  (throw 'return js3-beautify-STRING))
                (when (eq c ?\\)
                  ;; We've hit an escaped character
                  (setq c (js3-beautify-get-char))
                  (case c
                        (?b (setq c ?\b))
                        (?f (setq c ?\f))
                        (?n (setq c ?\n))
                        (?r (setq c ?\r))
                        (?t (setq c ?\t))
                        (?v (setq c ?\v))
                        (?u
                         (setq c1 (js3-beautify-read-unicode-escape))
                         (if js3-beautify-parse-ide-mode
                             (if c1
                                 (progn
                                   ;; just copy the string in IDE-mode
                                   (js3-beautify-add-to-string ?\\)
                                   (js3-beautify-add-to-string ?u)
                                   (dotimes (i 3)
                                     (js3-beautify-add-to-string (js3-beautify-get-char)))
                                   (setq c (js3-beautify-get-char))) ; added at end of loop
                               ;; flag it as an invalid escape
                               (js3-beautify-report-warning "msg.invalid.escape"
                                                   nil (- js3-beautify-ts-cursor 2) 6))
                           ;; Get 4 hex digits; if the u escape is not
                           ;; followed by 4 hex digits, use 'u' + the
                           ;; literal character sequence that follows.
                           (js3-beautify-add-to-string ?u)
                           (setq escape-val 0)
                           (dotimes (i 4)
                             (setq c (js3-beautify-get-char)
                                   escape-val (js3-beautify-x-digit-to-int c escape-val))
                             (if (minusp escape-val)
                                 (throw 'continue nil))
                             (js3-beautify-add-to-string c))
                           ;; prepare for replace of stored 'u' sequence by escape value
                           (setq js3-beautify-ts-string-buffer (nthcdr 5 js3-beautify-ts-string-buffer)
                                 c escape-val)))
                        (?x
                         ;; Get 2 hex digits, defaulting to 'x'+literal
                         ;; sequence, as above.
                         (setq c (js3-beautify-get-char)
                               escape-val (js3-beautify-x-digit-to-int c 0))
                         (if (minusp escape-val)
                             (progn
                               (js3-beautify-add-to-string ?x)
                               (throw 'continue nil))
                           (setq c1 c
                                 c (js3-beautify-get-char)
                                 escape-val (js3-beautify-x-digit-to-int c escape-val))
                           (if (minusp escape-val)
                               (progn
                                 (js3-beautify-add-to-string ?x)
                                 (js3-beautify-add-to-string c1)
                                 (throw 'continue nil))
                             ;; got 2 hex digits
                             (setq c escape-val))))
                        (?\n
                         ;; Remove line terminator after escape to follow
                         ;; SpiderMonkey and C/C++
                         (setq c (js3-beautify-get-char))
                         (throw 'continue nil))
                        (t
                         (when (and (<= ?0 c) (< c ?8))
                           (setq val (- c ?0)
                                 c (js3-beautify-get-char))
                           (when (and (<= ?0 c) (< c ?8))
                             (setq val (- (+ (* 8 val) c) ?0)
                                   c (js3-beautify-get-char))
                             (when (and (<= ?0 c)
                                        (< c ?8)
                                        (< val #o37))
                               ;; c is 3rd char of octal sequence only
                               ;; if the resulting val <= 0377
                               (setq val (- (+ (* 8 val) c) ?0)
                                     c (js3-beautify-get-char))))
                           (js3-beautify-unget-char)
                           (setq c val)))))
                (js3-beautify-add-to-string c)
                (setq c (js3-beautify-get-char)))))
          (setq js3-beautify-ts-string (js3-beautify-get-string-from-buffer))
          (throw 'return js3-beautify-STRING))
        (case c
              (?\;
               (throw 'return js3-beautify-SEMI))
              (?\[
               (throw 'return js3-beautify-LB))
              (?\]
               (throw 'return js3-beautify-RB))
              (?{
               (throw 'return js3-beautify-LC))
              (?}
               (throw 'return js3-beautify-RC))
              (?\(
               (throw 'return js3-beautify-LP))
              (?\)
               (throw 'return js3-beautify-RP))
              (?,
               (throw 'return js3-beautify-COMMA))
              (??
               (throw 'return js3-beautify-HOOK))
              (?:
	       (throw 'return js3-beautify-COLON))
              (?.
	       (throw 'return js3-beautify-DOT))
              (?|
               (if (js3-beautify-match-char ?|)
                   (throw 'return js3-beautify-OR)
                 (if (js3-beautify-match-char ?=)
                     (js3-beautify-ts-return js3-beautify-ASSIGN_BITOR)
                   (throw 'return js3-beautify-BITOR))))
              (?^
               (if (js3-beautify-match-char ?=)
                   (js3-beautify-ts-return js3-beautify-ASSIGN_BITOR)
                 (throw 'return js3-beautify-BITXOR)))
              (?&
               (if (js3-beautify-match-char ?&)
                   (throw 'return js3-beautify-AND)
                 (if (js3-beautify-match-char ?=)
                     (js3-beautify-ts-return js3-beautify-ASSIGN_BITAND)
                   (throw 'return js3-beautify-BITAND))))
              (?=
               (if (js3-beautify-match-char ?=)
                   (if (js3-beautify-match-char ?=)
                       (js3-beautify-ts-return js3-beautify-SHEQ)
                     (throw 'return js3-beautify-EQ))
                 (throw 'return js3-beautify-ASSIGN)))
              (?!
               (if (js3-beautify-match-char ?=)
                   (if (js3-beautify-match-char ?=)
                       (js3-beautify-ts-return js3-beautify-SHNE)
                     (js3-beautify-ts-return js3-beautify-NE))
                 (throw 'return js3-beautify-NOT)))
              (?<
               ;; NB:treat HTML begin-comment as comment-till-eol
               (when (js3-beautify-match-char ?!)
                 (when (js3-beautify-match-char ?-)
                   (when (js3-beautify-match-char ?-)
                     (js3-beautify-skip-line)
                     (setq js3-beautify-ts-comment-type 'html)
                     (throw 'return js3-beautify-COMMENT)))
                 (js3-beautify-unget-char))
               (if (js3-beautify-match-char ?<)
                   (if (js3-beautify-match-char ?=)
                       (js3-beautify-ts-return js3-beautify-ASSIGN_LSH)
                     (js3-beautify-ts-return js3-beautify-LSH))
                 (if (js3-beautify-match-char ?=)
                     (js3-beautify-ts-return js3-beautify-LE)
                   (throw 'return js3-beautify-LT))))
              (?>
               (if (js3-beautify-match-char ?>)
                   (if (js3-beautify-match-char ?>)
                       (if (js3-beautify-match-char ?=)
                           (js3-beautify-ts-return js3-beautify-ASSIGN_URSH)
                         (js3-beautify-ts-return js3-beautify-URSH))
                     (if (js3-beautify-match-char ?=)
                         (js3-beautify-ts-return js3-beautify-ASSIGN_RSH)
                       (js3-beautify-ts-return js3-beautify-RSH)))
                 (if (js3-beautify-match-char ?=)
                     (js3-beautify-ts-return js3-beautify-GE)
                   (throw 'return js3-beautify-GT))))
              (?*
               (if (js3-beautify-match-char ?=)
                   (js3-beautify-ts-return js3-beautify-ASSIGN_MUL)
                 (throw 'return js3-beautify-MUL)))
              (?/
               ;; is it a // comment?
               (when (js3-beautify-match-char ?/)
                 (setq js3-beautify-token-beg (- js3-beautify-ts-cursor 2))
                 (js3-beautify-skip-line)
                 (setq js3-beautify-ts-comment-type 'line)
                 (incf js3-beautify-token-end)
                 (throw 'return js3-beautify-COMMENT))
               ;; is it a /* comment?
               (when (js3-beautify-match-char ?*)
                 (setq look-for-slash nil
                       js3-beautify-token-beg (- js3-beautify-ts-cursor 2)
                       js3-beautify-ts-comment-type
                       (if (js3-beautify-match-char ?*)
                           (progn
                             (setq look-for-slash t)
                             'jsdoc)
                         'block))
                 (while t
                   (setq c (js3-beautify-get-char))
                   (cond
                    ((eq c js3-beautify-EOF_CHAR)
                     (setq js3-beautify-token-end (1- js3-beautify-ts-cursor))
                     (js3-beautify-report-error "msg.unterminated.comment")
                     (throw 'return js3-beautify-COMMENT))
                    ((eq c ?*)
                     (setq look-for-slash t))
                    ((eq c ?/)
                     (if look-for-slash
                         (js3-beautify-ts-return js3-beautify-COMMENT)))
                    (t
                     (setq look-for-slash nil
                           js3-beautify-token-end js3-beautify-ts-cursor)))))
               (if (js3-beautify-match-char ?=)
                   (js3-beautify-ts-return js3-beautify-ASSIGN_DIV)
                 (throw 'return js3-beautify-DIV)))
              (?#
               (when js3-beautify-skip-preprocessor-directives
                 (js3-beautify-skip-line)
                 (setq js3-beautify-ts-comment-type 'preprocessor
                       js3-beautify-token-end js3-beautify-ts-cursor)
                 (throw 'return js3-beautify-COMMENT))
               (throw 'return js3-beautify-ERROR))
              (?%
               (if (js3-beautify-match-char ?=)
                   (js3-beautify-ts-return js3-beautify-ASSIGN_MOD)
                 (throw 'return js3-beautify-MOD)))
              (?~
               (throw 'return js3-beautify-BITNOT))
              (?+
               (if (js3-beautify-match-char ?=)
                   (js3-beautify-ts-return js3-beautify-ASSIGN_ADD)
                 (if (js3-beautify-match-char ?+)
                     (js3-beautify-ts-return js3-beautify-INC)
                   (throw 'return js3-beautify-ADD))))
              (?-
               (cond
                ((js3-beautify-match-char ?=)
                 (setq c js3-beautify-ASSIGN_SUB))
                ((js3-beautify-match-char ?-)
                 (unless js3-beautify-ts-dirty-line
                   ;; treat HTML end-comment after possible whitespace
                   ;; after line start as comment-until-eol
                   (when (js3-beautify-match-char ?>)
                     (js3-beautify-skip-line)
                     (setq js3-beautify-ts-comment-type 'html)
                     (throw 'return js3-beautify-COMMENT)))
                 (setq c js3-beautify-DEC))
                (t
                 (setq c js3-beautify-SUB)))
               (setq js3-beautify-ts-dirty-line t)
               (js3-beautify-ts-return c))
              (otherwise
               (js3-beautify-report-scan-error "msg.illegal.character")))))))

(defun js3-beautify-read-regexp (start-token)
  "Called by parser when it gets / or /= in literal context."
  (let (c
        err
        in-class  ; inside a '[' .. ']' character-class
        flags
        (continue t))
    (setq js3-beautify-token-beg js3-beautify-ts-cursor
          js3-beautify-ts-string-buffer nil
          js3-beautify-ts-regexp-flags nil)
    (if (eq start-token js3-beautify-ASSIGN_DIV)
        ;; mis-scanned /=
        (js3-beautify-add-to-string ?=)
      (if (neq start-token js3-beautify-DIV)
          (error "failed assertion")))
    (while (and (not err)
                (or (/= (setq c (js3-beautify-get-char)) ?/)
                    in-class))
      (cond
       ((or (= c ?\n)
            (= c js3-beautify-EOF_CHAR))
        (setq js3-beautify-token-end (1- js3-beautify-ts-cursor)
              err t
              js3-beautify-ts-string (js3-beautify-collect-string js3-beautify-ts-string-buffer))
        (js3-beautify-report-error "msg.unterminated.re.lit"))
       (t (cond
           ((= c ?\\)
            (js3-beautify-add-to-string c)
            (setq c (js3-beautify-get-char)))
           ((= c ?\[)
            (setq in-class t))
           ((= c ?\])
            (setq in-class nil)))
          (js3-beautify-add-to-string c))))
    (unless err
      (while continue
        (cond
         ((js3-beautify-match-char ?g)
          (push ?g flags))
         ((js3-beautify-match-char ?i)
          (push ?i flags))
         ((js3-beautify-match-char ?m)
          (push ?m flags))
         (t
          (setq continue nil))))
      (if (js3-beautify-alpha-p (js3-beautify-peek-char))
          (js3-beautify-report-scan-error "msg.invalid.re.flag" t
                                 js3-beautify-ts-cursor 1))
      (setq js3-beautify-ts-string (js3-beautify-collect-string js3-beautify-ts-string-buffer)
            js3-beautify-ts-regexp-flags (js3-beautify-collect-string flags)
            js3-beautify-token-end js3-beautify-ts-cursor)
      ;; tell `parse-partial-sexp' to ignore this range of chars
      (js3-beautify-record-text-property
       js3-beautify-token-beg js3-beautify-token-end 'syntax-class '(2)))))

(defun js3-beautify-scanner-get-line ()
  "Return the text of the current scan line."
  (buffer-substring (point-at-bol) (point-at-eol)))

(provide 'js3-beautify-scan)

;;; js3-beautify-scan.el ends here
;;; js3-beautify-messages:  localizable messages for js3-beautify

;;; Commentary:

;; Messages are copied from Rhino's Messages.properties.
;; Many of the Java-specific messages have been elided.
;; Add any js3-beautify-specific ones at the end, so we can keep
;; this file synced with changes to Rhino's.
;;
;; TODO:
;;  - move interpreter messages into separate file

;;; Code:

(defvar js3-beautify-message-table
  (make-hash-table :test 'equal :size 250)
  "Contains localized messages for js3-beautify.")

;; TODO:  construct this hashtable at compile-time.
(defmacro js3-beautify-msg (key &rest strings)
  `(puthash ,key (funcall #'concat ,@strings)
            js3-beautify-message-table))

(defun js3-beautify-get-msg (msg-key)
  "Look up a localized message.
MSG-KEY is a list of (MSG ARGS).  If the message takes parameters,
the correct number of ARGS must be provided."
  (let* ((key (if (listp msg-key) (car msg-key) msg-key))
         (args (if (listp msg-key) (cdr msg-key)))
         (msg (gethash key js3-beautify-message-table)))
    (if msg
        (apply #'format msg args)
      key)))  ; default to showing the key

(js3-beautify-msg "msg.dup.parms"
         "Duplicate parameter name '%s'.")

(js3-beautify-msg "msg.too.big.jump"
         "Program too complex: jump offset too big.")

(js3-beautify-msg "msg.too.big.index"
         "Program too complex: internal index exceeds 64K limit.")

(js3-beautify-msg "msg.while.compiling.fn"
         "Encountered code generation error while compiling function '%s': %s")

(js3-beautify-msg "msg.while.compiling.script"
         "Encountered code generation error while compiling script: %s")

;; Context
(js3-beautify-msg "msg.ctor.not.found"
         "Constructor for '%s' not found.")

(js3-beautify-msg "msg.not.ctor"
         "'%s' is not a constructor.")

;; FunctionObject
(js3-beautify-msg "msg.varargs.ctor"
         "Method or constructor '%s' must be static "
         "with the signature (Context cx, Object[] args, "
         "Function ctorObj, boolean inNewExpr) "
         "to define a variable arguments constructor.")

(js3-beautify-msg "msg.varargs.fun"
         "Method '%s' must be static with the signature "
         "(Context cx, Scriptable thisObj, Object[] args, Function funObj) "
         "to define a variable arguments function.")

(js3-beautify-msg "msg.incompat.call"
         "Method '%s' called on incompatible object.")

(js3-beautify-msg "msg.bad.parms"
         "Unsupported parameter type '%s' in method '%s'.")

(js3-beautify-msg "msg.bad.method.return"
         "Unsupported return type '%s' in method '%s'.")

(js3-beautify-msg "msg.bad.ctor.return"
         "Construction of objects of type '%s' is not supported.")

(js3-beautify-msg "msg.no.overload"
         "Method '%s' occurs multiple times in class '%s'.")

(js3-beautify-msg "msg.method.not.found"
         "Method '%s' not found in '%s'.")

;; IRFactory

(js3-beautify-msg "msg.bad.for.in.lhs"
         "Invalid left-hand side of for..in loop.")

(js3-beautify-msg "msg.mult.index"
         "Only one variable allowed in for..in loop.")

(js3-beautify-msg "msg.bad.for.in.destruct"
         "Left hand side of for..in loop must be an array of "
         "length 2 to accept key/value pair.")

(js3-beautify-msg "msg.cant.convert"
         "Can't convert to type '%s'.")

(js3-beautify-msg "msg.bad.assign.left"
         "Invalid assignment left-hand side.")

(js3-beautify-msg "msg.bad.decr"
         "Invalid decerement operand.")

(js3-beautify-msg "msg.bad.incr"
         "Invalid increment operand.")

(js3-beautify-msg "msg.bad.yield"
         "yield must be in a function.")

(js3-beautify-msg "msg.yield.parenthesized"
         "yield expression must be parenthesized.")

;; NativeGlobal
(js3-beautify-msg "msg.cant.call.indirect"
         "Function '%s' must be called directly, and not by way of a "
         "function of another name.")

(js3-beautify-msg "msg.eval.nonstring"
         "Calling eval() with anything other than a primitive "
         "string value will simply return the value. "
         "Is this what you intended?")

(js3-beautify-msg "msg.eval.nonstring.strict"
         "Calling eval() with anything other than a primitive "
         "string value is not allowed in strict mode.")

(js3-beautify-msg "msg.bad.destruct.op"
         "Invalid destructuring assignment operator")

;; NativeCall
(js3-beautify-msg "msg.only.from.new"
         "'%s' may only be invoked from a `new' expression.")

(js3-beautify-msg "msg.deprec.ctor"
         "The '%s' constructor is deprecated.")

;; NativeFunction
(js3-beautify-msg "msg.no.function.ref.found"
         "no source found to decompile function reference %s")

(js3-beautify-msg "msg.arg.isnt.array"
         "second argument to Function.prototype.apply must be an array")

;; NativeGlobal
(js3-beautify-msg "msg.bad.esc.mask"
         "invalid string escape mask")

;; NativeRegExp
(js3-beautify-msg "msg.bad.quant"
         "Invalid quantifier %s")

(js3-beautify-msg "msg.overlarge.backref"
         "Overly large back reference %s")

(js3-beautify-msg "msg.overlarge.min"
         "Overly large minimum %s")

(js3-beautify-msg "msg.overlarge.max"
         "Overly large maximum %s")

(js3-beautify-msg "msg.zero.quant"
         "Zero quantifier %s")

(js3-beautify-msg "msg.max.lt.min"
         "Maximum %s less than minimum")

(js3-beautify-msg "msg.unterm.quant"
         "Unterminated quantifier %s")

(js3-beautify-msg "msg.unterm.paren"
         "Unterminated parenthetical %s")

(js3-beautify-msg "msg.unterm.class"
         "Unterminated character class %s")

(js3-beautify-msg "msg.bad.range"
         "Invalid range in character class.")

(js3-beautify-msg "msg.trail.backslash"
         "Trailing \\ in regular expression.")

(js3-beautify-msg "msg.re.unmatched.right.paren"
         "unmatched ) in regular expression.")

(js3-beautify-msg "msg.no.regexp"
         "Regular expressions are not available.")

(js3-beautify-msg "msg.bad.backref"
         "back-reference exceeds number of capturing parentheses.")

(js3-beautify-msg "msg.bad.regexp.compile"
         "Only one argument may be specified if the first "
         "argument to RegExp.prototype.compile is a RegExp object.")

;; Parser
(js3-beautify-msg "msg.got.syntax.errors"
         "Compilation produced %s syntax errors.")

(js3-beautify-msg "msg.var.redecl"
         "TypeError: redeclaration of var %s.")

(js3-beautify-msg "msg.const.redecl"
         "TypeError: redeclaration of const %s.")

(js3-beautify-msg "msg.let.redecl"
         "TypeError: redeclaration of variable %s.")

(js3-beautify-msg "msg.parm.redecl"
         "TypeError: redeclaration of formal parameter %s.")

(js3-beautify-msg "msg.fn.redecl"
         "TypeError: redeclaration of function %s.")

(js3-beautify-msg "msg.let.decl.not.in.block"
         "SyntaxError: let declaration not directly within block")

;; NodeTransformer
(js3-beautify-msg "msg.dup.label"
         "duplicated label")

(js3-beautify-msg "msg.undef.label"
         "undefined label")

(js3-beautify-msg "msg.bad.break"
         "unlabelled break must be inside loop or switch")

(js3-beautify-msg "msg.continue.outside"
         "continue must be inside loop")

(js3-beautify-msg "msg.continue.nonloop"
         "continue can only use labels of iteration statements")

(js3-beautify-msg "msg.bad.throw.eol"
         "Line terminator is not allowed between the throw "
         "keyword and throw expression.")

(js3-beautify-msg "msg.no.paren.parms"
         "missing ( before function parameters.")

(js3-beautify-msg "msg.no.parm"
         "missing formal parameter")

(js3-beautify-msg "msg.no.paren.after.parms"
         "missing ) after formal parameters")

(js3-beautify-msg "msg.no.brace.body"
         "missing '{' before function body")

(js3-beautify-msg "msg.no.brace.after.body"
         "missing } after function body")

(js3-beautify-msg "msg.no.paren.cond"
         "missing ( before condition")

(js3-beautify-msg "msg.no.paren.after.cond"
         "missing ) after condition")

(js3-beautify-msg "msg.no.semi.stmt"
         "missing ; before statement")

(js3-beautify-msg "msg.missing.semi"
         "missing ; after statement")

(js3-beautify-msg "msg.no.name.after.dot"
         "missing name after . operator")

(js3-beautify-msg "msg.no.bracket.index"
         "missing ] in index expression")

(js3-beautify-msg "msg.no.paren.switch"
         "missing ( before switch expression")

(js3-beautify-msg "msg.no.paren.after.switch"
         "missing ) after switch expression")

(js3-beautify-msg "msg.no.brace.switch"
         "missing '{' before switch body")

(js3-beautify-msg "msg.bad.switch"
         "invalid switch statement")

(js3-beautify-msg "msg.no.colon.case"
         "missing : after case expression")

(js3-beautify-msg "msg.double.switch.default"
         "double default label in the switch statement")

(js3-beautify-msg "msg.no.while.do"
         "missing while after do-loop body")

(js3-beautify-msg "msg.no.paren.for"
         "missing ( after for")

(js3-beautify-msg "msg.no.semi.for"
         "missing ; after for-loop initializer")

(js3-beautify-msg "msg.no.semi.for.cond"
         "missing ; after for-loop condition")

(js3-beautify-msg "msg.in.after.for.name"
         "missing in after for")

(js3-beautify-msg "msg.no.paren.for.ctrl"
         "missing ) after for-loop control")

(js3-beautify-msg "msg.no.paren.with"
         "missing ( before with-statement object")

(js3-beautify-msg "msg.no.paren.after.with"
         "missing ) after with-statement object")

(js3-beautify-msg "msg.no.paren.after.let"
         "missing ( after let")

(js3-beautify-msg "msg.no.paren.let"
         "missing ) after variable list")

(js3-beautify-msg "msg.no.curly.let"
         "missing } after let statement")

(js3-beautify-msg "msg.bad.return"
         "invalid return")

(js3-beautify-msg "msg.no.brace.block"
         "missing } in compound statement")

(js3-beautify-msg "msg.bad.label"
         "invalid label")

(js3-beautify-msg "msg.bad.var"
         "missing variable name")

(js3-beautify-msg "msg.bad.var.init"
         "invalid variable initialization")

(js3-beautify-msg "msg.no.colon.cond"
         "missing : in conditional expression")

(js3-beautify-msg "msg.no.paren.arg"
         "missing ) after argument list")

(js3-beautify-msg "msg.no.bracket.arg"
         "missing ] after element list")

(js3-beautify-msg "msg.bad.prop"
         "invalid property id")

(js3-beautify-msg "msg.no.colon.prop"
         "missing : after property id")

(js3-beautify-msg "msg.no.brace.prop"
         "missing } after property list")

(js3-beautify-msg "msg.no.paren"
         "missing ) in parenthetical")

(js3-beautify-msg "msg.reserved.id"
         "identifier is a reserved word")

(js3-beautify-msg "msg.no.paren.catch"
         "missing ( before catch-block condition")

(js3-beautify-msg "msg.bad.catchcond"
         "invalid catch block condition")

(js3-beautify-msg "msg.catch.unreachable"
         "any catch clauses following an unqualified catch are unreachable")

(js3-beautify-msg "msg.no.brace.try"
         "missing '{' before try block")

(js3-beautify-msg "msg.no.brace.catchblock"
         "missing '{' before catch-block body")

(js3-beautify-msg "msg.try.no.catchfinally"
         "'try' without 'catch' or 'finally'")

(js3-beautify-msg "msg.no.return.value"
         "function %s does not always return a value")

(js3-beautify-msg "msg.anon.no.return.value"
         "anonymous function does not always return a value")

(js3-beautify-msg "msg.return.inconsistent"
         "return statement is inconsistent with previous usage")

(js3-beautify-msg "msg.generator.returns"
         "TypeError: generator function '%s' returns a value")

(js3-beautify-msg "msg.anon.generator.returns"
         "TypeError: anonymous generator function returns a value")

(js3-beautify-msg "msg.syntax"
         "syntax error")

(js3-beautify-msg "msg.unexpected.eof"
         "Unexpected end of file")

(js3-beautify-msg "msg.too.deep.parser.recursion"
         "Too deep recursion while parsing")

(js3-beautify-msg "msg.no.side.effects"
         "Code has no side effects")

(js3-beautify-msg "msg.extra.trailing.comma"
         "Trailing comma is not legal in an ECMA-262 object initializer")

(js3-beautify-msg "msg.array.trailing.comma"
         "Trailing comma yields different behavior across browsers")

(js3-beautify-msg "msg.equal.as.assign"
         (concat "Test for equality (==) mistyped as assignment (=)?"
                 " (parenthesize to suppress warning)"))

(js3-beautify-msg "msg.var.hides.arg"
         "Variable %s hides argument")

(js3-beautify-msg "msg.destruct.assign.no.init"
         "Missing = in destructuring declaration")

;; ScriptRuntime
(js3-beautify-msg "msg.no.properties"
         "%s has no properties.")

(js3-beautify-msg "msg.invalid.iterator"
         "Invalid iterator value")

(js3-beautify-msg "msg.iterator.primitive"
         "__iterator__ returned a primitive value")

(js3-beautify-msg "msg.assn.create.strict"
         "Assignment to undeclared variable %s")

(js3-beautify-msg "msg.ref.undefined.prop"
         "Reference to undefined property '%s'")

(js3-beautify-msg "msg.prop.not.found"
         "Property %s not found.")

(js3-beautify-msg "msg.invalid.type"
         "Invalid JavaScript value of type %s")

(js3-beautify-msg "msg.primitive.expected"
         "Primitive type expected (had %s instead)")

(js3-beautify-msg "msg.namespace.expected"
         "Namespace object expected to left of :: (found %s instead)")

(js3-beautify-msg "msg.null.to.object"
         "Cannot convert null to an object.")

(js3-beautify-msg "msg.undef.to.object"
         "Cannot convert undefined to an object.")

(js3-beautify-msg "msg.cyclic.value"
         "Cyclic %s value not allowed.")

(js3-beautify-msg "msg.is.not.defined"
         "'%s' is not defined.")

(js3-beautify-msg "msg.undef.prop.read"
         "Cannot read property '%s' from %s")

(js3-beautify-msg "msg.undef.prop.write"
         "Cannot set property '%s' of %s to '%s'")

(js3-beautify-msg "msg.undef.prop.delete"
         "Cannot delete property '%s' of %s")

(js3-beautify-msg "msg.undef.method.call"
         "Cannot call method '%s' of %s")

(js3-beautify-msg "msg.undef.with"
         "Cannot apply 'with' to %s")

(js3-beautify-msg "msg.isnt.function"
         "%s is not a function, it is %s.")

(js3-beautify-msg "msg.isnt.function.in"
         "Cannot call property %s in object %s. "
         "It is not a function, it is '%s'.")

(js3-beautify-msg "msg.function.not.found"
         "Cannot find function %s.")

(js3-beautify-msg "msg.function.not.found.in"
         "Cannot find function %s in object %s.")

(js3-beautify-msg "msg.no.ref.to.get"
         "%s is not a reference to read reference value.")

(js3-beautify-msg "msg.no.ref.to.set"
         "%s is not a reference to set reference value to %s.")

(js3-beautify-msg "msg.no.ref.from.function"
         "Function %s can not be used as the left-hand "
         "side of assignment or as an operand of ++ or -- operator.")

(js3-beautify-msg "msg.bad.default.value"
         "Object's getDefaultValue() method returned an object.")

(js3-beautify-msg "msg.instanceof.not.object"
         "Can't use instanceof on a non-object.")

(js3-beautify-msg "msg.instanceof.bad.prototype"
         "'prototype' property of %s is not an object.")

(js3-beautify-msg "msg.bad.radix"
         "illegal radix %s.")

;; ScriptableObject
(js3-beautify-msg "msg.default.value"
         "Cannot find default value for object.")

(js3-beautify-msg "msg.zero.arg.ctor"
         "Cannot load class '%s' which has no zero-parameter constructor.")

(js3-beautify-msg "msg.ctor.multiple.parms"
         "Can't define constructor or class %s since more than "
         "one constructor has multiple parameters.")

(js3-beautify-msg "msg.extend.scriptable"
         "%s must extend ScriptableObject in order to define property %s.")

(js3-beautify-msg "msg.bad.getter.parms"
         "In order to define a property, getter %s must have zero "
         "parameters or a single ScriptableObject parameter.")

(js3-beautify-msg "msg.obj.getter.parms"
         "Expected static or delegated getter %s to take "
         "a ScriptableObject parameter.")

(js3-beautify-msg "msg.getter.static"
         "Getter and setter must both be static or neither be static.")

(js3-beautify-msg "msg.setter.return"
         "Setter must have void return type: %s")

(js3-beautify-msg "msg.setter2.parms"
         "Two-parameter setter must take a ScriptableObject as "
         "its first parameter.")

(js3-beautify-msg "msg.setter1.parms"
         "Expected single parameter setter for %s")

(js3-beautify-msg "msg.setter2.expected"
         "Expected static or delegated setter %s to take two parameters.")

(js3-beautify-msg "msg.setter.parms"
         "Expected either one or two parameters for setter.")

(js3-beautify-msg "msg.setter.bad.type"
         "Unsupported parameter type '%s' in setter '%s'.")

(js3-beautify-msg "msg.add.sealed"
         "Cannot add a property to a sealed object: %s.")

(js3-beautify-msg "msg.remove.sealed"
         "Cannot remove a property from a sealed object: %s.")

(js3-beautify-msg "msg.modify.sealed"
         "Cannot modify a property of a sealed object: %s.")

(js3-beautify-msg "msg.modify.readonly"
         "Cannot modify readonly property: %s.")

;; TokenStream
(js3-beautify-msg "msg.missing.exponent"
         "missing exponent")

(js3-beautify-msg "msg.caught.nfe"
         "number format error")

(js3-beautify-msg "msg.unterminated.string.lit"
         "unterminated string literal")

(js3-beautify-msg "msg.unterminated.comment"
         "unterminated comment")

(js3-beautify-msg "msg.unterminated.re.lit"
         "unterminated regular expression literal")

(js3-beautify-msg "msg.invalid.re.flag"
         "invalid flag after regular expression")

(js3-beautify-msg "msg.no.re.input.for"
         "no input for %s")

(js3-beautify-msg "msg.illegal.character"
         "illegal character")

(js3-beautify-msg "msg.invalid.escape"
         "invalid Unicode escape sequence")

;; TokensStream warnings
(js3-beautify-msg "msg.bad.octal.literal"
         "illegal octal literal digit %s; "
         "interpreting it as a decimal digit")

(js3-beautify-msg "msg.reserved.keyword"
         "illegal usage of future reserved keyword %s; "
         "interpreting it as ordinary identifier")

(js3-beautify-msg "msg.script.is.not.constructor"
         "Script objects are not constructors.")

;; Arrays
(js3-beautify-msg "msg.arraylength.bad"
         "Inappropriate array length.")

;; Arrays
(js3-beautify-msg "msg.arraylength.too.big"
         "Array length %s exceeds supported capacity limit.")

;; URI
(js3-beautify-msg "msg.bad.uri"
         "Malformed URI sequence.")

;; Number
(js3-beautify-msg "msg.bad.precision"
         "Precision %s out of range.")

;; NativeGenerator
(js3-beautify-msg "msg.send.newborn"
         "Attempt to send value to newborn generator")

(js3-beautify-msg "msg.already.exec.gen"
         "Already executing generator")

(js3-beautify-msg "msg.StopIteration.invalid"
         "StopIteration may not be changed to an arbitrary object.")

;; Interpreter
(js3-beautify-msg "msg.yield.closing"
         "Yield from closing generator")

(provide 'js3-beautify-messages)

;; js3-beautify-messages.el ends here
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
;;; js3-beautify-indent.el --- indentation for js3-beautify

;;; Code:

(defconst js3-beautify-possibly-braceless-keyword-re
  (regexp-opt
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with" "let" "each")
   'words)
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js3-beautify-indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defconst js3-beautify-indent-lazy-operator-re
  (concat "[-+*/%<>=&^|?:]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions in lazy-operator-first style.")

(defconst js3-beautify-indent-operator-first-re
  (concat "[-+*/%<>!=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions with operator-first style.")

(defconst js3-beautify-indent-brace-re
  "[[({]"
  "Regexp matching opening braces that affect indentation.")

(defconst js3-beautify-indent-operator-brace-re
  "[[(]"
  "Regexp matching opening braces that affect operator indentation.")

(defconst js3-beautify-skip-newlines-re
  "[ \t\n]*"
  "Regexp matching any amount of trailing whitespace and newlines.")

(defconst js3-beautify-opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defun js3-beautify-backward-sws ()
  "Move backward through whitespace and comments."
  (interactive)
  (while (forward-comment -1)))

(defun js3-beautify-forward-sws ()
  "Move forward through whitespace and comments."
  (interactive)
  (while (forward-comment 1)))

(defun js3-beautify-beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at js3-beautify-opt-cpp-start))
          t
        (goto-char here)
        nil))))

;; This function has horrible results if you're typing an array
;; such as [[1, 2], [3, 4], [5, 6]].  Bounce indenting -really- sucks
;; in conjunction with electric-indent, so just disabling it.
(defsubst js3-beautify-code-at-bol-p ()
  "Return t if the first character on line is non-whitespace."
  nil)

(defun js3-beautify-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (let ((cmd (lookup-key (current-global-map) key)))
    (if (commandp cmd)
        (call-interactively cmd)))
  ;; don't do the electric keys inside comments or strings,
  ;; and don't do bounce-indent with them.
  (let ((parse-state (parse-partial-sexp (point-min) (point)))
        (js3-beautify-bounce-indent-p (js3-beautify-code-at-bol-p)))
    (unless (or (nth 3 parse-state)
                (nth 4 parse-state))
      (indent-according-to-mode))))


(defun js3-beautify-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `js3-beautify-re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (js3-beautify-beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (point-at-eol) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (js3-beautify-beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun js3-beautify-re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'js3-beautify-re-search-backward-inner)
               ((> count 0) #'js3-beautify-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))


(defun js3-beautify-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js3-beautify-re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (js3-beautify-beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (point-at-bol) t)
	     (when (not (string= "" (match-string 1)))
	       (forward-char)))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (js3-beautify-beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun js3-beautify-re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (js3-beautify-re-search-forward regexp bound noerror (if count (- count) -1)))


(defun js3-beautify-looking-back (regexp)
  "This function returns t if regexp matches text before point, ending at point, and nil otherwise.

This function is similar to `looking-back' but ignores comments and strings"
  (save-excursion
    (let ((r (if (and (= ?\= (elt regexp (1- (length regexp))))
		      (= ?\\ (elt regexp (- (length regexp) 2))))
		 regexp
	       (concat regexp "\\="))))
      (numberp (js3-beautify-re-search-backward r (point-min) t)))))

(defun js3-beautify-looking-at (regexp)
  "This function returns t if regexp matches text after point, beginning at point, and nil otherwise.

This function is similar to `looking-at' but ignores comments and strings"
  (save-excursion
    (let ((r (if (and (= ?\= (elt regexp 1))
		      (= ?\\ (elt regexp 0)))
		 regexp
	       (concat "\\=" regexp))))
      (numberp (js3-beautify-re-search-forward r nil t)))))

(defun js3-beautify-looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at js3-beautify-indent-operator-re)
         (or (not (= (following-char) ?\:))
             (save-excursion
               (and (js3-beautify-re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (= (following-char) ?\?)))))))


(defun js3-beautify-continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js3-beautify-looking-at-operator-p)
        (and (js3-beautify-re-search-backward "\n" nil t)
             (progn
               (skip-chars-backward " \t")
               (or (bobp) (backward-char))
               (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (js3-beautify-looking-at-operator-p)
                    (and (progn (backward-char)
                                (not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun js3-beautify-end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
        (if (save-excursion
              (skip-chars-backward (concat js3-beautify-skip-newlines-re "}"))
              (looking-at (concat js3-beautify-skip-newlines-re "}")))
            (save-excursion
              (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
          (js3-beautify-re-search-backward "\\_<do\\_>" (point-at-bol) t)
          (or (looking-at "\\_<do\\_>")
              (let ((saved-indent (current-indentation)))
                (while (and (js3-beautify-re-search-backward "^\\s-*\\_<" nil t)
                            (/= (current-indentation) saved-indent)))
                (and (looking-at "\\s-*\\_<do\\_>")
                     (not (js3-beautify-re-search-forward
                           "\\_<while\\_>" (point-at-eol) t))
                     (= (current-indentation) saved-indent)))))))))


(defun js3-beautify-backward-whitespace ()
  "Helper function for `js3-beautify-proper-indentation'.
Skip backwards over whitespace and comments."
  (let ((rv nil))
    (when (js3-beautify-looking-back "[ \t\n]")
      (setq rv t)
      (js3-beautify-re-search-backward (concat "[^ \t\n]" js3-beautify-skip-newlines-re)
			      (point-min) t)
      (forward-char))
    rv))


(defun js3-beautify-backward-sexp ()
  "Helper function for `js3-beautify-proper-indentation'.
Go backwards over matched braces, rather than whole expressions.
Only skip over strings while looking for braces.
Functionality does not exactly match backward-sexp."
  (let ((brackets 0)
	(rv nil))
    (while (js3-beautify-looking-back (concat "[]})]" js3-beautify-skip-newlines-re))
      (setq rv t)
      (js3-beautify-re-search-backward (concat "[]})]"
				      js3-beautify-skip-newlines-re)
			      (point-min) t)
      (cond
       ((= (following-char) ?\])
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-beautify-re-search-backward "[][]" (point-min) t)
          (cond
           ((= (following-char) ?\])
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\[)
            (setq brackets (1- brackets))))))

       ((= (following-char) ?\})
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-beautify-re-search-backward "[}{]" (point-min) t)
          (cond
           ((= (following-char) ?\})
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\{)
            (setq brackets (1- brackets))))))

       ((= (following-char) ?\))
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-beautify-re-search-backward "[)(]" (point-min) t)
          (cond
           ((= (following-char) ?\))
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\()
            (setq brackets (1- brackets))))))))
    rv))


(defun js3-beautify-backward-clean ()
  "Helper function for `js3-beautify-proper-indentation'.
Calls js3-beautify-backward-sexp and js3-beautify-backward-whitespace until they are done."
  (let ((rv nil))
    (while (or (js3-beautify-backward-whitespace) (js3-beautify-backward-sexp))
      (setq rv t))
    rv))


(defun js3-beautify-ctrl-statement-indentation ()
  "Helper function for `js3-beautify-proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (= (following-char) ?\{))
                 (progn
                   (js3-beautify-re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at js3-beautify-possibly-braceless-keyword-re))
                 (not (js3-beautify-end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) js3-beautify-indent-level)))))

(defun js3-beautify-get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c js3-beautify-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun js3-beautify-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond

     ;;inside a comment - indent like c
     ((nth 4 parse-status)
      (js3-beautify-get-c-offset 'c (nth 8 parse-status)))

     ;;inside a string - indent to 0 since you can't do that.
     ((nth 8 parse-status) 0)

     ;;comma-first and operator-first
     ((or
       (and (not js3-beautify-lazy-commas)
	    (= (following-char) ?\,))
       (and (not js3-beautify-lazy-operators)
	    (looking-at js3-beautify-indent-operator-first-re)
	    (or (not (= (following-char) ?\.))
		(not js3-beautify-lazy-dots))))
      (let ((node (js3-beautify-node-at-point))
	    (char (following-char)))
	(let ((spos
	       (save-excursion
		 (cond

		  ((and
		    node
;;;		    helpful debugging
;;;		    (message (number-to-string (js3-beautify-node-type node)))
;;;		    (js3-beautify-print-ast node)
;;;		    (message (number-to-string (js3-beautify-node-abs node)))
		    (js3-beautify-node-type node)
		    (= js3-beautify-VAR (js3-beautify-node-type node))) ; var node
		   (goto-char (js3-beautify-node-abs node))
		   (+ (current-column) 2))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-RETURN (js3-beautify-node-type node)))
		   (goto-char (js3-beautify-node-abs node))
		   (+ (current-column) 5))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-ARRAYLIT (js3-beautify-node-type node)))
		   (goto-char (js3-beautify-node-abs node))
		   (js3-beautify-re-search-forward "[[]" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-OBJECTLIT (js3-beautify-node-type node)))
		   (goto-char (js3-beautify-node-abs node))
		   (js3-beautify-re-search-forward "{" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-FUNCTION (js3-beautify-node-type node)))
		   (goto-char (js3-beautify-node-abs node))
		   (js3-beautify-re-search-forward "(" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-CALL (js3-beautify-node-type node)))
		   (goto-char (js3-beautify-node-abs node))
		   (js3-beautify-re-search-forward "(" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (>= (js3-beautify-node-type node) 9)
		    (<= (js3-beautify-node-type node) 18))    ; binary operators
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-COMMA (js3-beautify-node-type node))) ; comma operator
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-GETPROP (js3-beautify-node-type node))) ; dot operator
		   (goto-char (js3-beautify-node-abs node))
		   (if (js3-beautify-looking-at ".*\\..*")
		       (progn (js3-beautify-re-search-forward "\\." nil t)
			      (backward-char)
			      (current-column))
		     (+ (current-column)
			js3-beautify-expr-indent-offset js3-beautify-indent-level)))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (>= (js3-beautify-node-type node) 19)
		    (<= (js3-beautify-node-type node) 24))    ; 2-char binary operators
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 2)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= (js3-beautify-node-type node) 25))    ; 3-char binary operators
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 3)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (>= (js3-beautify-node-type node) 103)
		    (<= (js3-beautify-node-type node) 104))    ; logical and/or
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 2)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= js3-beautify-ASSIGN (js3-beautify-node-type node))) ; assignment
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (>= (js3-beautify-node-type node) 90)
		    (<= (js3-beautify-node-type node) 97))    ; assignment 2-char
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 2)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (>= (js3-beautify-node-type node) 98)
		    (<= (js3-beautify-node-type node) 99))    ; assignment 3-char
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 3)
		   (current-column))

		  ((and
		    node
		    (js3-beautify-node-type node)
		    (= (js3-beautify-node-type node) 100))    ; assignment 4-char
		   (goto-char (js3-beautify-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 4)
		   (current-column))

		  (t
		   (js3-beautify-backward-clean)
		   (cond
		    ((js3-beautify-looking-back (concat "[,([{].*" js3-beautify-skip-newlines-re))
		     (js3-beautify-re-search-backward (concat "[,([{].*"
						     js3-beautify-skip-newlines-re)
					     (point-min) t)
		     (current-column))

		    ((js3-beautify-looking-back (concat "\\<var\\>.*"
					       js3-beautify-skip-newlines-re))
		     (js3-beautify-re-search-backward (concat "\\<var\\>.*"
						     js3-beautify-skip-newlines-re)
					     (point-min) t)
		     (+ (current-column) 2))

		    ((js3-beautify-looking-back (concat "\\<return\\>.*"
					       js3-beautify-skip-newlines-re))
		     (js3-beautify-re-search-backward (concat "\\<return\\>.*"
						     js3-beautify-skip-newlines-re)
					     (point-min) t)
		     (+ (current-column) 5))

		    ((js3-beautify-looking-back (concat "[,([{]\\(.\\|\n\\)*"
					       js3-beautify-skip-newlines-re))
		     (js3-beautify-re-search-backward (concat "[,([{]\\(.\\|\n\\)*"
						     js3-beautify-skip-newlines-re)
					     (point-min) t)
		     (current-column))

		    (t
		     nil)))))))
	  (if spos
	      spos
	    (+ js3-beautify-indent-level js3-beautify-expr-indent-offset)))))

     ;;lazy comma-first
     ((and js3-beautify-lazy-commas
	   (= (following-char) ?\,))
      (save-excursion
	(js3-beautify-backward-sexp)
	(cond

	 ((js3-beautify-looking-back (concat "^[ \t]*,.*" js3-beautify-skip-newlines-re))
	  (js3-beautify-re-search-backward (concat "^[ \t]*,.*" js3-beautify-skip-newlines-re)
				  (point-min) t)
	  (back-to-indentation)
	  (current-column))

	 ((looking-back (concat "^[ \t]*[^ \t\n].*" js3-beautify-skip-newlines-re))
	  (re-search-backward (concat "^[ \t]*[^ \t\n].*" js3-beautify-skip-newlines-re)
			      (point-min) t)
	  (back-to-indentation)
	  (if (< (current-column) 2)
	      (current-column)
	    (- (current-column) 2)))

	 (t
	  (+ js3-beautify-indent-level js3-beautify-expr-indent-offset)))))

     ;;lazy dot-first
     ((and js3-beautify-lazy-dots
	   (= (following-char) ?\.))
      (save-excursion
	(js3-beautify-backward-sexp)
	(if (looking-back (concat "^[ \t]*[^ \t\n].*" js3-beautify-skip-newlines-re))
	    (progn
	      (re-search-backward (concat "^[ \t]*[^ \t\n].*"
					  js3-beautify-skip-newlines-re)
				  (point-min) t)
	      (back-to-indentation)
	      (+ (current-column) js3-beautify-indent-level))
	  (+ js3-beautify-indent-level js3-beautify-expr-indent-offset))))

     ;;lazy operator-first
     ((and js3-beautify-lazy-operators
	   (looking-at js3-beautify-indent-lazy-operator-re))
      (save-excursion
	(js3-beautify-backward-sexp)
	(if (looking-back (concat "^[ \t]*[^ \t\n].*" js3-beautify-skip-newlines-re))
	    (progn
	      (re-search-backward (concat "^[ \t]*[^ \t\n].*"
					  js3-beautify-skip-newlines-re)
				  (point-min) t)
	      (back-to-indentation)
	      (if (or (looking-at js3-beautify-indent-lazy-operator-re)
		      (< (current-column) 2))
		  (current-column)
		(- (current-column) 2)))
	  (+ js3-beautify-indent-level js3-beautify-expr-indent-offset))))

     ;;var special case for non-comma-first continued var statements
     ((and js3-beautify-pretty-vars
	   (looking-at "[^]})]")
	   (not (looking-at "\\<var\\>"))
           (js3-beautify-node-at-point)
           (js3-beautify-node-parent (js3-beautify-node-at-point))
           (js3-beautify-node-type (js3-beautify-node-parent (js3-beautify-node-at-point)))
           (= js3-beautify-VAR (js3-beautify-node-type (js3-beautify-node-parent (js3-beautify-node-at-point)))))
      (save-excursion
        (js3-beautify-re-search-backward "\\<var\\>" (point-min) t)
        (+ (current-column) 4)))

     ;;indent control statement body without braces, if applicable
     ((js3-beautify-ctrl-statement-indentation))

     ;;c preprocessor - indent to 0
     ((eq (char-after) ?#) 0)

     ;;we're in a cpp macro - indent to 4 why not
     ((save-excursion (js3-beautify-beginning-of-macro)) 4)

     ;;inside a parenthetical grouping
     ((nth 1 parse-status)
      ;; A single closing paren/bracket should be indented at the
      ;; same level as the opening statement.
      (let ((same-indent-p (looking-at
                            "[]})]"))
            (continued-expr-p (js3-beautify-continued-expression-p)))
        (goto-char (nth 1 parse-status)) ; go to the opening char
        (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
            (progn ; nothing following the opening paren/bracket
              (skip-syntax-backward " ")
              (when (eq (char-before) ?\)) (backward-list)) ;skip arg list
	      (if (and (not js3-beautify-consistent-level-indent-inner-bracket)
		       (js3-beautify-looking-back (concat
					  ":"
					  js3-beautify-skip-newlines-re
					  "\\<function\\>"
					  js3-beautify-skip-newlines-re)))
		  (progn
		    (js3-beautify-re-search-backward (concat
					     ":"
					     js3-beautify-skip-newlines-re
					     "\\<function\\>"
					     js3-beautify-skip-newlines-re))
		    (js3-beautify-backward-clean)
		    (if (looking-back "[{[(,][^{[(,\n]*")
			(progn
			  (js3-beautify-re-search-backward "[{[(,][^{[(,\n]*")
			  (forward-char)
			  (js3-beautify-re-search-forward "[ \t]*"))
		      (progn
			(js3-beautify-re-search-backward "^")
			(back-to-indentation))))
		(back-to-indentation))
              (cond (same-indent-p
                     (current-column))
                    (continued-expr-p
                     (+ (current-column) (* 2 js3-beautify-indent-level)
                        js3-beautify-expr-indent-offset))
                    (t
                     (+ (current-column) js3-beautify-indent-level
                        (case (char-after (nth 1 parse-status))
                              (?\( js3-beautify-paren-indent-offset)
                              (?\[ js3-beautify-square-indent-offset)
                              (?\{ js3-beautify-curly-indent-offset))))))
          ;; If there is something following the opening
          ;; paren/bracket, everything else should be indented at
          ;; the same level.
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

     ;;in a continued expression not handled by earlier cases
     ((js3-beautify-continued-expression-p)
      (+ js3-beautify-indent-level js3-beautify-expr-indent-offset))

     ;;if none of these cases, then indent to 0
     (t 0))))

(defun js3-beautify-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (when js3-beautify-reparse-on-indent (js3-beautify-reparse))
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (js3-beautify-proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;; js3-beautify-indent.el ends here
;;; js3-beautify-foot.el

(eval-when-compile
  (require 'cl))

(defun js3-beautify ()
  "Beautify JavaScript code in the current buffer."
  (interactive)
  (js3-beautify-check-compat)
  (set-syntax-table js3-beautify-syntax-table)
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

  (set (make-local-variable 'forward-sexp-function) #'js3-beautify-forward-sexp)
  (setq js3-beautify-buffer-dirty-p t
        js3-beautify-parsing nil)
  (js3-beautify-reparse)
  (js3-beautify-print-tree js3-beautify-ast)
  (erase-buffer)
  (insert js3-beautify-curstr)
  (delete-trailing-whitespace)
  (js3-beautify-reparse)
  (indent-region (point-min) (point-max) nil)
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
  (setq js3-beautify-curstr "")
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
  (let ((parent (js3-beautify-node-parent-script-or-fn (js3-beautify-node-at-point))))
    (if (not (js3-beautify-function-node-p parent))
        ;; punt:  skip over next script-level element beyond point
        (js3-beautify-forward-sibling)
      (goto-char (+ 1 (+ (js3-beautify-node-abs-pos parent)
                         (js3-beautify-node-len parent)))))))

(defun js3-beautify-mark-defun (&optional allow-extend)
  "Put mark at end of this function, point at beginning.
The function marked is the one that contains point."
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
