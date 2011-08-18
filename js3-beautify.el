;;; js3-beautify.el -- a JavaScript beautifier based on js3-mode
;;;

;;; js3-bfy-head.el

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

;;; js3-bfy-head.el ends here
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

(defvar js3-bfy-curstr "")
(defvar js3-bfy-curln "")
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
;;; js3-bfy-util.el -- JavaScript utilities

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

(defun js3-bfy-delete-if (predicate list)
  "Remove all items satisfying PREDICATE in LIST."
  (loop for item in list
        if (not (funcall predicate item))
        collect item))

(defun js3-bfy-position (element list)
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

(defun js3-bfy-find-if (predicate list)
  "Find first item satisfying PREDICATE in LIST."
  (let (result)
    (while (and list (not result))
      (if (funcall predicate (car list))
          (setq result (car list)))
      (setq list (cdr list)))
    result))

;;; end Emacs 21 compat

(defmacro js3-bfy-time (form)
  "Evaluate FORM, discard result, and return elapsed time in sec"
  (let ((beg (make-symbol "--js3-bfy-time-beg--"))
        (delta (make-symbol "--js3-bfy-time-end--")))
    `(let ((,beg (current-time))
           ,delta)
       ,form
       (/ (truncate (* (- (float-time (current-time))
                          (float-time ,beg))
		       10000))
          10000.0))))

(def-edebug-spec js3-bfy-time t)

(defsubst neq (expr1 expr2)
  "Return (not (eq expr1 expr2))."
  (not (eq expr1 expr2)))

(defsubst js3-bfy-same-line (pos)
  "Return t if POS is on the same line as current point."
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defsubst js3-bfy-same-line-2 (p1 p2)
  "Return t if p1 is on the same line as p2."
  (save-excursion
    (goto-char p1)
    (js3-bfy-same-line p2)))

(defun js3-bfy-code-bug ()
  "Signal an error when we encounter an unexpected code path."
  (error "failed assertion"))

(defsubst js3-bfy-record-text-property (beg end prop value)
  "Set a text property."
  (apply #'put-text-property beg end prop value))

;; I'd like to associate errors with nodes, but for now the
;; easiest thing to do is get the context info from the last token.
(defsubst js3-bfy-record-parse-error (msg &optional arg pos len)
  (push (list (list msg arg)
              (or pos js3-bfy-token-beg)
              (or len (- js3-bfy-token-end js3-bfy-token-beg)))
        js3-bfy-parsed-errors))

(defsubst js3-bfy-report-error (msg &optional msg-arg pos len)
  "Signal a syntax error or record a parse error."
  (if js3-bfy-recover-from-parse-errors
      (js3-bfy-record-parse-error msg msg-arg pos len)
    (signal 'js3-bfy-syntax-error
            (list msg
                  js3-bfy-ts-lineno
                  (save-excursion
                    (goto-char js3-bfy-ts-cursor)
                    (current-column))
                  js3-bfy-ts-hit-eof))))

(defsubst js3-bfy-report-warning (msg &optional msg-arg pos len)
  (if js3-bfy-compiler-report-warning-as-error
      (js3-bfy-report-error msg msg-arg pos len)
    (push (list (list msg msg-arg)
                (or pos js3-bfy-token-beg)
                (or len (- js3-bfy-token-end js3-bfy-token-beg)))
          js3-bfy-parsed-warnings)))

(defsubst js3-bfy-add-strict-warning (msg-id &optional msg-arg beg end)
  (if js3-bfy-compiler-strict-mode
      (js3-bfy-report-warning msg-id msg-arg beg
			      (and beg end (- end beg)))))

(put 'js3-bfy-syntax-error 'error-conditions
     '(error syntax-error js3-bfy-syntax-error))
(put 'js3-bfy-syntax-error 'error-message "Syntax error")

(put 'js3-bfy-parse-error 'error-conditions
     '(error parse-error js3-bfy-parse-error))
(put 'js3-bfy-parse-error 'error-message "Parse error")

(defmacro js3-bfy-clear-flag (flags flag)
  `(setq ,flags (logand ,flags (lognot ,flag))))

(defmacro js3-bfy-set-flag (flags flag)
  "Logical-or FLAG into FLAGS."
  `(setq ,flags (logior ,flags ,flag)))

(defsubst js3-bfy-flag-set-p (flags flag)
  (/= 0 (logand flags flag)))

(defsubst js3-bfy-flag-not-set-p (flags flag)
  (zerop (logand flags flag)))

;; Stolen shamelessly from James Clark's nxml-mode.
(defmacro js3-bfy-with-unmodifying-text-property-changes (&rest body)
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

(put 'js3-bfy-with-unmodifying-text-property-changes 'lisp-indent-function 0)
(def-edebug-spec js3-bfy-with-unmodifying-text-property-changes t)

(defmacro js3-bfy-with-underscore-as-word-syntax (&rest body)
  "Evaluate BODY with the _ character set to be word-syntax."
  (let ((old-syntax (make-symbol "old-syntax")))
    `(let ((,old-syntax (string (char-syntax ?_))))
       (unwind-protect
           (progn
             (modify-syntax-entry ?_ "w" js3-bfy-syntax-table)
             ,@body)
         (modify-syntax-entry ?_ ,old-syntax js3-bfy-syntax-table)))))

(put 'js3-bfy-with-underscore-as-word-syntax 'lisp-indent-function 0)
(def-edebug-spec js3-bfy-with-underscore-as-word-syntax t)

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

(provide 'js3-bfy-util)

;;; js3-bfy-util.el ends here
;;; js3-bfy-scan.el --- JavaScript scanner

;;; Commentary:

;; A port of Mozilla Rhino's scanner.
;; Corresponds to Rhino files Token.java and TokenStream.java.

;;; Code:


(eval-when-compile
  (require 'cl))

(defvar js3-bfy-tokens nil
  "List of all defined token names.")  ; initialized in `js3-bfy-token-names'

(defconst js3-bfy-token-names
  (let* ((names (make-vector js3-bfy-num-tokens -1))
         (case-fold-search nil)  ; only match js3-bfy-UPPER_CASE
         (syms (apropos-internal "^js3-bfy-\\(?:[A-Z_]+\\)")))
    (loop for sym in syms
          for i from 0
          do
          (unless (or (memq sym '(js3-bfy-EOF_CHAR js3-bfy-ERROR))
                      (not (boundp sym)))
            (aset names (symbol-value sym)         ; code, e.g. 152
                  (substring (symbol-name sym) 8)) ; name, e.g. "LET"
            (push sym js3-bfy-tokens)))
    names)
  "Vector mapping int values to token string names, sans `js3-bfy-' prefix.")

(defun js3-bfy-token-name (tok)
  "Return a string name for TOK, a token symbol or code.
Signals an error if it's not a recognized token."
  (let ((code tok))
    (if (symbolp tok)
        (setq code (symbol-value tok)))
    (if (eq code -1)
        "ERROR"
      (if (and (numberp code)
               (not (minusp code))
               (< code js3-bfy-num-tokens))
          (aref js3-bfy-token-names code)
        (error "Invalid token: %s" code)))))

(defsubst js3-bfy-token-sym (tok)
  "Return symbol for TOK given its code, e.g. 'js3-bfy-LP for code 86."
  (intern (js3-bfy-token-name tok)))

(defconst js3-bfy-token-codes
  (let ((table (make-hash-table :test 'eq :size 256)))
    (loop for name across js3-bfy-token-names
          for sym = (intern (concat "js3-bfy-" name))
          do
          (puthash sym (symbol-value sym) table))
    ;; clean up a few that are "wrong" in Rhino's token codes
    (puthash 'js3-bfy-DELETE js3-bfy-DELPROP table)
    table)
  "Hashtable mapping token symbols to their bytecodes.")

(defsubst js3-bfy-token-code (sym)
  "Return code for token symbol SYM, e.g. 86 for 'js3-bfy-LP."
  (or (gethash sym js3-bfy-token-codes)
      (error "Invalid token symbol: %s " sym)))  ; signal code bug

(defsubst js3-bfy-report-scan-error (msg &optional no-throw beg len)
  (setq js3-bfy-token-end js3-bfy-ts-cursor)
  (js3-bfy-report-error msg nil
			(or beg js3-bfy-token-beg)
			(or len (- js3-bfy-token-end js3-bfy-token-beg)))
  (unless no-throw
    (throw 'return js3-bfy-ERROR)))

(defsubst js3-bfy-get-string-from-buffer ()
  "Reverse the char accumulator and return it as a string."
  (setq js3-bfy-token-end js3-bfy-ts-cursor)
  (if js3-bfy-ts-string-buffer
      (apply #'string (nreverse js3-bfy-ts-string-buffer))
    ""))

;; TODO:  could potentially avoid a lot of consing by allocating a
;; char buffer the way Rhino does.
(defsubst js3-bfy-add-to-string (c)
  (push c js3-bfy-ts-string-buffer))

;; Note that when we "read" the end-of-file, we advance js3-bfy-ts-cursor
;; to (1+ (point-max)), which lets the scanner treat end-of-file like
;; any other character:  when it's not part of the current token, we
;; unget it, allowing it to be read again by the following call.
(defsubst js3-bfy-unget-char ()
  (decf js3-bfy-ts-cursor))

;; Rhino distinguishes \r and \n line endings.  We don't need to
;; because we only scan from Emacs buffers, which always use \n.
(defsubst js3-bfy-get-char ()
  "Read and return the next character from the input buffer.
Increments `js3-bfy-ts-lineno' if the return value is a newline char.
Updates `js3-bfy-ts-cursor' to the point after the returned char.
Returns `js3-bfy-EOF_CHAR' if we hit the end of the buffer.
Also updates `js3-bfy-ts-hit-eof' and `js3-bfy-ts-line-start' as needed."
  (let (c)
    ;; check for end of buffer
    (if (>= js3-bfy-ts-cursor (point-max))
        (setq js3-bfy-ts-hit-eof t
              js3-bfy-ts-cursor (1+ js3-bfy-ts-cursor)
              c js3-bfy-EOF_CHAR)  ; return value
      ;; otherwise read next char
      (setq c (char-before (incf js3-bfy-ts-cursor)))
      ;; if we read a newline, update counters
      (if (= c ?\n)
          (setq js3-bfy-ts-line-start js3-bfy-ts-cursor
                js3-bfy-ts-lineno (1+ js3-bfy-ts-lineno)))
      ;; TODO:  skip over format characters
      c)))

(defsubst js3-bfy-read-unicode-escape ()
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
   (let ((s (buffer-substring-no-properties js3-bfy-ts-cursor
                                            (+ 4 js3-bfy-ts-cursor))))
     (if (string-match "[a-zA-Z0-9]\\{4\\}" s)
         (read (concat "?\\u" s))))))

(defsubst js3-bfy-match-char (test)
  "Consume and return next character if it matches TEST, a character.
Returns nil and consumes nothing if TEST is not the next character."
  (let ((c (js3-bfy-get-char)))
    (if (eq c test)
        t
      (js3-bfy-unget-char)
      nil)))

(defsubst js3-bfy-peek-char ()
  (prog1
      (js3-bfy-get-char)
    (js3-bfy-unget-char)))

(defsubst js3-bfy-java-identifier-start-p (c)
  (or
   (memq c '(?$ ?_))
   (char-is-uppercase c)
   (char-is-lowercase c)))

(defsubst js3-bfy-java-identifier-part-p (c)
  "Implementation of java.lang.Character.isJavaIdentifierPart()"
  ;; TODO:  make me Unicode-friendly.  See comments above.
  (or
   (memq c '(?$ ?_))
   (char-is-uppercase c)
   (char-is-lowercase c)
   (and (>= c ?0) (<= c ?9))))

(defsubst js3-bfy-alpha-p (c)
  (cond ((and (<= ?A c) (<= c ?Z)) t)
        ((and (<= ?a c) (<= c ?z)) t)
        (t nil)))

(defsubst js3-bfy-digit-p (c)
  (and (<= ?0 c) (<= c ?9)))

(defsubst js3-bfy-js-space-p (c)
  (if (<= c 127)
      (memq c '(#x20 #x9 #xB #xC #xD))
    (or
     (eq c #xA0)
     ;; TODO:  change this nil to check for Unicode space character
     nil)))

(defconst js3-bfy-eol-chars (list js3-bfy-EOF_CHAR ?\n ?\r))

(defsubst js3-bfy-skip-line ()
  "Skip to end of line"
  (let (c)
    (while (not (memq (setq c (js3-bfy-get-char)) js3-bfy-eol-chars)))
    (js3-bfy-unget-char)
    (setq js3-bfy-token-end js3-bfy-ts-cursor)))

(defun js3-bfy-init-scanner (&optional buf line)
  "Create token stream for BUF starting on LINE.
BUF defaults to current-buffer and line defaults to 1.

A buffer can only have one scanner active at a time, which yields
dramatically simpler code than using a defstruct.  If you need to
have simultaneous scanners in a buffer, copy the regions to scan
into temp buffers."
  (save-excursion
    (when buf
      (set-buffer buf))
    (setq js3-bfy-ts-dirty-line nil
          js3-bfy-ts-regexp-flags nil
          js3-bfy-ts-string ""
          js3-bfy-ts-number nil
          js3-bfy-ts-hit-eof nil
          js3-bfy-ts-line-start 0
          js3-bfy-ts-lineno (or line 1)
          js3-bfy-ts-line-end-char -1
          js3-bfy-ts-cursor (point-min)
          js3-bfy-ts-string-buffer nil)))

(defconst js3-bfy-keywords
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
;; E.g. enum isn't in the tokens, and delete is js3-bfy-DELPROP.
(defconst js3-bfy-kwd-tokens
  (let ((table (make-vector js3-bfy-num-tokens nil))
        (tokens
         (list js3-bfy-BREAK
               js3-bfy-CASE js3-bfy-CATCH js3-bfy-CONST js3-bfy-CONTINUE
               js3-bfy-DEBUGGER js3-bfy-DEFAULT js3-bfy-DELPROP js3-bfy-DO
               js3-bfy-ELSE
               js3-bfy-FALSE js3-bfy-FINALLY js3-bfy-FOR js3-bfy-FUNCTION
               js3-bfy-IF js3-bfy-IN js3-bfy-INSTANCEOF js3-bfy-IMPORT
               js3-bfy-LET
               js3-bfy-NEW js3-bfy-NULL
               js3-bfy-RETURN
               js3-bfy-SWITCH
               js3-bfy-THIS js3-bfy-THROW js3-bfy-TRUE js3-bfy-TRY js3-bfy-TYPEOF
               js3-bfy-VAR
               js3-bfy-WHILE js3-bfy-WITH
               js3-bfy-YIELD)))
    (dolist (i tokens)
      (aset table i t))
    (aset table js3-bfy-STRING t)
    (aset table js3-bfy-REGEXP t)
    (aset table js3-bfy-COMMENT t)
    (aset table js3-bfy-THIS t)
    (aset table js3-bfy-VOID t)
    (aset table js3-bfy-NULL t)
    (aset table js3-bfy-TRUE t)
    (aset table js3-bfy-FALSE t)
    table)
  "Vector whose values are non-nil for tokens that are keywords.")

(defconst js3-bfy-reserved-words
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

(defconst js3-bfy-keyword-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js3-bfy-keywords
          do (puthash
              (symbol-name k)                            ; instanceof
              (intern (concat "js3-bfy-"
                              (upcase (symbol-name k)))) ; js3-bfy-INSTANCEOF
              table))
    table)
  "JavaScript keywords by name, mapped to their symbols.")

(defconst js3-bfy-reserved-word-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js3-bfy-reserved-words
          do
          (puthash (symbol-name k) 'js3-bfy-RESERVED table))
    table)
  "JavaScript reserved words by name, mapped to 'js3-bfy-RESERVED.")

(defsubst js3-bfy-collect-string (buf)
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

(defun js3-bfy-string-to-keyword (s)
  "Return token for S, a string, if S is a keyword or reserved word.
Returns a symbol such as 'js3-bfy-BREAK, or nil if not keyword/reserved."
  (or (gethash s js3-bfy-keyword-names)
      (gethash s js3-bfy-reserved-word-names)))

(defsubst js3-bfy-ts-set-char-token-bounds ()
  "Used when next token is one character."
  (setq js3-bfy-token-beg (1- js3-bfy-ts-cursor)
        js3-bfy-token-end js3-bfy-ts-cursor))

(defsubst js3-bfy-ts-return (token)
  "Return an N-character TOKEN from `js3-bfy-get-token'.
Updates `js3-bfy-token-end' accordingly."
  (setq js3-bfy-token-end js3-bfy-ts-cursor)
  (throw 'return token))

(defsubst js3-bfy-x-digit-to-int (c accumulator)
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

(defun js3-bfy-get-token ()
  "Return next JavaScript token, an int such as js3-bfy-RETURN."
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
          (setq c (js3-bfy-get-char))
          (cond
           ((eq c js3-bfy-EOF_CHAR)
            (js3-bfy-ts-set-char-token-bounds)
            (throw 'return js3-bfy-EOF))
           ((eq c ?\n)
            (js3-bfy-ts-set-char-token-bounds)
            (setq js3-bfy-ts-dirty-line nil)
            (throw 'return js3-bfy-EOL))
           ((not (js3-bfy-js-space-p c))
            (if (/= c ?-)               ; in case end of HTML comment
                (setq js3-bfy-ts-dirty-line t))
            (setq continue nil))))
        ;; Assume the token will be 1 char - fixed up below.
        (js3-bfy-ts-set-char-token-bounds)
        ;; identifier/keyword/instanceof?
        ;; watch out for starting with a <backslash>
        (cond
         ((eq c ?\\)
          (setq c (js3-bfy-get-char))
          (if (eq c ?u)
              (setq identifier-start t
                    is-unicode-escape-start t
                    js3-bfy-ts-string-buffer nil)
            (setq identifier-start nil)
            (js3-bfy-unget-char)
            (setq c ?\\)))
         (t
          (when (setq identifier-start (js3-bfy-java-identifier-start-p c))
            (setq js3-bfy-ts-string-buffer nil)
            (js3-bfy-add-to-string c))))
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
                      (setq c (js3-bfy-get-char)
                            escape-val (js3-bfy-x-digit-to-int c escape-val))
                      ;; Next check takes care of c < 0 and bad escape
                      (if (minusp escape-val)
                          (throw 'break nil)))
                    (if (minusp escape-val)
                        (js3-bfy-report-scan-error "msg.invalid.escape" t))
                    (js3-bfy-add-to-string escape-val)
                    (setq is-unicode-escape-start nil))
                (setq c (js3-bfy-get-char))
                (cond
                 ((eq c ?\\)
                  (setq c (js3-bfy-get-char))
                  (if (eq c ?u)
                      (setq is-unicode-escape-start t
                            contains-escape t)
                    (js3-bfy-report-scan-error "msg.illegal.character" t)))
                 (t
                  (if (or (eq c js3-bfy-EOF_CHAR)
                          (not (js3-bfy-java-identifier-part-p c)))
                      (throw 'break nil))
                  (js3-bfy-add-to-string c))))))
          (js3-bfy-unget-char)
          (setq str (js3-bfy-get-string-from-buffer))
          (unless contains-escape
            ;; OPT we shouldn't have to make a string (object!) to
            ;; check if it's a keyword.
            ;; Return the corresponding token if it's a keyword
            (when (setq result (js3-bfy-string-to-keyword str))
              (if (and (< js3-bfy-language-version 170)
                       (memq result '(js3-bfy-LET js3-bfy-YIELD)))
                  ;; LET and YIELD are tokens only in 1.7 and later
                  (setq result 'js3-bfy-NAME))
              (if (neq result 'js3-bfy-RESERVED)
                  (throw 'return (js3-bfy-token-code result)))
              (js3-bfy-report-warning "msg.reserved.keyword" str)))
          ;; If we want to intern these as Rhino does, just use (intern str)
          (setq js3-bfy-ts-string str)
          (throw 'return js3-bfy-NAME))     ; end identifier/kwd check
        ;; is it a number?
        (when (or (js3-bfy-digit-p c)
                  (and (eq c ?.) (js3-bfy-digit-p (js3-bfy-peek-char))))
          (setq js3-bfy-ts-string-buffer nil
                base 10)
          (when (eq c ?0)
            (setq c (js3-bfy-get-char))
            (cond
             ((or (eq c ?x) (eq c ?X))
              (setq base 16)
              (setq c (js3-bfy-get-char)))
             ((js3-bfy-digit-p c)
              (setq base 8))
             (t
              (js3-bfy-add-to-string ?0))))
          (if (eq base 16)
              (while (<= 0 (js3-bfy-x-digit-to-int c 0))
                (js3-bfy-add-to-string c)
                (setq c (js3-bfy-get-char)))
            (while (and (<= ?0 c) (<= c ?9))
              ;; We permit 08 and 09 as decimal numbers, which
              ;; makes our behavior a superset of the ECMA
              ;; numeric grammar.  We might not always be so
              ;; permissive, so we warn about it.
              (when (and (eq base 8) (>= c ?8))
                (js3-bfy-report-warning "msg.bad.octal.literal"
					(if (eq c ?8) "8" "9"))
                (setq base 10))
              (js3-bfy-add-to-string c)
              (setq c (js3-bfy-get-char))))
          (setq is-integer t)
          (when (and (eq base 10) (memq c '(?. ?e ?E)))
            (setq is-integer nil)
            (when (eq c ?.)
              (loop do
                    (js3-bfy-add-to-string c)
                    (setq c (js3-bfy-get-char))
                    while (js3-bfy-digit-p c)))
            (when (memq c '(?e ?E))
              (js3-bfy-add-to-string c)
              (setq c (js3-bfy-get-char))
              (when (memq c '(?+ ?-))
                (js3-bfy-add-to-string c)
                (setq c (js3-bfy-get-char)))
              (unless (js3-bfy-digit-p c)
                (js3-bfy-report-scan-error "msg.missing.exponent" t))
              (loop do
                    (js3-bfy-add-to-string c)
                    (setq c (js3-bfy-get-char))
                    while (js3-bfy-digit-p c))))
          (js3-bfy-unget-char)
          (setq js3-bfy-ts-string (js3-bfy-get-string-from-buffer)
                js3-bfy-ts-number
                (if (and (eq base 10) (not is-integer))
                    (string-to-number js3-bfy-ts-string)
                  ;; TODO:  call runtime number-parser.  Some of it is in
                  ;; js3-bfy-util.el, but I need to port ScriptRuntime.stringToNumber.
                  (string-to-number js3-bfy-ts-string)))
          (throw 'return js3-bfy-NUMBER))
        ;; is it a string?
        (when (memq c '(?\" ?\'))
          ;; We attempt to accumulate a string the fast way, by
          ;; building it directly out of the reader.  But if there
          ;; are any escaped characters in the string, we revert to
          ;; building it out of a string buffer.
          (setq quote-char c
                js3-bfy-ts-string-buffer nil
                c (js3-bfy-get-char))
          (catch 'break
            (while (/= c quote-char)
              (catch 'continue
                (when (or (eq c ?\n) (eq c js3-bfy-EOF_CHAR))
                  (js3-bfy-unget-char)
                  (setq js3-bfy-token-end js3-bfy-ts-cursor)
                  (js3-bfy-report-error "msg.unterminated.string.lit")
                  (throw 'return js3-bfy-STRING))
                (when (eq c ?\\)
                  ;; We've hit an escaped character
                  (setq c (js3-bfy-get-char))
                  (case c
                        (?b (setq c ?\b))
                        (?f (setq c ?\f))
                        (?n (setq c ?\n))
                        (?r (setq c ?\r))
                        (?t (setq c ?\t))
                        (?v (setq c ?\v))
                        (?u
                         (setq c1 (js3-bfy-read-unicode-escape))
                         (if js3-bfy-parse-ide-mode
                             (if c1
                                 (progn
                                   ;; just copy the string in IDE-mode
                                   (js3-bfy-add-to-string ?\\)
                                   (js3-bfy-add-to-string ?u)
                                   (dotimes (i 3)
                                     (js3-bfy-add-to-string (js3-bfy-get-char)))
                                   (setq c (js3-bfy-get-char))) ; added at end of loop
                               ;; flag it as an invalid escape
                               (js3-bfy-report-warning "msg.invalid.escape"
						       nil (- js3-bfy-ts-cursor 2) 6))
                           ;; Get 4 hex digits; if the u escape is not
                           ;; followed by 4 hex digits, use 'u' + the
                           ;; literal character sequence that follows.
                           (js3-bfy-add-to-string ?u)
                           (setq escape-val 0)
                           (dotimes (i 4)
                             (setq c (js3-bfy-get-char)
                                   escape-val (js3-bfy-x-digit-to-int c escape-val))
                             (if (minusp escape-val)
                                 (throw 'continue nil))
                             (js3-bfy-add-to-string c))
                           ;; prepare for replace of stored 'u' sequence by escape value
                           (setq js3-bfy-ts-string-buffer (nthcdr 5 js3-bfy-ts-string-buffer)
                                 c escape-val)))
                        (?x
                         ;; Get 2 hex digits, defaulting to 'x'+literal
                         ;; sequence, as above.
                         (setq c (js3-bfy-get-char)
                               escape-val (js3-bfy-x-digit-to-int c 0))
                         (if (minusp escape-val)
                             (progn
                               (js3-bfy-add-to-string ?x)
                               (throw 'continue nil))
                           (setq c1 c
                                 c (js3-bfy-get-char)
                                 escape-val (js3-bfy-x-digit-to-int c escape-val))
                           (if (minusp escape-val)
                               (progn
                                 (js3-bfy-add-to-string ?x)
                                 (js3-bfy-add-to-string c1)
                                 (throw 'continue nil))
                             ;; got 2 hex digits
                             (setq c escape-val))))
                        (?\n
                         ;; Remove line terminator after escape to follow
                         ;; SpiderMonkey and C/C++
                         (setq c (js3-bfy-get-char))
                         (throw 'continue nil))
                        (t
                         (when (and (<= ?0 c) (< c ?8))
                           (setq val (- c ?0)
                                 c (js3-bfy-get-char))
                           (when (and (<= ?0 c) (< c ?8))
                             (setq val (- (+ (* 8 val) c) ?0)
                                   c (js3-bfy-get-char))
                             (when (and (<= ?0 c)
                                        (< c ?8)
                                        (< val #o37))
                               ;; c is 3rd char of octal sequence only
                               ;; if the resulting val <= 0377
                               (setq val (- (+ (* 8 val) c) ?0)
                                     c (js3-bfy-get-char))))
                           (js3-bfy-unget-char)
                           (setq c val)))))
                (js3-bfy-add-to-string c)
                (setq c (js3-bfy-get-char)))))
          (setq js3-bfy-ts-string (js3-bfy-get-string-from-buffer))
          (throw 'return js3-bfy-STRING))
        (case c
              (?\;
               (throw 'return js3-bfy-SEMI))
              (?\[
               (throw 'return js3-bfy-LB))
              (?\]
               (throw 'return js3-bfy-RB))
              (?{
               (throw 'return js3-bfy-LC))
              (?}
               (throw 'return js3-bfy-RC))
              (?\(
               (throw 'return js3-bfy-LP))
              (?\)
               (throw 'return js3-bfy-RP))
              (?,
               (throw 'return js3-bfy-COMMA))
              (??
               (throw 'return js3-bfy-HOOK))
              (?:
	       (throw 'return js3-bfy-COLON))
              (?.
	       (throw 'return js3-bfy-DOT))
              (?|
               (if (js3-bfy-match-char ?|)
                   (throw 'return js3-bfy-OR)
                 (if (js3-bfy-match-char ?=)
                     (js3-bfy-ts-return js3-bfy-ASSIGN_BITOR)
                   (throw 'return js3-bfy-BITOR))))
              (?^
               (if (js3-bfy-match-char ?=)
                   (js3-bfy-ts-return js3-bfy-ASSIGN_BITOR)
                 (throw 'return js3-bfy-BITXOR)))
              (?&
               (if (js3-bfy-match-char ?&)
                   (throw 'return js3-bfy-AND)
                 (if (js3-bfy-match-char ?=)
                     (js3-bfy-ts-return js3-bfy-ASSIGN_BITAND)
                   (throw 'return js3-bfy-BITAND))))
              (?=
               (if (js3-bfy-match-char ?=)
                   (if (js3-bfy-match-char ?=)
                       (js3-bfy-ts-return js3-bfy-SHEQ)
                     (throw 'return js3-bfy-EQ))
                 (throw 'return js3-bfy-ASSIGN)))
              (?!
               (if (js3-bfy-match-char ?=)
                   (if (js3-bfy-match-char ?=)
                       (js3-bfy-ts-return js3-bfy-SHNE)
                     (js3-bfy-ts-return js3-bfy-NE))
                 (throw 'return js3-bfy-NOT)))
              (?<
               ;; NB:treat HTML begin-comment as comment-till-eol
               (when (js3-bfy-match-char ?!)
                 (when (js3-bfy-match-char ?-)
                   (when (js3-bfy-match-char ?-)
                     (js3-bfy-skip-line)
                     (setq js3-bfy-ts-comment-type 'html)
                     (throw 'return js3-bfy-COMMENT)))
                 (js3-bfy-unget-char))
               (if (js3-bfy-match-char ?<)
                   (if (js3-bfy-match-char ?=)
                       (js3-bfy-ts-return js3-bfy-ASSIGN_LSH)
                     (js3-bfy-ts-return js3-bfy-LSH))
                 (if (js3-bfy-match-char ?=)
                     (js3-bfy-ts-return js3-bfy-LE)
                   (throw 'return js3-bfy-LT))))
              (?>
               (if (js3-bfy-match-char ?>)
                   (if (js3-bfy-match-char ?>)
                       (if (js3-bfy-match-char ?=)
                           (js3-bfy-ts-return js3-bfy-ASSIGN_URSH)
                         (js3-bfy-ts-return js3-bfy-URSH))
                     (if (js3-bfy-match-char ?=)
                         (js3-bfy-ts-return js3-bfy-ASSIGN_RSH)
                       (js3-bfy-ts-return js3-bfy-RSH)))
                 (if (js3-bfy-match-char ?=)
                     (js3-bfy-ts-return js3-bfy-GE)
                   (throw 'return js3-bfy-GT))))
              (?*
               (if (js3-bfy-match-char ?=)
                   (js3-bfy-ts-return js3-bfy-ASSIGN_MUL)
                 (throw 'return js3-bfy-MUL)))
              (?/
               ;; is it a // comment?
               (when (js3-bfy-match-char ?/)
                 (setq js3-bfy-token-beg (- js3-bfy-ts-cursor 2))
                 (js3-bfy-skip-line)
                 (setq js3-bfy-ts-comment-type 'line)
                 (incf js3-bfy-token-end)
                 (throw 'return js3-bfy-COMMENT))
               ;; is it a /* comment?
               (when (js3-bfy-match-char ?*)
                 (setq look-for-slash nil
                       js3-bfy-token-beg (- js3-bfy-ts-cursor 2)
                       js3-bfy-ts-comment-type
                       (if (js3-bfy-match-char ?*)
                           (progn
                             (setq look-for-slash t)
                             'jsdoc)
                         'block))
                 (while t
                   (setq c (js3-bfy-get-char))
                   (cond
                    ((eq c js3-bfy-EOF_CHAR)
                     (setq js3-bfy-token-end (1- js3-bfy-ts-cursor))
                     (js3-bfy-report-error "msg.unterminated.comment")
                     (throw 'return js3-bfy-COMMENT))
                    ((eq c ?*)
                     (setq look-for-slash t))
                    ((eq c ?/)
                     (if look-for-slash
                         (js3-bfy-ts-return js3-bfy-COMMENT)))
                    (t
                     (setq look-for-slash nil
                           js3-bfy-token-end js3-bfy-ts-cursor)))))
               (if (js3-bfy-match-char ?=)
                   (js3-bfy-ts-return js3-bfy-ASSIGN_DIV)
                 (throw 'return js3-bfy-DIV)))
              (?#
               (when js3-bfy-skip-preprocessor-directives
                 (js3-bfy-skip-line)
                 (setq js3-bfy-ts-comment-type 'preprocessor
                       js3-bfy-token-end js3-bfy-ts-cursor)
                 (throw 'return js3-bfy-COMMENT))
               (throw 'return js3-bfy-ERROR))
              (?%
               (if (js3-bfy-match-char ?=)
                   (js3-bfy-ts-return js3-bfy-ASSIGN_MOD)
                 (throw 'return js3-bfy-MOD)))
              (?~
               (throw 'return js3-bfy-BITNOT))
              (?+
               (if (js3-bfy-match-char ?=)
                   (js3-bfy-ts-return js3-bfy-ASSIGN_ADD)
                 (if (js3-bfy-match-char ?+)
                     (js3-bfy-ts-return js3-bfy-INC)
                   (throw 'return js3-bfy-ADD))))
              (?-
               (cond
                ((js3-bfy-match-char ?=)
                 (setq c js3-bfy-ASSIGN_SUB))
                ((js3-bfy-match-char ?-)
                 (unless js3-bfy-ts-dirty-line
                   ;; treat HTML end-comment after possible whitespace
                   ;; after line start as comment-until-eol
                   (when (js3-bfy-match-char ?>)
                     (js3-bfy-skip-line)
                     (setq js3-bfy-ts-comment-type 'html)
                     (throw 'return js3-bfy-COMMENT)))
                 (setq c js3-bfy-DEC))
                (t
                 (setq c js3-bfy-SUB)))
               (setq js3-bfy-ts-dirty-line t)
               (js3-bfy-ts-return c))
              (otherwise
               (js3-bfy-report-scan-error "msg.illegal.character")))))))

(defun js3-bfy-read-regexp (start-token)
  "Called by parser when it gets / or /= in literal context."
  (let (c
        err
        in-class  ; inside a '[' .. ']' character-class
        flags
        (continue t))
    (setq js3-bfy-token-beg js3-bfy-ts-cursor
          js3-bfy-ts-string-buffer nil
          js3-bfy-ts-regexp-flags nil)
    (if (eq start-token js3-bfy-ASSIGN_DIV)
        ;; mis-scanned /=
        (js3-bfy-add-to-string ?=)
      (if (neq start-token js3-bfy-DIV)
          (error "failed assertion")))
    (while (and (not err)
                (or (/= (setq c (js3-bfy-get-char)) ?/)
                    in-class))
      (cond
       ((or (= c ?\n)
            (= c js3-bfy-EOF_CHAR))
        (setq js3-bfy-token-end (1- js3-bfy-ts-cursor)
              err t
              js3-bfy-ts-string (js3-bfy-collect-string js3-bfy-ts-string-buffer))
        (js3-bfy-report-error "msg.unterminated.re.lit"))
       (t (cond
           ((= c ?\\)
            (js3-bfy-add-to-string c)
            (setq c (js3-bfy-get-char)))
           ((= c ?\[)
            (setq in-class t))
           ((= c ?\])
            (setq in-class nil)))
          (js3-bfy-add-to-string c))))
    (unless err
      (while continue
        (cond
         ((js3-bfy-match-char ?g)
          (push ?g flags))
         ((js3-bfy-match-char ?i)
          (push ?i flags))
         ((js3-bfy-match-char ?m)
          (push ?m flags))
         (t
          (setq continue nil))))
      (if (js3-bfy-alpha-p (js3-bfy-peek-char))
          (js3-bfy-report-scan-error "msg.invalid.re.flag" t
				     js3-bfy-ts-cursor 1))
      (setq js3-bfy-ts-string (js3-bfy-collect-string js3-bfy-ts-string-buffer)
            js3-bfy-ts-regexp-flags (js3-bfy-collect-string flags)
            js3-bfy-token-end js3-bfy-ts-cursor)
      ;; tell `parse-partial-sexp' to ignore this range of chars
      (js3-bfy-record-text-property
       js3-bfy-token-beg js3-bfy-token-end 'syntax-class '(2)))))

(defun js3-bfy-scanner-get-line ()
  "Return the text of the current scan line."
  (buffer-substring (point-at-bol) (point-at-eol)))

(provide 'js3-bfy-scan)

;;; js3-bfy-scan.el ends here
;;; js3-bfy-messages:  localizable messages for js3-bfy

;;; Commentary:

;; Messages are copied from Rhino's Messages.properties.
;; Many of the Java-specific messages have been elided.
;; Add any js3-bfy-specific ones at the end, so we can keep
;; this file synced with changes to Rhino's.
;;
;; TODO:
;;  - move interpreter messages into separate file

;;; Code:

(defvar js3-bfy-message-table
  (make-hash-table :test 'equal :size 250)
  "Contains localized messages for js3-bfy.")

;; TODO:  construct this hashtable at compile-time.
(defmacro js3-bfy-msg (key &rest strings)
  `(puthash ,key (funcall #'concat ,@strings)
            js3-bfy-message-table))

(defun js3-bfy-get-msg (msg-key)
  "Look up a localized message.
MSG-KEY is a list of (MSG ARGS).  If the message takes parameters,
the correct number of ARGS must be provided."
  (let* ((key (if (listp msg-key) (car msg-key) msg-key))
         (args (if (listp msg-key) (cdr msg-key)))
         (msg (gethash key js3-bfy-message-table)))
    (if msg
        (apply #'format msg args)
      key)))  ; default to showing the key

(js3-bfy-msg "msg.dup.parms"
	     "Duplicate parameter name '%s'.")

(js3-bfy-msg "msg.too.big.jump"
	     "Program too complex: jump offset too big.")

(js3-bfy-msg "msg.too.big.index"
	     "Program too complex: internal index exceeds 64K limit.")

(js3-bfy-msg "msg.while.compiling.fn"
	     "Encountered code generation error while compiling function '%s': %s")

(js3-bfy-msg "msg.while.compiling.script"
	     "Encountered code generation error while compiling script: %s")

;; Context
(js3-bfy-msg "msg.ctor.not.found"
	     "Constructor for '%s' not found.")

(js3-bfy-msg "msg.not.ctor"
	     "'%s' is not a constructor.")

;; FunctionObject
(js3-bfy-msg "msg.varargs.ctor"
	     "Method or constructor '%s' must be static "
	     "with the signature (Context cx, Object[] args, "
	     "Function ctorObj, boolean inNewExpr) "
	     "to define a variable arguments constructor.")

(js3-bfy-msg "msg.varargs.fun"
	     "Method '%s' must be static with the signature "
	     "(Context cx, Scriptable thisObj, Object[] args, Function funObj) "
	     "to define a variable arguments function.")

(js3-bfy-msg "msg.incompat.call"
	     "Method '%s' called on incompatible object.")

(js3-bfy-msg "msg.bad.parms"
	     "Unsupported parameter type '%s' in method '%s'.")

(js3-bfy-msg "msg.bad.method.return"
	     "Unsupported return type '%s' in method '%s'.")

(js3-bfy-msg "msg.bad.ctor.return"
	     "Construction of objects of type '%s' is not supported.")

(js3-bfy-msg "msg.no.overload"
	     "Method '%s' occurs multiple times in class '%s'.")

(js3-bfy-msg "msg.method.not.found"
	     "Method '%s' not found in '%s'.")

;; IRFactory

(js3-bfy-msg "msg.bad.for.in.lhs"
	     "Invalid left-hand side of for..in loop.")

(js3-bfy-msg "msg.mult.index"
	     "Only one variable allowed in for..in loop.")

(js3-bfy-msg "msg.bad.for.in.destruct"
	     "Left hand side of for..in loop must be an array of "
	     "length 2 to accept key/value pair.")

(js3-bfy-msg "msg.cant.convert"
	     "Can't convert to type '%s'.")

(js3-bfy-msg "msg.bad.assign.left"
	     "Invalid assignment left-hand side.")

(js3-bfy-msg "msg.bad.decr"
	     "Invalid decerement operand.")

(js3-bfy-msg "msg.bad.incr"
	     "Invalid increment operand.")

(js3-bfy-msg "msg.bad.yield"
	     "yield must be in a function.")

(js3-bfy-msg "msg.yield.parenthesized"
	     "yield expression must be parenthesized.")

;; NativeGlobal
(js3-bfy-msg "msg.cant.call.indirect"
	     "Function '%s' must be called directly, and not by way of a "
	     "function of another name.")

(js3-bfy-msg "msg.eval.nonstring"
	     "Calling eval() with anything other than a primitive "
	     "string value will simply return the value. "
	     "Is this what you intended?")

(js3-bfy-msg "msg.eval.nonstring.strict"
	     "Calling eval() with anything other than a primitive "
	     "string value is not allowed in strict mode.")

(js3-bfy-msg "msg.bad.destruct.op"
	     "Invalid destructuring assignment operator")

;; NativeCall
(js3-bfy-msg "msg.only.from.new"
	     "'%s' may only be invoked from a `new' expression.")

(js3-bfy-msg "msg.deprec.ctor"
	     "The '%s' constructor is deprecated.")

;; NativeFunction
(js3-bfy-msg "msg.no.function.ref.found"
	     "no source found to decompile function reference %s")

(js3-bfy-msg "msg.arg.isnt.array"
	     "second argument to Function.prototype.apply must be an array")

;; NativeGlobal
(js3-bfy-msg "msg.bad.esc.mask"
	     "invalid string escape mask")

;; NativeRegExp
(js3-bfy-msg "msg.bad.quant"
	     "Invalid quantifier %s")

(js3-bfy-msg "msg.overlarge.backref"
	     "Overly large back reference %s")

(js3-bfy-msg "msg.overlarge.min"
	     "Overly large minimum %s")

(js3-bfy-msg "msg.overlarge.max"
	     "Overly large maximum %s")

(js3-bfy-msg "msg.zero.quant"
	     "Zero quantifier %s")

(js3-bfy-msg "msg.max.lt.min"
	     "Maximum %s less than minimum")

(js3-bfy-msg "msg.unterm.quant"
	     "Unterminated quantifier %s")

(js3-bfy-msg "msg.unterm.paren"
	     "Unterminated parenthetical %s")

(js3-bfy-msg "msg.unterm.class"
	     "Unterminated character class %s")

(js3-bfy-msg "msg.bad.range"
	     "Invalid range in character class.")

(js3-bfy-msg "msg.trail.backslash"
	     "Trailing \\ in regular expression.")

(js3-bfy-msg "msg.re.unmatched.right.paren"
	     "unmatched ) in regular expression.")

(js3-bfy-msg "msg.no.regexp"
	     "Regular expressions are not available.")

(js3-bfy-msg "msg.bad.backref"
	     "back-reference exceeds number of capturing parentheses.")

(js3-bfy-msg "msg.bad.regexp.compile"
	     "Only one argument may be specified if the first "
	     "argument to RegExp.prototype.compile is a RegExp object.")

;; Parser
(js3-bfy-msg "msg.got.syntax.errors"
	     "Compilation produced %s syntax errors.")

(js3-bfy-msg "msg.var.redecl"
	     "TypeError: redeclaration of var %s.")

(js3-bfy-msg "msg.const.redecl"
	     "TypeError: redeclaration of const %s.")

(js3-bfy-msg "msg.let.redecl"
	     "TypeError: redeclaration of variable %s.")

(js3-bfy-msg "msg.parm.redecl"
	     "TypeError: redeclaration of formal parameter %s.")

(js3-bfy-msg "msg.fn.redecl"
	     "TypeError: redeclaration of function %s.")

(js3-bfy-msg "msg.let.decl.not.in.block"
	     "SyntaxError: let declaration not directly within block")

;; NodeTransformer
(js3-bfy-msg "msg.dup.label"
	     "duplicated label")

(js3-bfy-msg "msg.undef.label"
	     "undefined label")

(js3-bfy-msg "msg.bad.break"
	     "unlabelled break must be inside loop or switch")

(js3-bfy-msg "msg.continue.outside"
	     "continue must be inside loop")

(js3-bfy-msg "msg.continue.nonloop"
	     "continue can only use labels of iteration statements")

(js3-bfy-msg "msg.bad.throw.eol"
	     "Line terminator is not allowed between the throw "
	     "keyword and throw expression.")

(js3-bfy-msg "msg.no.paren.parms"
	     "missing ( before function parameters.")

(js3-bfy-msg "msg.no.parm"
	     "missing formal parameter")

(js3-bfy-msg "msg.no.paren.after.parms"
	     "missing ) after formal parameters")

(js3-bfy-msg "msg.no.brace.body"
	     "missing '{' before function body")

(js3-bfy-msg "msg.no.brace.after.body"
	     "missing } after function body")

(js3-bfy-msg "msg.no.paren.cond"
	     "missing ( before condition")

(js3-bfy-msg "msg.no.paren.after.cond"
	     "missing ) after condition")

(js3-bfy-msg "msg.no.semi.stmt"
	     "missing ; before statement")

(js3-bfy-msg "msg.missing.semi"
	     "missing ; after statement")

(js3-bfy-msg "msg.no.name.after.dot"
	     "missing name after . operator")

(js3-bfy-msg "msg.no.bracket.index"
	     "missing ] in index expression")

(js3-bfy-msg "msg.no.paren.switch"
	     "missing ( before switch expression")

(js3-bfy-msg "msg.no.paren.after.switch"
	     "missing ) after switch expression")

(js3-bfy-msg "msg.no.brace.switch"
	     "missing '{' before switch body")

(js3-bfy-msg "msg.bad.switch"
	     "invalid switch statement")

(js3-bfy-msg "msg.no.colon.case"
	     "missing : after case expression")

(js3-bfy-msg "msg.double.switch.default"
	     "double default label in the switch statement")

(js3-bfy-msg "msg.no.while.do"
	     "missing while after do-loop body")

(js3-bfy-msg "msg.no.paren.for"
	     "missing ( after for")

(js3-bfy-msg "msg.no.semi.for"
	     "missing ; after for-loop initializer")

(js3-bfy-msg "msg.no.semi.for.cond"
	     "missing ; after for-loop condition")

(js3-bfy-msg "msg.in.after.for.name"
	     "missing in after for")

(js3-bfy-msg "msg.no.paren.for.ctrl"
	     "missing ) after for-loop control")

(js3-bfy-msg "msg.no.paren.with"
	     "missing ( before with-statement object")

(js3-bfy-msg "msg.no.paren.after.with"
	     "missing ) after with-statement object")

(js3-bfy-msg "msg.no.paren.after.let"
	     "missing ( after let")

(js3-bfy-msg "msg.no.paren.let"
	     "missing ) after variable list")

(js3-bfy-msg "msg.no.curly.let"
	     "missing } after let statement")

(js3-bfy-msg "msg.bad.return"
	     "invalid return")

(js3-bfy-msg "msg.no.brace.block"
	     "missing } in compound statement")

(js3-bfy-msg "msg.bad.label"
	     "invalid label")

(js3-bfy-msg "msg.bad.var"
	     "missing variable name")

(js3-bfy-msg "msg.bad.var.init"
	     "invalid variable initialization")

(js3-bfy-msg "msg.no.colon.cond"
	     "missing : in conditional expression")

(js3-bfy-msg "msg.no.paren.arg"
	     "missing ) after argument list")

(js3-bfy-msg "msg.no.bracket.arg"
	     "missing ] after element list")

(js3-bfy-msg "msg.bad.prop"
	     "invalid property id")

(js3-bfy-msg "msg.no.colon.prop"
	     "missing : after property id")

(js3-bfy-msg "msg.no.brace.prop"
	     "missing } after property list")

(js3-bfy-msg "msg.no.paren"
	     "missing ) in parenthetical")

(js3-bfy-msg "msg.reserved.id"
	     "identifier is a reserved word")

(js3-bfy-msg "msg.no.paren.catch"
	     "missing ( before catch-block condition")

(js3-bfy-msg "msg.bad.catchcond"
	     "invalid catch block condition")

(js3-bfy-msg "msg.catch.unreachable"
	     "any catch clauses following an unqualified catch are unreachable")

(js3-bfy-msg "msg.no.brace.try"
	     "missing '{' before try block")

(js3-bfy-msg "msg.no.brace.catchblock"
	     "missing '{' before catch-block body")

(js3-bfy-msg "msg.try.no.catchfinally"
	     "'try' without 'catch' or 'finally'")

(js3-bfy-msg "msg.no.return.value"
	     "function %s does not always return a value")

(js3-bfy-msg "msg.anon.no.return.value"
	     "anonymous function does not always return a value")

(js3-bfy-msg "msg.return.inconsistent"
	     "return statement is inconsistent with previous usage")

(js3-bfy-msg "msg.generator.returns"
	     "TypeError: generator function '%s' returns a value")

(js3-bfy-msg "msg.anon.generator.returns"
	     "TypeError: anonymous generator function returns a value")

(js3-bfy-msg "msg.syntax"
	     "syntax error")

(js3-bfy-msg "msg.unexpected.eof"
	     "Unexpected end of file")

(js3-bfy-msg "msg.too.deep.parser.recursion"
	     "Too deep recursion while parsing")

(js3-bfy-msg "msg.no.side.effects"
	     "Code has no side effects")

(js3-bfy-msg "msg.extra.trailing.comma"
	     "Trailing comma is not legal in an ECMA-262 object initializer")

(js3-bfy-msg "msg.array.trailing.comma"
	     "Trailing comma yields different behavior across browsers")

(js3-bfy-msg "msg.equal.as.assign"
	     (concat "Test for equality (==) mistyped as assignment (=)?"
		     " (parenthesize to suppress warning)"))

(js3-bfy-msg "msg.var.hides.arg"
	     "Variable %s hides argument")

(js3-bfy-msg "msg.destruct.assign.no.init"
	     "Missing = in destructuring declaration")

;; ScriptRuntime
(js3-bfy-msg "msg.no.properties"
	     "%s has no properties.")

(js3-bfy-msg "msg.invalid.iterator"
	     "Invalid iterator value")

(js3-bfy-msg "msg.iterator.primitive"
	     "__iterator__ returned a primitive value")

(js3-bfy-msg "msg.assn.create.strict"
	     "Assignment to undeclared variable %s")

(js3-bfy-msg "msg.ref.undefined.prop"
	     "Reference to undefined property '%s'")

(js3-bfy-msg "msg.prop.not.found"
	     "Property %s not found.")

(js3-bfy-msg "msg.invalid.type"
	     "Invalid JavaScript value of type %s")

(js3-bfy-msg "msg.primitive.expected"
	     "Primitive type expected (had %s instead)")

(js3-bfy-msg "msg.namespace.expected"
	     "Namespace object expected to left of :: (found %s instead)")

(js3-bfy-msg "msg.null.to.object"
	     "Cannot convert null to an object.")

(js3-bfy-msg "msg.undef.to.object"
	     "Cannot convert undefined to an object.")

(js3-bfy-msg "msg.cyclic.value"
	     "Cyclic %s value not allowed.")

(js3-bfy-msg "msg.is.not.defined"
	     "'%s' is not defined.")

(js3-bfy-msg "msg.undef.prop.read"
	     "Cannot read property '%s' from %s")

(js3-bfy-msg "msg.undef.prop.write"
	     "Cannot set property '%s' of %s to '%s'")

(js3-bfy-msg "msg.undef.prop.delete"
	     "Cannot delete property '%s' of %s")

(js3-bfy-msg "msg.undef.method.call"
	     "Cannot call method '%s' of %s")

(js3-bfy-msg "msg.undef.with"
	     "Cannot apply 'with' to %s")

(js3-bfy-msg "msg.isnt.function"
	     "%s is not a function, it is %s.")

(js3-bfy-msg "msg.isnt.function.in"
	     "Cannot call property %s in object %s. "
	     "It is not a function, it is '%s'.")

(js3-bfy-msg "msg.function.not.found"
	     "Cannot find function %s.")

(js3-bfy-msg "msg.function.not.found.in"
	     "Cannot find function %s in object %s.")

(js3-bfy-msg "msg.no.ref.to.get"
	     "%s is not a reference to read reference value.")

(js3-bfy-msg "msg.no.ref.to.set"
	     "%s is not a reference to set reference value to %s.")

(js3-bfy-msg "msg.no.ref.from.function"
	     "Function %s can not be used as the left-hand "
	     "side of assignment or as an operand of ++ or -- operator.")

(js3-bfy-msg "msg.bad.default.value"
	     "Object's getDefaultValue() method returned an object.")

(js3-bfy-msg "msg.instanceof.not.object"
	     "Can't use instanceof on a non-object.")

(js3-bfy-msg "msg.instanceof.bad.prototype"
	     "'prototype' property of %s is not an object.")

(js3-bfy-msg "msg.bad.radix"
	     "illegal radix %s.")

;; ScriptableObject
(js3-bfy-msg "msg.default.value"
	     "Cannot find default value for object.")

(js3-bfy-msg "msg.zero.arg.ctor"
	     "Cannot load class '%s' which has no zero-parameter constructor.")

(js3-bfy-msg "msg.ctor.multiple.parms"
	     "Can't define constructor or class %s since more than "
	     "one constructor has multiple parameters.")

(js3-bfy-msg "msg.extend.scriptable"
	     "%s must extend ScriptableObject in order to define property %s.")

(js3-bfy-msg "msg.bad.getter.parms"
	     "In order to define a property, getter %s must have zero "
	     "parameters or a single ScriptableObject parameter.")

(js3-bfy-msg "msg.obj.getter.parms"
	     "Expected static or delegated getter %s to take "
	     "a ScriptableObject parameter.")

(js3-bfy-msg "msg.getter.static"
	     "Getter and setter must both be static or neither be static.")

(js3-bfy-msg "msg.setter.return"
	     "Setter must have void return type: %s")

(js3-bfy-msg "msg.setter2.parms"
	     "Two-parameter setter must take a ScriptableObject as "
	     "its first parameter.")

(js3-bfy-msg "msg.setter1.parms"
	     "Expected single parameter setter for %s")

(js3-bfy-msg "msg.setter2.expected"
	     "Expected static or delegated setter %s to take two parameters.")

(js3-bfy-msg "msg.setter.parms"
	     "Expected either one or two parameters for setter.")

(js3-bfy-msg "msg.setter.bad.type"
	     "Unsupported parameter type '%s' in setter '%s'.")

(js3-bfy-msg "msg.add.sealed"
	     "Cannot add a property to a sealed object: %s.")

(js3-bfy-msg "msg.remove.sealed"
	     "Cannot remove a property from a sealed object: %s.")

(js3-bfy-msg "msg.modify.sealed"
	     "Cannot modify a property of a sealed object: %s.")

(js3-bfy-msg "msg.modify.readonly"
	     "Cannot modify readonly property: %s.")

;; TokenStream
(js3-bfy-msg "msg.missing.exponent"
	     "missing exponent")

(js3-bfy-msg "msg.caught.nfe"
	     "number format error")

(js3-bfy-msg "msg.unterminated.string.lit"
	     "unterminated string literal")

(js3-bfy-msg "msg.unterminated.comment"
	     "unterminated comment")

(js3-bfy-msg "msg.unterminated.re.lit"
	     "unterminated regular expression literal")

(js3-bfy-msg "msg.invalid.re.flag"
	     "invalid flag after regular expression")

(js3-bfy-msg "msg.no.re.input.for"
	     "no input for %s")

(js3-bfy-msg "msg.illegal.character"
	     "illegal character")

(js3-bfy-msg "msg.invalid.escape"
	     "invalid Unicode escape sequence")

;; TokensStream warnings
(js3-bfy-msg "msg.bad.octal.literal"
	     "illegal octal literal digit %s; "
	     "interpreting it as a decimal digit")

(js3-bfy-msg "msg.reserved.keyword"
	     "illegal usage of future reserved keyword %s; "
	     "interpreting it as ordinary identifier")

(js3-bfy-msg "msg.script.is.not.constructor"
	     "Script objects are not constructors.")

;; Arrays
(js3-bfy-msg "msg.arraylength.bad"
	     "Inappropriate array length.")

;; Arrays
(js3-bfy-msg "msg.arraylength.too.big"
	     "Array length %s exceeds supported capacity limit.")

;; URI
(js3-bfy-msg "msg.bad.uri"
	     "Malformed URI sequence.")

;; Number
(js3-bfy-msg "msg.bad.precision"
	     "Precision %s out of range.")

;; NativeGenerator
(js3-bfy-msg "msg.send.newborn"
	     "Attempt to send value to newborn generator")

(js3-bfy-msg "msg.already.exec.gen"
	     "Already executing generator")

(js3-bfy-msg "msg.StopIteration.invalid"
	     "StopIteration may not be changed to an arbitrary object.")

;; Interpreter
(js3-bfy-msg "msg.yield.closing"
	     "Yield from closing generator")

(provide 'js3-bfy-messages)

;; js3-bfy-messages.el ends here
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
;;; js3-bfy-parse.el --- JavaScript parser

;; Commentary:

;; This is based on Rhino's parser and tries to follow its code
;; structure as closely as practical, so that changes to the Rhino
;; parser can easily be propagated into this code.  However, Rhino
;; does not currently generate a usable AST representation, at least
;; from an IDE perspective, so we build our own more suitable AST.

;; The AST node structures are defined in `js3-bfy-ast.el'.
;; Every parser function that creates and returns an AST node has
;; the following responsibilities:

;;   1) set the node start to the absolute buffer start position
;;   2) set the node length to include any closing chars (RC, SEMI)
;;   3) fix up any child-node starts to be relative to this node
;;   4) set any field positions (e.g. keywords) relative to this node
;;   5) report any child nodes with `js3-bfy-node-add-children'
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

;; The editing mode that uses this parser, `js3-bfy', directs the
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


(defconst js3-bfy-version "1.8.0"
  "Version of JavaScript supported, plus minor js3 version.")

(defsubst js3-bfy-node-end (n)
  "Computes the absolute end of node N.
Use with caution!  Assumes `js3-bfy-node-pos' is -absolute-, which
is only true until the node is added to its parent; i.e., while parsing."
  (+ (js3-bfy-node-pos n)
     (js3-bfy-node-len n)))

(defsubst js3-bfy-record-comment ()
  (push (make-js3-bfy-comment-node :len (- js3-bfy-token-end js3-bfy-token-beg)
				   :format js3-bfy-ts-comment-type)
        js3-bfy-scanned-comments))

;; This function is called depressingly often, so it should be fast.
;; Most of the time it's looking at the same token it peeked before.
(defsubst js3-bfy-peek-token ()
  "Returns the next token without consuming it.
If previous token was consumed, calls scanner to get new token.
If previous token was -not- consumed, returns it (idempotent).

This function will not return a newline (js3-bfy-EOL) - instead, it
gobbles newlines until it finds a non-newline token, and flags
that token as appearing just after a newline.

This function will also not return a js3-bfy-COMMENT.  Instead, it
records comments found in `js3-bfy-scanned-comments'.  If the token
returned by this function immediately follows a jsdoc comment,
the token is flagged as such.

Note that this function always returned the un-flagged token!
The flags, if any, are saved in `js3-bfy-current-flagged-token'."
  (if (/= js3-bfy-current-flagged-token js3-bfy-EOF) ; last token not consumed
      js3-bfy-current-token  ; most common case - return already-peeked token
    (let ((tt (js3-bfy-get-token))          ; call scanner
          saw-eol)
      ;; process comments and whitespace
      (while (or (= tt js3-bfy-EOL)
                 (= tt js3-bfy-COMMENT))
        (if (= tt js3-bfy-EOL)
            (setq saw-eol t)
          (setq saw-eol nil)
          (if js3-bfy-record-comments
              (js3-bfy-record-comment)))
        (setq tt (js3-bfy-get-token)))  ; call scanner
      (setq js3-bfy-current-token tt
            js3-bfy-current-flagged-token (if saw-eol
					      (logior tt js3-bfy-ti-after-eol)
					    tt))
      tt)))  ; return unflagged token

(defsubst js3-bfy-peek-flagged-token ()
  "Returns the current token along with any flags set for it."
  (js3-bfy-peek-token)
  js3-bfy-current-flagged-token)

(defsubst js3-bfy-consume-token ()
  (setq js3-bfy-current-flagged-token js3-bfy-EOF))

(defsubst js3-bfy-next-token ()
  (prog1
      (js3-bfy-peek-token)
    (js3-bfy-consume-token)))

(defsubst js3-bfy-next-flagged-token ()
  (js3-bfy-peek-token)
  (prog1 js3-bfy-current-flagged-token
    (js3-bfy-consume-token)))

(defsubst js3-bfy-match-token (match)
  "Consume and return t if next token matches MATCH, a bytecode.
Returns nil and consumes nothing if MATCH is not the next token."
  (if (/= (js3-bfy-peek-token) match)
      nil
    (js3-bfy-consume-token)
    t))

(defsubst js3-bfy-valid-prop-name-token (tt)
  (or (= tt js3-bfy-NAME)
      (and js3-bfy-allow-keywords-as-property-names
           (plusp tt)
           (aref js3-bfy-kwd-tokens tt))))

(defsubst js3-bfy-match-prop-name ()
  "Consume token and return t if next token is a valid property name.
It's valid if it's a js3-bfy-NAME, or `js3-bfy-allow-keywords-as-property-names'
is non-nil and it's a keyword token."
  (if (js3-bfy-valid-prop-name-token (js3-bfy-peek-token))
      (progn
        (js3-bfy-consume-token)
        t)
    nil))

(defsubst js3-bfy-must-match-prop-name (msg-id &optional pos len)
  (if (js3-bfy-match-prop-name)
      t
    (js3-bfy-report-error msg-id nil pos len)
    nil))

(defsubst js3-bfy-peek-token-or-eol ()
  "Return js3-bfy-EOL if the current token immediately follows a newline.
Else returns the current token.  Used in situations where we don't
consider certain token types valid if they are preceded by a newline.
One example is the postfix ++ or -- operator, which has to be on the
same line as its operand."
  (let ((tt (js3-bfy-peek-token)))
    ;; Check for last peeked token flags
    (if (js3-bfy-flag-set-p js3-bfy-current-flagged-token js3-bfy-ti-after-eol)
        js3-bfy-EOL
      tt)))

(defsubst js3-bfy-set-check-for-label ()
  (assert (= (logand js3-bfy-current-flagged-token js3-bfy-clear-ti-mask) js3-bfy-NAME))
  (js3-bfy-set-flag js3-bfy-current-flagged-token js3-bfy-ti-check-label))

(defsubst js3-bfy-must-match (token msg-id &optional pos len)
  "Match next token to token code TOKEN, or record a syntax error.
MSG-ID is the error message to report if the match fails.
Returns t on match, nil if no match."
  (if (js3-bfy-match-token token)
      t
    (js3-bfy-report-error msg-id nil pos len)
    nil))

(defsubst js3-bfy-inside-function ()
  (plusp js3-bfy-nesting-of-function))

(defsubst js3-bfy-set-requires-activation ()
  (if (js3-bfy-function-node-p js3-bfy-current-script-or-fn)
      (setf (js3-bfy-function-node-needs-activation js3-bfy-current-script-or-fn) t)))

(defsubst js3-bfy-check-activation-name (name token)
  (when (js3-bfy-inside-function)
    ;; skip language-version 1.2 check from Rhino
    (if (or (string= "arguments" name)
            (and js3-bfy-compiler-activation-names  ; only used in codegen
                 (gethash name js3-bfy-compiler-activation-names)))
        (js3-bfy-set-requires-activation))))

(defsubst js3-bfy-set-is-generator ()
  (if (js3-bfy-function-node-p js3-bfy-current-script-or-fn)
      (setf (js3-bfy-function-node-is-generator js3-bfy-current-script-or-fn) t)))

(defsubst js3-bfy-push-scope (scope)
  "Push SCOPE, a `js3-bfy-scope', onto the lexical scope chain."
  (assert (js3-bfy-scope-p scope))
  (assert (null (js3-bfy-scope-parent-scope scope)))
  (assert (neq js3-bfy-current-scope scope))
  (setf (js3-bfy-scope-parent-scope scope) js3-bfy-current-scope
        js3-bfy-current-scope scope))

(defsubst js3-bfy-pop-scope ()
  (setq js3-bfy-current-scope
        (js3-bfy-scope-parent-scope js3-bfy-current-scope)))

(defsubst js3-bfy-enter-loop (loop-node)
  (push loop-node js3-bfy-loop-set)
  (push loop-node js3-bfy-loop-and-switch-set)
  (js3-bfy-push-scope loop-node)
  ;; Tell the current labeled statement (if any) its statement,
  ;; and set the jump target of the first label to the loop.
  ;; These are used in `js3-bfy-parse-continue' to verify that the
  ;; continue target is an actual labeled loop.  (And for codegen.)
  (when js3-bfy-labeled-stmt
    (setf (js3-bfy-labeled-stmt-node-stmt js3-bfy-labeled-stmt) loop-node
          (js3-bfy-label-node-loop (car (js3-bfy-labeled-stmt-node-labels
					 js3-bfy-labeled-stmt))) loop-node)))

(defsubst js3-bfy-exit-loop ()
  (pop js3-bfy-loop-set)
  (pop js3-bfy-loop-and-switch-set)
  (js3-bfy-pop-scope))

(defsubst js3-bfy-enter-switch (switch-node)
  (push switch-node js3-bfy-loop-and-switch-set))

(defsubst js3-bfy-exit-switch ()
  (pop js3-bfy-loop-and-switch-set))

(defun js3-bfy-parse (&optional buf cb)
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
      (setq js3-bfy-scanned-comments nil
            js3-bfy-parsed-errors nil
            js3-bfy-parsed-warnings nil
            js3-bfy-label-set nil)
      (js3-bfy-init-scanner)
      (setq ast (js3-bfy-with-unmodifying-text-property-changes
                 (js3-bfy-do-parse)))
      (unless js3-bfy-ts-hit-eof
        (js3-bfy-report-error "msg.got.syntax.errors" (length js3-bfy-parsed-errors)))
      (setf (js3-bfy-ast-root-errors ast) js3-bfy-parsed-errors
            (js3-bfy-ast-root-warnings ast) js3-bfy-parsed-warnings)
      (run-hooks 'js3-bfy-parse-finished-hook)
      ast)))

;; Corresponds to Rhino's Parser.parse() method.
(defun js3-bfy-do-parse ()
  "Parse current buffer starting from current point.
Scanner should be initialized."
  (let ((pos js3-bfy-ts-cursor)
        (end js3-bfy-ts-cursor)  ; in case file is empty
        root n tt)
    ;; initialize buffer-local parsing vars
    (setf root (make-js3-bfy-ast-root :buffer (buffer-name) :pos pos)
          js3-bfy-current-script-or-fn root
          js3-bfy-current-scope root
          js3-bfy-current-flagged-token js3-bfy-EOF
          js3-bfy-nesting-of-function 0
          js3-bfy-labeled-stmt nil)
    (while (/= (setq tt (js3-bfy-peek-token)) js3-bfy-EOF)
      (if (= tt js3-bfy-FUNCTION)
          (progn
            (js3-bfy-consume-token)
            (setq n (js3-bfy-parse-function (if js3-bfy-called-by-compile-function
						'FUNCTION_EXPRESSION
					      'FUNCTION_STATEMENT))))
        ;; not a function - parse a statement
        (setq n (js3-bfy-parse-statement)))
      ;; add function or statement to script
      (setq end (js3-bfy-node-end n))
      (js3-bfy-block-node-push root n))
    ;; add comments to root in lexical order
    (when js3-bfy-scanned-comments
      ;; if we find a comment beyond end of normal kids, use its end
      (setq end (max end (js3-bfy-node-end (first js3-bfy-scanned-comments))))
      (dolist (comment js3-bfy-scanned-comments)
        (push comment (js3-bfy-ast-root-comments root))
        (js3-bfy-node-add-children root comment)))
    (setf (js3-bfy-node-len root) (- end pos))
    root))

(defun js3-bfy-function-parser ()
  (js3-bfy-consume-token)
  (js3-bfy-parse-function 'FUNCTION_EXPRESSION_STATEMENT))

(defun js3-bfy-parse-function-closure-body (fn-node)
  "Parse a JavaScript 1.8 function closure body."
  (let ((js3-bfy-nesting-of-function (1+ js3-bfy-nesting-of-function)))
    (if js3-bfy-ts-hit-eof
        (js3-bfy-report-error "msg.no.brace.body" nil
			      (js3-bfy-node-pos fn-node)
			      (- js3-bfy-ts-cursor (js3-bfy-node-pos fn-node)))
      (js3-bfy-node-add-children fn-node
				 (setf (js3-bfy-function-node-body fn-node)
				       (js3-bfy-parse-expr))))))

(defun js3-bfy-parse-function-body (fn-node)
  (js3-bfy-must-match js3-bfy-LC "msg.no.brace.body"
		      (js3-bfy-node-pos fn-node)
		      (- js3-bfy-ts-cursor (js3-bfy-node-pos fn-node)))
  (let ((pos js3-bfy-token-beg)         ; LC position
        (pn (make-js3-bfy-block-node))  ; starts at LC position
        tt
        end)
    (incf js3-bfy-nesting-of-function)
    (unwind-protect
        (while (not (or (= (setq tt (js3-bfy-peek-token)) js3-bfy-ERROR)
                        (= tt js3-bfy-EOF)
                        (= tt js3-bfy-RC)))
          (js3-bfy-block-node-push pn (if (/= tt js3-bfy-FUNCTION)
					  (js3-bfy-parse-statement)
					(js3-bfy-consume-token)
					(js3-bfy-parse-function 'FUNCTION_STATEMENT))))
      (decf js3-bfy-nesting-of-function))
    (setq end js3-bfy-token-end)  ; assume no curly and leave at current token
    (if (js3-bfy-must-match js3-bfy-RC "msg.no.brace.after.body" pos)
        (setq end js3-bfy-token-end))
    (setf (js3-bfy-node-pos pn) pos
          (js3-bfy-node-len pn) (- end pos))
    (setf (js3-bfy-function-node-body fn-node) pn)
    (js3-bfy-node-add-children fn-node pn)
    pn))

(defun js3-bfy-parse-function-params (fn-node pos)
  (if (js3-bfy-match-token js3-bfy-RP)
      (setf (js3-bfy-function-node-rp fn-node) (- js3-bfy-token-beg pos))
    (let (params len param)
      (loop for tt = (js3-bfy-peek-token)
            do
            (cond
             ;; destructuring param
             ((or (= tt js3-bfy-LB) (= tt js3-bfy-LC))
              (push (js3-bfy-parse-primary-expr) params))
             ;; simple name
             (t
              (js3-bfy-must-match js3-bfy-NAME "msg.no.parm")
              (setq param (js3-bfy-create-name-node))
              (js3-bfy-define-symbol js3-bfy-LP js3-bfy-ts-string param)
              (push param params)))
            while
            (js3-bfy-match-token js3-bfy-COMMA))
      (if (js3-bfy-must-match js3-bfy-RP "msg.no.paren.after.parms")
          (setf (js3-bfy-function-node-rp fn-node) (- js3-bfy-token-beg pos)))
      (dolist (p params)
        (js3-bfy-node-add-children fn-node p)
        (push p (js3-bfy-function-node-params fn-node))))))

(defsubst js3-bfy-check-inconsistent-return-warning (fn-node name)
  "Possibly show inconsistent-return warning.
Last token scanned is the close-curly for the function body."
  (when (and js3-bfy-show-strict-warnings
             js3-bfy-strict-inconsistent-return-warning
             (not (js3-bfy-has-consistent-return-usage
                   (js3-bfy-function-node-body fn-node))))
    ;; Have it extend from close-curly to bol or beginning of block.
    (let ((pos (save-excursion
                 (goto-char js3-bfy-token-end)
                 (max (js3-bfy-node-abs-pos (js3-bfy-function-node-body fn-node))
                      (point-at-bol))))
          (end js3-bfy-token-end))
      (if (plusp (js3-bfy-name-node-length name))
          (js3-bfy-add-strict-warning "msg.no.return.value"
				      (js3-bfy-name-node-name name) pos end)
        (js3-bfy-add-strict-warning "msg.anon.no.return.value" nil pos end)))))

(defun js3-bfy-parse-function (function-type)
  "Function parser.  FUNCTION-TYPE is a symbol."
  (let ((pos js3-bfy-token-beg)  ; start of 'function' keyword
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
     ((js3-bfy-match-token js3-bfy-NAME)
      (setq name (js3-bfy-create-name-node t)
            name-beg js3-bfy-token-beg
            name-end js3-bfy-token-end)
      (unless (js3-bfy-match-token js3-bfy-LP)
        (when js3-bfy-allow-member-expr-as-function-name
          ;; function foo.bar(...)
          (setq member-expr-node name
                name nil
                member-expr-node (js3-bfy-parse-member-expr-tail
                                  nil member-expr-node)))
        (js3-bfy-must-match js3-bfy-LP "msg.no.paren.parms")))
     ((js3-bfy-match-token js3-bfy-LP)
      nil)  ; anonymous function:  leave name as null
     (t
      ;; function random-member-expr(...)
      (when js3-bfy-allow-member-expr-as-function-name
        ;; Note that memberExpr can not start with '(' like
        ;; in function (1+2).toString(), because 'function (' already
        ;; processed as anonymous function
        (setq member-expr-node (js3-bfy-parse-member-expr)))
      (js3-bfy-must-match js3-bfy-LP "msg.no.paren.parms")))
    (if (= js3-bfy-current-token js3-bfy-LP)  ; eventually matched LP?
        (setq lp js3-bfy-token-beg))
    (if member-expr-node
	(setq synthetic-type 'FUNCTION_EXPRESSION))
    (if (and (neq synthetic-type 'FUNCTION_EXPRESSION)
             (plusp (js3-bfy-name-node-length name)))
        ;; Function statements define a symbol in the enclosing scope
        (js3-bfy-define-symbol js3-bfy-FUNCTION (js3-bfy-name-node-name name) fn-node))
    (setf fn-node (make-js3-bfy-function-node :pos pos
					      :name name
					      :form function-type
					      :lp (if lp (- lp pos))))
    (if (or (js3-bfy-inside-function) (plusp js3-bfy-nesting-of-with))
        ;; 1. Nested functions are not affected by the dynamic scope flag
        ;;    as dynamic scope is already a parent of their scope.
        ;; 2. Functions defined under the with statement also immune to
        ;;    this setup, in which case dynamic scope is ignored in favor
        ;;    of the with object.
        (setf (js3-bfy-function-node-ignore-dynamic fn-node) t))
    ;; dynamically bind all the per-function variables
    (let ((js3-bfy-current-script-or-fn fn-node)
          (js3-bfy-current-scope fn-node)
          (js3-bfy-nesting-of-with 0)
          (js3-bfy-end-flags 0)
          js3-bfy-label-set
          js3-bfy-loop-set
          js3-bfy-loop-and-switch-set)
      (js3-bfy-parse-function-params fn-node pos)
      (if (and (>= js3-bfy-language-version 180)
               (/= (js3-bfy-peek-token) js3-bfy-LC))
          (js3-bfy-parse-function-closure-body fn-node)
        (js3-bfy-parse-function-body fn-node))
      (if name
          (js3-bfy-node-add-children fn-node name))
      (js3-bfy-check-inconsistent-return-warning fn-node name)
      ;; Function expressions define a name only in the body of the
      ;; function, and only if not hidden by a parameter name
      (if (and name
               (eq synthetic-type 'FUNCTION_EXPRESSION)
               (null (js3-bfy-scope-get-symbol js3-bfy-current-scope
					       (js3-bfy-name-node-name name))))
          (js3-bfy-define-symbol js3-bfy-FUNCTION
				 (js3-bfy-name-node-name name)
				 fn-node)))
    (setf (js3-bfy-node-len fn-node) (- js3-bfy-ts-cursor pos)
          (js3-bfy-function-node-member-expr fn-node) member-expr-node) ; may be nil
    ;; Rhino doesn't do this, but we need it for finding undeclared vars.
    ;; We wait until after parsing the function to set its parent scope,
    ;; since `js3-bfy-define-symbol' needs the defining-scope check to stop
    ;; at the function boundary when checking for redeclarations.
    (setf (js3-bfy-scope-parent-scope fn-node) js3-bfy-current-scope)
    fn-node))

(defun js3-bfy-parse-statements (&optional parent)
  "Parse a statement list.  Last token consumed must be js3-bfy-LC.

PARENT can be a `js3-bfy-block-node', in which case the statements are
appended to PARENT.  Otherwise a new `js3-bfy-block-node' is created
and returned.

This function does not match the closing js3-bfy-RC: the caller
matches the RC so it can provide a suitable error message if not
matched.  This means it's up to the caller to set the length of
the node to include the closing RC.  The node start pos is set to
the absolute buffer start position, and the caller should fix it
up to be relative to the parent node.  All children of this block
node are given relative start positions and correct lengths."
  (let ((pn (or parent (make-js3-bfy-block-node)))
        tt)
    (setf (js3-bfy-node-pos pn) js3-bfy-token-beg)
    (while (and (> (setq tt (js3-bfy-peek-token)) js3-bfy-EOF)
                (/= tt js3-bfy-RC))
      (js3-bfy-block-node-push pn (js3-bfy-parse-statement)))
    pn))

(defun js3-bfy-parse-statement ()
  (let (tt pn beg end)
    ;; coarse-grained user-interrupt check - needs work
    (and js3-bfy-parse-interruptable-p
         (input-pending-p)
         (throw 'interrupted t))
    (setq pn (js3-bfy-statement-helper))
    ;; no-side-effects warning check
    (unless (js3-bfy-node-has-side-effects pn)
      (setq end (js3-bfy-node-end pn))
      (save-excursion
        (goto-char end)
        (setq beg (max (js3-bfy-node-pos pn) (point-at-bol))))
      (js3-bfy-add-strict-warning "msg.no.side.effects" nil beg end))
    pn))

;; These correspond to the switch cases in Parser.statementHelper
(defconst js3-bfy-parsers
  (let ((parsers (make-vector js3-bfy-num-tokens
                              #'js3-bfy-parse-expr-stmt)))
    (aset parsers js3-bfy-BREAK     #'js3-bfy-parse-break)
    (aset parsers js3-bfy-CONST     #'js3-bfy-parse-const-var)
    (aset parsers js3-bfy-CONTINUE  #'js3-bfy-parse-continue)
    (aset parsers js3-bfy-DEBUGGER  #'js3-bfy-parse-debugger)
    (aset parsers js3-bfy-DO        #'js3-bfy-parse-do)
    (aset parsers js3-bfy-FOR       #'js3-bfy-parse-for)
    (aset parsers js3-bfy-FUNCTION  #'js3-bfy-function-parser)
    (aset parsers js3-bfy-IF        #'js3-bfy-parse-if)
    (aset parsers js3-bfy-LC        #'js3-bfy-parse-block)
    (aset parsers js3-bfy-LET       #'js3-bfy-parse-let-stmt)
    (aset parsers js3-bfy-NAME      #'js3-bfy-parse-name-or-label)
    (aset parsers js3-bfy-RETURN    #'js3-bfy-parse-ret-yield)
    (aset parsers js3-bfy-SEMI      #'js3-bfy-parse-semi)
    (aset parsers js3-bfy-SWITCH    #'js3-bfy-parse-switch)
    (aset parsers js3-bfy-THROW     #'js3-bfy-parse-throw)
    (aset parsers js3-bfy-TRY       #'js3-bfy-parse-try)
    (aset parsers js3-bfy-VAR       #'js3-bfy-parse-const-var)
    (aset parsers js3-bfy-WHILE     #'js3-bfy-parse-while)
    (aset parsers js3-bfy-WITH      #'js3-bfy-parse-with)
    (aset parsers js3-bfy-YIELD     #'js3-bfy-parse-ret-yield)
    parsers)
  "A vector mapping token types to parser functions.")

(defsubst js3-bfy-parse-warn-missing-semi (beg end)
  (and js3-bfy-show-strict-warnings
       js3-bfy-strict-missing-semi-warning
       (js3-bfy-add-strict-warning
        "msg.missing.semi" nil
        ;; back up to beginning of statement or line
        (max beg (save-excursion
                   (goto-char end)
                   (point-at-bol)))
        end)))

(defconst js3-bfy-no-semi-insertion
  (list js3-bfy-IF
        js3-bfy-SWITCH
        js3-bfy-WHILE
        js3-bfy-DO
        js3-bfy-FOR
        js3-bfy-TRY
        js3-bfy-WITH
        js3-bfy-LC
        js3-bfy-ERROR
        js3-bfy-SEMI
        js3-bfy-FUNCTION)
  "List of tokens that don't do automatic semicolon insertion.")

(defconst js3-bfy-autoinsert-semi-and-warn
  (list js3-bfy-ERROR js3-bfy-EOF js3-bfy-RC))

(defun js3-bfy-statement-helper ()
  (let* ((tt (js3-bfy-peek-token))
         (first-tt tt)
         (beg js3-bfy-token-beg)
         (parser (if (= tt js3-bfy-ERROR)
                     #'js3-bfy-parse-semi
                   (aref js3-bfy-parsers tt)))
         pn
         tt-flagged)
    ;; If the statement is set, then it's been told its label by now.
    (and js3-bfy-labeled-stmt
         (js3-bfy-labeled-stmt-node-stmt js3-bfy-labeled-stmt)
         (setq js3-bfy-labeled-stmt nil))
    (setq pn (funcall parser))
    ;; Don't do auto semi insertion for certain statement types.
    (unless (or (memq first-tt js3-bfy-no-semi-insertion)
                (js3-bfy-labeled-stmt-node-p pn))
      (js3-bfy-auto-insert-semicolon pn))
    pn))

(defun js3-bfy-auto-insert-semicolon (pn)
  (let* ((tt-flagged (js3-bfy-peek-flagged-token))
         (tt (logand tt-flagged js3-bfy-clear-ti-mask))
         (pos (js3-bfy-node-pos pn)))
    (cond
     ((= tt js3-bfy-SEMI)
      ;; Consume ';' as a part of expression
      (js3-bfy-consume-token)
      ;; extend the node bounds to include the semicolon.
      (setf (js3-bfy-node-len pn) (- js3-bfy-token-end pos)))
     ((memq tt js3-bfy-autoinsert-semi-and-warn)
      ;; Autoinsert ;
      (js3-bfy-parse-warn-missing-semi pos (js3-bfy-node-end pn)))
     (t
      (if (js3-bfy-flag-not-set-p tt-flagged js3-bfy-ti-after-eol)
          ;; Report error if no EOL or autoinsert ';' otherwise
          (js3-bfy-report-error "msg.no.semi.stmt")
        (js3-bfy-parse-warn-missing-semi pos (js3-bfy-node-end pn)))))))

(defun js3-bfy-parse-condition ()
  "Parse a parenthesized boolean expression, e.g. in an if- or while-stmt.
The parens are discarded and the expression node is returned.
The `pos' field of the return value is set to an absolute position
that must be fixed up by the caller.
Return value is a list (EXPR LP RP), with absolute paren positions."
  (let (pn lp rp)
    (if (js3-bfy-must-match js3-bfy-LP "msg.no.paren.cond")
        (setq lp js3-bfy-token-beg))
    (setq pn (js3-bfy-parse-expr))
    (if (js3-bfy-must-match js3-bfy-RP "msg.no.paren.after.cond")
        (setq rp js3-bfy-token-beg))
    ;; Report strict warning on code like "if (a = 7) ..."
    (if (and js3-bfy-strict-cond-assign-warning
             (js3-bfy-assign-node-p pn))
        (js3-bfy-add-strict-warning "msg.equal.as.assign" nil
				    (js3-bfy-node-pos pn)
				    (+ (js3-bfy-node-pos pn)
				       (js3-bfy-node-len pn))))
    (list pn lp rp)))

(defun js3-bfy-parse-if ()
  "Parser for if-statement.  Last matched token must be js3-bfy-IF."
  (let ((pos js3-bfy-token-beg)
        cond
        if-true
        if-false
        else-pos
        end
        pn)
    (js3-bfy-consume-token)
    (setq cond (js3-bfy-parse-condition)
          if-true (js3-bfy-parse-statement)
          if-false (if (js3-bfy-match-token js3-bfy-ELSE)
                       (progn
                         (setq else-pos (- js3-bfy-token-beg pos))
                         (js3-bfy-parse-statement)))
          end (js3-bfy-node-end (or if-false if-true))
          pn (make-js3-bfy-if-node :pos pos
				   :len (- end pos)
				   :condition (car cond)
				   :then-part if-true
				   :else-part if-false
				   :else-pos else-pos
				   :lp (js3-bfy-relpos (second cond) pos)
				   :rp (js3-bfy-relpos (third cond) pos)))
    (js3-bfy-node-add-children pn (car cond) if-true if-false)
    pn))

(defun js3-bfy-parse-switch ()
  "Parser for if-statement.  Last matched token must be js3-bfy-SWITCH."
  (let ((pos js3-bfy-token-beg)
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
    (js3-bfy-consume-token)
    (if (js3-bfy-must-match js3-bfy-LP "msg.no.paren.switch")
        (setq lp js3-bfy-token-beg))
    (setq discriminant (js3-bfy-parse-expr)
          pn (make-js3-bfy-switch-node :discriminant discriminant
				       :pos pos
				       :lp (js3-bfy-relpos lp pos)))
    (js3-bfy-node-add-children pn discriminant)
    (js3-bfy-enter-switch pn)
    (unwind-protect
        (progn
          (if (js3-bfy-must-match js3-bfy-RP "msg.no.paren.after.switch")
              (setf (js3-bfy-switch-node-rp pn) (- js3-bfy-token-beg pos)))
          (js3-bfy-must-match js3-bfy-LC "msg.no.brace.switch")
          (catch 'break
            (while t
              (setq tt (js3-bfy-next-token)
                    case-pos js3-bfy-token-beg)
              (cond
               ((= tt js3-bfy-RC)
                (setf (js3-bfy-node-len pn) (- js3-bfy-token-end pos))
                (throw 'break nil))  ; done
               ((= tt js3-bfy-CASE)
                (setq case-expr (js3-bfy-parse-expr))
                (js3-bfy-must-match js3-bfy-COLON "msg.no.colon.case"))
               ((= tt js3-bfy-DEFAULT)
                (if has-default
                    (js3-bfy-report-error "msg.double.switch.default"))
                (setq has-default t
                      case-expr nil)
                (js3-bfy-must-match js3-bfy-COLON "msg.no.colon.case"))
               (t
                (js3-bfy-report-error "msg.bad.switch")
                (throw 'break nil)))
              (setq case-node (make-js3-bfy-case-node :pos case-pos
						      :len (- js3-bfy-token-end case-pos)
						      :expr case-expr))
              (js3-bfy-node-add-children case-node case-expr)
              (while (and (/= (setq tt (js3-bfy-peek-token)) js3-bfy-RC)
                          (/= tt js3-bfy-CASE)
                          (/= tt js3-bfy-DEFAULT)
                          (/= tt js3-bfy-EOF))
                (setf stmt (js3-bfy-parse-statement)
                      (js3-bfy-node-len case-node) (- (js3-bfy-node-end stmt) case-pos))
                (js3-bfy-block-node-push case-node stmt))
              (push case-node cases)))
          ;; add cases last, as pushing reverses the order to be correct
          (dolist (kid cases)
            (js3-bfy-node-add-children pn kid)
            (push kid (js3-bfy-switch-node-cases pn)))
          pn)  ; return value
      (js3-bfy-exit-switch))))

(defun js3-bfy-parse-while ()
  "Parser for while-statement.  Last matched token must be js3-bfy-WHILE."
  (let ((pos js3-bfy-token-beg)
        (pn (make-js3-bfy-while-node))
        cond
        body)
    (js3-bfy-consume-token)
    (js3-bfy-enter-loop pn)
    (unwind-protect
        (progn
          (setf cond (js3-bfy-parse-condition)
                (js3-bfy-while-node-condition pn) (car cond)
                body (js3-bfy-parse-statement)
                (js3-bfy-while-node-body pn) body
                (js3-bfy-node-len pn) (- (js3-bfy-node-end body) pos)
                (js3-bfy-while-node-lp pn) (js3-bfy-relpos (second cond) pos)
                (js3-bfy-while-node-rp pn) (js3-bfy-relpos (third cond) pos))
          (js3-bfy-node-add-children pn body (car cond)))
      (js3-bfy-exit-loop))
    pn))

(defun js3-bfy-parse-do ()
  "Parser for do-statement.  Last matched token must be js3-bfy-DO."
  (let ((pos js3-bfy-token-beg)
        (pn (make-js3-bfy-do-node))
        cond
        body
        end)
    (js3-bfy-consume-token)
    (js3-bfy-enter-loop pn)
    (unwind-protect
        (progn
          (setq body (js3-bfy-parse-statement))
          (js3-bfy-must-match js3-bfy-WHILE "msg.no.while.do")
          (setf (js3-bfy-do-node-while-pos pn) (- js3-bfy-token-beg pos)
                cond (js3-bfy-parse-condition)
                (js3-bfy-do-node-condition pn) (car cond)
                (js3-bfy-do-node-body pn) body
                end js3-bfy-ts-cursor
                (js3-bfy-do-node-lp pn) (js3-bfy-relpos (second cond) pos)
                (js3-bfy-do-node-rp pn) (js3-bfy-relpos (third cond) pos))
          (js3-bfy-node-add-children pn (car cond) body))
      (js3-bfy-exit-loop))
    ;; Always auto-insert semicolon to follow SpiderMonkey:
    ;; It is required by ECMAScript but is ignored by the rest of
    ;; world; see bug 238945
    (if (js3-bfy-match-token js3-bfy-SEMI)
        (setq end js3-bfy-ts-cursor))
    (setf (js3-bfy-node-len pn) (- end pos))
    pn))

(defun js3-bfy-parse-for ()
  "Parser for for-statement.  Last matched token must be js3-bfy-FOR.
Parses for, for-in, and for each-in statements."
  (let ((for-pos js3-bfy-token-beg)
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
    (js3-bfy-consume-token)
    ;; See if this is a for each () instead of just a for ()
    (when (js3-bfy-match-token js3-bfy-NAME)
      (if (string= "each" js3-bfy-ts-string)
	  (setq is-for-each t
		each-pos (- js3-bfy-token-beg for-pos)) ; relative
        (js3-bfy-report-error "msg.no.paren.for")))
    (if (js3-bfy-must-match js3-bfy-LP "msg.no.paren.for")
        (setq lp (- js3-bfy-token-beg for-pos)))
    (setq tt (js3-bfy-peek-token))
    ;; parse init clause
    (let ((js3-bfy-in-for-init t))  ; set as dynamic variable
      (cond
       ((= tt js3-bfy-SEMI)
        (setq init (make-js3-bfy-empty-expr-node)))
       ((or (= tt js3-bfy-VAR) (= tt js3-bfy-LET))
        (js3-bfy-consume-token)
        (setq init (js3-bfy-parse-variables tt js3-bfy-token-beg)))
       (t
        (setq init (js3-bfy-parse-expr)))))
    (if (js3-bfy-match-token js3-bfy-IN)
        (setq is-for-in t
              in-pos (- js3-bfy-token-beg for-pos)
              cond (js3-bfy-parse-expr))  ; object over which we're iterating
      ;; else ordinary for loop - parse cond and incr
      (js3-bfy-must-match js3-bfy-SEMI "msg.no.semi.for")
      (setq cond (if (= (js3-bfy-peek-token) js3-bfy-SEMI)
                     (make-js3-bfy-empty-expr-node) ; no loop condition
                   (js3-bfy-parse-expr)))
      (js3-bfy-must-match js3-bfy-SEMI "msg.no.semi.for.cond")
      (setq tmp-pos js3-bfy-token-end
            incr (if (= (js3-bfy-peek-token) js3-bfy-RP)
                     (make-js3-bfy-empty-expr-node :pos tmp-pos)
                   (js3-bfy-parse-expr))))
    (if (js3-bfy-must-match js3-bfy-RP "msg.no.paren.for.ctrl")
        (setq rp (- js3-bfy-token-beg for-pos)))
    (if (not is-for-in)
        (setq pn (make-js3-bfy-for-node :init init
					:condition cond
					:update incr
					:lp lp
					:rp rp))
      ;; cond could be null if 'in obj' got eaten by the init node.
      (if (js3-bfy-infix-node-p init)
          ;; it was (foo in bar) instead of (var foo in bar)
          (setq cond (js3-bfy-infix-node-right init)
                init (js3-bfy-infix-node-left init))
        (if (and (js3-bfy-var-decl-node-p init)
                 (> (length (js3-bfy-var-decl-node-kids init)) 1))
            (js3-bfy-report-error "msg.mult.index")))
      (setq pn (make-js3-bfy-for-in-node :iterator init
					 :object cond
					 :in-pos in-pos
					 :foreach-p is-for-each
					 :each-pos each-pos
					 :lp lp
					 :rp rp)))
    (unwind-protect
        (progn
          (js3-bfy-enter-loop pn)
          ;; We have to parse the body -after- creating the loop node,
          ;; so that the loop node appears in the js3-bfy-loop-set, allowing
          ;; break/continue statements to find the enclosing loop.
          (setf body (js3-bfy-parse-statement)
                (js3-bfy-loop-node-body pn) body
                (js3-bfy-node-pos pn) for-pos
                (js3-bfy-node-len pn) (- (js3-bfy-node-end body) for-pos))
          (js3-bfy-node-add-children pn init cond incr body))
      ;; finally
      (js3-bfy-exit-loop))
    pn))

(defun js3-bfy-parse-try ()
  "Parser for try-statement.  Last matched token must be js3-bfy-TRY."
  (let ((try-pos js3-bfy-token-beg)
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
    (js3-bfy-consume-token)
    (if (/= (js3-bfy-peek-token) js3-bfy-LC)
        (js3-bfy-report-error "msg.no.brace.try"))
    (setq try-block (js3-bfy-parse-statement)
          try-end (js3-bfy-node-end try-block)
          peek (js3-bfy-peek-token))
    (cond
     ((= peek js3-bfy-CATCH)
      (while (js3-bfy-match-token js3-bfy-CATCH)
        (setq catch-pos js3-bfy-token-beg
              guard-kwd nil
              catch-cond nil
              lp nil
              rp nil)
        (if saw-default-catch
            (js3-bfy-report-error "msg.catch.unreachable"))
        (if (js3-bfy-must-match js3-bfy-LP "msg.no.paren.catch")
            (setq lp (- js3-bfy-token-beg catch-pos)))
        (js3-bfy-must-match js3-bfy-NAME "msg.bad.catchcond")
	(js3-bfy-push-scope (make-js3-bfy-scope))
        (setq var-name (js3-bfy-create-name-node))
	(js3-bfy-define-symbol js3-bfy-LET (js3-bfy-name-node-name var-name) var-name t)
        (if (js3-bfy-match-token js3-bfy-IF)
            (setq guard-kwd (- js3-bfy-token-beg catch-pos)
                  catch-cond (js3-bfy-parse-expr))
          (setq saw-default-catch t))
        (if (js3-bfy-must-match js3-bfy-RP "msg.bad.catchcond")
            (setq rp (- js3-bfy-token-beg catch-pos)))
        (js3-bfy-must-match js3-bfy-LC "msg.no.brace.catchblock")
        (setq block (js3-bfy-parse-statements)
              try-end (js3-bfy-node-end block)
              catch-node (make-js3-bfy-catch-node :pos catch-pos
						  :var-name var-name
						  :guard-expr catch-cond
						  :guard-kwd guard-kwd
						  :block block
						  :lp lp
						  :rp rp))
	(js3-bfy-pop-scope)
        (if (js3-bfy-must-match js3-bfy-RC "msg.no.brace.after.body")
            (setq try-end js3-bfy-token-beg))
        (setf (js3-bfy-node-len block) (- try-end (js3-bfy-node-pos block))
              (js3-bfy-node-len catch-node) (- try-end catch-pos))
        (js3-bfy-node-add-children catch-node var-name catch-cond block)
        (push catch-node catch-blocks)))
     ((/= peek js3-bfy-FINALLY)
      (js3-bfy-must-match js3-bfy-FINALLY "msg.try.no.catchfinally"
			  (js3-bfy-node-pos try-block)
			  (- (setq try-end (js3-bfy-node-end try-block))
			     (js3-bfy-node-pos try-block)))))
    (when (js3-bfy-match-token js3-bfy-FINALLY)
      (setq finally-pos js3-bfy-token-beg
            block (js3-bfy-parse-statement)
            try-end (js3-bfy-node-end block)
            finally-block (make-js3-bfy-finally-node :pos finally-pos
						     :len (- try-end finally-pos)
						     :body block))
      (js3-bfy-node-add-children finally-block block))
    (setq pn (make-js3-bfy-try-node :pos try-pos
				    :len (- try-end try-pos)
				    :try-block try-block
				    :finally-block finally-block))
    (js3-bfy-node-add-children pn try-block finally-block)
    ;; push them onto the try-node, which reverses and corrects their order
    (dolist (cb catch-blocks)
      (js3-bfy-node-add-children pn cb)
      (push cb (js3-bfy-try-node-catch-clauses pn)))
    pn))

(defun js3-bfy-parse-throw ()
  "Parser for throw-statement.  Last matched token must be js3-bfy-THROW."
  (let ((pos js3-bfy-token-beg)
        expr
        pn)
    (js3-bfy-consume-token)
    (if (= (js3-bfy-peek-token-or-eol) js3-bfy-EOL)
        ;; ECMAScript does not allow new lines before throw expression,
        ;; see bug 256617
        (js3-bfy-report-error "msg.bad.throw.eol"))
    (setq expr (js3-bfy-parse-expr)
          pn (make-js3-bfy-throw-node :pos pos
				      :len (- (js3-bfy-node-end expr) pos)
				      :expr expr))
    (js3-bfy-node-add-children pn expr)
    pn))

(defsubst js3-bfy-match-jump-label-name (label-name)
  "If break/continue specified a label, return that label's labeled stmt.
Returns the corresponding `js3-bfy-labeled-stmt-node', or if LABEL-NAME
does not match an existing label, reports an error and returns nil."
  (let ((bundle (cdr (assoc label-name js3-bfy-label-set))))
    (if (null bundle)
        (js3-bfy-report-error "msg.undef.label"))
    bundle))

(defun js3-bfy-parse-break ()
  "Parser for break-statement.  Last matched token must be js3-bfy-BREAK."
  (let ((pos js3-bfy-token-beg)
        (end js3-bfy-token-end)
        break-target ; statement to break from
        break-label  ; in "break foo", name-node representing the foo
        labels       ; matching labeled statement to break to
        pn)
    (js3-bfy-consume-token)  ; `break'
    (when (eq (js3-bfy-peek-token-or-eol) js3-bfy-NAME)
      (js3-bfy-consume-token)
      (setq break-label (js3-bfy-create-name-node)
            end (js3-bfy-node-end break-label)
            ;; matchJumpLabelName only matches if there is one
            labels (js3-bfy-match-jump-label-name js3-bfy-ts-string)
            break-target (if labels (car (js3-bfy-labeled-stmt-node-labels labels)))))
    (unless (or break-target break-label)
      ;; no break target specified - try for innermost enclosing loop/switch
      (if (null js3-bfy-loop-and-switch-set)
          (unless break-label
            (js3-bfy-report-error "msg.bad.break" nil pos (length "break")))
        (setq break-target (car js3-bfy-loop-and-switch-set))))
    (setq pn (make-js3-bfy-break-node :pos pos
				      :len (- end pos)
				      :label break-label
				      :target break-target))
    (js3-bfy-node-add-children pn break-label)  ; but not break-target
    pn))

(defun js3-bfy-parse-continue ()
  "Parser for continue-statement.  Last matched token must be js3-bfy-CONTINUE."
  (let ((pos js3-bfy-token-beg)
        (end js3-bfy-token-end)
        label   ; optional user-specified label, a `js3-bfy-name-node'
        labels  ; current matching labeled stmt, if any
        target  ; the `js3-bfy-loop-node' target of this continue stmt
        pn)
    (js3-bfy-consume-token)  ; `continue'
    (when (= (js3-bfy-peek-token-or-eol) js3-bfy-NAME)
      (js3-bfy-consume-token)
      (setq label (js3-bfy-create-name-node)
            end (js3-bfy-node-end label)
            ;; matchJumpLabelName only matches if there is one
            labels (js3-bfy-match-jump-label-name js3-bfy-ts-string)))
    (cond
     ((null labels)  ; no current label to go to
      (if (null js3-bfy-loop-set)  ; no loop to continue to
          (js3-bfy-report-error "msg.continue.outside" nil pos
				(length "continue"))
        (setq target (car js3-bfy-loop-set))))  ; innermost enclosing loop
     (t
      (if (js3-bfy-loop-node-p (js3-bfy-labeled-stmt-node-stmt labels))
          (setq target (js3-bfy-labeled-stmt-node-stmt labels))
        (js3-bfy-report-error "msg.continue.nonloop" nil pos (- end pos)))))
    (setq pn (make-js3-bfy-continue-node :pos pos
					 :len (- end pos)
					 :label label
					 :target target))
    (js3-bfy-node-add-children pn label)  ; but not target - it's not our child
    pn))

(defun js3-bfy-parse-with ()
  "Parser for with-statement.  Last matched token must be js3-bfy-WITH."
  (js3-bfy-consume-token)
  (let ((pos js3-bfy-token-beg)
        obj body pn lp rp)
    (if (js3-bfy-must-match js3-bfy-LP "msg.no.paren.with")
        (setq lp js3-bfy-token-beg))
    (setq obj (js3-bfy-parse-expr))
    (if (js3-bfy-must-match js3-bfy-RP "msg.no.paren.after.with")
        (setq rp js3-bfy-token-beg))
    (let ((js3-bfy-nesting-of-with (1+ js3-bfy-nesting-of-with)))
      (setq body (js3-bfy-parse-statement)))
    (setq pn (make-js3-bfy-with-node :pos pos
				     :len (- (js3-bfy-node-end body) pos)
				     :object obj
				     :body body
				     :lp (js3-bfy-relpos lp pos)
				     :rp (js3-bfy-relpos rp pos)))
    (js3-bfy-node-add-children pn obj body)
    pn))

(defun js3-bfy-parse-const-var ()
  "Parser for var- or const-statement.
Last matched token must be js3-bfy-CONST or js3-bfy-VAR."
  (let ((tt (js3-bfy-peek-token))
        (pos js3-bfy-token-beg)
        expr
        pn)
    (js3-bfy-consume-token)
    (setq expr (js3-bfy-parse-variables tt js3-bfy-token-beg)
          pn (make-js3-bfy-expr-stmt-node :pos pos
					  :len (- (js3-bfy-node-end expr) pos)
					  :expr expr))
    (js3-bfy-node-add-children pn expr)
    pn))

(defsubst js3-bfy-wrap-with-expr-stmt (pos expr &optional add-child)
  (let ((pn (make-js3-bfy-expr-stmt-node :pos pos
					 :len (js3-bfy-node-len expr)
					 :type (if (js3-bfy-inside-function)
						   js3-bfy-EXPR_VOID
						 js3-bfy-EXPR_RESULT)
					 :expr expr)))
    (if add-child
        (js3-bfy-node-add-children pn expr))
    pn))

(defun js3-bfy-parse-let-stmt ()
  "Parser for let-statement.  Last matched token must be js3-bfy-LET."
  (js3-bfy-consume-token)
  (let ((pos js3-bfy-token-beg)
        expr
        pn)
    (if (= (js3-bfy-peek-token) js3-bfy-LP)
        ;; let expression in statement context
        (setq expr (js3-bfy-parse-let pos 'statement)
              pn (js3-bfy-wrap-with-expr-stmt pos expr t))
      ;; else we're looking at a statement like let x=6, y=7;
      (setf expr (js3-bfy-parse-variables js3-bfy-LET pos)
            pn (js3-bfy-wrap-with-expr-stmt pos expr t)
            (js3-bfy-node-type pn) js3-bfy-EXPR_RESULT))
    pn))

(defun js3-bfy-parse-ret-yield ()
  (js3-bfy-parse-return-or-yield (js3-bfy-peek-token) nil))

(defconst js3-bfy-parse-return-stmt-enders
  (list js3-bfy-SEMI js3-bfy-RC js3-bfy-EOF js3-bfy-EOL js3-bfy-ERROR js3-bfy-RB js3-bfy-RP js3-bfy-YIELD))

(defsubst js3-bfy-now-all-set (before after mask)
  "Return whether or not the bits in the mask have changed to all set.
BEFORE is bits before change, AFTER is bits after change, and MASK is
the mask for bits.  Returns t if all the bits in the mask are set in AFTER
but not BEFORE."
  (and (/= (logand before mask) mask)
       (= (logand after mask) mask)))

(defun js3-bfy-parse-return-or-yield (tt expr-context)
  (let ((pos js3-bfy-token-beg)
        (end js3-bfy-token-end)
        (before js3-bfy-end-flags)
        (inside-function (js3-bfy-inside-function))
        e
        ret
        name)
    (unless inside-function
      (js3-bfy-report-error (if (eq tt js3-bfy-RETURN)
				"msg.bad.return"
			      "msg.bad.yield")))
    (js3-bfy-consume-token)
    ;; This is ugly, but we don't want to require a semicolon.
    (unless (memq (js3-bfy-peek-token-or-eol) js3-bfy-parse-return-stmt-enders)
      (setq e (js3-bfy-parse-expr)
            end (js3-bfy-node-end e)))
    (cond
     ((eq tt js3-bfy-RETURN)
      (js3-bfy-set-flag js3-bfy-end-flags (if (null e)
					      js3-bfy-end-returns
					    js3-bfy-end-returns-value))
      (setq ret (make-js3-bfy-return-node :pos pos
					  :len (- end pos)
					  :retval e))
      (js3-bfy-node-add-children ret e)
      ;; See if we need a strict mode warning.
      ;; TODO:  The analysis done by `js3-bfy-has-consistent-return-usage' is
      ;; more thorough and accurate than this before/after flag check.
      ;; E.g. if there's a finally-block that always returns, we shouldn't
      ;; show a warning generated by inconsistent returns in the catch blocks.
      ;; Basically `js3-bfy-has-consistent-return-usage' needs to keep more state,
      ;; so we know which returns/yields to highlight, and we should get rid of
      ;; all the checking in `js3-bfy-parse-return-or-yield'.
      (if (and js3-bfy-strict-inconsistent-return-warning
               (js3-bfy-now-all-set before js3-bfy-end-flags
				    (logior js3-bfy-end-returns js3-bfy-end-returns-value)))
          (js3-bfy-add-strict-warning "msg.return.inconsistent" nil pos end)))
     (t
      (unless (js3-bfy-inside-function)
        (js3-bfy-report-error "msg.bad.yield"))
      (js3-bfy-set-flag js3-bfy-end-flags js3-bfy-end-yields)
      (setq ret (make-js3-bfy-yield-node :pos pos
					 :len (- end pos)
					 :value e))
      (js3-bfy-node-add-children ret e)
      (unless expr-context
        (setq e ret
              ret (js3-bfy-wrap-with-expr-stmt pos e t))
        (js3-bfy-set-requires-activation)
        (js3-bfy-set-is-generator))))
    ;; see if we are mixing yields and value returns.
    (when (and inside-function
               (js3-bfy-now-all-set before js3-bfy-end-flags
				    (logior js3-bfy-end-yields js3-bfy-end-returns-value)))
      (setq name (js3-bfy-function-name js3-bfy-current-script-or-fn))
      (if (zerop (length name))
          (js3-bfy-report-error "msg.anon.generator.returns" nil pos (- end pos))
        (js3-bfy-report-error "msg.generator.returns" name pos (- end pos))))
    ret))

(defun js3-bfy-parse-debugger ()
  (js3-bfy-consume-token)
  (make-js3-bfy-keyword-node :type js3-bfy-DEBUGGER))

(defun js3-bfy-parse-block ()
  "Parser for a curly-delimited statement block.
Last token matched must be js3-bfy-LC."
  (let ((pos js3-bfy-token-beg)
        (pn (make-js3-bfy-scope)))
    (js3-bfy-consume-token)
    (js3-bfy-push-scope pn)
    (unwind-protect
        (progn
          (js3-bfy-parse-statements pn)
          (js3-bfy-must-match js3-bfy-RC "msg.no.brace.block")
          (setf (js3-bfy-node-len pn) (- js3-bfy-token-end pos)))
      (js3-bfy-pop-scope))
    pn))

;; for js3-bfy-ERROR too, to have a node for error recovery to work on
(defun js3-bfy-parse-semi ()
  "Parse a statement or handle an error.
Last matched token is js3-bfy-SEMI or js3-bfy-ERROR."
  (let ((tt (js3-bfy-peek-token)) pos len)
    (js3-bfy-consume-token)
    (if (eq tt js3-bfy-SEMI)
        (make-js3-bfy-empty-expr-node :len 1)
      (setq pos js3-bfy-token-beg
            len (- js3-bfy-token-beg pos))
      (js3-bfy-report-error "msg.syntax" nil pos len)
      (make-js3-bfy-error-node :pos pos :len len))))

(defun js3-bfy-record-label (label bundle)
  ;; current token should be colon that `js3-bfy-parse-primary-expr' left untouched
  (js3-bfy-consume-token)
  (let ((name (js3-bfy-label-node-name label))
        labeled-stmt
        dup)
    (when (setq labeled-stmt (cdr (assoc name js3-bfy-label-set)))
      ;; flag both labels if possible when used in editing mode
      (if (and js3-bfy-parse-ide-mode
               (setq dup (js3-bfy-get-label-by-name labeled-stmt name)))
          (js3-bfy-report-error "msg.dup.label" nil
				(js3-bfy-node-abs-pos dup) (js3-bfy-node-len dup)))
      (js3-bfy-report-error "msg.dup.label" nil
			    (js3-bfy-node-pos label) (js3-bfy-node-len label)))
    (js3-bfy-labeled-stmt-node-add-label bundle label)
    (js3-bfy-node-add-children bundle label)
    ;; Add one reference to the bundle per label in `js3-bfy-label-set'
    (push (cons name bundle) js3-bfy-label-set)))

(defun js3-bfy-parse-name-or-label ()
  "Parser for identifier or label.  Last token matched must be js3-bfy-NAME.
Called when we found a name in a statement context.  If it's a label, we gather
up any following labels and the next non-label statement into a
`js3-bfy-labeled-stmt-node' bundle and return that.  Otherwise we parse an
expression and return it wrapped in a `js3-bfy-expr-stmt-node'."
  (let ((pos js3-bfy-token-beg)
        (end js3-bfy-token-end)
        expr
        stmt
        pn
        bundle
        (continue t))
    ;; set check for label and call down to `js3-bfy-parse-primary-expr'
    (js3-bfy-set-check-for-label)
    (setq expr (js3-bfy-parse-expr))
    (if (/= (js3-bfy-node-type expr) js3-bfy-LABEL)
        ;; Parsed non-label expression - wrap with expression stmt.
        (setq pn (js3-bfy-wrap-with-expr-stmt pos expr t))
      ;; else parsed a label
      (setq bundle (make-js3-bfy-labeled-stmt-node :pos pos))
      (js3-bfy-record-label expr bundle)
      ;; look for more labels
      (while (and continue (= (js3-bfy-peek-token) js3-bfy-NAME))
        (js3-bfy-set-check-for-label)
        (setq expr (js3-bfy-parse-expr))
        (if (/= (js3-bfy-node-type expr) js3-bfy-LABEL)
            (progn
              (setq stmt (js3-bfy-wrap-with-expr-stmt (js3-bfy-node-pos expr) expr t)
                    continue nil)
              (js3-bfy-auto-insert-semicolon stmt))
          (js3-bfy-record-label expr bundle)))
      ;; no more labels; now parse the labeled statement
      (unwind-protect
          (unless stmt
            (let ((js3-bfy-labeled-stmt bundle))  ; bind dynamically
              (setq stmt (js3-bfy-statement-helper))))
        ;; remove the labels for this statement from the global set
        (dolist (label (js3-bfy-labeled-stmt-node-labels bundle))
          (setq js3-bfy-label-set (remove label js3-bfy-label-set))))
      (setf (js3-bfy-labeled-stmt-node-stmt bundle) stmt
            (js3-bfy-node-len bundle) (- (js3-bfy-node-end stmt) pos))
      (js3-bfy-node-add-children bundle stmt)
      bundle)))

(defun js3-bfy-parse-expr-stmt ()
  "Default parser in statement context, if no recognized statement found."
  (js3-bfy-wrap-with-expr-stmt js3-bfy-token-beg (js3-bfy-parse-expr) t))

(defun js3-bfy-parse-variables (decl-type pos)
  "Parse a comma-separated list of variable declarations.
Could be a 'var', 'const' or 'let' expression, possibly in a for-loop initializer.

DECL-TYPE is a token value: either VAR, CONST, or LET depending on context.
For 'var' or 'const', the keyword should be the token last scanned.

POS is the position where the node should start. It's sometimes the
var/const/let keyword, and other times the beginning of the first token
in the first variable declaration.

Returns the parsed `js3-bfy-var-decl-node' expression node."
  (let* ((result (make-js3-bfy-var-decl-node :decl-type decl-type
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
            tt (js3-bfy-peek-token)
            kid-pos js3-bfy-token-beg
            end js3-bfy-token-end
            init nil)
      (if (or (= tt js3-bfy-LB) (= tt js3-bfy-LC))
          ;; Destructuring assignment, e.g., var [a, b] = ...
          (setq destructuring (js3-bfy-parse-primary-expr)
                end (js3-bfy-node-end destructuring))
        ;; Simple variable name
        (when (js3-bfy-must-match js3-bfy-NAME "msg.bad.var")
          (setq name (js3-bfy-create-name-node)
                nbeg js3-bfy-token-beg
                nend js3-bfy-token-end
                end nend)
          (js3-bfy-define-symbol decl-type js3-bfy-ts-string name js3-bfy-in-for-init)))
      (when (js3-bfy-match-token js3-bfy-ASSIGN)
        (setq init (js3-bfy-parse-assign-expr)
              end (js3-bfy-node-end init)))
      (setq vi (make-js3-bfy-var-init-node :pos kid-pos
					   :len (- end kid-pos)
					   :type decl-type))
      (if destructuring
          (progn
            (if (and (null init) (not js3-bfy-in-for-init))
                (js3-bfy-report-error "msg.destruct.assign.no.init"))
            (setf (js3-bfy-var-init-node-target vi) destructuring))
        (setf (js3-bfy-var-init-node-target vi) name))
      (setf (js3-bfy-var-init-node-initializer vi) init)
      (js3-bfy-node-add-children vi name destructuring init)
      (js3-bfy-block-node-push result vi)
      (unless (js3-bfy-match-token js3-bfy-COMMA)
        (setq continue nil)))
    (setf (js3-bfy-node-len result) (- end pos))
    result))

(defun js3-bfy-parse-let (pos &optional stmt-p)
  "Parse a let expression or statement.
A let-expression is of the form `let (vars) expr'.
A let-statment is of the form `let (vars) {statements}'.
The third form of let is a variable declaration list, handled
by `js3-bfy-parse-variables'."
  (let ((pn (make-js3-bfy-let-node :pos pos))
        beg vars body)
    (if (js3-bfy-must-match js3-bfy-LP "msg.no.paren.after.let")
        (setf (js3-bfy-let-node-lp pn) (- js3-bfy-token-beg pos)))
    (js3-bfy-push-scope pn)
    (unwind-protect
        (progn
          (setq vars (js3-bfy-parse-variables js3-bfy-LET js3-bfy-token-beg))
          (if (js3-bfy-must-match js3-bfy-RP "msg.no.paren.let")
              (setf (js3-bfy-let-node-rp pn) (- js3-bfy-token-beg pos)))
          (if (and stmt-p (eq (js3-bfy-peek-token) js3-bfy-LC))
              ;; let statement
              (progn
                (js3-bfy-consume-token)
                (setf beg js3-bfy-token-beg  ; position stmt at LC
                      body (js3-bfy-parse-statements))
                (js3-bfy-must-match js3-bfy-RC "msg.no.curly.let")
                (setf (js3-bfy-node-len body) (- js3-bfy-token-end beg)
                      (js3-bfy-node-len pn) (- js3-bfy-token-end pos)
                      (js3-bfy-let-node-body pn) body
                      (js3-bfy-node-type pn) js3-bfy-LET))
            ;; let expression
            (setf body (js3-bfy-parse-expr)
                  (js3-bfy-node-len pn) (- (js3-bfy-node-end body) pos)
                  (js3-bfy-let-node-body pn) body))
          (js3-bfy-node-add-children pn vars body))
      (js3-bfy-pop-scope))
    pn))

(defsubst js3-bfy-define-new-symbol (decl-type name node &optional scope)
  (js3-bfy-scope-put-symbol (or scope js3-bfy-current-scope)
			    name
			    (make-js3-bfy-symbol decl-type name node)))

(defun js3-bfy-define-symbol (decl-type name &optional node ignore-not-in-block)
  "Define a symbol in the current scope.
If NODE is non-nil, it is the AST node associated with the symbol."
  (let* ((defining-scope (js3-bfy-get-defining-scope js3-bfy-current-scope name))
         (symbol (if defining-scope
                     (js3-bfy-scope-get-symbol defining-scope name)))
         (sdt (if symbol (js3-bfy-symbol-decl-type symbol) -1)))
    (cond
     ((and symbol ; already defined
           (or (= sdt js3-bfy-CONST) ; old version is const
               (= decl-type js3-bfy-CONST) ; new version is const
               ;; two let-bound vars in this block have same name
               (and (= sdt js3-bfy-LET)
                    (eq defining-scope js3-bfy-current-scope))))
      (js3-bfy-report-error
       (cond
        ((= sdt js3-bfy-CONST) "msg.const.redecl")
        ((= sdt js3-bfy-LET) "msg.let.redecl")
        ((= sdt js3-bfy-VAR) "msg.var.redecl")
        ((= sdt js3-bfy-FUNCTION) "msg.function.redecl")
        (t "msg.parm.redecl"))
       name))
     ((= decl-type js3-bfy-LET)
      (if (and (not ignore-not-in-block)
               (or (= (js3-bfy-node-type js3-bfy-current-scope) js3-bfy-IF)
                   (js3-bfy-loop-node-p js3-bfy-current-scope)))
          (js3-bfy-report-error "msg.let.decl.not.in.block")
        (js3-bfy-define-new-symbol decl-type name node
				   js3-bfy-current-script-or-fn)))
     ((or (= decl-type js3-bfy-VAR)
          (= decl-type js3-bfy-CONST)
          (= decl-type js3-bfy-FUNCTION))
      (if symbol
          (if (and js3-bfy-strict-var-redeclaration-warning (= sdt js3-bfy-VAR))
              (js3-bfy-add-strict-warning "msg.var.redecl" name)
            (if (and js3-bfy-strict-var-hides-function-arg-warning (= sdt js3-bfy-LP))
                (js3-bfy-add-strict-warning "msg.var.hides.arg" name)))
        (js3-bfy-define-new-symbol decl-type name node)))
     ((= decl-type js3-bfy-LP)
      (if symbol
          ;; must be duplicate parameter. Second parameter hides the
          ;; first, so go ahead and add the second pararameter
          (js3-bfy-report-warning "msg.dup.parms" name))
      (js3-bfy-define-new-symbol decl-type name node))
     (t (js3-bfy-code-bug)))))

(defun js3-bfy-parse-expr ()
  (let* ((pn (js3-bfy-parse-assign-expr))
         (pos (js3-bfy-node-pos pn))
         left
         right
         op-pos)
    (while (js3-bfy-match-token js3-bfy-COMMA)
      (setq op-pos (- js3-bfy-token-beg pos))  ; relative
      (if (= (js3-bfy-peek-token) js3-bfy-YIELD)
          (js3-bfy-report-error "msg.yield.parenthesized"))
      (setq right (js3-bfy-parse-assign-expr)
            left pn
            pn (make-js3-bfy-infix-node :type js3-bfy-COMMA
					:pos pos
					:len (- js3-bfy-ts-cursor pos)
					:op-pos op-pos
					:left left
					:right right))
      (js3-bfy-node-add-children pn left right))
    pn))

(defun js3-bfy-parse-assign-expr ()
  (let ((tt (js3-bfy-peek-token))
        (pos js3-bfy-token-beg)
        pn
        left
        right
        op-pos)
    (if (= tt js3-bfy-YIELD)
        (js3-bfy-parse-return-or-yield tt t)
      ;; not yield - parse assignment expression
      (setq pn (js3-bfy-parse-cond-expr)
            tt (js3-bfy-peek-token))
      (when (and (<= js3-bfy-first-assign tt)
                 (<= tt js3-bfy-last-assign))
        (js3-bfy-consume-token)
        (setq op-pos (- js3-bfy-token-beg pos)  ; relative
              left pn
              right (js3-bfy-parse-assign-expr)
              pn (make-js3-bfy-assign-node :type tt
					   :pos pos
					   :len (- (js3-bfy-node-end right) pos)
					   :op-pos op-pos
					   :left left
					   :right right))
        (js3-bfy-node-add-children pn left right))
      pn)))

(defun js3-bfy-parse-cond-expr ()
  (let ((pos js3-bfy-token-beg)
        (pn (js3-bfy-parse-or-expr))
        test-expr
        if-true
        if-false
        q-pos
        c-pos)
    (when (js3-bfy-match-token js3-bfy-HOOK)
      (setq q-pos (- js3-bfy-token-beg pos)
            if-true (js3-bfy-parse-assign-expr))
      (js3-bfy-must-match js3-bfy-COLON "msg.no.colon.cond")
      (setq c-pos (- js3-bfy-token-beg pos)
            if-false (js3-bfy-parse-assign-expr)
            test-expr pn
            pn (make-js3-bfy-cond-node :pos pos
				       :len (- (js3-bfy-node-end if-false) pos)
				       :test-expr test-expr
				       :true-expr if-true
				       :false-expr if-false
				       :q-pos q-pos
				       :c-pos c-pos))
      (js3-bfy-node-add-children pn test-expr if-true if-false))
    pn))

(defun js3-bfy-make-binary (type left parser)
  "Helper for constructing a binary-operator AST node.
LEFT is the left-side-expression, already parsed, and the
binary operator should have just been matched.
PARSER is a function to call to parse the right operand,
or a `js3-bfy-node' struct if it has already been parsed."
  (let* ((pos (js3-bfy-node-pos left))
         (op-pos (- js3-bfy-token-beg pos))
         (right (if (js3-bfy-node-p parser)
                    parser
                  (funcall parser)))
         (pn (make-js3-bfy-infix-node :type type
				      :pos pos
				      :len (- (js3-bfy-node-end right) pos)
				      :op-pos op-pos
				      :left left
				      :right right)))
    (js3-bfy-node-add-children pn left right)
    pn))

(defun js3-bfy-parse-or-expr ()
  (let ((pn (js3-bfy-parse-and-expr)))
    (when (js3-bfy-match-token js3-bfy-OR)
      (setq pn (js3-bfy-make-binary js3-bfy-OR
				    pn
				    'js3-bfy-parse-or-expr)))
    pn))

(defun js3-bfy-parse-and-expr ()
  (let ((pn (js3-bfy-parse-bit-or-expr)))
    (when (js3-bfy-match-token js3-bfy-AND)
      (setq pn (js3-bfy-make-binary js3-bfy-AND
				    pn
				    'js3-bfy-parse-and-expr)))
    pn))

(defun js3-bfy-parse-bit-or-expr ()
  (let ((pn (js3-bfy-parse-bit-xor-expr)))
    (while (js3-bfy-match-token js3-bfy-BITOR)
      (setq pn (js3-bfy-make-binary js3-bfy-BITOR
				    pn
				    'js3-bfy-parse-bit-xor-expr)))
    pn))

(defun js3-bfy-parse-bit-xor-expr ()
  (let ((pn (js3-bfy-parse-bit-and-expr)))
    (while (js3-bfy-match-token js3-bfy-BITXOR)
      (setq pn (js3-bfy-make-binary js3-bfy-BITXOR
				    pn
				    'js3-bfy-parse-bit-and-expr)))
    pn))

(defun js3-bfy-parse-bit-and-expr ()
  (let ((pn (js3-bfy-parse-eq-expr)))
    (while (js3-bfy-match-token js3-bfy-BITAND)
      (setq pn (js3-bfy-make-binary js3-bfy-BITAND
				    pn
				    'js3-bfy-parse-eq-expr)))
    pn))

(defconst js3-bfy-parse-eq-ops
  (list js3-bfy-EQ js3-bfy-NE js3-bfy-SHEQ js3-bfy-SHNE))

(defun js3-bfy-parse-eq-expr ()
  (let ((pn (js3-bfy-parse-rel-expr))
        tt)
    (while (memq (setq tt (js3-bfy-peek-token)) js3-bfy-parse-eq-ops)
      (js3-bfy-consume-token)
      (setq pn (js3-bfy-make-binary tt
				    pn
				    'js3-bfy-parse-rel-expr)))
    pn))

(defconst js3-bfy-parse-rel-ops
  (list js3-bfy-IN js3-bfy-INSTANCEOF js3-bfy-LE js3-bfy-LT js3-bfy-GE js3-bfy-GT))

(defun js3-bfy-parse-rel-expr ()
  (let ((pn (js3-bfy-parse-shift-expr))
        (continue t)
        tt)
    (while continue
      (setq tt (js3-bfy-peek-token))
      (cond
       ((and js3-bfy-in-for-init (= tt js3-bfy-IN))
        (setq continue nil))
       ((memq tt js3-bfy-parse-rel-ops)
        (js3-bfy-consume-token)
        (setq pn (js3-bfy-make-binary tt pn 'js3-bfy-parse-shift-expr)))
       (t
        (setq continue nil))))
    pn))

(defconst js3-bfy-parse-shift-ops
  (list js3-bfy-LSH js3-bfy-URSH js3-bfy-RSH))

(defun js3-bfy-parse-shift-expr ()
  (let ((pn (js3-bfy-parse-add-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js3-bfy-peek-token))
      (if (memq tt js3-bfy-parse-shift-ops)
          (progn
            (js3-bfy-consume-token)
            (setq pn (js3-bfy-make-binary tt pn 'js3-bfy-parse-add-expr)))
        (setq continue nil)))
    pn))

(defun js3-bfy-parse-add-expr ()
  (let ((pn (js3-bfy-parse-mul-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js3-bfy-peek-token))
      (if (or (= tt js3-bfy-ADD) (= tt js3-bfy-SUB))
          (progn
            (js3-bfy-consume-token)
            (setq pn (js3-bfy-make-binary tt pn 'js3-bfy-parse-mul-expr)))
        (setq continue nil)))
    pn))

(defconst js3-bfy-parse-mul-ops
  (list js3-bfy-MUL js3-bfy-DIV js3-bfy-MOD))

(defun js3-bfy-parse-mul-expr ()
  (let ((pn (js3-bfy-parse-unary-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js3-bfy-peek-token))
      (if (memq tt js3-bfy-parse-mul-ops)
          (progn
            (js3-bfy-consume-token)
            (setq pn (js3-bfy-make-binary tt pn 'js3-bfy-parse-unary-expr)))
        (setq continue nil)))
    pn))

(defsubst js3-bfy-make-unary (type parser &rest args)
  "Make a unary node of type TYPE.
PARSER is either a node (for postfix operators) or a function to call
to parse the operand (for prefix operators)."
  (let* ((pos js3-bfy-token-beg)
         (postfix (js3-bfy-node-p parser))
         (expr (if postfix
                   parser
                 (apply parser args)))
         end
         pn)
    (if postfix  ; e.g. i++
        (setq pos (js3-bfy-node-pos expr)
              end js3-bfy-token-end)
      (setq end (js3-bfy-node-end expr)))
    (setq pn (make-js3-bfy-unary-node :type type
				      :pos pos
				      :len (- end pos)
				      :operand expr))
    (js3-bfy-node-add-children pn expr)
    pn))

(defconst js3-bfy-incrementable-node-types
  (list js3-bfy-NAME js3-bfy-GETPROP js3-bfy-GETELEM js3-bfy-GET_REF js3-bfy-CALL)
  "Node types that can be the operand of a ++ or -- operator.")

(defsubst js3-bfy-check-bad-inc-dec (tt beg end unary)
  (unless (memq (js3-bfy-node-type (js3-bfy-unary-node-operand unary))
                js3-bfy-incrementable-node-types)
    (js3-bfy-report-error (if (= tt js3-bfy-INC)
			      "msg.bad.incr"
			    "msg.bad.decr")
			  nil beg (- end beg))))

(defun js3-bfy-parse-unary-expr ()
  (let ((tt (js3-bfy-peek-token))
        pn expr beg end)
    (cond
     ((or (= tt js3-bfy-VOID)
          (= tt js3-bfy-NOT)
          (= tt js3-bfy-BITNOT)
          (= tt js3-bfy-TYPEOF))
      (js3-bfy-consume-token)
      (js3-bfy-make-unary tt 'js3-bfy-parse-unary-expr))
     ((= tt js3-bfy-ADD)
      (js3-bfy-consume-token)
      ;; Convert to special POS token in decompiler and parse tree
      (js3-bfy-make-unary js3-bfy-POS 'js3-bfy-parse-unary-expr))
     ((= tt js3-bfy-SUB)
      (js3-bfy-consume-token)
      ;; Convert to special NEG token in decompiler and parse tree
      (js3-bfy-make-unary js3-bfy-NEG 'js3-bfy-parse-unary-expr))
     ((or (= tt js3-bfy-INC)
          (= tt js3-bfy-DEC))
      (js3-bfy-consume-token)
      (prog1
          (setq beg js3-bfy-token-beg
                end js3-bfy-token-end
                expr (js3-bfy-make-unary tt 'js3-bfy-parse-member-expr t))
        (js3-bfy-check-bad-inc-dec tt beg end expr)))
     ((= tt js3-bfy-DELPROP)
      (js3-bfy-consume-token)
      (js3-bfy-make-unary js3-bfy-DELPROP 'js3-bfy-parse-unary-expr))
     ((= tt js3-bfy-ERROR)
      (js3-bfy-consume-token)
      (make-js3-bfy-error-node))  ; try to continue
     (t
      (setq pn (js3-bfy-parse-member-expr t)
            ;; Don't look across a newline boundary for a postfix incop.
            tt (js3-bfy-peek-token-or-eol))
      (when (or (= tt js3-bfy-INC) (= tt js3-bfy-DEC))
        (js3-bfy-consume-token)
        (setf expr pn
              pn (js3-bfy-make-unary tt expr))
        (js3-bfy-node-set-prop pn 'postfix t)
        (js3-bfy-check-bad-inc-dec tt js3-bfy-token-beg js3-bfy-token-end pn))
      pn))))


(defun js3-bfy-parse-argument-list ()
  "Parse an argument list and return it as a lisp list of nodes.
Returns the list in reverse order.  Consumes the right-paren token."
  (let (result)
    (unless (js3-bfy-match-token js3-bfy-RP)
      (loop do
            (if (= (js3-bfy-peek-token) js3-bfy-YIELD)
                (js3-bfy-report-error "msg.yield.parenthesized"))
            (push (js3-bfy-parse-assign-expr) result)
            while
            (js3-bfy-match-token js3-bfy-COMMA))
      (js3-bfy-must-match js3-bfy-RP "msg.no.paren.arg")
      result)))

(defun js3-bfy-parse-member-expr (&optional allow-call-syntax)
  (let ((tt (js3-bfy-peek-token))
        pn
        pos
        target
        args
        beg
        end
        init
        tail)
    (if (/= tt js3-bfy-NEW)
        (setq pn (js3-bfy-parse-primary-expr))
      ;; parse a 'new' expression
      (js3-bfy-consume-token)
      (setq pos js3-bfy-token-beg
            beg pos
            target (js3-bfy-parse-member-expr)
            end (js3-bfy-node-end target)
            pn (make-js3-bfy-new-node :pos pos
				      :target target
				      :len (- end pos)))
      (js3-bfy-node-add-children pn target)
      (when (js3-bfy-match-token js3-bfy-LP)
        ;; Add the arguments to pn, if any are supplied.
        (setf beg pos  ; start of "new" keyword
              pos js3-bfy-token-beg
              args (nreverse (js3-bfy-parse-argument-list))
              (js3-bfy-new-node-args pn) args
              end js3-bfy-token-end
              (js3-bfy-new-node-lp pn) (- pos beg)
              (js3-bfy-new-node-rp pn) (- end 1 beg))
        (apply #'js3-bfy-node-add-children pn args))
      (when (and js3-bfy-allow-rhino-new-expr-initializer
                 (js3-bfy-match-token js3-bfy-LC))
        (setf init (js3-bfy-parse-object-literal)
              end (js3-bfy-node-end init)
              (js3-bfy-new-node-initializer pn) init)
        (js3-bfy-node-add-children pn init))
      (setf (js3-bfy-node-len pn) (- beg pos)))  ; end outer if
    (js3-bfy-parse-member-expr-tail allow-call-syntax pn)))

(defun js3-bfy-parse-member-expr-tail (allow-call-syntax pn)
  "Parse a chain of property/array accesses or function calls.
Includes parsing for E4X operators like `..' and `.@'.
If ALLOW-CALL-SYNTAX is nil, stops when we encounter a left-paren.
Returns an expression tree that includes PN, the parent node."
  (let ((beg (js3-bfy-node-pos pn))
        tt
        (continue t))
    (while continue
      (setq tt (js3-bfy-peek-token))
      (cond
       ((= tt js3-bfy-DOT)
        (setq pn (js3-bfy-parse-property-access tt pn)))
       ((= tt js3-bfy-LB)
        (setq pn (js3-bfy-parse-element-get pn)))
       ((= tt js3-bfy-LP)
        (if allow-call-syntax
            (setq pn (js3-bfy-parse-function-call pn))
          (setq continue nil)))
       (t
        (setq continue nil))))
    pn))

(defun js3-bfy-parse-element-get (pn)
  "Parse an element-get expression, e.g. foo[bar].
Last token parsed must be `js3-bfy-RB'."
  (let ((lb js3-bfy-token-beg)
        (pos (js3-bfy-node-pos pn))
        rb
        expr)
    (js3-bfy-consume-token)
    (setq expr (js3-bfy-parse-expr))
    (if (js3-bfy-must-match js3-bfy-RB "msg.no.bracket.index")
        (setq rb js3-bfy-token-beg))
    (setq pn (make-js3-bfy-elem-get-node :target pn
					 :pos pos
					 :element expr
					 :lb (js3-bfy-relpos lb pos)
					 :rb (js3-bfy-relpos rb pos)
					 :len (- js3-bfy-token-end pos)))
    (js3-bfy-node-add-children pn
			       (js3-bfy-elem-get-node-target pn)
			       (js3-bfy-elem-get-node-element pn))
    pn))

(defun js3-bfy-parse-function-call (pn)
  (let (args
        (pos (js3-bfy-node-pos pn)))
    (js3-bfy-consume-token)
    (setq pn (make-js3-bfy-call-node :pos pos
				     :target pn
				     :lp (- js3-bfy-token-beg pos)))
    (js3-bfy-node-add-children pn (js3-bfy-call-node-target pn))
    ;; Add the arguments to pn, if any are supplied.
    (setf args (nreverse (js3-bfy-parse-argument-list))
          (js3-bfy-call-node-rp pn) (- js3-bfy-token-beg pos)
          (js3-bfy-call-node-args pn) args)
    (apply #'js3-bfy-node-add-children pn args)
    (setf (js3-bfy-node-len pn) (- js3-bfy-ts-cursor pos))
    pn))

(defun js3-bfy-parse-property-access (tt pn)
  "Parse a property access."
  (let (name
	(pos (js3-bfy-node-pos pn))
	end
        ref  ; right side of . operator
        result)
    (js3-bfy-consume-token)
    (js3-bfy-must-match-prop-name "msg.no.name.after.dot")
    (setq name (js3-bfy-create-name-node t js3-bfy-GETPROP)
	  end (js3-bfy-node-end name)
	  result (make-js3-bfy-prop-get-node :left pn
					     :pos pos
					     :right name
					     :len (- end
						     pos)))
    (js3-bfy-node-add-children result pn name)
    result))


(defun js3-bfy-parse-primary-expr ()
  "Parses a literal (leaf) expression of some sort.
Includes complex literals such as functions, object-literals,
array-literals, array comprehensions and regular expressions."
  (let ((tt-flagged (js3-bfy-next-flagged-token))
        pn      ; parent node  (usually return value)
        tt
        px-pos  ; paren-expr pos
        len
        flags   ; regexp flags
        expr)
    (setq tt js3-bfy-current-token)
    (cond
     ((= tt js3-bfy-FUNCTION)
      (js3-bfy-parse-function 'FUNCTION_EXPRESSION))
     ((= tt js3-bfy-LB)
      (js3-bfy-parse-array-literal))
     ((= tt js3-bfy-LC)
      (js3-bfy-parse-object-literal))
     ((= tt js3-bfy-LET)
      (js3-bfy-parse-let js3-bfy-token-beg))
     ((= tt js3-bfy-LP)
      (setq px-pos js3-bfy-token-beg
            expr (js3-bfy-parse-expr))
      (js3-bfy-must-match js3-bfy-RP "msg.no.paren")
      (setq pn (make-js3-bfy-paren-node :pos px-pos
					:expr expr
					:len (- js3-bfy-token-end px-pos)))
      (js3-bfy-node-add-children pn (js3-bfy-paren-node-expr pn))
      pn)
     ((= tt js3-bfy-NAME)
      (js3-bfy-parse-name tt-flagged tt))
     ((= tt js3-bfy-NUMBER)
      (make-js3-bfy-number-node))
     ((= tt js3-bfy-STRING)
      (make-js3-bfy-string-node))
     ((or (= tt js3-bfy-DIV) (= tt js3-bfy-ASSIGN_DIV))
      ;; Got / or /= which in this context means a regexp literal
      (setq px-pos js3-bfy-token-beg)
      (js3-bfy-read-regexp tt)
      (setq flags js3-bfy-ts-regexp-flags
            js3-bfy-ts-regexp-flags nil)
      (prog1
	  (make-js3-bfy-regexp-node :pos px-pos
				    :len (- js3-bfy-ts-cursor px-pos)
				    :value js3-bfy-ts-string
				    :flags flags)
	(js3-bfy-record-text-property px-pos js3-bfy-ts-cursor 'syntax-table '(2))))
     ((or (= tt js3-bfy-NULL)
          (= tt js3-bfy-THIS)
          (= tt js3-bfy-FALSE)
          (= tt js3-bfy-TRUE))
      (make-js3-bfy-keyword-node :type tt))
     ((= tt js3-bfy-RESERVED)
      (js3-bfy-report-error "msg.reserved.id")
      (make-js3-bfy-name-node))
     ((= tt js3-bfy-ERROR)
      ;; the scanner or one of its subroutines reported the error.
      (make-js3-bfy-error-node))
     ((= tt js3-bfy-EOF)
      (setq px-pos (point-at-bol)
            len (- js3-bfy-ts-cursor px-pos))
      (js3-bfy-report-error "msg.unexpected.eof" nil px-pos len)
      (make-js3-bfy-error-node :pos px-pos :len len))
     (t
      (js3-bfy-report-error "msg.syntax")
      (make-js3-bfy-error-node)))))

(defun js3-bfy-parse-name (tt-flagged tt)
  (let ((name js3-bfy-ts-string)
        (name-pos js3-bfy-token-beg)
	node)
    (if (and (js3-bfy-flag-set-p tt-flagged js3-bfy-ti-check-label)
             (= (js3-bfy-peek-token) js3-bfy-COLON))
	(make-js3-bfy-label-node :pos name-pos
				 :len (- js3-bfy-token-end name-pos)
				 :name name)
      ;; Otherwise not a label, just a name.  Unfortunately peeking
      ;; the next token to check for a colon has biffed js3-bfy-token-beg
      ;; and js3-bfy-token-end.  We store the name's bounds in buffer vars
      ;; and `js3-bfy-create-name-node' uses them.
      (js3-bfy-save-name-token-data name-pos name)
      (setq node (js3-bfy-create-name-node 'check-activation))
      node)))

(defsubst js3-bfy-parse-warn-trailing-comma (msg pos elems comma-pos)
  (js3-bfy-add-strict-warning
   msg nil
   ;; back up from comma to beginning of line or array/objlit
   (max (if elems
            (js3-bfy-node-pos (car elems))
          pos)
        (save-excursion
          (goto-char comma-pos)
          (back-to-indentation)
          (point)))
   comma-pos))

(defun js3-bfy-parse-array-literal ()
  (let ((pos js3-bfy-token-beg)
        (end js3-bfy-token-end)
        (after-lb-or-comma t)
        after-comma
        tt
        elems
        pn
        (continue t))
    (while continue
      (setq tt (js3-bfy-peek-token))
      (cond
       ;; comma
       ((= tt js3-bfy-COMMA)
        (js3-bfy-consume-token)
        (setq after-comma js3-bfy-token-end)
        (if (not after-lb-or-comma)
            (setq after-lb-or-comma t)
          (push nil elems)))
       ;; end of array
       ((or (= tt js3-bfy-RB)
            (= tt js3-bfy-EOF))  ; prevent infinite loop
        (if (= tt js3-bfy-EOF)
            (js3-bfy-report-error "msg.no.bracket.arg" nil pos)
          (js3-bfy-consume-token))
        (setq continue nil
              end js3-bfy-token-end
              pn (make-js3-bfy-array-node :pos pos
					  :len (- js3-bfy-ts-cursor pos)
					  :elems (nreverse elems)))
        (apply #'js3-bfy-node-add-children pn (js3-bfy-array-node-elems pn))
        (when after-comma
          (js3-bfy-parse-warn-trailing-comma "msg.array.trailing.comma"
					     pos elems after-comma)))
       ;; array comp
       ((and (>= js3-bfy-language-version 170)
             (= tt js3-bfy-FOR)          ; check for array comprehension
             (not after-lb-or-comma) ; "for" can't follow a comma
             elems                   ; must have at least 1 element
             (not (cdr elems)))      ; but no 2nd element
        (setf continue nil
              pn (js3-bfy-parse-array-comprehension (car elems) pos)))
       ;; another element
       (t
        (unless after-lb-or-comma
          (js3-bfy-report-error "msg.no.bracket.arg"))
        (push (js3-bfy-parse-assign-expr) elems)
        (setq after-lb-or-comma nil
              after-comma nil))))
    pn))

(defun js3-bfy-parse-array-comprehension (expr pos)
  "Parse a JavaScript 1.7 Array Comprehension.
EXPR is the first expression after the opening left-bracket.
POS is the beginning of the LB token preceding EXPR.
We should have just parsed the 'for' keyword before calling this function."
  (let (loops
        filter
        if-pos
        result)
    (while (= (js3-bfy-peek-token) js3-bfy-FOR)
      (push (js3-bfy-parse-array-comp-loop) loops))
    (when (= (js3-bfy-peek-token) js3-bfy-IF)
      (js3-bfy-consume-token)
      (setq if-pos (- js3-bfy-token-beg pos)  ; relative
            filter (js3-bfy-parse-condition)))
    (js3-bfy-must-match js3-bfy-RB "msg.no.bracket.arg" pos)
    (setq result (make-js3-bfy-array-comp-node :pos pos
					       :len (- js3-bfy-ts-cursor pos)
					       :result expr
					       :loops (nreverse loops)
					       :filter (car filter)
					       :lp (js3-bfy-relpos (second filter) pos)
					       :rp (js3-bfy-relpos (third filter) pos)
					       :if-pos if-pos))
    (apply #'js3-bfy-node-add-children result expr (car filter)
           (js3-bfy-array-comp-node-loops result))
    result))

(defun js3-bfy-parse-array-comp-loop ()
  "Parse a 'for [each] (foo in bar)' expression in an Array comprehension.
Last token peeked should be the initial FOR."
  (let ((pos js3-bfy-token-beg)
        (pn (make-js3-bfy-array-comp-loop-node))
        tt
        iter
        obj
        foreach-p
        in-pos
        each-pos
        lp
        rp)
    (assert (= (js3-bfy-next-token) js3-bfy-FOR))  ; consumes token
    (js3-bfy-push-scope pn)
    (unwind-protect
        (progn
          (when (js3-bfy-match-token js3-bfy-NAME)
            (if (string= js3-bfy-ts-string "each")
		(setq foreach-p t
		      each-pos (- js3-bfy-token-beg pos)) ; relative
              (js3-bfy-report-error "msg.no.paren.for")))
          (if (js3-bfy-must-match js3-bfy-LP "msg.no.paren.for")
              (setq lp (- js3-bfy-token-beg pos)))
          (setq tt (js3-bfy-peek-token))
          (cond
           ((or (= tt js3-bfy-LB)
                (= tt js3-bfy-LC))
            ;; handle destructuring assignment
            (setq iter (js3-bfy-parse-primary-expr)))
           ((js3-bfy-valid-prop-name-token tt)
            (js3-bfy-consume-token)
            (setq iter (js3-bfy-create-name-node)))
           (t
            (js3-bfy-report-error "msg.bad.var")))
          ;; Define as a let since we want the scope of the variable to
          ;; be restricted to the array comprehension
          (if (js3-bfy-name-node-p iter)
              (js3-bfy-define-symbol js3-bfy-LET (js3-bfy-name-node-name iter) pn t))
          (if (js3-bfy-must-match js3-bfy-IN "msg.in.after.for.name")
              (setq in-pos (- js3-bfy-token-beg pos)))
          (setq obj (js3-bfy-parse-expr))
          (if (js3-bfy-must-match js3-bfy-RP "msg.no.paren.for.ctrl")
              (setq rp (- js3-bfy-token-beg pos)))
          (setf (js3-bfy-node-pos pn) pos
                (js3-bfy-node-len pn) (- js3-bfy-ts-cursor pos)
                (js3-bfy-array-comp-loop-node-iterator pn) iter
                (js3-bfy-array-comp-loop-node-object pn) obj
                (js3-bfy-array-comp-loop-node-in-pos pn) in-pos
                (js3-bfy-array-comp-loop-node-each-pos pn) each-pos
                (js3-bfy-array-comp-loop-node-foreach-p pn) foreach-p
                (js3-bfy-array-comp-loop-node-lp pn) lp
                (js3-bfy-array-comp-loop-node-rp pn) rp)
          (js3-bfy-node-add-children pn iter obj))
      (js3-bfy-pop-scope))
    pn))

(defun js3-bfy-parse-object-literal ()
  (let ((pos js3-bfy-token-beg)
        tt
        elems
        result
        after-comma
        (continue t))
    (while continue
      (setq tt (js3-bfy-peek-token))
      (cond
       ;; {foo: ...}, {'foo': ...}, {get foo() {...}}, or {set foo(x) {...}}
       ((or (js3-bfy-valid-prop-name-token tt)
            (= tt js3-bfy-STRING))
        (setq after-comma nil
              result (js3-bfy-parse-named-prop tt))
        (if (and (null result)
                 (not js3-bfy-recover-from-parse-errors))
            (setq continue nil)
          (push result elems)))
       ;; {12: x} or {10.7: x}
       ((= tt js3-bfy-NUMBER)
        (js3-bfy-consume-token)
        (setq after-comma nil)
        (push (js3-bfy-parse-plain-property (make-js3-bfy-number-node)) elems))
       ;; trailing comma
       ((= tt js3-bfy-RC)
        (setq continue nil)
        (if after-comma
            (js3-bfy-parse-warn-trailing-comma "msg.extra.trailing.comma"
					       pos elems after-comma)))
       (t
        (js3-bfy-report-error "msg.bad.prop")
        (unless js3-bfy-recover-from-parse-errors
          (setq continue nil))))         ; end switch
      (if (js3-bfy-match-token js3-bfy-COMMA)
          (setq after-comma js3-bfy-token-end)
        (setq continue nil)))           ; end loop
    (js3-bfy-must-match js3-bfy-RC "msg.no.brace.prop")
    (setq result (make-js3-bfy-object-node :pos pos
					   :len (- js3-bfy-ts-cursor pos)
					   :elems (nreverse elems)))
    (apply #'js3-bfy-node-add-children result (js3-bfy-object-node-elems result))
    result))

(defun js3-bfy-parse-named-prop (tt)
  "Parse a name, string, or getter/setter object property."
  (js3-bfy-consume-token)
  (let ((string-prop (and (= tt js3-bfy-STRING)
                          (make-js3-bfy-string-node)))
        expr
        (ppos js3-bfy-token-beg)
        (pend js3-bfy-token-end)
        (name (js3-bfy-create-name-node))
        (prop js3-bfy-ts-string))
    (if (and (= tt js3-bfy-NAME)
             (= (js3-bfy-peek-token) js3-bfy-NAME)
             (or (string= prop "get")
                 (string= prop "set")))
        (progn
          ;; getter/setter prop
          (js3-bfy-consume-token)
          (setq name (js3-bfy-create-name-node)) ; discard get/set & use peeked name
          (js3-bfy-parse-getter-setter-prop ppos name (string= prop "get")))
      ;; regular prop
      (setq expr (js3-bfy-parse-plain-property (or string-prop name))))))

(defun js3-bfy-parse-plain-property (prop)
  "Parse a non-getter/setter property in an object literal.
PROP is the node representing the property:  a number, name or string."
  (js3-bfy-must-match js3-bfy-COLON "msg.no.colon.prop")
  (let* ((pos (js3-bfy-node-pos prop))
         (colon (- js3-bfy-token-beg pos))
         (expr (js3-bfy-parse-assign-expr))
         (result (make-js3-bfy-object-prop-node
                  :pos pos
                  ;; don't include last consumed token in length
                  :len (- (+ (js3-bfy-node-pos expr)
                             (js3-bfy-node-len expr))
                          pos)
                  :left prop
                  :right expr
                  :op-pos colon)))
    (js3-bfy-node-add-children result prop expr)
    result))

(defun js3-bfy-parse-getter-setter-prop (pos prop get-p)
  "Parse getter or setter property in an object literal.
JavaScript syntax is:

{ get foo() {...}, set foo(x) {...} }

POS is the start position of the `get' or `set' keyword.
PROP is the `js3-bfy-name-node' representing the property name.
GET-P is non-nil if the keyword was `get'."
  (let ((type (if get-p js3-bfy-GET js3-bfy-SET))
        result
        end
        (fn (js3-bfy-parse-function 'FUNCTION_EXPRESSION)))
    ;; it has to be an anonymous function, as we already parsed the name
    (if (/= (js3-bfy-node-type fn) js3-bfy-FUNCTION)
        (js3-bfy-report-error "msg.bad.prop")
      (if (plusp (length (js3-bfy-function-name fn)))
          (js3-bfy-report-error "msg.bad.prop")))
    (js3-bfy-node-set-prop fn 'GETTER_SETTER type)  ; for codegen
    (setq end (js3-bfy-node-end fn)
          result (make-js3-bfy-getter-setter-node :type type
						  :pos pos
						  :len (- end pos)
						  :left prop
						  :right fn))
    (js3-bfy-node-add-children result prop fn)
    result))

(defun js3-bfy-create-name-node (&optional check-activation-p token)
  "Create a name node using the token info from last scanned name.
In some cases we need to either synthesize a name node, or we lost
the name token information by peeking.  If the TOKEN parameter is
not `js3-bfy-NAME', then we use the token info saved in instance vars."
  (let ((beg js3-bfy-token-beg)
        (s js3-bfy-ts-string)
        name)
    (when (/= js3-bfy-current-token js3-bfy-NAME)
      (setq beg (or js3-bfy-prev-name-token-start js3-bfy-ts-cursor)
            s js3-bfy-prev-name-token-string
            js3-bfy-prev-name-token-start nil
            js3-bfy-prev-name-token-string nil))
    (setq name (make-js3-bfy-name-node :pos beg
				       :name s
				       :len (length s)))
    (if check-activation-p
        (js3-bfy-check-activation-name s (or token js3-bfy-NAME)))
    name))

(provide 'js3-bfy-parse)

;;; js3-bfy-parse.el ends here
;;; js3-bfy-indent.el --- indentation for js3-bfy

;;; Code:

(defconst js3-bfy-possibly-braceless-keyword-re
  (regexp-opt
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with" "let" "each")
   'words)
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js3-bfy-indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defconst js3-bfy-indent-operator-first-re
  (concat "[-+*/%<>!=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions with operator-first style.")

(defconst js3-bfy-indent-brace-re
  "[[({]"
  "Regexp matching opening braces that affect indentation.")

(defconst js3-bfy-indent-operator-brace-re
  "[[(]"
  "Regexp matching opening braces that affect operator indentation.")

(defconst js3-bfy-skip-newlines-re
  "[ \t\n]*"
  "Regexp matching any amount of trailing whitespace and newlines.")

(defconst js3-bfy-opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defun js3-bfy-backward-sws ()
  "Move backward through whitespace and comments."
  (interactive)
  (while (forward-comment -1)))

(defun js3-bfy-forward-sws ()
  "Move forward through whitespace and comments."
  (interactive)
  (while (forward-comment 1)))

(defun js3-bfy-beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at js3-bfy-opt-cpp-start))
          t
        (goto-char here)
        nil))))

;; This function has horrible results if you're typing an array
;; such as [[1, 2], [3, 4], [5, 6]].  Bounce indenting -really- sucks
;; in conjunction with electric-indent, so just disabling it.
(defsubst js3-bfy-code-at-bol-p ()
  "Return t if the first character on line is non-whitespace."
  nil)

(defun js3-bfy-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (let ((cmd (lookup-key (current-global-map) key)))
    (if (commandp cmd)
        (call-interactively cmd)))
  ;; don't do the electric keys inside comments or strings,
  ;; and don't do bounce-indent with them.
  (let ((parse-state (parse-partial-sexp (point-min) (point)))
        (js3-bfy-bounce-indent-p (js3-bfy-code-at-bol-p)))
    (unless (or (nth 3 parse-state)
                (nth 4 parse-state))
      (indent-according-to-mode))))


(defun js3-bfy-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `js3-bfy-re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (js3-bfy-beginning-of-macro)
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
                  (js3-bfy-beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun js3-bfy-re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'js3-bfy-re-search-backward-inner)
               ((> count 0) #'js3-bfy-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))


(defun js3-bfy-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js3-bfy-re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (js3-bfy-beginning-of-macro)
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
                  (js3-bfy-beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun js3-bfy-re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (js3-bfy-re-search-forward regexp bound noerror (if count (- count) -1)))


(defun js3-bfy-looking-back (regexp)
  "This function returns t if regexp matches text before point, ending at point, and nil otherwise.

This function is similar to `looking-back' but ignores comments and strings"
  (save-excursion
    (let ((r (if (and (= ?\= (elt regexp (1- (length regexp))))
		      (= ?\\ (elt regexp (- (length regexp) 2))))
		 regexp
	       (concat regexp "\\="))))
      (numberp (js3-bfy-re-search-backward r (point-min) t)))))

(defun js3-bfy-looking-at (regexp)
  "This function returns t if regexp matches text after point, beginning at point, and nil otherwise.

This function is similar to `looking-at' but ignores comments and strings"
  (save-excursion
    (let ((r (if (and (= ?\= (elt regexp 1))
		      (= ?\\ (elt regexp 0)))
		 regexp
	       (concat "\\=" regexp))))
      (numberp (js3-bfy-re-search-forward r nil t)))))

(defun js3-bfy-looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at js3-bfy-indent-operator-re)
         (or (not (= (following-char) ?\:))
             (save-excursion
               (and (js3-bfy-re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (= (following-char) ?\?)))))))


(defun js3-bfy-continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js3-bfy-looking-at-operator-p)
        (and (js3-bfy-re-search-backward "\n" nil t)
             (progn
               (skip-chars-backward " \t")
               (or (bobp) (backward-char))
               (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (js3-bfy-looking-at-operator-p)
                    (and (progn (backward-char)
                                (not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun js3-bfy-end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
        (if (save-excursion
              (skip-chars-backward (concat js3-bfy-skip-newlines-re "}"))
              (looking-at (concat js3-bfy-skip-newlines-re "}")))
            (save-excursion
              (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
          (js3-bfy-re-search-backward "\\_<do\\_>" (point-at-bol) t)
          (or (looking-at "\\_<do\\_>")
              (let ((saved-indent (current-indentation)))
                (while (and (js3-bfy-re-search-backward "^\\s-*\\_<" nil t)
                            (/= (current-indentation) saved-indent)))
                (and (looking-at "\\s-*\\_<do\\_>")
                     (not (js3-bfy-re-search-forward
                           "\\_<while\\_>" (point-at-eol) t))
                     (= (current-indentation) saved-indent)))))))))


(defun js3-bfy-backward-whitespace ()
  "Helper function for `js3-bfy-proper-indentation'.
Skip backwards over whitespace and comments."
  (let ((rv nil))
    (when (js3-bfy-looking-back "[ \t\n]")
      (setq rv t)
      (js3-bfy-re-search-backward (concat "[^ \t\n]" js3-bfy-skip-newlines-re)
				  (point-min) t)
      (forward-char))
    rv))


(defun js3-bfy-backward-sexp ()
  "Helper function for `js3-bfy-proper-indentation'.
Go backwards over matched braces, rather than whole expressions.
Only skip over strings while looking for braces.
Functionality does not exactly match backward-sexp."
  (let ((brackets 0)
	(rv nil))
    (while (js3-bfy-looking-back (concat "[]})]" js3-bfy-skip-newlines-re))
      (setq rv t)
      (js3-bfy-re-search-backward (concat "[]})]"
					  js3-bfy-skip-newlines-re)
				  (point-min) t)
      (cond
       ((= (following-char) ?\])
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-bfy-re-search-backward "[][]" (point-min) t)
          (cond
           ((= (following-char) ?\])
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\[)
            (setq brackets (1- brackets))))))

       ((= (following-char) ?\})
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-bfy-re-search-backward "[}{]" (point-min) t)
          (cond
           ((= (following-char) ?\})
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\{)
            (setq brackets (1- brackets))))))

       ((= (following-char) ?\))
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-bfy-re-search-backward "[)(]" (point-min) t)
          (cond
           ((= (following-char) ?\))
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\()
            (setq brackets (1- brackets))))))))
    rv))


(defun js3-bfy-backward-clean ()
  "Helper function for `js3-bfy-proper-indentation'.
Calls js3-bfy-backward-sexp and js3-bfy-backward-whitespace until they are done."
  (let ((rv nil))
    (while (or (js3-bfy-backward-whitespace) (js3-bfy-backward-sexp))
      (setq rv t))
    rv))


(defun js3-bfy-ctrl-statement-indentation ()
  "Helper function for `js3-bfy-proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (= (following-char) ?\{))
                 (progn
                   (js3-bfy-re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at js3-bfy-possibly-braceless-keyword-re))
                 (not (js3-bfy-end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) js3-bfy-indent-level)))))

(defun js3-bfy-get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c js3-bfy-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun js3-bfy-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond

     ;;inside a comment - indent like c
     ((nth 4 parse-status)
      (js3-bfy-get-c-offset 'c (nth 8 parse-status)))

     ;;inside a string - indent to 0 since you can't do that.
     ((nth 8 parse-status) 0)

     ;;comma-first and operator-first
     ((or
       (= (following-char) ?\,)
       (looking-at js3-bfy-indent-operator-first-re))
      (let ((node (js3-bfy-node-at-point))
	    (char (following-char)))
	(let ((spos
	       (save-excursion
		 (cond

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-VAR (js3-bfy-node-type node))) ; var node
		   (goto-char (js3-bfy-node-abs node))
		   (+ (current-column) 2))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-RETURN (js3-bfy-node-type node)))
		   (goto-char (js3-bfy-node-abs node))
		   (+ (current-column) 5))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-ARRAYLIT (js3-bfy-node-type node)))
		   (goto-char (js3-bfy-node-abs node))
		   (js3-bfy-re-search-forward "[[]" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-OBJECTLIT (js3-bfy-node-type node)))
		   (goto-char (js3-bfy-node-abs node))
		   (js3-bfy-re-search-forward "{" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-FUNCTION (js3-bfy-node-type node)))
		   (goto-char (js3-bfy-node-abs node))
		   (js3-bfy-re-search-forward "(" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-CALL (js3-bfy-node-type node)))
		   (goto-char (js3-bfy-node-abs node))
		   (js3-bfy-re-search-forward "(" nil t)
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (>= (js3-bfy-node-type node) 9)
		    (<= (js3-bfy-node-type node) 18))    ; binary operators
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-COMMA (js3-bfy-node-type node))) ; comma operator
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-GETPROP (js3-bfy-node-type node))) ; dot operator
		   (goto-char (js3-bfy-node-abs node))
		   (if (js3-bfy-looking-at ".*\\..*")
		       (progn (js3-bfy-re-search-forward "\\." nil t)
			      (backward-char)
			      (current-column))
		     (+ (current-column)
			js3-bfy-expr-indent-offset js3-bfy-indent-level)))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (>= (js3-bfy-node-type node) 19)
		    (<= (js3-bfy-node-type node) 24))    ; 2-char binary operators
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 2)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= (js3-bfy-node-type node) 25))    ; 3-char binary operators
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 3)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (>= (js3-bfy-node-type node) 103)
		    (<= (js3-bfy-node-type node) 104))    ; logical and/or
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 2)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= js3-bfy-ASSIGN (js3-bfy-node-type node))) ; assignment
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (>= (js3-bfy-node-type node) 90)
		    (<= (js3-bfy-node-type node) 97))    ; assignment 2-char
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 2)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (>= (js3-bfy-node-type node) 98)
		    (<= (js3-bfy-node-type node) 99))    ; assignment 3-char
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 3)
		   (current-column))

		  ((and
		    node
		    (js3-bfy-node-type node)
		    (= (js3-bfy-node-type node) 100))    ; assignment 4-char
		   (goto-char (js3-bfy-node-abs node))
		   (when (= (preceding-char) ?\ )
		     (backward-char))
		   (backward-char 4)
		   (current-column))

		  (t
		   (js3-bfy-backward-clean)
		   (cond
		    ((js3-bfy-looking-back (concat "[,([{].*" js3-bfy-skip-newlines-re))
		     (js3-bfy-re-search-backward (concat "[,([{].*"
							 js3-bfy-skip-newlines-re)
						 (point-min) t)
		     (current-column))

		    ((js3-bfy-looking-back (concat "\\<var\\>.*"
						   js3-bfy-skip-newlines-re))
		     (js3-bfy-re-search-backward (concat "\\<var\\>.*"
							 js3-bfy-skip-newlines-re)
						 (point-min) t)
		     (+ (current-column) 2))

		    ((js3-bfy-looking-back (concat "\\<return\\>.*"
						   js3-bfy-skip-newlines-re))
		     (js3-bfy-re-search-backward (concat "\\<return\\>.*"
							 js3-bfy-skip-newlines-re)
						 (point-min) t)
		     (+ (current-column) 5))

		    ((js3-bfy-looking-back (concat "[,([{]\\(.\\|\n\\)*"
						   js3-bfy-skip-newlines-re))
		     (js3-bfy-re-search-backward (concat "[,([{]\\(.\\|\n\\)*"
							 js3-bfy-skip-newlines-re)
						 (point-min) t)
		     (current-column))

		    (t
		     nil)))))))
	  (if spos
	      spos
	    (+ js3-bfy-indent-level js3-bfy-expr-indent-offset)))))

     ;;indent control statement body without braces, if applicable
     ((js3-bfy-ctrl-statement-indentation))

     ;;c preprocessor - indent to 0
     ((eq (char-after) ?#) 0)

     ;;we're in a cpp macro - indent to 4 why not
     ((save-excursion (js3-bfy-beginning-of-macro)) 4)

     ;;inside a parenthetical grouping
     ((nth 1 parse-status)
      ;; A single closing paren/bracket should be indented at the
      ;; same level as the opening statement.
      (let ((same-indent-p (looking-at
                            "[]})]"))
            (continued-expr-p (js3-bfy-continued-expression-p)))
        (goto-char (nth 1 parse-status)) ; go to the opening char
        (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
            (progn ; nothing following the opening paren/bracket
              (skip-syntax-backward " ")
              (when (eq (char-before) ?\)) (backward-list)) ;skip arg list
	      (if (and (not js3-bfy-consistent-level-indent-inner-bracket)
		       (js3-bfy-looking-back (concat
					      "\\(:\\|,\\)"
					      js3-bfy-skip-newlines-re
					      "\\<function\\>"
					      js3-bfy-skip-newlines-re)))
		  (progn
		    (js3-bfy-re-search-backward (concat
						 "\\(:\\|,\\)"
						 js3-bfy-skip-newlines-re
						 "\\<function\\>"
						 js3-bfy-skip-newlines-re))
		    (js3-bfy-backward-clean)
		    (if (looking-back "[{[(,][^{[(,\n]*")
			(progn
			  (js3-bfy-re-search-backward "[{[(,][^{[(,\n]*")
			  (forward-char)
			  (js3-bfy-re-search-forward "[ \t]*"))
		      (progn
			(js3-bfy-re-search-backward "^")
			(back-to-indentation)
			(while (\= (char-after) ?f)
			  (forward-char)))))
		(back-to-indentation))
              (cond (same-indent-p
                     (current-column))
                    (continued-expr-p
                     (+ (current-column) (* 2 js3-bfy-indent-level)
                        js3-bfy-expr-indent-offset))
                    (t
                     (+ (current-column) js3-bfy-indent-level
                        (case (char-after (nth 1 parse-status))
                              (?\( js3-bfy-paren-indent-offset)
                              (?\[ js3-bfy-square-indent-offset)
                              (?\{ js3-bfy-curly-indent-offset))))))
          ;; If there is something following the opening
          ;; paren/bracket, everything else should be indented at
          ;; the same level.
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

     ;;in a continued expression not handled by earlier cases
     ((js3-bfy-continued-expression-p)
      (+ js3-bfy-indent-level js3-bfy-expr-indent-offset))

     ;;if none of these cases, then indent to 0
     (t 0))))

(defun js3-bfy-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (js3-bfy-reparse)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (js3-bfy-proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;; js3-bfy-indent.el ends here
;;; js3-bfy-foot.el

(eval-when-compile
  (require 'cl))

(defun js3-beautify ()
  "Beautify JavaScript code in the current buffer."
  (interactive)
  (js3-bfy-check-compat)
  (set-syntax-table js3-bfy-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq local-abbrev-table js3-bfy-abbrev-table)
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'js3-bfy-indent-line)
  (set (make-local-variable 'indent-tabs-mode) js3-bfy-indent-tabs-mode)

  (set (make-local-variable 'before-save-hook) #'js3-bfy-before-save)
  (set (make-local-variable 'next-error-function) #'js3-bfy-next-error)
  (set (make-local-variable 'beginning-of-defun-function) #'js3-bfy-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'js3-bfy-end-of-defun)
  ;; We un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; this is necessary to make `show-paren-function' work properly
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; needed for M-x rgrep, among other things
  (put 'js3-beautify 'find-tag-default-function #'js3-bfy-find-tag)

  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-buffer-is-cc-mode t
        c-comment-prefix-regexp js3-bfy-comment-prefix-regexp
        c-comment-start-regexp "/[*/]\\|\\s|"
        c-paragraph-start js3-bfy-paragraph-start
        c-paragraph-separate "$"
        comment-start-skip js3-bfy-comment-start-skip
        c-syntactic-ws-start js3-bfy-syntactic-ws-start
        c-syntactic-ws-end js3-bfy-syntactic-ws-end
        c-syntactic-eol js3-bfy-syntactic-eol)
  (if js3-bfy-emacs22
      (c-setup-paragraph-variables))

  (set (make-local-variable 'forward-sexp-function) #'js3-bfy-forward-sexp)
  (setq js3-bfy-buffer-dirty-p t
        js3-bfy-parsing nil)
  (js3-bfy-reparse)
  (save-excursion
    (setq js3-bfy-current-buffer (current-buffer))
    (js3-bfy-print-tree js3-bfy-ast)
    (set-buffer (get-buffer-create js3-bfy-temp-buffer))
    (mark-whole-buffer)
    (let ((min (point-min)) (max (- (point-max) 1)))
      (set-buffer js3-bfy-current-buffer)
      (erase-buffer)
      (insert-buffer-substring (get-buffer-create js3-bfy-temp-buffer) min max))
    (delete-trailing-whitespace)
    (js3-bfy-reparse)
    (goto-char (point-min))
    (while (= (forward-line) 0)
      (indent-according-to-mode)))
  (js3-bfy-exit))

(defun js3-bfy-check-compat ()
  "Signal an error if we can't run with this version of Emacs."
  (if (and js3-bfy-must-byte-compile
           (not (byte-code-function-p (symbol-function 'js3-beautify))))
      (error "You must byte-compile js3-beautify before using it."))
  (if (and (boundp 'running-xemacs) running-xemacs)
      (error "js3-beautify is not compatible with XEmacs"))
  (unless (>= emacs-major-version 21)
    (error "js3-beautify requires GNU Emacs version 21 or higher")))

(defun js3-bfy-exit ()
  (setq js3-bfy-curstr "")
  (setq js3-bfy-ast nil))

(defun js3-bfy-before-save ()
  "Clean up whitespace before saving file.
You can disable this by customizing `js3-bfy-cleanup-whitespace'."
  (when js3-bfy-cleanup-whitespace
    (let ((col (current-column)))
      (delete-trailing-whitespace)
      ;; don't change trailing whitespace on current line
      (unless (eq (current-column) col)
        (indent-to col)))))

(defsubst js3-bfy-reset-timer ()
  (if js3-bfy-parse-timer
      (cancel-timer js3-bfy-parse-timer))
  (setq js3-bfy-parsing nil)
  (setq js3-bfy-parse-timer
        (run-with-idle-timer js3-bfy-idle-timer-delay nil #'js3-bfy-reparse)))

(defun js3-bfy-reparse ()
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it."
  (let (time
        interrupted-p
        (js3-bfy-compiler-strict-mode js3-bfy-show-strict-warnings))
    (unless js3-bfy-parsing
      (setq js3-bfy-parsing t)
      (unwind-protect
	  (js3-bfy-with-unmodifying-text-property-changes
	   (setq js3-bfy-buffer-dirty-p nil)
	   (if js3-bfy-verbose-parse-p
	       (message "parsing..."))
	   (setq time
		 (js3-bfy-time
		  (setq interrupted-p
			(catch 'interrupted
			  (setq js3-bfy-ast (js3-bfy-parse))
			  ;; if parsing is interrupted, comments and regex
			  ;; literals stay ignored by `parse-partial-sexp'
			  nil))))
	   (if interrupted-p
	       (progn
		 ;; unfinished parse => try again
		 (setq js3-bfy-buffer-dirty-p t)
		 (js3-bfy-reset-timer))
	     (if js3-bfy-verbose-parse-p
		 (message "Parse time: %s" time))))
	;; finally
        (setq js3-bfy-parsing nil)
        (unless interrupted-p
          (setq js3-bfy-parse-timer nil))))))

(defun js3-bfy-remove-suppressed-warnings ()
  "Take suppressed warnings out of the AST warnings list.
This ensures that the counts and `next-error' are correct."
  (setf (js3-bfy-ast-root-warnings js3-bfy-ast)
        (js3-bfy-delete-if
         (lambda (e)
           (let ((key (caar e)))
             (or
              (and (not js3-bfy-strict-trailing-comma-warning)
                   (string-match "trailing\\.comma" key))
              (and (not js3-bfy-strict-cond-assign-warning)
                   (string= key "msg.equal.as.assign"))
              (and js3-bfy-missing-semi-one-line-override
                   (string= key "msg.missing.semi")
                   (let* ((beg (second e))
                          (node (js3-bfy-node-at-point beg))
                          (fn (js3-bfy-find-parent-fn node))
                          (body (and fn (js3-bfy-function-node-body fn)))
                          (lc (and body (js3-bfy-node-abs-pos body)))
                          (rc (and lc (+ lc (js3-bfy-node-len body)))))
                     (and fn
                          (or (null body)
                              (save-excursion
                                (goto-char beg)
                                (and (js3-bfy-same-line lc)
                                     (js3-bfy-same-line rc))))))))))
         (js3-bfy-ast-root-warnings js3-bfy-ast))))

(defun js3-bfy-echo-error (old-point new-point)
  "Called by point-motion hooks."
  (let ((msg (get-text-property new-point 'help-echo)))
    (if msg
        (message msg))))

(defalias #'js3-bfy-echo-help #'js3-bfy-echo-error)

(defun js3-bfy-beginning-of-line ()
  "Toggles point between bol and first non-whitespace char in line.
Also moves past comment delimiters when inside comments."
  (let (node beg)
    (cond
     ((bolp)
      (back-to-indentation))
     ((looking-at "//")
      (skip-chars-forward "/ \t"))
     ((and (eq (char-after) ?*)
           (setq node (js3-bfy-comment-at-point))
           (memq (js3-bfy-comment-node-format node) '(jsdoc block))
           (save-excursion
             (skip-chars-backward " \t")
             (bolp)))
      (skip-chars-forward "\* \t"))
     (t
      (goto-char (point-at-bol))))))

(defun js3-bfy-end-of-line ()
  "Toggles point between eol and last non-whitespace char in line."
  (if (eolp)
      (skip-chars-backward " \t")
    (goto-char (point-at-eol))))

(defsubst js3-bfy-inside-string ()
  "Return non-nil if inside a string.
Actually returns the quote character that begins the string."
  (let ((parse-state (save-excursion
                       (parse-partial-sexp (point-min) (point)))))
    (nth 3 parse-state)))

(defsubst js3-bfy-inside-comment-or-string ()
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

(defun js3-bfy-wait-for-parse (callback)
  "Invoke CALLBACK when parsing is finished.
If parsing is already finished, calls CALLBACK immediately."
  (if (not js3-bfy-buffer-dirty-p)
      (funcall callback)
    (push callback js3-bfy-pending-parse-callbacks)
    (add-hook 'js3-bfy-parse-finished-hook #'js3-bfy-parse-finished)))

(defun js3-bfy-parse-finished ()
  "Invoke callbacks in `js3-bfy-pending-parse-callbacks'."
  ;; We can't let errors propagate up, since it prevents the
  ;; `js3-bfy-parse' method from completing normally and returning
  ;; the ast, which makes things mysteriously not work right.
  (unwind-protect
      (dolist (cb js3-bfy-pending-parse-callbacks)
        (condition-case err
            (funcall cb)
          (error (message "%s" err))))
    (setq js3-bfy-pending-parse-callbacks nil)))

(defun js3-bfy-function-at-point (&optional pos)
  "Return the innermost function node enclosing current point.
Returns nil if point is not in a function."
  (let ((node (js3-bfy-node-at-point pos)))
    (while (and node (not (js3-bfy-function-node-p node)))
      (setq node (js3-bfy-node-parent node)))
    (if (js3-bfy-function-node-p node)
        node)))

(defun js3-beautify-customize ()
  (interactive)
  (customize-group 'js3-bfy))

(defun js3-bfy-forward-sexp (&optional arg)
  "Move forward across one statement or balanced expression.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (setq arg (or arg 1))
  (if js3-bfy-buffer-dirty-p
      (js3-bfy-wait-for-parse #'js3-bfy-forward-sexp))
  (let (node end (start (point)))
    (cond
     ;; backward-sexp
     ;; could probably make this "better" for some cases:
     ;;  - if in statement block (e.g. function body), go to parent
     ;;  - infix exprs like (foo in bar) - maybe go to beginning
     ;;    of infix expr if in the right-side expression?
     ((and arg (minusp arg))
      (dotimes (i (- arg))
        (js3-bfy-backward-sws)
        (forward-char -1)  ; enter the node we backed up to
        (setq node (js3-bfy-node-at-point (point) t))
        (goto-char (if node
                       (js3-bfy-node-abs-pos node)
                     (point-min)))))
     (t
      ;; forward-sexp
      (js3-bfy-forward-sws)
      (dotimes (i arg)
        (js3-bfy-forward-sws)
        (setq node (js3-bfy-node-at-point (point) t)
              end (if node (+ (js3-bfy-node-abs-pos node)
                              (js3-bfy-node-len node))))
        (goto-char (or end (point-max))))))))

(defun js3-bfy-find-tag ()
  "Replacement for `find-tag-default'.
`find-tag-default' returns a ridiculous answer inside comments."
  (let (beg end)
    (js3-bfy-with-underscore-as-word-syntax
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

(defun js3-bfy-forward-sibling ()
  "Move to the end of the sibling following point in parent.
Returns non-nil if successful, or nil if there was no following sibling."
  (let* ((node (js3-bfy-node-at-point))
         (parent (js3-bfy-find-enclosing-fn node))
         sib)
    (when (setq sib (js3-bfy-node-find-child-after (point) parent))
      (goto-char (+ (js3-bfy-node-abs-pos sib)
                    (js3-bfy-node-len sib))))))

(defun js3-bfy-backward-sibling ()
  "Move to the beginning of the sibling node preceding point in parent.
Parent is defined as the enclosing script or function."
  (let* ((node (js3-bfy-node-at-point))
         (parent (js3-bfy-find-enclosing-fn node))
         sib)
    (when (setq sib (js3-bfy-node-find-child-before (point) parent))
      (goto-char (js3-bfy-node-abs-pos sib)))))

(defun js3-bfy-beginning-of-defun ()
  "Go to line on which current function starts, and return non-nil.
If we're not in a function, go to beginning of previous script-level element."
  (let ((parent (js3-bfy-node-parent-script-or-fn (js3-bfy-node-at-point)))
        pos sib)
    (cond
     ((and (js3-bfy-function-node-p parent)
           (not (eq (point) (setq pos (js3-bfy-node-abs-pos parent)))))
      (goto-char pos))
     (t
      (js3-bfy-backward-sibling)))))

(defun js3-bfy-end-of-defun ()
  "Go to the char after the last position of the current function.
If we're not in a function, skips over the next script-level element."
  (let ((parent (js3-bfy-node-parent-script-or-fn (js3-bfy-node-at-point))))
    (if (not (js3-bfy-function-node-p parent))
        ;; punt:  skip over next script-level element beyond point
        (js3-bfy-forward-sibling)
      (goto-char (+ 1 (+ (js3-bfy-node-abs-pos parent)
                         (js3-bfy-node-len parent)))))))

(defun js3-bfy-mark-defun (&optional allow-extend)
  "Put mark at end of this function, point at beginning.
The function marked is the one that contains point."
  (let (extended)
    (when (and allow-extend
               (or (and (eq last-command this-command) (mark t))
                   (and transient-mark-mode mark-active)))
      (let ((sib (save-excursion
                   (goto-char (mark))
                   (if (js3-bfy-forward-sibling)
                       (point))))
            node)
        (if sib
            (progn
              (set-mark sib)
              (setq extended t))
          ;; no more siblings - try extending to enclosing node
          (goto-char (mark t)))))
    (when (not extended)
      (let ((node (js3-bfy-node-at-point (point) t)) ; skip comments
            ast fn stmt parent beg end)
        (when (js3-bfy-ast-root-p node)
          (setq ast node
                node (or (js3-bfy-node-find-child-after (point) node)
                         (js3-bfy-node-find-child-before (point) node))))
        ;; only mark whole buffer if we can't find any children
        (if (null node)
            (setq node ast))
        (if (js3-bfy-function-node-p node)
            (setq parent node)
          (setq fn (js3-bfy-find-enclosing-fn node)
                stmt (if (or (null fn)
                             (js3-bfy-ast-root-p fn))
                         (js3-bfy-find-first-stmt node))
                parent (or stmt fn)))
        (setq beg (js3-bfy-node-abs-pos parent)
              end (+ beg (js3-bfy-node-len parent)))
        (push-mark beg)
        (goto-char end)
        (exchange-point-and-mark)))))

(defun js3-bfy-narrow-to-defun ()
  "Narrow to the function enclosing point."
  (let* ((node (js3-bfy-node-at-point (point) t))  ; skip comments
         (fn (if (js3-bfy-script-node-p node)
                 node
               (js3-bfy-find-enclosing-fn node)))
         (beg (js3-bfy-node-abs-pos fn)))
    (unless (js3-bfy-ast-root-p fn)
      (narrow-to-region beg (+ beg (js3-bfy-node-len fn))))))

(defalias 'js3r 'js3-bfy-reset)

(provide 'js3-beautify)

;;; js3-bfy-foot.el ends here

;;; js3-bfy.el ends here
