# abap-parse

Hand-rolled ABAP lexer and recursive descent parser in OCaml. No parser generators, no reserved keywords.

ABAP is what happens when you give COBOL a German passport and forty years of backwards compatibility. Keywords aren't reserved, periods end statements, colons introduce chains, and whitespace before a parenthesis is the difference between calling a function and merely thinking about it. This parser handles all of that.

**[Try it in the browser](https://zaneham.github.io/abap-parse/)** - paste ABAP, get an AST.

## What it does

```
ABAP source  →  Token stream  →  AST  →  S-expression output
```

```abap
DATA: lv_name TYPE string,
      lv_age  TYPE i VALUE 25.

IF lv_age > 18.
  WRITE: / lv_name, lv_age.
ENDIF.

DATA(lv_msg) = |Hello { lv_name }, age { lv_age }|.
```

```
(data LV_NAME : (type STRING))
(data LV_AGE : (type I) (value 25))
(if (> LV_AGE 18)
  (write / LV_NAME LV_AGE))
(inline-data LV_MSG = (template "Hello " {LV_NAME} ", age " {LV_AGE}))
```

## Install

### Pre-built binaries

Grab one from [Releases](https://github.com/Zaneham/abap-parse/releases). Single executable, nothing else needed.

### From source

You'll need OCaml 5.x and dune.

```sh
git clone https://github.com/Zaneham/abap-parse.git
cd abap-parse
opam exec -- dune build
```

## Usage

```sh
abap-parse --parse file.abap       # parse and print AST
abap-parse --lex file.abap         # dump token stream
```

## What it parses

### Declarations

DATA, CONSTANTS, TYPES, STATICS, FIELD-SYMBOLS. Single and chained (colon) forms. Structured types with BEGIN OF / END OF. Inline declarations like `DATA(x) = expr`.

### Expressions

Pratt expression parser, 10 precedence levels.

- Arithmetic: `+ - * / MOD **` (exponentiation is right-associative)
- String comparisons: `&& CO CN CA NA CS NS CP NP`
- Boolean: `AND OR NOT`
- Comparison: `= <> < > <= >= EQ NE LT GT LE GE IN BETWEEN`
- Member access: `-> => ~` with automatic method call detection
- Constructor expressions (7.40+): `NEW VALUE CONV CAST COND SWITCH REF CORRESPONDING`
- IS tests: `IS INITIAL / BOUND / ASSIGNED / SUPPLIED / INSTANCE OF`
- String templates: `|text { expr } text|`
- Table expressions: `itab[ key ]`

### Statements

IF/ELSEIF/ELSE, CASE/WHEN, DO, WHILE, LOOP AT, CHECK, CONTINUE, EXIT, RETURN, STOP, WRITE, CLEAR, FREE, ASSIGN TO, APPEND, INSERT, DELETE, SORT, READ, MODIFY, MOVE, MOVE-CORRESPONDING, FORM/PERFORM, REPORT, PROGRAM, INCLUDE.

### OO ABAP

CLASS DEFINITION/IMPLEMENTATION with inheritance, METHOD, INTERFACE, TRY/CATCH/CLEANUP, RAISE, CREATE OBJECT, CREATE DATA, CALL FUNCTION, CALL METHOD.

### Graceful degradation

Anything the parser doesn't recognise gets collected into `SUnknown` nodes instead of crashing. It always produces output.

## How it works

The **lexer** (`lib/lexer.ml`) is hand-rolled with a mode stack for string templates. It returns `(token, location, whitespace_before)` triples, because in ABAP that whitespace flag is load-bearing.

The **parser** (`lib/parser.ml`) is recursive descent with Pratt expression parsing. It disambiguates keywords from identifiers by context, and uses whitespace before parentheses to tell function calls apart from grouping.

The **AST** (`lib/ast.ml`) carries source locations on every node. The **printer** (`lib/printer.ml`) dumps it as S-expressions, mostly for staring at in disbelief.

## License

Apache 2.0. See [LICENSE](LICENSE).
