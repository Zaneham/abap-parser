* A comprehensive test of expression parsing.
* Every operator, every precedence level, every constructor.
* The Rosetta Stone of ABAP expressions, if the Rosetta Stone
* had been written by a committee and nobody could agree on the font.
REPORT z_expressions.

DATA: a TYPE i VALUE 10,
      b TYPE i VALUE 3,
      c TYPE i VALUE 2,
      d TYPE string VALUE 'hello',
      e TYPE string VALUE 'world'.

* Arithmetic precedence: the sacred hierarchy
DATA(r1) = a + b * c.
DATA(r2) = ( a + b ) * c.
DATA(r3) = a ** b ** c.
DATA(r4) = a * b + c * a - b.
DATA(r5) = a MOD b + c DIV a.
DATA(r6) = -a + b.
DATA(r7) = a - -b.

* String operations: comparing strings, a task that brings
* neither joy nor enlightenment
IF d CO 'helo'.
  WRITE: / 'CO: all characters found'.
ENDIF.
IF d CN 'xyz'.
  WRITE: / 'CN: not all characters match'.
ENDIF.
IF d CA 'aeiou'.
  WRITE: / 'CA: at least one vowel'.
ENDIF.
IF d NA 'xyz'.
  WRITE: / 'NA: none of those, thankfully'.
ENDIF.
IF d CS 'ell'.
  WRITE: / 'CS: contains substring'.
ENDIF.
IF d NS 'xyz'.
  WRITE: / 'NS: does not contain'.
ENDIF.
IF d CP 'h*'.
  WRITE: / 'CP: matches pattern'.
ENDIF.
IF d NP 'z*'.
  WRITE: / 'NP: does not match pattern'.
ENDIF.

* Boolean operators
IF a > 5 AND b < 10.
  WRITE: / 'AND works'.
ENDIF.
IF a > 100 OR b < 10.
  WRITE: / 'OR works'.
ENDIF.
IF NOT a > 100.
  WRITE: / 'NOT works'.
ENDIF.
IF a > 5 AND ( b < 10 OR c = 2 ).
  WRITE: / 'Mixed boolean with parens'.
ENDIF.

* Comparison bonanza
IF a EQ 10.
  WRITE: / 'EQ'.
ENDIF.
IF a NE 11.
  WRITE: / 'NE'.
ENDIF.
IF a GT 9.
  WRITE: / 'GT'.
ENDIF.
IF a LT 11.
  WRITE: / 'LT'.
ENDIF.
IF a GE 10.
  WRITE: / 'GE'.
ENDIF.
IF a LE 10.
  WRITE: / 'LE'.
ENDIF.

* IN and BETWEEN
IF a BETWEEN 1 AND 20.
  WRITE: / 'BETWEEN works'.
ENDIF.

* String concatenation: the ampersand and
DATA(lv_cat) = d && ' ' && e.
DATA(lv_cat2) = 'one' && ` two` && | three|.

* IS tests: the existential questions of ABAP
DATA lr_ref TYPE REF TO lcl_thing.

IF lr_ref IS INITIAL.
  WRITE: / 'IS INITIAL'.
ENDIF.
IF lr_ref IS NOT INITIAL.
  WRITE: / 'IS NOT INITIAL'.
ENDIF.

FIELD-SYMBOLS <fs> TYPE any.
IF <fs> IS ASSIGNED.
  WRITE: / 'IS ASSIGNED'.
ENDIF.
IF <fs> IS NOT ASSIGNED.
  WRITE: / 'IS NOT ASSIGNED'.
ENDIF.

* COND: the ternary operator ABAP always wanted
DATA(lv_cond) = COND string(
  WHEN a > 10 THEN 'big'
  WHEN a > 5 THEN 'medium'
  ELSE 'small' ).

* SWITCH: the other ternary operator ABAP always wanted
DATA(lv_switch) = SWITCH string( b
  WHEN 1 THEN 'one'
  WHEN 2 THEN 'two'
  WHEN 3 THEN 'three' ).

* VALUE: building tables from thin air and wishful thinking
TYPES: BEGIN OF ty_pair,
         key TYPE string,
         val TYPE i,
       END OF ty_pair.

DATA(lt_pairs) = VALUE #(
  ( key = 'alpha'   val = 1 )
  ( key = 'beta'    val = 2 )
  ( key = 'gamma'   val = 3 ) ).

* Constructor expressions with #
DATA(ls_pair) = VALUE ty_pair( key = 'delta' val = 4 ).

* Template expressions: the jewel in the 7.40 crown
DATA(lv_tmpl1) = |Simple: { a }|.
DATA(lv_tmpl2) = |Result: { a + b * c }|.
DATA(lv_tmpl3) = |Name: { d }, Value: { e }|.

* Member access chain
DATA lo_obj TYPE REF TO cl_something.
DATA(lv_chain1) = lo_obj->method1( ).
DATA(lv_chain2) = cl_factory=>create( ).
DATA(lv_chain3) = lo_obj->comp~method( ).

* Table expressions
DATA lt_table TYPE TABLE OF i.
DATA(lv_elem) = lt_table[ 1 ].

WRITE: / 'Expression gauntlet complete.'.
