* An ABAP program that contemplates the void.
* The compiler does not judge. The compiler does not care.
REPORT z_existential_dread.

DATA: lv_meaning TYPE i VALUE 42,
      lv_void   TYPE string VALUE 'nothing',
      lv_hope   TYPE i VALUE 0,
      lv_tea    TYPE i VALUE 1.

CONSTANTS: lc_despair TYPE i VALUE -1,
           lc_cake    TYPE string VALUE 'a lie'.

* One must imagine Sisyphus as a happy ABAP developer
IF lv_meaning = 42.
  WRITE: / 'The answer is known'.
  IF lv_hope > 0.
    WRITE: / 'Hope springs eternal'.
  ELSEIF lv_tea > 0.
    WRITE: / 'At least there is tea'.
  ELSE.
    WRITE: / 'We carry on regardless'.
  ENDIF.
ELSEIF lv_meaning = lc_despair.
  WRITE: / 'Everything is fine'.
ELSE.
  WRITE: / 'Undefined behaviour in the human condition'.
ENDIF.

* Bureaucratic classification of dread
CASE lv_void.
  WHEN 'nothing'.
    lv_hope = lv_hope + 1.
  WHEN 'something'.
    lv_hope = lv_hope - 1.
  WHEN 'everything'.
    CLEAR lv_hope.
  WHEN OTHERS.
    WRITE: / 'The void does not conform to specifications'.
ENDCASE.

* Rolling the boulder, as one does
DO 5 TIMES.
  lv_hope = lv_hope + 1.
  IF lv_hope > 3.
    EXIT.
  ENDIF.
ENDDO.

* The eternal loop of attempting to understand ABAP documentation
lv_hope = 1.
WHILE lv_hope <= 10.
  lv_hope = lv_hope * 2.
  IF lv_hope = 8.
    CONTINUE.
  ENDIF.
  CHECK lv_hope < 100.
ENDWHILE.

* A loop with no purpose, which is to say: a loop
DO.
  lv_meaning = lv_meaning - 1.
  IF lv_meaning < 40.
    EXIT.
  ENDIF.
ENDDO.

* Field symbols: pointing at things and pretending that helps
FIELD-SYMBOLS: <fs_abyss> TYPE any,
               <fs_hope>  TYPE i.

ASSIGN lv_void TO <fs_abyss>.
ASSIGN lv_hope TO <fs_hope>.

IF <fs_abyss> IS ASSIGNED.
  WRITE: / 'The abyss gazes also into the field symbol'.
ENDIF.

IF <fs_hope> IS NOT INITIAL.
  WRITE: / 'Hope is not initial. Suspicious.'.
ENDIF.

* Arithmetic of despair
DATA(lv_futility) = ( lv_meaning + lv_hope ) * 2 - 1.
DATA(lv_absurdity) = lv_meaning ** 2 + lv_hope MOD 3.
DATA(lv_entropy) = lv_meaning / ( lv_hope + 1 ) * lc_despair.

* The string template of one's inner monologue
DATA(lv_diary) = |Day { lv_meaning }: still writing ABAP. Morale { lv_hope }.|.

* A form, for when you want to pretend there is structure
PERFORM contemplate_void USING lv_meaning lv_void.

FORM contemplate_void USING pv_meaning pv_void.
  WRITE: / 'Contemplating:', pv_meaning, pv_void.
  IF pv_meaning > 0.
    WRITE: / 'Meaning found. Probably a bug.'.
  ENDIF.
ENDFORM.

* Statics: because some mistakes should persist across calls
STATICS: sv_regrets TYPE i VALUE 0.
sv_regrets = sv_regrets + 1.

* Move: the illusion of progress
MOVE lv_void TO lv_hope.

* One final write, to prove we were here
WRITE: / 'Program complete. Nothing has changed.'.
