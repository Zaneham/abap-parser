* Object-oriented programming, or: the bureaucratisation of despair.
* Now with classes, because apparently procedures weren't enough paperwork.
REPORT z_tea_ceremony.

INTERFACE lif_beverage.
  METHODS brew.
  METHODS pour.
  DATA mv_temperature TYPE i.
ENDINTERFACE.

CLASS lcl_kettle DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS boil.
    DATA mv_water_level TYPE i.
  PRIVATE SECTION.
    DATA mv_is_on TYPE i.
    METHODS check_element.
ENDCLASS.

CLASS lcl_kettle IMPLEMENTATION.
  METHOD constructor.
    mv_water_level = 0.
    mv_is_on = 0.
  ENDMETHOD.

  METHOD boil.
    IF mv_water_level <= 0.
      WRITE: / 'Cannot boil nothing. A metaphor, perhaps.'.
      RETURN.
    ENDIF.
    mv_is_on = 1.
    WRITE: / 'The kettle contemplates its purpose.'.
  ENDMETHOD.

  METHOD check_element.
    IF mv_is_on = 0.
      WRITE: / 'The element is cold. Like British hospitality.'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tea DEFINITION INHERITING FROM lcl_kettle.
  PUBLIC SECTION.
    METHODS steep.
    DATA: mv_brew_time TYPE i,
          mv_variety   TYPE string.
  PROTECTED SECTION.
    DATA mv_milk_first TYPE i.
ENDCLASS.

CLASS lcl_tea IMPLEMENTATION.
  METHOD steep.
    mv_brew_time = 4.
    WRITE: / 'Steeping for', mv_brew_time, 'minutes.'.
    WRITE: / 'Patience is a virtue. ABAP developers have many virtues.'.
  ENDMETHOD.
ENDCLASS.

* Exception handling: the art of catching things that should not
* have been thrown in the first place
DATA: lo_kettle TYPE REF TO lcl_kettle,
      lv_crisis TYPE string.

TRY.
    CREATE OBJECT lo_kettle.
    lo_kettle->mv_water_level = 500.
    lo_kettle->boil( ).

    " Nested try, because one layer of error handling is never enough
    TRY.
        lo_kettle->check_element( ).
      CATCH cx_root INTO lv_crisis.
        WRITE: / 'The element has failed. As have we all.'.
    ENDTRY.

  CATCH cx_root INTO lv_crisis.
    WRITE: / 'Something has gone wrong with the tea.'.
    WRITE: / 'This is a national emergency.'.
  CLEANUP.
    CLEAR lo_kettle.
    WRITE: / 'Cleaning up. The kettle is free now.'.
ENDTRY.

* Method chaining via variables, because ABAP doesn't do fluent interfaces
DATA lo_tea TYPE REF TO lcl_tea.
CREATE OBJECT lo_tea.
lo_tea->mv_variety = 'Earl Grey'.
lo_tea->mv_water_level = 250.
lo_tea->boil( ).
lo_tea->steep( ).

* The CASE for different teas
CASE lo_tea->mv_variety.
  WHEN 'Earl Grey'.
    WRITE: / 'Bergamot detected. Civilisation maintained.'.
  WHEN 'Builder''s'.
    WRITE: / 'Strong enough to stand a spoon in.'.
  WHEN OTHERS.
    WRITE: / 'Unrecognised variety. Defaulting to PG Tips.'.
ENDCASE.

* Constructor expressions: the 7.40 modernisation that made
* ABAP look almost like a programming language
DATA(lv_judgement) = COND string(
  WHEN lo_tea->mv_brew_time < 3 THEN 'Too weak. Barely tea.'
  WHEN lo_tea->mv_brew_time > 5 THEN 'Stewed. A war crime.'
  ELSE 'Acceptable. Carry on.' ).

DATA(lv_verdict) = SWITCH string( lo_tea->mv_milk_first
  WHEN 0 THEN 'Milk after. Correct.'
  WHEN 1 THEN 'Milk first. Barbarian.' ).

WRITE: / lv_judgement.
WRITE: / lv_verdict.

* Templates for the tea report
DATA(lv_report) = |Tea: { lo_tea->mv_variety }, Water: { lo_tea->mv_water_level }ml|.
WRITE: / lv_report.

* RAISE, because sometimes one must protest
IF lo_tea->mv_brew_time < 1.
  RAISE cx_sy_arithmetic_error.
ENDIF.

WRITE: / 'The ceremony is complete. God save the King.'.
