* This is a comment
REPORT ztest.

DATA: lv_name TYPE string,
      lv_age  TYPE i VALUE 25.

lv_name = 'Hello World'.

IF lv_age > 18.
  WRITE: / lv_name, lv_age.  "inline comment
ENDIF.

DATA(lv_msg) = |Hello { lv_name }, age { lv_age }|.

FIELD-SYMBOLS <fs> TYPE any.
ASSIGN lv_name TO <fs>.

cl_class=>static_method( ).
obj->instance_method( ).
result = a + b * c ** 2.
text = 'can''t stop' && ` won't stop`.

* New lexer features
/SAPTOOLS/CL_UTILS=>do_something( ).
lr_data ?= lr_object.
##NO_TEXT
if_intf~method( ).
sy-subrc = 0.
