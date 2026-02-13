* A program about forms. Forms about programs. Programs about forms about programs.
* The snake eats its own tail and files it in triplicate.
REPORT z_bureaucracy.

* Structured data: because even chaos needs a filing system
DATA: BEGIN OF ls_form_27b,
        applicant   TYPE string,
        reason      TYPE string,
        stamps      TYPE i,
        approved    TYPE i,
      END OF ls_form_27b.

DATA: BEGIN OF ls_receipt,
        form_id    TYPE string,
        timestamp  TYPE i,
      END OF ls_receipt.

TYPES: BEGIN OF ty_complaint,
         subject  TYPE string,
         urgency  TYPE i,
         ignored  TYPE i,
       END OF ty_complaint.

DATA: lt_complaints TYPE TABLE OF ty_complaint,
      ls_complaint  TYPE ty_complaint.

CONSTANTS: lc_max_stamps  TYPE i VALUE 7,
           lc_min_reasons TYPE i VALUE 3.

* Fill in Form 27B/6, as required by subsection 4, paragraph 12
ls_form_27b-applicant = 'A. Buttle'.
ls_form_27b-reason = 'Information retrieval'.
ls_form_27b-stamps = 0.
ls_form_27b-approved = 0.

* The stamp accumulation loop
DO lc_max_stamps TIMES.
  ls_form_27b-stamps = ls_form_27b-stamps + 1.
  IF ls_form_27b-stamps MOD 2 = 0.
    WRITE: / 'Stamp', ls_form_27b-stamps, 'applied (even stamps are suspicious)'.
  ENDIF.
ENDDO.

* Approval logic: a flowchart of human suffering
IF ls_form_27b-stamps >= lc_max_stamps.
  IF ls_form_27b-reason CS 'retrieval'.
    IF ls_form_27b-applicant CP 'A.*'.
      ls_form_27b-approved = 1.
      WRITE: / 'Form 27B/6: APPROVED (by accident)'.
    ELSE.
      WRITE: / 'Form 27B/6: DENIED (wrong initial)'.
    ENDIF.
  ELSE.
    WRITE: / 'Form 27B/6: DENIED (insufficient reason)'.
  ENDIF.
ELSE.
  WRITE: / 'Form 27B/6: DENIED (insufficient stamps)'.
ENDIF.

* Constructor expressions for generating complaints
DATA(lv_header) = |Form { ls_form_27b-stamps }/{ lc_max_stamps } stamps applied|.
WRITE: / lv_header.

DATA(lv_assessment) = COND string(
  WHEN ls_form_27b-approved = 1 THEN 'Approved. Prepare for audit.'
  WHEN ls_form_27b-stamps > 5 THEN 'Nearly. Try again Monday.'
  ELSE 'Denied. See Form 27B/7 for appeal process.' ).

* Filing complaints
ls_complaint-subject = 'The queue'.
ls_complaint-urgency = 3.
ls_complaint-ignored = 1.
APPEND ls_complaint TO lt_complaints.

ls_complaint-subject = 'The other queue'.
ls_complaint-urgency = 5.
ls_complaint-ignored = 1.
APPEND ls_complaint TO lt_complaints.

ls_complaint-subject = 'Being told to join the first queue again'.
ls_complaint-urgency = 9.
ls_complaint-ignored = 1.
APPEND ls_complaint TO lt_complaints.

* Process complaints (ignore them, obviously)
LOOP AT lt_complaints.
  WRITE: / 'Complaint noted and filed under: ignored'.
ENDLOOP.

SORT lt_complaints.
DELETE lt_complaints.

* The VALUE constructor: building things from nothing, an ABAP speciality
DATA(lt_procedures) = VALUE #(
  ( subject = 'Complain' urgency = 1 ignored = 1 )
  ( subject = 'Appeal' urgency = 2 ignored = 1 )
  ( subject = 'Complain about appeal' urgency = 3 ignored = 1 ) ).

* Subroutines: for when you want to put a fence around your confusion
PERFORM file_form USING ls_form_27b-applicant ls_form_27b-reason.
PERFORM stamp_form USING ls_form_27b-stamps.

FORM file_form USING pv_applicant pv_reason.
  WRITE: / 'Filing for:', pv_applicant.
  WRITE: / 'Reason:', pv_reason.
  WRITE: / 'Please allow 6-8 weeks for processing.'.
ENDFORM.

FORM stamp_form USING pv_stamps.
  DATA lv_verdict TYPE string.
  IF pv_stamps >= lc_max_stamps.
    lv_verdict = 'Sufficient'.
  ELSE.
    lv_verdict = 'Insufficient. Return to window 4.'.
  ENDIF.
  WRITE: / 'Stamps:', pv_stamps, '-', lv_verdict.
ENDFORM.

* Complex arithmetic: the maths of bureaucracy
DATA: lv_wait_time   TYPE i,
      lv_queue_pos   TYPE i VALUE 47,
      lv_windows     TYPE i VALUE 3,
      lv_lunch_break TYPE i VALUE 1.

lv_wait_time = ( lv_queue_pos / ( lv_windows - lv_lunch_break ) ) ** 2 + 15.

DATA(lv_eta) = |Estimated wait: { lv_wait_time } minutes. Sincerely, Management.|.
WRITE: / lv_eta.

* Between: a surprisingly useful operator for a language with so few of them
IF lv_queue_pos BETWEEN 1 AND 50.
  WRITE: / 'You are in the queue. This is your life now.'.
ENDIF.

* Free everything. Liberation at last.
FREE lt_complaints.
CLEAR ls_form_27b.

WRITE: / 'All forms processed. The queue is empty. Nobody is happy.'.
