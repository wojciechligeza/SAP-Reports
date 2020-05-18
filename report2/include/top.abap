*&---------------------------------------------------------------------*
*&  Include           Z02_LIGEZA_REPORT_TOP
*&---------------------------------------------------------------------*
TABLES zusersdb.
DATA:
      go_alv_provider TYPE REF TO z02_alv_provider,

      gt_salary TYPE rseloption,
      gt_curr   TYPE rseloption,
      gt_workp  TYPE rseloption.