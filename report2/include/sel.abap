*&---------------------------------------------------------------------*
*&  Include           Z02_LIGEZA_REPORT_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  p_salary FOR zusersdb-salary,
  p_curr   FOR zusersdb-currency NO INTERVALS,
  p_workp  FOR zusersdb-workposition NO INTERVALS.
SELECTION-SCREEN END OF BLOCK a.