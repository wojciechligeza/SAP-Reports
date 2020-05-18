*&---------------------------------------------------------------------*
*&  Include           Z7_WL_TOP
*&---------------------------------------------------------------------*
TABLES vbak.
DATA: go_model    TYPE REF TO z7_wl_model,
      go_view     TYPE REF TO z7_wl_view,
      gt_order    TYPE rseloption,
      gt_category TYPE rseloption,
      gt_type     TYPE rseloption,
      gt_date     TYPE rseloption.