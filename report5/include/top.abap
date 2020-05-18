*&---------------------------------------------------------------------*
*&  Include           Z5_WL_TOP
*&---------------------------------------------------------------------*
TABLES z03_inv_header.
DATA: go_alv_provider TYPE REF TO z5_alv_provider,
      go_container    TYPE REF TO cl_gui_custom_container,
      gv_okcode       TYPE syucomm,
      gt_headerid     TYPE rseloption,
      gt_date         TYPE rseloption,
      gt_currency     TYPE rseloption.