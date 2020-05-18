*&---------------------------------------------------------------------*
*&  Include           Z3_INV_WL_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
  PARAMETER:
    p_path TYPE string,
    p_test RADIOBUTTON GROUP mode,
    p_real RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK a.