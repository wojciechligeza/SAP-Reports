*&---------------------------------------------------------------------*
*&  Include           Z_REPORT1_LIGEZA_SEL
*&---------------------------------------------------------------------*

  "$. Region Select a file from pop-up window( file_open_dialog function )
  SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
  PARAMETERS p_path TYPE string.
  SELECTION-SCREEN END OF BLOCK a.
  "$. Endregion Select a file from pop-up window( file_open_dialog function )