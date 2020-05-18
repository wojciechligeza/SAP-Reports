*&---------------------------------------------------------------------*
*&  Include           Z5_WL_EVENTS
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_id-low.
  lcl=>help_parameter_id( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_curr-low.
  lcl=>help_parameter_curr( ).