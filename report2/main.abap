REPORT z02_ligeza_report MESSAGE-ID z02_ligeza_messages.

INCLUDE z02_ligeza_report_top.
INCLUDE z02_ligeza_report_sel.
INCLUDE z02_ligeza_report_lcl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_curr-low.
  lcl=>help_currency( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_workp-low.
  lcl=>help_workposition( ).

START-OF-SELECTION.
  lcl=>instantiate_alv_provider( ).
  lcl=>run_alv_provider( ).