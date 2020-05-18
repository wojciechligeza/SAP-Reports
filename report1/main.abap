REPORT z_report1_ligeza.

INCLUDE z_report1_ligeza_top.
INCLUDE z_report1_ligeza_sel.
INCLUDE z_report1_ligeza_lcl.

INITIALIZATION.
  lcl=>instantiate_file_provider( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  lcl=>open_file_dialog( ).

START-OF-SELECTION.
  lcl=>run( ).