*&---------------------------------------------------------------------*
*&  Include           Z_REPORT1_LIGEZA_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF gs_pesel,
            pesel TYPE zusersdb-pesel,
       END OF gs_pesel.

DATA go_file_provider type REF TO z_file_provider.