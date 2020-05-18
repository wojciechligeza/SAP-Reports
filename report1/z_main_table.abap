CLASS z_main_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA:
      lt_data    TYPE STANDARD TABLE OF zusersdb,
      lwa_data   LIKE LINE OF lt_data,

      lt_current TYPE STANDARD TABLE OF zusersdb,
      lt_insert  TYPE STANDARD TABLE OF zusersdb,
      lt_update  TYPE STANDARD TABLE OF zusersdb.

    METHODS:
      insert_data,
      update_data.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS Z_MAIN_TABLE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_MAIN_TABLE->INSERT_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD insert_data.
    INSERT zusersdb FROM TABLE lt_insert.
    IF sy-dbcnt > 0.
      MESSAGE 'Data added in database.' TYPE 'S'.
    ELSE.
      MESSAGE 'Data not saved!' TYPE 'W'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_MAIN_TABLE->UPDATE_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_data.
    MODIFY zusersdb FROM  TABLE lt_update.
    IF sy-dbcnt > 0.
      MESSAGE 'Data updated in database.' TYPE 'S'.
    ELSE.
      MESSAGE 'Data not updated!' TYPE 'W'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.