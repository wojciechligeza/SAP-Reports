*&---------------------------------------------------------------------*
*&  Include           Z_REPORT1_LIGEZA_LCL
*&---------------------------------------------------------------------*
CLASS lcl DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      instantiate_file_provider,
      open_file_dialog,
      run.

ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD instantiate_file_provider.

    go_file_provider = NEW #( ).

  ENDMETHOD.

  METHOD open_file_dialog.

    TRY.
        go_file_provider->select_file( ).
        DATA(lv_filetitle) = go_file_provider->get_file( ).
        DATA(lv_first_line) = lv_filetitle[ 1 ].
        p_path = lv_first_line.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD run.

    DATA:
      lt_pesel_temp    TYPE STANDARD TABLE OF gs_pesel,
      lt_pesel_compare TYPE STANDARD TABLE OF gs_pesel,
      lo_main_table    TYPE REF TO z_main_table,
      lv_salary_temp   TYPE string.

    "$. Region Insert data from file to main_table object
    lo_main_table = NEW #( ).
    DATA(lt_file_table) = go_file_provider->upload_file( p_path ).

    LOOP AT lt_file_table ASSIGNING FIELD-SYMBOL(<lp_file_table>).
      SPLIT <lp_file_table> AT ',' INTO
         lo_main_table->lwa_data-pesel
         lo_main_table->lwa_data-firstname
         lo_main_table->lwa_data-lastname
         lo_main_table->lwa_data-address
         lo_main_table->lwa_data-workposition
         lv_salary_temp
         lo_main_table->lwa_data-currency.

      lo_main_table->lwa_data-mandt = sy-mandt.
      lo_main_table->lwa_data-salary = CONV #( lv_salary_temp ).
      APPEND lo_main_table->lwa_data TO lo_main_table->lt_data.
      APPEND lo_main_table->lwa_data-pesel TO lt_pesel_temp.
    ENDLOOP.
    "$. Endregion Insert data from file to lo_main_table object

    SELECT *
      FROM zusersdb
      INTO CORRESPONDING FIELDS OF TABLE lo_main_table->lt_current.

    DATA(counter)      = 0.
    DATA(row_update)   = 0.
    DATA(row_insert)   = 0.
    LOOP AT lo_main_table->lt_data INTO lo_main_table->lwa_data.
      "$. Region Estimate if data contains duplicates
      lt_pesel_compare = lt_pesel_temp.
      DELETE lt_pesel_compare WHERE NOT pesel = lo_main_table->lwa_data-pesel.
      counter = lines( lt_pesel_compare ).
      IF counter > 1 .
        MESSAGE 'PESEL Numbers are duplicated!' TYPE 'E'.
      ENDIF.
      "$. Endregion Estimate if data contains duplicates

      "$. Region Decide if data to update either to insert

      IF line_exists( lo_main_table->lt_current[ pesel = lo_main_table->lwa_data-pesel ] ).
        APPEND lo_main_table->lwa_data TO lo_main_table->lt_update.
        row_update = row_update + 1.
      ELSE.
        APPEND lo_main_table->lwa_data TO lo_main_table->lt_insert.
        row_insert = row_insert + 1.
      ENDIF.

*      READ TABLE lo_main_table->lt_current WITH KEY pesel = lo_main_table->lwa_data-pesel TRANSPORTING NO FIELDS.
*      IF sy-subrc <> 0.
*        APPEND lo_main_table->lwa_data TO lo_main_table->lt_insert.
*      ELSE.
*        APPEND lo_main_table->lwa_data TO lo_main_table->lt_update.
*      ENDIF.

      "$. Endregion Decide if data to update either to insert
    ENDLOOP.


    IF row_update > 0 AND row_insert = 0.
      lo_main_table->update_data( ).
    ELSEIF row_insert > 0 AND row_update = 0.
      lo_main_table->insert_data( ).
    ELSEIF row_update > 0 AND row_insert > 0.
      lo_main_table->insert_data( ).
      lo_main_table->update_data( ).
    ENDIF.

    "$. Region Display a result to the user
    IF row_insert > 0.
      WRITE 'ADDED RECORDS'.
      NEW-LINE.
      WRITE:'PESEL', 20 'FIRSTNAME', 40 'LASTNAME', 60 'ADDRESS', 80 'WORKPOSITION', 100 'SALARY', 120 'CURRENCY', 140 'CLIENT SERVER'.
      ULINE.
      LOOP AT lo_main_table->lt_insert INTO lo_main_table->lwa_data.
        WRITE:
              lo_main_table->lwa_data-pesel,
          20  lo_main_table->lwa_data-firstname,
          40  lo_main_table->lwa_data-lastname,
          60  lo_main_table->lwa_data-address,
          80  lo_main_table->lwa_data-workposition,
          100 lo_main_table->lwa_data-salary,
          120 lo_main_table->lwa_data-currency,
          140 lo_main_table->lwa_data-mandt.
        NEW-LINE.
      ENDLOOP.
      NEW-LINE.
      WRITE: | ROW INSERTED :  { row_insert } |.
      NEW-LINE.
      ULINE.
    ENDIF.

    IF row_update > 0.
      WRITE 'UPDATED RECORDS'.
      NEW-LINE.
      WRITE:'PESEL', 20 'FIRSTNAME', 40 'LASTNAME', 60 'ADDRESS', 80 'WORKPOSITION', 100 'SALARY', 120 'CURRENCY', 140 'CLIENT SERVER'.
      ULINE.
      LOOP AT lo_main_table->lt_update INTO lo_main_table->lwa_data.
        WRITE:
              lo_main_table->lwa_data-pesel,
          20  lo_main_table->lwa_data-firstname,
          40  lo_main_table->lwa_data-lastname,
          60  lo_main_table->lwa_data-address,
          80  lo_main_table->lwa_data-workposition,
          100 lo_main_table->lwa_data-salary,
          120 lo_main_table->lwa_data-currency,
          140 lo_main_table->lwa_data-mandt.
        NEW-LINE.
      ENDLOOP.
      NEW-LINE.
      WRITE: | ROW UPDATED :  { row_update } |.
      NEW-LINE.
      ULINE.
    ENDIF.
    "$. Endregion Display a result to the user

  ENDMETHOD.
ENDCLASS.