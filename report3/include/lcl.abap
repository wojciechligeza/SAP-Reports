*&---------------------------------------------------------------------*
*&  Include           Z3_INV_WL_LCL
*&---------------------------------------------------------------------*
CLASS lcl DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      instantiate_file_provider,
      open_file_dialog.

ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD instantiate_file_provider.

    go_file_provider = NEW #( ).

  ENDMETHOD.

  METHOD open_file_dialog.

    TRY.
        go_file_provider->select_file( ).
        DATA(lt_file) = go_file_provider->get_file( ).
        DATA(lv_first_line) = CONV string( lt_file[ 1 ] ).
        p_path = lv_first_line.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF z3_inv_validator=>validate_file_format( lv_first_line ) = abap_false.
      MESSAGE w003(z3_inv_messages).
      CLEAR p_path.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.