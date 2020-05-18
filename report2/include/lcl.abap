*&---------------------------------------------------------------------*
*&  Include           Z02_LIGEZA_REPORT_LCL
*&---------------------------------------------------------------------*
CLASS lcl DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ls_curr,
        currency TYPE zusersdb-currency,
        ktext    TYPE tcurt-ktext,
      END OF ls_curr,

      BEGIN OF ls_workp,
        workposition TYPE zusersdb-workposition,
      END OF ls_workp.

    CLASS-DATA:

      it_curr_helper  TYPE STANDARD TABLE OF ls_curr,
      it_workp_helper TYPE STANDARD TABLE OF ls_workp.

    CLASS-METHODS:
      instantiate_alv_provider,
      run_alv_provider,
      help_currency,
      help_workposition.

ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD instantiate_alv_provider.

    MOVE-CORRESPONDING p_salary[] TO gt_salary.
    MOVE-CORRESPONDING p_curr[]   TO gt_curr.
    MOVE-CORRESPONDING p_workp[]  TO gt_workp.

    go_alv_provider = NEW #(
                            im_salary = gt_salary
                            im_curr   = gt_curr
                            im_workp  = gt_workp
     ).

  ENDMETHOD.

  METHOD run_alv_provider.

    go_alv_provider->create_alv( ).

    IF gt_curr IS NOT INITIAL.
      go_alv_provider->get_first_currency( ).
      go_alv_provider->fill_extending_columns( ).
      go_alv_provider->prepare_extended_data( ).
    ELSE.
      go_alv_provider->prepare_data( ).
    ENDIF.

    go_alv_provider->display_alv( ).

  ENDMETHOD.

  METHOD help_currency.

    SELECT DISTINCT maint~currency, auxi~ktext
      FROM zusersdb AS maint
      INNER JOIN tcurt AS auxi
      ON maint~currency = auxi~waers
      WHERE auxi~spras = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @it_curr_helper.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CURRENCY'
        window_title    = 'Available records'
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_CURR-LOW'
      TABLES
        value_tab       = it_curr_helper
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE s003(z02_ligeza_messages).
    ENDIF.

  ENDMETHOD.

  METHOD help_workposition.

    SELECT DISTINCT workposition
    FROM zusersdb
    INTO CORRESPONDING FIELDS OF TABLE it_workp_helper.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'WORKPOSITION'
        window_title    = 'Available records'
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_WORKP-LOW'
      TABLES
        value_tab       = it_workp_helper
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE s003(z02_ligeza_messages).
    ENDIF.

  ENDMETHOD.
ENDCLASS.