*&---------------------------------------------------------------------*
*&  Include           Z5_WL_LCL
*&---------------------------------------------------------------------*
CLASS lcl DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ls_headerid,
             headerid     TYPE z03_inv_header-headerid,
             address      TYPE z03_inv_header-address,
             invoice_date TYPE z03_inv_header-invoice_date,
             cost         TYPE z03_inv_header-cost,
             currency     TYPE z03_inv_header-currency,
           END OF ls_headerid,

           BEGIN OF ls_currency,
             currency TYPE z03_inv_header-currency,
             ktext    TYPE tcurt-ktext,
           END OF ls_currency.

    CLASS-DATA: it_headerid TYPE STANDARD TABLE OF ls_headerid,
                it_currency TYPE STANDARD TABLE OF ls_currency.

    CLASS-METHODS: help_parameter_id,
                   help_parameter_curr.

ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD help_parameter_id.

    SELECT DISTINCT headerid, address, invoice_date, cost, currency
      FROM z03_inv_header
      INTO CORRESPONDING FIELDS OF TABLE @it_headerid.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'HEADERID'
        window_title    = 'Available records'
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_ID-LOW'
      TABLES
        value_tab       = it_headerid
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD help_parameter_curr.

      SELECT DISTINCT maint~currency, auxi~ktext
      FROM z03_inv_header AS maint
      INNER JOIN tcurt AS auxi
      ON maint~currency = auxi~waers
      WHERE auxi~spras = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @it_currency.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CURRENCY'
        window_title    = 'Available records'
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_CURR-LOW'
      TABLES
        value_tab       = it_currency
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.