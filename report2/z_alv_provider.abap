CLASS z02_alv_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          VALUE(im_salary) TYPE rseloption OPTIONAL
          VALUE(im_curr)   TYPE rseloption OPTIONAL
          VALUE(im_workp)  TYPE rseloption OPTIONAL,

      create_alv,
      prepare_data,
      prepare_extended_data,
      display_alv,
      get_first_currency,
      fill_extending_columns,
      convert_currency
        IMPORTING
          VALUE(lv_curr_key)       TYPE z02_curr_extend-currency
          VALUE(lv_amount)         TYPE z02_curr_extend-salary
        RETURNING
          VALUE(lv_converted_curr) TYPE z02_curr_extend-sal_conv.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      _lt_salary        TYPE rseloption,
      _lt_curr          TYPE rseloption,
      _lt_workp         TYPE rseloption,

      _lv_first_opt     TYPE string,

      _lt_data          TYPE STANDARD TABLE OF z02_curr_extend,
      _lt_data_extended TYPE STANDARD TABLE OF z02_curr_extend,

      _lo_alv           TYPE REF TO cl_salv_table.

ENDCLASS.



CLASS Z02_ALV_PROVIDER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_SALARY                      TYPE        RSELOPTION(optional)
* | [--->] IM_CURR                        TYPE        RSELOPTION(optional)
* | [--->] IM_WORKP                       TYPE        RSELOPTION(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    _lt_salary  = im_salary.
    _lt_curr    = im_curr.
    _lt_workp   = im_workp.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->CONVERT_CURRENCY
* +-------------------------------------------------------------------------------------------------+
* | [--->] LV_CURR_KEY                    TYPE        Z02_CURR_EXTEND-CURRENCY
* | [--->] LV_AMOUNT                      TYPE        Z02_CURR_EXTEND-SALARY
* | [<-()] LV_CONVERTED_CURR              TYPE        Z02_CURR_EXTEND-SAL_CONV
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_currency.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = lv_amount
        foreign_currency = lv_curr_key
        local_currency   = _lv_first_opt
      IMPORTING
        local_amount     = lv_converted_curr
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.
    IF sy-subrc <> 0.
      MESSAGE s000(z02_ligeza_messages).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->CREATE_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_alv.

    TRY .
        cl_salv_table=>factory(
              IMPORTING
                r_salv_table = _lo_alv
               CHANGING
                 t_table = _lt_data
            ).
      CATCH cx_salv_msg INTO DATA(factory_message).
        MESSAGE e398(00) WITH factory_message->get_text( ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->DISPLAY_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_alv.

    _lo_alv->get_functions( )->set_all( ).
    _lo_alv->get_columns( )->set_optimize( ).
    _lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    TRY.
        _lo_alv->get_columns( )->get_column( 'MANDT' )->set_technical( abap_false ).
      CATCH cx_salv_not_found.
        MESSAGE s001(z02_ligeza_messages).
    ENDTRY.

    IF _lv_first_opt IS INITIAL.
      TRY.
          _lo_alv->get_columns( )->get_column( 'CURR_CONV' )->set_technical( abap_false ).
          _lo_alv->get_columns( )->get_column( 'SAL_CONV' )->set_technical( abap_false ).
        CATCH cx_salv_not_found.
          MESSAGE s001(z02_ligeza_messages).
      ENDTRY.
    ENDIF.

    _lo_alv->display( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->FILL_EXTENDING_COLUMNS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_extending_columns.

    SELECT * FROM zusersdb
          INTO CORRESPONDING FIELDS OF TABLE _lt_data_extended.

    IF sy-subrc = 0.
      LOOP AT _lt_data_extended INTO DATA(ltl).
        IF ltl-currency <> _lv_first_opt.
          ltl-sal_conv = convert_currency(
                     lv_curr_key = ltl-currency
                     lv_amount   = ltl-salary
                     ).
        ELSE.
          ltl-sal_conv = ltl-salary.
        ENDIF.
        ltl-curr_conv = _lv_first_opt.
        APPEND ltl TO _lt_data.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->GET_FIRST_CURRENCY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_first_currency.

    TRY.
        _lv_first_opt = _lt_curr[ 1 ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE s002(z02_ligeza_messages).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->PREPARE_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_data.

    SELECT * FROM zusersdb
      INTO CORRESPONDING FIELDS OF TABLE _lt_data WHERE
      salary IN _lt_salary AND
      currency IN _lt_curr AND
      workposition IN _lt_workp.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z02_ALV_PROVIDER->PREPARE_EXTENDED_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_extended_data.

    DELETE _lt_data WHERE NOT (
        sal_conv     IN _lt_salary AND
        workposition IN _lt_workp ).

  ENDMETHOD.
ENDCLASS.