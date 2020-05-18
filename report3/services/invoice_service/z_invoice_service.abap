CLASS z_invoice_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          !im_filetable TYPE filetable,
      split_csv_file,
      decide_table_status,
      display_tables,
      save_in_db_table,
      run_validator
        RETURNING
          VALUE(rt_result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF _ls_csv_file,
        type                TYPE string,
        status              TYPE string,
        headerid            TYPE string,
        positionid          TYPE string,
        date                TYPE string,
        address_or_material TYPE string,
        price               TYPE string,
        currency            TYPE string,
        quantity            TYPE string,
        unit                TYPE string,
        number_line         TYPE i,
      END OF _ls_csv_file.

    DATA:
      _lt_filetable    TYPE filetable,
      _lt_csv_file     TYPE STANDARD TABLE OF _ls_csv_file,
      _lt_headers      TYPE STANDARD TABLE OF zheaderextended,
      _lt_positions    TYPE STANDARD TABLE OF zpositionextended,
      _lt_db_headers   TYPE STANDARD TABLE OF z03_inv_header,
      _lt_db_positions TYPE STANDARD TABLE OF z03_inv_position.

    METHODS:
      _lock_db,
      _unlock_db.

ENDCLASS.



CLASS Z_INVOICE_SERVICE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_INVOICE_SERVICE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_FILETABLE                   TYPE        FILETABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    _lt_filetable = im_filetable.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_INVOICE_SERVICE->DECIDE_TABLE_STATUS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD decide_table_status.

    DATA: ls_header   TYPE zheaderextended,
          ls_position TYPE zpositionextended.

    LOOP AT _lt_csv_file ASSIGNING FIELD-SYMBOL(<lp_csv_file_row>).
      CASE <lp_csv_file_row>-type.
        WHEN 'N'.
          ls_header-client       = sy-mandt.
          ls_header-headerid     = <lp_csv_file_row>-headerid.
          ls_header-invoice_date = <lp_csv_file_row>-date.
          ls_header-address      = <lp_csv_file_row>-address_or_material.
          ls_header-cost         = <lp_csv_file_row>-price.
          ls_header-currency     = <lp_csv_file_row>-currency.
          ls_header-number_line  = <lp_csv_file_row>-number_line.
          ls_header-status       = <lp_csv_file_row>-status.

          APPEND ls_header TO _lt_headers.
        WHEN 'P'.
          ls_position-client       = sy-mandt.
          ls_position-headerid     = <lp_csv_file_row>-headerid.
          ls_position-positionid   = <lp_csv_file_row>-positionid.
          ls_position-invoice_date = <lp_csv_file_row>-date.
          ls_position-materialname = <lp_csv_file_row>-address_or_material.
          ls_position-price        = <lp_csv_file_row>-price.
          ls_position-currency     = <lp_csv_file_row>-currency.
          ls_position-quantity     = <lp_csv_file_row>-quantity.
          ls_position-unit         = <lp_csv_file_row>-unit.
          ls_position-number_line  = <lp_csv_file_row>-number_line.
          ls_position-status       = <lp_csv_file_row>-status.

          APPEND ls_position TO _lt_positions.
        WHEN OTHERS.
          MESSAGE w004(z3_inv_messages).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_INVOICE_SERVICE->DISPLAY_TABLES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_tables.

    cl_demo_output=>display( _lt_db_headers ).
    cl_demo_output=>display( _lt_db_positions ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_INVOICE_SERVICE->RUN_VALIDATOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run_validator.

    DATA(lo_inv_logger) = NEW z3_inv_logger( ).

    lo_inv_logger->create_logger( ).

    DATA(lo_inv_validator) = NEW z3_inv_validator(
                                 im_inv_logger = lo_inv_logger
                                 im_inv_headers = _lt_headers
                                 im_inv_positions = _lt_positions
                             ).

    DATA: lv_validation_passed TYPE i,
          lv_validation_failed TYPE i.
    TRY.
        lo_inv_validator->validate_price( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_price_not_equal INTO DATA(ex_price).
        lo_inv_logger->add_log( ex_price->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    TRY.
        lo_inv_validator->validate_currency( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_currency_not_equal INTO DATA(ex_curr).
        lo_inv_logger->add_log( ex_curr->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    TRY.
        lo_inv_validator->validate_headers( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_headers_not_unique INTO DATA(ex_headers).
        lo_inv_logger->add_log( ex_headers->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    TRY.
        lo_inv_validator->validate_positions( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_positions_not_unique INTO DATA(ex_positions).
        lo_inv_logger->add_log( ex_positions->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    TRY .
        lo_inv_validator->validate_date( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_date_not_correct INTO DATA(ex_date).
        lo_inv_logger->add_log( ex_date->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    TRY .
        lo_inv_validator->validate_header_no_position( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_header_has_no_position INTO DATA(ex_header_no_position).
        lo_inv_logger->add_log( ex_header_no_position->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    TRY .
        lo_inv_validator->validate_position_no_header( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_position_has_no_header INTO DATA(ex_position_has_no_header).
        lo_inv_logger->add_log( ex_position_has_no_header->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    TRY .
        lo_inv_validator->validate_entry( ).
        lv_validation_passed = lv_validation_passed + 1.
      CATCH zcx_entry_exist INTO DATA(ex_entry).
        lo_inv_logger->add_log( ex_entry->get_text( ) ).
        lv_validation_failed = lv_validation_failed + 1.
    ENDTRY.

    IF lv_validation_passed > 0 AND lv_validation_failed = 0.
      rt_result = abap_true.
    ELSE.
      lo_inv_logger->display_logs( ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_INVOICE_SERVICE->SAVE_IN_DB_TABLE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save_in_db_table.

    DATA lv_header_db_count TYPE i.

    _lt_db_headers = CORRESPONDING #( _lt_headers[] ).
    _lt_db_positions = CORRESPONDING #( _lt_positions[] ).

    me->_lock_db( ).
    MODIFY z03_inv_header FROM TABLE _lt_db_headers.
    lv_header_db_count = sy-dbcnt.
    MODIFY z03_inv_position FROM TABLE _lt_db_positions.
    me->_unlock_db( ).

    IF sy-dbcnt > 0 AND lv_header_db_count > 0.
      MESSAGE s008(z3_inv_messages).
    ELSE.
      MESSAGE w009(z3_inv_messages).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_INVOICE_SERVICE->SPLIT_CSV_FILE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD split_csv_file.

    DATA:
      ls_row         TYPE _ls_csv_file.

    LOOP AT _lt_filetable ASSIGNING FIELD-SYMBOL(<lp_filetable_row>).
      ls_row-number_line = sy-tabix.
      SPLIT <lp_filetable_row> AT ',' INTO
        ls_row-type
        ls_row-status
        ls_row-headerid
        ls_row-positionid
        ls_row-date
        ls_row-address_or_material
        ls_row-price
        ls_row-currency
        ls_row-quantity
        ls_row-unit.
      APPEND ls_row TO _lt_csv_file.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z_INVOICE_SERVICE->_LOCK_DB
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _lock_db.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = 'z03_inv_header'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE s005(z3_inv_messages) DISPLAY LIKE 'E'.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = 'z03_inv_position'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE w005(z3_inv_messages).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z_INVOICE_SERVICE->_UNLOCK_DB
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _unlock_db.

    CALL FUNCTION 'DEQUEUE_ALL'.

  ENDMETHOD.
ENDCLASS.