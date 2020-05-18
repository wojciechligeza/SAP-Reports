CLASS z3_inv_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF ls_msg,
        msgid TYPE sy-msgid VALUE 'Z3_EX_MSG',
        msgno TYPE sy-msgno VALUE 000,
        attr1 TYPE scx_attrname VALUE space,
        attr2 TYPE scx_attrname VALUE space,
        attr3 TYPE scx_attrname VALUE space,
        attr4 TYPE scx_attrname VALUE space,
      END OF ls_msg.

    CLASS-DATA:
      lv_if_format_is_correct TYPE abap_bool VALUE abap_true.

    CLASS-METHODS:
      validate_file_format
        IMPORTING
          !im_filename     TYPE string
        RETURNING
          VALUE(rt_result) TYPE abap_bool.

    METHODS:
      constructor
        IMPORTING
          !im_inv_logger    TYPE REF TO z3_inv_logger
          !im_inv_headers   TYPE z3_inv_ttype
          !im_inv_positions TYPE z3_position_ttype,
      validate_price
        RAISING zcx_price_not_equal,
      validate_currency
        RAISING zcx_currency_not_equal,
      validate_headers
        RAISING zcx_headers_not_unique,
      validate_positions
        RAISING zcx_positions_not_unique,
      validate_date
        RAISING zcx_date_not_correct,
      validate_entry
        RAISING zcx_entry_exist,
      validate_header_no_position
        RAISING zcx_header_has_no_position,
      validate_position_no_header
        RAISING zcx_position_has_no_header.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      _lo_inv_logger    TYPE REF TO z3_inv_logger,
      _lt_inv_headers   TYPE STANDARD TABLE OF zheaderextended,
      _lt_inv_positions TYPE STANDARD TABLE OF zpositionextended.

ENDCLASS.



CLASS Z3_INV_VALIDATOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_INV_LOGGER                  TYPE REF TO Z3_INV_LOGGER
* | [--->] IM_INV_HEADERS                 TYPE        Z3_INV_TTYPE
* | [--->] IM_INV_POSITIONS               TYPE        Z3_POSITION_TTYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    _lt_inv_headers = im_inv_headers.
    _lt_inv_positions = im_inv_positions.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_CURRENCY
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_CURRENCY_NOT_EQUAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_currency.

    DATA: lv_count_positive TYPE i,
          lv_count_negative TYPE i.

    LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header>).
      LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position>)
        WHERE headerid = <lp_header>-headerid.
        IF <lp_header>-currency = <lp_position>-currency.
          lv_count_positive = lv_count_positive + 1.
        ELSE.
          lv_count_negative = lv_count_negative + 1.
        ENDIF.
      ENDLOOP.
      IF lv_count_negative > 0.
        DATA(ls_msg_curr) = zcx_currency_not_equal=>msg_curr.
        ls_msg_curr-attr1 = <lp_header>-number_line.
        RAISE EXCEPTION TYPE zcx_currency_not_equal
          EXPORTING
            textid = ls_msg_curr.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_DATE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_DATE_NOT_CORRECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_date.

    DATA: lv_count_positive TYPE i,
          lv_count_negative TYPE i.

    LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header>).
      IF <lp_header>-invoice_date > sy-datum.
        lv_count_negative = lv_count_negative + 1.
        DATA(ls_msg_date) = ls_msg.
        ls_msg_date-msgno = 001.
        ls_msg_date-attr1 = <lp_header>-number_line.
        RAISE EXCEPTION TYPE zcx_date_not_correct
          EXPORTING
            textid = ls_msg_date.
      ELSE.
        lv_count_positive = lv_count_positive + 1.
      ENDIF.
    ENDLOOP.

    LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position>).
      IF <lp_position>-invoice_date > sy-datum.
        lv_count_negative = lv_count_negative + 1.
        DATA(ls_msg_date_position) = ls_msg.
        ls_msg_date_position-msgno = 002.
        ls_msg_date_position-attr1 = <lp_position>-number_line.
        RAISE EXCEPTION TYPE zcx_date_not_correct
          EXPORTING
            textid = ls_msg_date_position.
      ELSE.
        lv_count_positive = lv_count_positive + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_ENTRY
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_ENTRY_EXIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_entry.

    DATA: lv_positive TYPE abap_bool VALUE abap_true,
          lv_msg_conv TYPE string.

    SELECT headerid
      FROM z03_inv_header
      INTO TABLE @DATA(lt_headers_entries).

    SELECT headerid, positionid
      FROM z03_inv_position
      INTO TABLE @DATA(lt_positions_entries).

    IF lines( lt_headers_entries ) > 0 AND lines( lt_positions_entries ) > 0.
      LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header>).
        LOOP AT lt_headers_entries ASSIGNING FIELD-SYMBOL(<lp_header_entry>)
          WHERE headerid = <lp_header>-headerid.
          IF <lp_header>-status = 'N'.
                lv_positive = abap_false.
                DATA(ls_msg_entry) = ls_msg.
                ls_msg_entry-msgno = 003.
                ls_msg_entry-attr1 = <lp_header>-number_line.
                RAISE EXCEPTION TYPE zcx_entry_exist
                  EXPORTING
                    textid = ls_msg_entry.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position>).
        LOOP AT lt_positions_entries ASSIGNING FIELD-SYMBOL(<lp_position_entry>).
          IF <lp_position_entry>-positionid = <lp_position>-positionid AND
             <lp_position_entry>-headerid = <lp_position>-headerid AND
             <lp_position>-status = 'N'.
                lv_positive = abap_false.
                DATA(ls_msg_entry_position) = ls_msg.
                ls_msg_entry_position-msgno = 003.
                ls_msg_entry_position-attr1 = <lp_position>-number_line.
                RAISE EXCEPTION TYPE zcx_entry_exist
                  EXPORTING
                    textid = ls_msg_entry_position.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method Z3_INV_VALIDATOR=>VALIDATE_FILE_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_FILENAME                    TYPE        STRING
* | [<-()] RT_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_file_format.

*   REGEX
    TRY.
        DATA(lv_int_offset) = strlen( im_filename ) - 4.
        DATA(lv_string_extension) = im_filename+lv_int_offset(4).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_string_extension = '.csv'.
      rt_result = abap_true.
    ELSE.
      rt_result = abap_false.
      lv_if_format_is_correct = abap_false.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_HEADERS
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_HEADERS_NOT_UNIQUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_headers.

    DATA:
      lv_count_positive TYPE i,
      lv_count_negative TYPE i,
      lv_slow_step      TYPE i.


    LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header_slow_step>).
      lv_slow_step = sy-tabix.
      LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header_fast_step>).
        IF sy-tabix <> lv_slow_step.
          IF <lp_header_slow_step>-headerid <> <lp_header_fast_step>-headerid.
            lv_count_positive = lv_count_positive + 1.
          ELSE.
            lv_count_negative = lv_count_negative + 1.
            DATA(ls_msg_header) = ls_msg.
            ls_msg_header-msgno = 004.
            ls_msg_header-attr1 = <lp_header_slow_step>-number_line.
            RAISE EXCEPTION TYPE zcx_headers_not_unique
              EXPORTING
                textid = ls_msg_header.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_HEADER_NO_POSITION
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_HEADER_HAS_NO_POSITION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_header_no_position.

    DATA: lv_positive             TYPE abap_bool,
          lv_header_with_position TYPE abap_bool.

    LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header>).
      LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position>).
        IF <lp_header>-headerid = <lp_position>-headerid.
          lv_header_with_position = abap_true.
        ENDIF.
      ENDLOOP.
      IF lv_header_with_position = abap_false.
        lv_positive = abap_false.
        DATA(ls_msg_header) = ls_msg.
        ls_msg_header-msgno = 006.
        ls_msg_header-attr1 = <lp_header>-number_line.
        RAISE EXCEPTION TYPE zcx_header_has_no_position
          EXPORTING
            textid = ls_msg_header.
      ENDIF.
      CLEAR lv_header_with_position.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_POSITIONS
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_POSITIONS_NOT_UNIQUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_positions.

    DATA:
      lv_count_positive TYPE i VALUE 0,
      lv_count_negative TYPE i VALUE 0,
      lv_slow_step      TYPE i VALUE 0.

    LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position_slow_step>).
      lv_slow_step = sy-tabix.
      LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position_fast_step>).
        IF lv_slow_step <> sy-tabix.
          IF <lp_position_slow_step>-headerid = <lp_position_fast_step>-headerid AND
             <lp_position_slow_step>-positionid = <lp_position_fast_step>-positionid.
            lv_count_negative = lv_count_negative + 1.
            DATA(ls_msg_position) = ls_msg.
            ls_msg_position-msgno = 005.
            ls_msg_position-attr1 = <lp_position_slow_step>-number_line.
            RAISE EXCEPTION TYPE zcx_positions_not_unique
              EXPORTING
                textid = ls_msg_position.
          ELSE.
            lv_count_positive = lv_count_positive + 1.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_POSITION_NO_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_POSITION_HAS_NO_HEADER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_position_no_header.

    DATA: lv_positive             TYPE abap_bool,
          lv_position_with_header TYPE abap_bool.

    LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position>).
      LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header>).
        IF <lp_position>-headerid = <lp_header>-headerid.
          lv_position_with_header = abap_true.
        ENDIF.
      ENDLOOP.
      IF lv_position_with_header = abap_false.
        lv_positive = abap_false.
        DATA(ls_msg_position) = ls_msg.
        ls_msg_position-msgno = 007.
        ls_msg_position-attr1 = <lp_position>-number_line.
        RAISE EXCEPTION TYPE zcx_position_has_no_header
          EXPORTING
            textid = ls_msg_position.
      ENDIF.
      CLEAR lv_position_with_header.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z3_INV_VALIDATOR->VALIDATE_PRICE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_PRICE_NOT_EQUAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_price.

    DATA: lv_positive TYPE abap_bool,
          lv_sum      TYPE i.

    LOOP AT _lt_inv_headers ASSIGNING FIELD-SYMBOL(<lp_header>).
      LOOP AT _lt_inv_positions ASSIGNING FIELD-SYMBOL(<lp_position>).
        IF <lp_header>-headerid = <lp_position>-headerid.
          lv_sum = lv_sum + <lp_position>-price * <lp_position>-quantity.
        ENDIF.
      ENDLOOP.
      IF <lp_header>-cost <> lv_sum.
        lv_positive = abap_false.
        DATA(ls_msg_price) = ls_msg.
        ls_msg_price-msgno = 008.
        ls_msg_price-attr1 = <lp_header>-number_line.
        RAISE EXCEPTION TYPE zcx_price_not_equal
          EXPORTING
            textid = ls_msg_price.
      ENDIF.
      CLEAR lv_sum.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.