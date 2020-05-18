CLASS z5_handle_events DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          im_alv       TYPE REF TO cl_salv_table
          im_positions TYPE z5_position_tt,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING
            e_salv_function.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      _lo_alv       TYPE REF TO cl_salv_table,
      _lt_headers   TYPE STANDARD TABLE OF z03_inv_header,
      _lt_positions TYPE STANDARD TABLE OF z03_inv_position,
      _lt_selected  TYPE STANDARD TABLE OF z03_inv_position,
      _fm_name      TYPE rs38l_fnam.

    METHODS:
      _get_selections,
      _call_smartform_name,
      _print_smartform,
      _save_smartform.
ENDCLASS.



CLASS Z5_HANDLE_EVENTS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z5_HANDLE_EVENTS->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_ALV                         TYPE REF TO CL_SALV_TABLE
* | [--->] IM_POSITIONS                   TYPE        Z5_POSITION_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    _lo_alv = im_alv.
    _lt_positions = im_positions.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z5_HANDLE_EVENTS->ON_USER_COMMAND
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_SALV_FUNCTION                LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_user_command.

    me->_get_selections( ).
    me->_call_smartform_name( ).

    CASE e_salv_function.
      WHEN 'print_PDF'.
        me->_print_smartform( ).
      WHEN 'save_PDF'.
        me->_save_smartform( ).
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_HANDLE_EVENTS->_CALL_SMARTFORM_NAME
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _call_smartform_name.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'Z5_PRINT_PDF'
      IMPORTING
        fm_name            = _fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_HANDLE_EVENTS->_GET_SELECTIONS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_selections.

    DATA lt_rows TYPE salv_t_row.

    TRY.
        lt_rows = _lo_alv->get_selections( )->get_selected_rows( ).

        DATA(lv_row) = _lt_positions[ lt_rows[ 1 ] ].

        SELECT * FROM z03_inv_position
          INTO CORRESPONDING FIELDS OF TABLE _lt_selected
          WHERE headerid = lv_row-headerid.

        SELECT * FROM z03_inv_header
          INTO CORRESPONDING FIELDS OF TABLE _lt_headers
          WHERE headerid = lv_row-headerid.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE s003(z5_messages).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_HANDLE_EVENTS->_PRINT_SMARTFORM
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _print_smartform.

    CALL FUNCTION _fm_name
      EXPORTING
        im_t_inv_header   = _lt_headers
        im_t_inv_position = _lt_selected
      EXCEPTIONS
        formatting_error  = 1
        internal_error    = 2
        send_error        = 3
        user_canceled     = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_HANDLE_EVENTS->_SAVE_SMARTFORM
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _save_smartform.

    DATA: ls_locl       TYPE ssfcompop,
          ls_control    TYPE ssfctrlop,
          lv_job_output TYPE ssfcrescl,
          lt_lines      TYPE TABLE OF tline,
          lv_size       TYPE i,
          lv_useraction TYPE i,
          lv_filename   TYPE string,
          lv_path       TYPE string,
          lv_fullpath   TYPE string.

    ls_locl-tdprinter = 'SAPWIN'.
    ls_control-no_dialog = 'X'.
    ls_control-getotf = 'X'.

    CALL FUNCTION _fm_name
      EXPORTING
        im_t_inv_header    = _lt_headers
        im_t_inv_position  = _lt_selected
        output_options     = ls_locl
        control_parameters = ls_control
      IMPORTING
        job_output_info    = lv_job_output
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = lv_size
      TABLES
        otf                   = lv_job_output-otfdata
        lines                 = lt_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title              = 'Save PDF'
        default_extension         = 'PDF'
        default_file_name         = 'Invoice.pdf'
      CHANGING
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
        user_action               = lv_useraction
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.

    IF lv_useraction <> 0.
      RETURN.
    ENDIF.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize            = lv_size
        filename                = lv_filename
        filetype                = 'BIN'
      CHANGING
        data_tab                = lt_lines
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
ENDCLASS.