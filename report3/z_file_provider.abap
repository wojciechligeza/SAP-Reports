CLASS z_file_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      get_file
        RETURNING
          VALUE(lt_file) TYPE zwojtek_filetype_tt,
      set_file
        IMPORTING
          !lt_file TYPE zwojtek_filetype_tt,
      select_file,
      upload_file
        IMPORTING
          !lv_file_path    TYPE string
        RETURNING
          VALUE(lt_return) TYPE zwojtek_filetype_tt.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      _lt_file        TYPE zwojtek_filetype_tt,
      _lv_read_status TYPE i.
ENDCLASS.



CLASS Z_FILE_PROVIDER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_FILE_PROVIDER->GET_FILE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] LT_FILE                        TYPE        ZWOJTEK_FILETYPE_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_file.
    lt_file = _lt_file.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_FILE_PROVIDER->SELECT_FILE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD select_file.

    DATA lv_title TYPE string.

    lv_title = text-001.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = lv_title
*       default_filename        = '*.csv'
*       file_filter             = '*.csv'
*       default_extension       = 'csv'
      CHANGING
        file_table              = _lt_file
        rc                      = _lv_read_status
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    IF sy-subrc <> 0.
      MESSAGE s000(z3_inv_messages) DISPLAY LIKE 'E'.
    ENDIF.

    IF _lv_read_status < 0.
      MESSAGE s001(z3_inv_messages) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_FILE_PROVIDER->SET_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] LT_FILE                        TYPE        ZWOJTEK_FILETYPE_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_file.
    _lt_file = lt_file.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_FILE_PROVIDER->UPLOAD_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] LV_FILE_PATH                   TYPE        STRING
* | [<-()] LT_RETURN                      TYPE        ZWOJTEK_FILETYPE_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_file.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = lv_file_path
        ignore_cerr             = abap_true
      CHANGING
        data_tab                = lt_return
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.

    IF sy-subrc <> 0.
      MESSAGE s002(z3_inv_messages) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.