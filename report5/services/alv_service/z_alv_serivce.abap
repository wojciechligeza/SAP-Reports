CLASS z5_alv_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          im_headerid  TYPE rseloption
          im_date      TYPE rseloption
          im_currency  TYPE rseloption
          im_container TYPE REF TO cl_gui_custom_container,
      run.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: _lo_alv        TYPE REF TO cl_salv_table,
          _lo_container  TYPE REF TO cl_gui_custom_container,
          _lo_events     TYPE REF TO cl_salv_events_table,
          _lt_data       TYPE STANDARD TABLE OF z03_inv_position,
          _lt_headerid   TYPE rseloption,
          _lt_date       TYPE rseloption,
          _lt_currency   TYPE rseloption.
    METHODS:
      _prepare_data,
      _create_alv,
      _add_buttons,
      _register_events,
      _display_alv.

ENDCLASS.



CLASS Z5_ALV_PROVIDER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z5_ALV_PROVIDER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_HEADERID                    TYPE        RSELOPTION
* | [--->] IM_DATE                        TYPE        RSELOPTION
* | [--->] IM_CURRENCY                    TYPE        RSELOPTION
* | [--->] IM_CONTAINER                   TYPE REF TO CL_GUI_CUSTOM_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    _lt_headerid = im_headerid.
    _lt_date = im_date.
    _lt_currency = im_currency.
    _lo_container = im_container.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z5_ALV_PROVIDER->RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.

    me->_prepare_data( ).
    me->_create_alv( ).
    me->_add_buttons( ).
    me->_register_events( ).
    me->_display_alv( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_ALV_PROVIDER->_ADD_BUTTONS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _add_buttons.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lv_text      TYPE string.

    lo_functions = _lo_alv->get_functions( ).
    lo_functions->set_default( abap_true ).

    TRY.
        lv_text = text-b01.
        lo_functions->add_function(
          name     = 'print_PDF'
          text     = lv_text
          tooltip  = lv_text
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
        MESSAGE s000(z5_messages).
    ENDTRY.

    TRY.
        lv_text = text-b02.
        lo_functions->add_function(
          name     = 'save_PDF'
          text     = lv_text
          tooltip  = lv_text
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
        MESSAGE s001(z5_messages).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_ALV_PROVIDER->_CREATE_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _create_alv.
    IF cl_salv_table=>is_offline( ) = abap_false.
      _lo_container = NEW #( container_name = 'CONTAINER' ).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
        EXPORTING
          r_container    = _lo_container
          container_name = 'CONTAINER'
        IMPORTING
          r_salv_table = _lo_alv
        CHANGING
          t_table = _lt_data ).
      CATCH cx_salv_msg INTO DATA(factory_message).
        MESSAGE e398(00) WITH factory_message->get_text( ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_ALV_PROVIDER->_DISPLAY_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _display_alv.

    _lo_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>single ).
    _lo_alv->get_columns( )->set_optimize( ).
    _lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    TRY.
        _lo_alv->get_columns( )->get_column( 'CLIENT' )->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        MESSAGE s002(z5_messages).
    ENDTRY.
    _lo_alv->display( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_ALV_PROVIDER->_PREPARE_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _prepare_data.

    SELECT * FROM z03_inv_position
          INTO CORRESPONDING FIELDS OF TABLE _lt_data WHERE
          headerid IN _lt_headerid AND
          invoice_date IN _lt_date AND
          currency IN _lt_currency.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z5_ALV_PROVIDER->_REGISTER_EVENTS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _register_events.

    _lo_events = _lo_alv->get_event( ).

    DATA(lo_event_handler) = NEW z5_handle_events( im_alv = _lo_alv
                                                   im_positions = _lt_data ).

    SET HANDLER lo_event_handler->on_user_command FOR _lo_events.

  ENDMETHOD.
ENDCLASS.