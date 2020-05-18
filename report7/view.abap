CLASS z7_wl_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          im_model TYPE z7_data_tt,
      display_alv,
      display_demo,
      on_link_click
            FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
            row.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      _lo_alv   TYPE REF TO cl_salv_table,
      _lt_model TYPE STANDARD TABLE OF z7data.

    METHODS:
      _create_alv,
      _set_alv,
      _set_hotspot,
      _register_event.
ENDCLASS.



CLASS Z7_WL_VIEW IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z7_WL_VIEW->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_MODEL                       TYPE        Z7_DATA_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    _lt_model = im_model.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z7_WL_VIEW->DISPLAY_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_alv.

    me->_create_alv( ).
    me->_set_alv( ).
    me->_set_hotspot( ).
    me->_register_event( ).

    _lo_alv->display( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z7_WL_VIEW->DISPLAY_DEMO
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_demo.

    cl_demo_output=>display( _lt_model ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z7_WL_VIEW->ON_LINK_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_link_click.

    TRY.
        DATA(ls_model) = _lt_model[ row ].
      CATCH cx_sy_itab_line_not_found.
        MESSAGE w003(z7_messages).
    ENDTRY.
    SET PARAMETER ID 'AUN' FIELD ls_model-vbeln.
    CALL TRANSACTION 'VA12'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z7_WL_VIEW->_CREATE_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _create_alv.

    TRY .
        cl_salv_table=>factory(
              IMPORTING
                r_salv_table = _lo_alv
               CHANGING
                 t_table = _lt_model
            ).
      CATCH cx_salv_msg INTO DATA(factory_message).
        MESSAGE e398(00) WITH factory_message->get_text( ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z7_WL_VIEW->_REGISTER_EVENT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _register_event.

    DATA lo_events TYPE REF TO cl_salv_events_table.

    lo_events = _lo_alv->get_event( ).

    SET HANDLER me->on_link_click FOR lo_events.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z7_WL_VIEW->_SET_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _set_alv.

    _lo_alv->get_functions( )->set_all( ).
    _lo_alv->get_columns( )->set_optimize( ).
    _lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    TRY.
        _lo_alv->get_columns( )->get_column( 'ERDAT' )->set_visible( abap_false ).
        _lo_alv->get_columns( )->get_column( 'ERNAM' )->set_visible( abap_false ).
        _lo_alv->get_columns( )->get_column( 'MATNR' )->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        MESSAGE w001(z7_messages).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method Z7_WL_VIEW->_SET_HOTSPOT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _set_hotspot.

    DATA lo_col_tab  TYPE REF TO cl_salv_column_table.

    TRY.
        lo_col_tab ?= _lo_alv->get_columns( )->get_column( 'VBELN' ).
      CATCH cx_salv_not_found.
        MESSAGE w001(z7_messages).
    ENDTRY.

    TRY.
        lo_col_tab->set_cell_type( value = if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_data_error.
        MESSAGE w002(z7_messages).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.