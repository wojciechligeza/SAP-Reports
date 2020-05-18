CLASS zcl_im_me_process_po_cust DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_me_process_po_cust .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA _lv_items_invalid TYPE abap_bool.
ENDCLASS.



CLASS ZCL_IM_ME_PROCESS_PO_CUST IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* | [--->] IM_HOLD                        TYPE        MMPUR_BOOL
* | [--->] IM_PARK                        TYPE        MMPUR_BOOL(optional)
* | [<-->] CH_FAILED                      TYPE        MMPUR_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~check.

    IF sy-uname = 'USER_K11'.
      DATA: ls_items    TYPE purchase_order_items,
            ls_item     TYPE purchase_order_item,
            lv_item     TYPE mepoitem.

      CONSTANTS: lc_unit     TYPE ekpo-meins VALUE 'KG',
                 lc_quantity TYPE ekpo-menge VALUE 300.

      INCLUDE mm_messages_mac.

      ls_items = im_header->get_items( ).
      LOOP AT ls_items INTO ls_item.
        lv_item = ls_item-item->get_data( ).
        IF lv_item-meins = lc_unit AND lv_item-menge < lc_quantity.
          MESSAGE e000(z6_wl_messages) WITH lv_item-ebelp INTO gl_dummy.
          mmpur_message_forced 'E' 'Z6_WL_MESSAGES' '000' lv_item-ebelp '' '' ''.
          ch_failed = abap_true.
          im_header->invalidate( ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~CLOSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~close.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* | [--->] IM_INITIATOR                   TYPE        MEPO_INITIATOR(optional)
* | [<-->] CH_FIELDSELECTION              TYPE        TTYP_FIELDSELECTION_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~fieldselection_header.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* | [<-->] CH_KEY0                        TYPE        BREFN
* | [<-->] CH_KEY1                        TYPE        BREFN
* | [<-->] CH_KEY2                        TYPE        BREFN
* | [<-->] CH_KEY3                        TYPE        BREFN
* | [<-->] CH_KEY4                        TYPE        BREFN
* | [<-->] CH_KEY5                        TYPE        BREFN
* | [<-->] CH_KEY6                        TYPE        BREFN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~fieldselection_header_refkeys.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* | [--->] IM_ITEM                        TYPE REF TO IF_PURCHASE_ORDER_ITEM_MM
* | [<-->] CH_FIELDSELECTION              TYPE        TTYP_FIELDSELECTION_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~fieldselection_item.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_ITEM                        TYPE REF TO IF_PURCHASE_ORDER_ITEM_MM
* | [<-->] CH_KEY0                        TYPE        BREFN
* | [<-->] CH_KEY1                        TYPE        BREFN
* | [<-->] CH_KEY2                        TYPE        BREFN
* | [<-->] CH_KEY3                        TYPE        BREFN
* | [<-->] CH_KEY4                        TYPE        BREFN
* | [<-->] CH_KEY5                        TYPE        BREFN
* | [<-->] CH_KEY6                        TYPE        BREFN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~fieldselection_item_refkeys.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~INITIALIZE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~initialize.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~OPEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TRTYP                       TYPE        TRTYP
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* | [<-->] CH_VALID                       TYPE        MMPUR_BOOL
* | [<-->] CH_DISPLAY_ONLY                TYPE        MMPUR_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~open.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_EBELN                       TYPE        EBELN
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~post.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_ACCOUNT                     TYPE REF TO IF_PURCHASE_ORDER_ACCOUNT_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~process_account.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_HEADER                      TYPE REF TO IF_PURCHASE_ORDER_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~process_header.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~PROCESS_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_ITEM                        TYPE REF TO IF_PURCHASE_ORDER_ITEM_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~process_item.

    IF sy-uname = 'USER_K11'.
      DATA: ls_mepoitem   TYPE mepoitem,
            ls_mepoheader TYPE mepoheader.

      CONSTANTS: lc_type       TYPE ekko-bsart VALUE 'NB',
                 lc_unit       TYPE ekpo-meins VALUE 'KG',
                 lc_quantity   TYPE ekpo-menge VALUE 300.

      INCLUDE mm_messages_mac.

      ls_mepoitem = im_item->get_data( ).
      ls_mepoheader = im_item->get_header( )->get_data( ).
      IF ls_mepoheader-bsart = lc_type AND ls_mepoitem-meins = lc_unit AND ls_mepoitem-menge < lc_quantity.
        DATA(lv_ebelp_out) = |{ ls_mepoitem-ebelp ALPHA = OUT }|.
        MESSAGE w000(z6_wl_messages) WITH lv_ebelp_out INTO gl_dummy.
        mmpur_message_forced 'W' 'Z6_WL_MESSAGES' '000' lv_ebelp_out '' '' ''.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IM_ME_PROCESS_PO_CUST->IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_SCHEDULE                    TYPE REF TO IF_PURCHASE_ORDER_SCHEDULE_MM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_ex_me_process_po_cust~process_schedule.
  ENDMETHOD.
ENDCLASS.