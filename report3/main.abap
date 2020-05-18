REPORT z3_inv_wl MESSAGE-ID z3_inv_messages.

INCLUDE z3_inv_wl_top.
INCLUDE z3_inv_wl_sel.
INCLUDE z3_inv_wl_lcl.

INITIALIZATION.
  lcl=>instantiate_file_provider( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  lcl=>open_file_dialog( ).

START-OF-SELECTION.

  DATA(gt_filetable) = go_file_provider->upload_file( p_path ).

  go_invoice_service = NEW #( gt_filetable ).
  go_invoice_service->split_csv_file( ).
  go_invoice_service->decide_table_status( ).

  IF p_test = abap_true.
    IF go_invoice_service->run_validator( ) = abap_true.
      MESSAGE s006(z3_inv_messages).
    ELSE.
      MESSAGE w007(z3_inv_messages).
    ENDIF.
  ELSEIF p_real = abap_true.
    IF go_invoice_service->run_validator( ) = abap_true.
      go_invoice_service->save_in_db_table( ).
      go_invoice_service->display_tables( ).
    ELSE.
      MESSAGE w010(z3_inv_messages).
    ENDIF.
  ENDIF.