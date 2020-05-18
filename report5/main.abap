REPORT z5_wl MESSAGE-ID z5_messages.

INCLUDE z5_wl_top.
INCLUDE z5_wl_sel.
INCLUDE z5_wl_lcl.
INCLUDE z5_wl_events.

START-OF-SELECTION.
  gt_headerid = CORRESPONDING #( p_id[] ).
  gt_date = CORRESPONDING #( p_date[] ).
  gt_currency = CORRESPONDING #( p_curr[] ).
  go_alv_provider = NEW #( im_headerid = gt_headerid
                           im_date = gt_date
                           im_currency = gt_currency
                           im_container = go_container ).
  CALL SCREEN 100.

MODULE d0100_pbo OUTPUT.
  SET PF-STATUS 'STANDARD'.
  IF go_container IS NOT BOUND.
    go_alv_provider->run( ).
  ENDIF.
ENDMODULE.

MODULE d0100_pai INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.