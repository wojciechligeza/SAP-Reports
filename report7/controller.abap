REPORT z7_wl.

INCLUDE z7_wl_top.
INCLUDE z7_wl_sel.

START-OF-SELECTION.
  gt_order    = CORRESPONDING #( p_no[] ).
  gt_category = CORRESPONDING #( p_cat[] ).
  gt_type     = CORRESPONDING #( p_type[] ).
  gt_date     = CORRESPONDING #( p_date[] ).

  go_model = NEW #( im_organization = p_org
                    im_distribution = p_distr
                    im_division = p_div
                    im_order = gt_order
                    im_category = gt_category
                    im_type = gt_type
                    im_date = gt_date ).

  go_model->fetch_data( ).
  DATA(it_model) = go_model->get_data( ).
  go_view = NEW #( it_model ).

  IF p_alv = abap_true.
    go_view->display_alv( ).
  ELSE.
    go_view->display_demo( ).
  ENDIF.