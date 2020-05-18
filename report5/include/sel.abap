*&---------------------------------------------------------------------*
*&  Include           Z5_WL_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.

SELECT-OPTIONS p_id FOR z03_inv_header-headerid NO INTERVALS.
SELECT-OPTIONS p_date FOR z03_inv_header-invoice_date.
SELECT-OPTIONS p_curr FOR z03_inv_header-currency NO INTERVALS.

SELECTION-SCREEN END OF BLOCK a.