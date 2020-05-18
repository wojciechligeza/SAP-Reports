*&---------------------------------------------------------------------*
*&  Include           Z7_WL_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-000.
PARAMETERS:
  p_org   TYPE vbak-vkorg OBLIGATORY DEFAULT '9E10',
  p_distr TYPE vbak-vtweg OBLIGATORY DEFAULT 'E1',
  p_div   TYPE vbak-spart OBLIGATORY DEFAULT 'E2'.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  p_no   FOR vbak-vbeln NO INTERVALS DEFAULT '*1*',
  p_cat  FOR vbak-vbtyp NO INTERVALS,
  p_type FOR vbak-auart NO INTERVALS,
  p_date FOR vbak-erdat DEFAULT '20200101' OPTION GE.
SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE text-002.
PARAMETERS:
  p_alv  RADIOBUTTON GROUP mode,
  p_demo RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK c.