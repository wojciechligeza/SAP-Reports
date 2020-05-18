CLASS z7_wl_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          VALUE(im_organization) TYPE vbak-vkorg
          VALUE(im_distribution) TYPE vbak-vtweg
          VALUE(im_division)     TYPE vbak-spart
          im_order               TYPE rseloption
          im_category            TYPE rseloption
          im_type                TYPE rseloption
          im_date                TYPE rseloption,
      fetch_data,
      get_data
        RETURNING
          VALUE(rt_data) TYPE z7_data_tt.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      _lv_organization TYPE vbak-vkorg,
      _lv_distribution TYPE vbak-vtweg,
      _lv_division     TYPE vbak-spart,
      _lt_order        TYPE rseloption,
      _lt_category     TYPE rseloption,
      _lt_type         TYPE rseloption,
      _lt_date         TYPE rseloption,
      _lt_data         TYPE STANDARD TABLE OF z7data.
ENDCLASS.



CLASS Z7_WL_MODEL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z7_WL_MODEL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_ORGANIZATION                TYPE        VBAK-VKORG
* | [--->] IM_DISTRIBUTION                TYPE        VBAK-VTWEG
* | [--->] IM_DIVISION                    TYPE        VBAK-SPART
* | [--->] IM_ORDER                       TYPE        RSELOPTION
* | [--->] IM_CATEGORY                    TYPE        RSELOPTION
* | [--->] IM_TYPE                        TYPE        RSELOPTION
* | [--->] IM_DATE                        TYPE        RSELOPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    _lv_organization = im_organization.
    _lv_distribution = im_distribution.
    _lv_division = im_division.
    _lt_order = im_order.
    _lt_category = im_category.
    _lt_type = im_type.
    _lt_date = im_date.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z7_WL_MODEL->FETCH_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fetch_data.

    SELECT tmain~vbeln,
           tmain~auart,
           tmain~erdat,
           tmain~ernam,
           tmain~kunnr,
           tauxis~name1,
           tauxip~posnr,
           tauxip~matnr,
           tauxip~arktx,
           tauxip~netwr,
           tauxip~waerk
    FROM vbak AS tmain
    INNER JOIN vbap AS tauxip
    ON tauxip~vbeln = tmain~vbeln
    LEFT OUTER JOIN kna1 AS tauxis
    ON tauxis~kunnr = tmain~kunnr
    WHERE
          tmain~vkorg EQ @_lv_organization AND
          tmain~vtweg EQ @_lv_distribution AND
          tmain~spart EQ @_lv_division AND
          tmain~vbeln IN @_lt_order AND
          tmain~vbtyp IN @_lt_category AND
          tmain~auart IN @_lt_type AND
          tmain~erdat IN @_lt_date
     INTO CORRESPONDING FIELDS OF TABLE @_lt_data.

    IF sy-dbcnt = 0.
      MESSAGE w000(z7_messages).
      RETURN.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z7_WL_MODEL->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_DATA                        TYPE        Z7_DATA_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data.
    rt_data = _lt_data.
  ENDMETHOD.
ENDCLASS.