*&---------------------------------------------------------------------*
*& Report ZKD_MATERIAL_CHANGE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkd_material_change.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN END OF BLOCK b1.
*----------------------------------------------------------------------*
*       CLASS lcl_material_change DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_material_change DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.
  PRIVATE SECTION.
    METHODS change_data.
ENDCLASS.
DATA go_material_change TYPE REF TO lcl_material_change.
*----------------------------------------------------------------------*
*       CLASS lcl_material_change IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_material_change IMPLEMENTATION.
  METHOD execute.
    change_data( ).
  ENDMETHOD.                    "execute
  METHOD change_data.
    DATA: lt_return              TYPE STANDARD TABLE OF bapi_matreturn2,
          ls_return              TYPE bapiret2,
          lt_MATERIALDESCRIPTION TYPE STANDARD TABLE OF bapi_makt.
    DATA(ls_headdata) = VALUE bapimathead(
    material    = 'PC_10406'
*    ind_sector  =
*    matl_type   =
*    basic_view  =
*    SALES_VIEW        =
*    PURCHASE_VIEW     =
*    MRP_VIEW          =
*    FORECAST_VIEW     =
*    WORK_SCHED_VIEW   =
*    PRT_VIEW          =
*    STORAGE_VIEW      =
*    WAREHOUSE_VIEW    =
*    QUALITY_VIEW      =
*    ACCOUNT_VIEW      =
*    COST_VIEW         =
*    INP_FLD_CHECK     =
*    MATERIAL_EXTERNAL =
*    MATERIAL_GUID     =
*    MATERIAL_VERSION  =
*    MATERIAL_LONG     =
).
    DATA(ls_clientdata) = VALUE bapi_mara(
    pur_status = '01'
*      DEL_FLAG       =
      matl_group     = '01'
*      OLD_MAT_NO     =
*      BASE_UOM       =
*      BASE_UOM_ISO   =
*      PO_UNIT        =
*      PO_UNIT_ISO    =
*      DOCUMENT       =
*      DOC_TYPE       =
*      DOC_VERS       =
*      DOC_FORMAT     =
*      DOC_CHG_NO     =
*      PAGE_NO        =
*      NO_SHEETS      =
*      PROD_MEMO      =
*      PAGEFORMAT     =
*      SIZE_DIM       =
*      BASIC_MATL     =
*      STD_DESCR      =
*      DSN_OFFICE     =
*      PUR_VALKEY     =
*      NET_WEIGHT     =
    ).
    DATA(ls_clientdatax) = VALUE bapi_marax(
    pur_status = abap_true
*      DEL_FLAG       =
      matl_group     = abap_true
*      OLD_MAT_NO     =
*      BASE_UOM       =
*      BASE_UOM_ISO   =
*      PO_UNIT        =
*      PO_UNIT_ISO    =
*      DOCUMENT       =
*      DOC_TYPE       =
*      DOC_VERS       =
*      DOC_FORMAT     =
*      DOC_CHG_NO     =
*      PAGE_NO        =
*      NO_SHEETS      =
*      PROD_MEMO      =
*      PAGEFORMAT     =
*      SIZE_DIM       =
*      BASIC_MATL     =
*      STD_DESCR      =
*      DSN_OFFICE     =
*      PUR_VALKEY     =
*      NET_WEIGHT     =
    ).
*    APPEND VALUE #(
*
*    ) TO lt_MATERIALDESCRIPTION.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata       = ls_headdata
        clientdata     = ls_clientdata
        clientdatax    = ls_clientdatax
*       PLANTDATA      =
*       PLANTDATAX     =
*       FORECASTPARAMETERS  =
*       FORECASTPARAMETERSX =
*       PLANNINGDATA   =
*       PLANNINGDATAX  =
*       STORAGELOCATIONDATA =
*       STORAGELOCATIONDATAX        =
*       VALUATIONDATA  =
*       VALUATIONDATAX =
*       WAREHOUSENUMBERDATA =
*       WAREHOUSENUMBERDATAX        =
*       SALESDATA      =
*       SALESDATAX     =
*       STORAGETYPEDATA     =
*       STORAGETYPEDATAX    =
*       FLAG_ONLINE    = ' '
*       FLAG_CAD_CALL  = ' '
*       NO_DEQUEUE     = ' '
*       NO_ROLLBACK_WORK    = ' '
*       CLIENTDATACWM  =
*       CLIENTDATACWMX =
*       VALUATIONDATACWM    =
*       VALUATIONDATACWMX   =
*       MATPLSTADATA   =
*       MATPLSTADATAX  =
*       MARC_APS_EXTDATA    =
*       MARC_APS_EXTDATAX   =
      IMPORTING
        return         = ls_return
      TABLES
*       materialdescription = lt_MATERIALDESCRIPTION
*       UNITSOFMEASURE =
*       UNITSOFMEASUREX     =
*       INTERNATIONALARTNOS =
*       MATERIALLONGTEXT    =
*       TAXCLASSIFICATIONS  =
        returnmessages = lt_return
*       PRTDATA        =
*       PRTDATAX       =
*       EXTENSIONIN    =
*       EXTENSIONINX   =
*       UNITSOFMEASURECWM   =
*       UNITSOFMEASURECWMX  =
*       SEGMRPGENERALDATA   =
*       SEGMRPGENERALDATAX  =
*       SEGMRPQUANTITYDATA  =
*       SEGMRPQUANTITYDATAX =
*       SEGVALUATIONTYPE    =
*       SEGVALUATIONTYPEX   =
*       SEGSALESSTATUS =
*       SEGSALESSTATUSX     =
*       SEGWEIGHTVOLUME     =
*       SEGWEIGHTVOLUMEX    =
*       DEMAND_PENALTYDATA  =
*       DEMAND_PENALTYDATAX =
*       NFMCHARGEWEIGHTS    =
*       NFMCHARGEWEIGHTSX   =
*       NFMSTRUCTURALWEIGHTS        =
*       NFMSTRUCTURALWEIGHTSX       =
      .

    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDMETHOD.                    "change_data
ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT go_material_change.
  go_material_change->execute( ).
