*&---------------------------------------------------------------------*
*& Report ZIDOC_MONITOR
*&---------------------------------------------------------------------*
REPORT zidoc_monitor.
*&---------------------------------------------------------------------*
*& Report ZSD_IDOC_MONITOR
*&---------------------------------------------------------------------*
*&  Author:       BUZDUGAN Octavian                                    *
*&  Requested by: Marta Perego                                         *
*&  Description: THELIOS_Bahia_FA_Custom Monitor for IDocs_Technical   *
*&  Ticket:   Z0QK922936                                               *
*&---------------------------------------------------------------------*

TABLES: edidc, e1edka1, knvp.

TYPES: BEGIN OF ty_idoc_header,
         partner                   TYPE edidc-sndprn,                   "Partner
         partner_type              TYPE edidc-rcvprt,                   "Partner Type
         idoc_number               TYPE edidc-docnum,                   "Idoc number
         idoc_status               TYPE icon_d,                         "Status
         idoc_creation_date        TYPE edidc-credat,                   "Created on
         po_number                 TYPE e1edk02-belnr,                   "OMS order number
         po_date                   TYPE sy-datum,                       "OMS order date
         sales_organization        TYPE vkorg,                          "Sales Organization
         sales_order_type          TYPE char256,
         agent_code                TYPE tvgrt-vkgrp,
         agent_description         TYPE tvgrt-bezei,
         sold_to_code              TYPE kna1-kunnr,
         sold_to_description       TYPE kna1-name1,
         ship_to_code              TYPE kna1-kunnr,
         ship_to_description       TYPE kna1-name1,
*AEAS_37777 >>>>>>>>>>>>
         bill_to_code              TYPE kna1-kunnr,
         bill_to_description       TYPE kna1-name1,
*AEAS_37777 <<<<<<<<<<<<<
         store_code                TYPE kna1-kunnr,
         store_description         TYPE kna1-name1,
         error_message_description TYPE edids-statxt,
         status                    TYPE edidc-status,

       END OF ty_idoc_header,
       tty_idoc_header TYPE STANDARD TABLE OF ty_idoc_header,

       BEGIN OF ty_idoc_items,
         docnum     TYPE edidc-docnum,          "Idoc number
         oms_ord_no TYPE e1edka1-ihrez,         "OMS Order Number
         posex      TYPE e1edp01-posex,         "Item position
         idntr      TYPE char40,                "Item SKU
         ean        TYPE ean11,                 "EAN
         menge      TYPE e1edp01-menge,         "Quantity
         delete     TYPE xfeld,                 "Item deletion
         index      TYPE i,                     "Segment position in edid4
       END OF ty_idoc_items,
       tty_idoc_items TYPE STANDARD TABLE OF ty_idoc_items,


*AEAS_37777 >>>>>>>>>>>>
       BEGIN OF ty_idoc_partn,
         fieldname TYPE fieldname,
         descr     TYPE text50,
         ag        TYPE text35,
         re        TYPE text35,
         we        TYPE text35,
         style     TYPE lvc_t_styl,
       END OF ty_idoc_partn,
       tty_idoc_partn TYPE STANDARD TABLE OF ty_idoc_partn.
*AEAS_37777 <<<<<<<<<<<<<
DATA: gv_ok_code_100 TYPE sy-ucomm,
      gv_ok_code_200 TYPE sy-ucomm,
      gv_ok_code_300 TYPE sy-ucomm,
      gv_ok_code_400 TYPE sy-ucomm.

DATA: gv_bpn_in     TYPE e1edka1-partn,    "Business Partner Number
      gv_sold_to    TYPE e1edka1-partn,    "Sold_to
      gv_ship_to    TYPE e1edka1-partn,    "Sship_to
*AEAS_37777 >>>>>>>>>>>>
      gv_bill_to    TYPE e1edka1-partn,     "Bill_to
*AEAS_37777 <<<<<<<<<<<<<
      gv_store_code TYPE e1edka1-partn,    "Store_code
      gv_fn_in      TYPE e1edka1-name1,    "First name
      gv_ln_in      TYPE e1edka1-name2,    "Last name
      gv_st_in      TYPE e1edka1-stras,    "Street
      gv_pc_in      TYPE e1edka1-pstlz,    "Postcode
      gv_city_in    TYPE e1edka1-ort01,    "City
      gv_cc_in      TYPE e1edka1-land1,    "Country Code
      gv_ph1_in     TYPE e1edka1-telf1,    "Phone 1
      gv_ph2_in     TYPE e1edka1-telf2,    "Phone 2
      gv_tlf_in     TYPE e1edka1-telfx,    "Telefax
      gv_tlb_in     TYPE e1edka1-telbx.    "Telebox

DATA: gv_docnum  TYPE edidc-docnum,
      gv_ponum   TYPE e1edk02-belnr,
      gv_parvwag TYPE e1edka1-parvw,
      gv_parvwwe TYPE e1edka1-parvw,
      gv_parvwjm TYPE e1edka1-parvw,
      gv_parvwre TYPE e1edka1-parvw.

DATA: gv_changed TYPE xfeld.

DATA: gt_edidd TYPE STANDARD TABLE OF edid4.  "Idoc data table
DATA: gt_edidc TYPE edidc_tt.                 "Idoc control table

DATA: go_alvgrid   TYPE REF TO cl_gui_alv_grid,
      go_alvgrid_i TYPE REF TO cl_gui_alv_grid.

DATA: ""go_alvgrid_partn   TYPE REF TO cl_gui_alv_grid,
      go_alvgrid_i_partn TYPE REF TO cl_gui_alv_grid.

DATA : go_partn_container TYPE REF TO cl_gui_dialogbox_container.

DATA: gt_header        TYPE tty_idoc_header,
      gt_header_temp   TYPE tty_idoc_header,
      gt_items         TYPE tty_idoc_items,
      gt_partn         TYPE tty_idoc_partn, ""WITH HEADER LINE.
      gv_idoc_no_click TYPE edidc-docnum,
      gs_partn         TYPE ty_idoc_partn.
DATA: gs_stbl TYPE lvc_s_stbl .
gs_stbl-row = 'X'.
gs_stbl-col = 'X'.

DATA: gv_invoic TYPE bool.


SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS: s_prn    FOR edidc-sndprn,      "Partner
                s_prtyp  FOR edidc-rcvprt,      "Partner Type
                s_docnum FOR edidc-docnum,      "Idoc number
                s_status FOR edidc-status,      "Status
                s_credat FOR edidc-credat OBLIGATORY,      "Creation date
                s_vkorg  FOR knvp-vkorg,      "Sales Organization
                s_omsnum FOR e1edka1-ihrez,     "PO Number
                s_mestyp FOR edidc-mestyp OBLIGATORY.      "Message type
SELECTION-SCREEN END OF BLOCK b1.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_prn-low.
*  PERFORM get_oms_system CHANGING s_prn-low.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_prn-high.
*  PERFORM get_oms_system CHANGING s_prn-high.

START-OF-SELECTION.
  PERFORM f_processing.


*&---------------------------------------------------------------------*
*& Form f_processing
*&---------------------------------------------------------------------*
FORM f_processing.


  DATA(lv_not_authorized) = abap_false.         "check user authorization

  PERFORM get_idocs CHANGING lv_not_authorized.          "CHANGING gt_header.
  IF gt_header IS NOT INITIAL.
    gt_header_temp = gt_header.
    PERFORM f_display_header. "USING gt_header.
    CALL SCREEN 100.
  ELSE.
    IF lv_not_authorized IS INITIAL.
      MESSAGE TEXT-m01 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE TEXT-e05 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_oms_system
*&---------------------------------------------------------------------*

FORM get_oms_system CHANGING cv_oms TYPE edidc-sndprn.
*
*  TYPES: BEGIN OF ty_dtype,
*           oms TYPE edidc-sndprn,
*         END OF ty_dtype,
*
*         tty_dtype TYPE STANDARD TABLE OF ty_dtype WITH EMPTY KEY.
*
*  DATA:
*    lt_values TYPE tty_dtype,
*    lt_return TYPE STANDARD TABLE OF ddshretval WITH EMPTY KEY.
*
*
*  lt_values = VALUE #( ( oms = 'FORSALES'(s01)   )
*                       ( oms = 'WEBORDER'(s02)   )
*                       ( oms = 'MOBIMEDIA'(s03)  )
*                       ( oms = 'SOFTWEAR'(s04)   )
*                       ( oms = 'NETEVEN'(s05)    )
*                       ( oms = 'SHIPPYPRO'(s06)  )
*                       ( oms = 'I_AMAZONES'(s07) )
*                       ( oms = 'I_AMAZONIT'(s08) )
*                       ( oms = 'I_AMAZONUK'(s09) )
*                       ( oms = 'I_AMAZONDE'(s10) )
*                       ( oms = 'I_AMAZONFR'(s11) )
*                       ( oms = 'I_AMAZONPL'(s12) )
*                       ( oms = 'I_AMAZONCZ'(s23) )
*                       ( oms = 'D_INTERSPO'(s13) )
*                       ( oms = 'F_DECATHL'(s14)  )
*                       ( oms = 'F_GOSPORT'(s15)  )
*                       ( oms = 'D_ZALANDO'(s16)  )
*                       ( oms = 'D_SPORTSCH'(s17) )
*                       ( oms = 'D_KAUFHOF'(s18)  )
*                       ( oms = 'FR11'(s19)       )
*                       ( oms = 'IT11'(s20)       )
*                       ( oms = 'F_SPORT2K'(s21)  )
*                       ( oms = 'D_BREUNINGER'(s22) )
*                       ( oms = 'B2B'(s23)       )
*                      ).
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'VALUE'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_values
*      return_tab      = lt_return
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc NE 0.
*    RETURN.
*  ENDIF.
*
*  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
*  cv_oms = ls_return-fieldval.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_idocs
*&---------------------------------------------------------------------*
FORM get_idocs CHANGING cv_error TYPE xfeld.


  DATA: lv_parvw    TYPE string,
        lv_stalight TYPE edi_slight,
        ls_e1edka1  TYPE e1edka1,
        ls_e1edk02  TYPE e1edk02,
        ls_e1edk14  TYPE e1edk14.

  DATA: lv_message TYPE string.

  SELECT sndprn, sndprt, credat, docnum, mestyp, status, rcvprn, direct
    FROM edidc
    INTO TABLE @DATA(lt_edidc)
    WHERE direct = '2'
      AND sndprn IN @s_prn
      AND docnum IN @s_docnum
      AND credat IN @s_credat
      AND status IN @s_status
      AND mestyp IN @s_mestyp.

  IF sy-subrc = 0.
    SELECT *
      FROM edids
      INTO TABLE @DATA(lt_edids)
      FOR ALL ENTRIES IN @lt_edidc
      WHERE docnum = @lt_edidc-docnum.

    IF sy-subrc IS INITIAL.

      SORT lt_edids BY docnum countr DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_edids COMPARING docnum.

    ENDIF.

    LOOP AT lt_edidc ASSIGNING FIELD-SYMBOL(<fs_edidc>).
      APPEND INITIAL LINE TO gt_header ASSIGNING FIELD-SYMBOL(<fs_header>).
      DATA(lv_index) = sy-tabix.

      <fs_header>-partner = <fs_edidc>-sndprn.
      <fs_header>-partner_type = <fs_edidc>-sndprt.
      <fs_header>-idoc_creation_date = <fs_edidc>-credat.
      <fs_header>-idoc_number = <fs_edidc>-docnum.
      <fs_header>-status = <fs_edidc>-status.

      CALL FUNCTION 'ISU_IDOC_GET_STATUS_ICON'
        EXPORTING
          x_docnum   = <fs_edidc>-docnum
        IMPORTING
          y_stalight = lv_stalight.

      IF sy-subrc = 0.
        CASE lv_stalight.
          WHEN '1'.
            <fs_header>-idoc_status = icon_yellow_light.
          WHEN '2'.
            <fs_header>-idoc_status = icon_green_light.
          WHEN '3'.
            <fs_header>-idoc_status = icon_red_light.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

      READ TABLE lt_edids ASSIGNING FIELD-SYMBOL(<fs_edids>) WITH KEY docnum = <fs_edidc>-docnum status = <fs_edidc>-status BINARY SEARCH.
      IF sy-subrc = 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = <fs_edids>-stamid
            msgnr               = <fs_edids>-stamno
            msgv1               = <fs_edids>-stapa1
            msgv2               = <fs_edids>-stapa2
            msgv3               = <fs_edids>-stapa3
            msgv4               = <fs_edids>-stapa4
          IMPORTING
            message_text_output = lv_message.
      ENDIF.

      SELECT *
        FROM edid4
        INTO TABLE @DATA(lt_edidd)
      WHERE docnum = @<fs_edidc>-docnum.

      IF lt_edidd IS NOT INITIAL.
*
        LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_adr>).
*
          CASE <fs_edidd_adr>-segnam.
            WHEN 'E1EDKA1'.

              CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
                CLEAR ls_e1edka1.
                ASSIGN COMPONENT 'SDATA' OF STRUCTURE <fs_edidd_adr> TO FIELD-SYMBOL(<fs_sdata>).
                IF <fs_sdata> IS ASSIGNED.
                  ls_e1edka1 = <fs_sdata>.
                ENDIF.
                UNASSIGN <fs_sdata>.
              ENDCATCH.
*
              CASE ls_e1edka1-parvw.
                WHEN 'AG'.
*                IF <fs_edidc>-mestyp = 'ORDERS'.
*                  SELECT partner UP TO 1 ROWS
*                    INTO @DATA(lv_partner)
*                    FROM but0id
*                    WHERE idnumber = @ls_e1edka1-ilnnr.
*                  ENDSELECT.
*                  <fs_header>-sold_to_code = lv_partner.
*                ELSE.
                  <fs_header>-sold_to_code = ls_e1edka1-partn.
*                ENDIF.

                  IF <fs_header>-sold_to_code IS NOT INITIAL.
                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = <fs_header>-sold_to_code
                      IMPORTING
                        output = <fs_header>-sold_to_code.

                    SELECT SINGLE name1
                      FROM kna1
                      INTO @DATA(lv_name1)
                      WHERE kunnr = @<fs_header>-sold_to_code.
                    IF sy-subrc = 0.
                      <fs_header>-sold_to_description = lv_name1.
                    ENDIF.
                  ENDIF.
*
*                <fs_header>-oms_ord_no = ls_e1edka1-ihrez.
                WHEN 'WE'.
*
                  <fs_header>-ship_to_code = ls_e1edka1-partn.

                  IF <fs_header>-ship_to_code IS NOT INITIAL.
                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = <fs_header>-ship_to_code
                      IMPORTING
                        output = <fs_header>-ship_to_code.
                    CLEAR lv_name1.

                    SELECT SINGLE name1
                      FROM kna1
                      INTO @lv_name1
                      WHERE kunnr = @<fs_header>-ship_to_code.
                    IF sy-subrc = 0.
                      <fs_header>-sold_to_description = lv_name1.
                    ENDIF.
                  ENDIF.


                WHEN 'JM'.

                  <fs_header>-store_code = ls_e1edka1-partn.
                  <fs_header>-store_description = ls_e1edka1-partn.

                  IF <fs_header>-store_code IS NOT INITIAL.
                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = <fs_header>-store_code
                      IMPORTING
                        output = <fs_header>-store_code.
                    CLEAR lv_name1.

                    SELECT SINGLE name1
                      FROM kna1
                      INTO @lv_name1
                      WHERE kunnr = @<fs_header>-store_code.
                    IF sy-subrc = 0.
                      <fs_header>-sold_to_description = lv_name1.
                    ENDIF.
                  ENDIF.
*AEAS_37777 >>>>>>>>>>>>
                WHEN 'RE'.
                  <fs_header>-bill_to_code = ls_e1edka1-partn.
                  <fs_header>-bill_to_description = ls_e1edka1-partn.

                  IF <fs_header>-bill_to_code IS NOT INITIAL.
                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = <fs_header>-bill_to_code
                      IMPORTING
                        output = <fs_header>-bill_to_code.
                    CLEAR lv_name1.

                    SELECT SINGLE name1
                      FROM kna1
                      INTO @lv_name1
                      WHERE kunnr = @<fs_header>-bill_to_code.
                    IF sy-subrc = 0.
                      <fs_header>-bill_to_description = lv_name1.
                    ENDIF.
                  ENDIF.
*AEAS_37777 <<<<<<<<<<<<<
                WHEN OTHERS.
              ENDCASE.
            WHEN 'E1EDK02'.
              CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
                CLEAR ls_e1edk02.
                ASSIGN COMPONENT 'SDATA' OF STRUCTURE <fs_edidd_adr> TO <fs_sdata>.
                IF <fs_sdata> IS ASSIGNED.
                  ls_e1edk02 = <fs_sdata>.
                ENDIF.
                UNASSIGN <fs_sdata>.
              ENDCATCH.

              IF ls_e1edk02-qualf = '001' .
                <fs_header>-po_number = ls_e1edk02-belnr.
                <fs_header>-po_date = ls_e1edk02-datum.
              ENDIF.
            WHEN 'E1EDK14'.

              CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
                CLEAR ls_e1edk14.
                ASSIGN COMPONENT 'SDATA' OF STRUCTURE <fs_edidd_adr> TO <fs_sdata>.
                IF <fs_sdata> IS ASSIGNED.
                  ls_e1edk14 = <fs_sdata>.
                ENDIF.
                UNASSIGN <fs_sdata>.
              ENDCATCH.
              CASE ls_e1edk14-qualf.
                WHEN '008'.
                  <fs_header>-sales_organization = ls_e1edk14-orgid.
                WHEN '010'.
                  IF ls_e1edk14-orgid IS NOT INITIAL.
                    <fs_header>-agent_code = ls_e1edk14-orgid.
                    SELECT SINGLE bezei
                      FROM tvgrt
                      INTO <fs_header>-agent_description
                      WHERE vkgrp = ls_e1edk14-orgid.
                  ENDIF.
                WHEN '012'.
                  <fs_header>-sales_order_type = ls_e1edk14-orgid.
                WHEN OTHERS.
              ENDCASE.
            WHEN OTHERS.
          ENDCASE.

        ENDLOOP.

      ENDIF.
      SELECT DISTINCT kunnr
               FROM knvp
               INTO TABLE @DATA(lt_knvp)
               WHERE kunn2 = @<fs_header>-ship_to_code
               AND vkorg = @<fs_header>-sales_organization.

      IF lines( lt_knvp ) EQ 0.
        lv_message = TEXT-e01.
      ELSEIF lines( lt_knvp ) GT 1.
        lv_message = TEXT-e02.
      ENDIF.

      <fs_header>-error_message_description = lv_message.

      CLEAR lv_message.

    ENDLOOP.

    PERFORM f_user_check CHANGING gt_header cv_error.

    DELETE gt_header WHERE sales_organization NOT IN s_vkorg.


    SORT gt_header BY idoc_number.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_UCOMM_0100
*&---------------------------------------------------------------------*
FORM f_ucomm_0100.
  DATA(lv_ok_code_0100) = gv_ok_code_100.
  CLEAR gv_ok_code_100.

  CASE lv_ok_code_0100.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EDIT'.
      PERFORM f_modify_row.
    WHEN 'REPROCESS'.
      PERFORM f_gen_idoc.
      PERFORM f_refresh_screen USING '0100'.
    WHEN 'ARCHIVE'.
      DATA lv_continue TYPE abap_bool.

      PERFORM f_popup_to_confirm CHANGING lv_continue.

      IF lv_continue IS NOT INITIAL.
        PERFORM f_set_idoc_status USING 0 abap_on.
        PERFORM f_refresh_screen USING '0100'.
      ENDIF.
    WHEN 'REFRESH'.
      PERFORM f_refresh_screen USING '0100'.
    WHEN 'EDIT_PRT'.
      PERFORM f_modify_partner.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_UCOMM_0200
*&---------------------------------------------------------------------*
FORM f_ucomm_0200.
  DATA(lv_ok_code_0200) = gv_ok_code_200.
  CLEAR gv_ok_code_200.

  CASE lv_ok_code_0200.
    WHEN 'CANCEL'.
      PERFORM f_clear_data.
      LEAVE TO SCREEN 0.
*    WHEN 'CONTINUE'.
*      gv_changed = abap_true.
*      PERFORM f_process_bp_data.
*      PERFORM f_clear_data.
*      go_alvgrid->refresh_table_display( EXPORTING is_stable = gs_stbl ).
*      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM f_process_bp_data.
      PERFORM f_gen_idoc.
      PERFORM f_refresh_screen USING '0'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_UCOMM_0300
*&---------------------------------------------------------------------*
FORM f_ucomm_0300.
  DATA(lv_ok_code_0300) = gv_ok_code_300.
  CLEAR gv_ok_code_300.

  CASE lv_ok_code_0300.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE_ITEMS'.
      PERFORM f_process_itm_data.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.


*AEAS_37777 >>>>>>>>>>>>
FORM f_ucomm_0400.
  DATA(lv_ok_code_0400) = gv_ok_code_400.
  CLEAR gv_ok_code_400.

  CASE lv_ok_code_0400.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*AEAS_37777 >>>>>>>>>>>>>>>


*CLASS lcl_event_handler DEFINITION .
*  PUBLIC SECTION.
*    METHODS:
*      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
*        IMPORTING e_row e_column es_row_no.
*
*  PRIVATE SECTION.
*ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*CLASS lcl_event_handler IMPLEMENTATION .
*
*  METHOD handle_double_click .
*    PERFORM handle_double_click USING e_row e_column es_row_no .
*  ENDMETHOD.
*
*ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM f_display_header.
  DATA: lo_ccontainer TYPE REF TO cl_gui_custom_container,
        lt_fieldcat   TYPE lvc_t_fcat,
        ls_layout     TYPE lvc_s_layo,
        lt_excl       TYPE ui_functions.

*  DATA: gr_event_handler TYPE REF TO lcl_event_handler.

  DATA : ls_variant TYPE disvariant.

  IF go_alvgrid IS NOT BOUND .
    CREATE OBJECT lo_ccontainer
      EXPORTING
        container_name              = 'HEAD_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT go_alvgrid
      EXPORTING
        i_parent          = lo_ccontainer
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    PERFORM f_build_field_catalog
          USING:
'PARTNER'                   'GT_HEADER' TEXT-t01  10 lt_fieldcat,
'PARTNER_TYPE'              'GT_HEADER' TEXT-t02   8 lt_fieldcat,
'IDOC_NUMBER'               'GT_HEADER' TEXT-t03  16 lt_fieldcat,
'IDOC_STATUS'               'GT_HEADER' TEXT-t04   7 lt_fieldcat,
'IDOC_CREATION_DATE'        'GT_HEADER' TEXT-t05  30 lt_fieldcat,
'PO_NUMBER'                 'GT_HEADER' TEXT-t06  30 lt_fieldcat,
'PO_DATE'                   'GT_HEADER' TEXT-t07  10 lt_fieldcat,
'SALES_ORGANIZATION'        'GT_HEADER' TEXT-t08  10 lt_fieldcat,
'SALES_ORDER_TYPE'          'GT_HEADER' TEXT-t09  10 lt_fieldcat,
'AGENT_CODE'                'GT_HEADER' TEXT-t10  10 lt_fieldcat,
'AGENT_DESCRIPTION'         'GT_HEADER' TEXT-t11  10 lt_fieldcat,
'SOLD_TO_CODE'              'GT_HEADER' TEXT-t12  10 lt_fieldcat,
'SOLD_TO_DESCRIPTION'       'GT_HEADER' TEXT-t13  10 lt_fieldcat,
'SHIP_TO_CODE'              'GT_HEADER' TEXT-t14  10 lt_fieldcat,
'SHIP_TO_DESCRIPTION'       'GT_HEADER' TEXT-t15  30 lt_fieldcat,
**AEAS_37777 >>>>>>>>>>>>
*'BILL_TO_CODE'              'GT_HEADER' TEXT-t23  30 lt_fieldcat,
*'BILL_TO_DESCRIPTION'       'GT_HEADER' TEXT-t24  30 lt_fieldcat,
*AEAS_37777 <<<<<<<<<<<<<
'STORE_CODE'                'GT_HEADER' TEXT-t17  30 lt_fieldcat,
'STORE_DESCRIPTION'         'GT_HEADER' TEXT-t18  30 lt_fieldcat,
'ERROR_MESSAGE_DESCRIPTION' 'GT_HEADER' TEXT-t16  30 lt_fieldcat.

    PERFORM f_prepare_layout CHANGING ls_layout.

    ls_variant-report = sy-repid.

    APPEND: cl_gui_alv_grid=>mc_fg_edit TO lt_excl.

    CALL METHOD go_alvgrid->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_excl
        is_variant                    = ls_variant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = gt_header
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    CALL METHOD go_alvgrid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

*  CREATE OBJECT gr_event_handler .

*  SET HANDLER gr_event_handler->handle_double_click FOR go_alvgrid .

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM f_build_field_catalog USING p_fname p_tabname p_coltext p_len ct_fieldcat TYPE lvc_t_fcat.

  DATA: ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = p_fname.
  ls_fcat-tabname   = p_tabname.

  ls_fcat-outputlen = p_len.
  IF NOT p_coltext IS INITIAL.
    ls_fcat-coltext   = p_coltext.
  ELSE.
    IF p_fname = 'ERROR_MESSAGE_DESCRIPTION'.
      ls_fcat-ref_table = 'EDIDS'.
      ls_fcat-ref_field  = p_fname.
    ELSE.
      ls_fcat-ref_table = 'EDIDC'.
      ls_fcat-ref_field  = p_fname.
    ENDIF.
  ENDIF.

  IF p_fname = 'INDEX'.
    ls_fcat-no_out = 'X'.
  ENDIF.

  APPEND ls_fcat TO ct_fieldcat.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
FORM f_prepare_layout  CHANGING p_layout TYPE lvc_s_layo.

  p_layout-zebra = 'X'.
  p_layout-sel_mode = 'A'.
  p_layout-cwidth_opt  = 'X'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM f_display_items.
  DATA: lo_ccontainer TYPE REF TO cl_gui_custom_container,
        lt_fieldcat   TYPE lvc_t_fcat,
        ls_layout     TYPE lvc_s_layo,
        lt_excl       TYPE ui_functions.

  IF go_alvgrid_i IS NOT BOUND .
    CREATE OBJECT lo_ccontainer
      EXPORTING
        container_name              = 'ITEMS_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT go_alvgrid_i
      EXPORTING
        i_parent          = lo_ccontainer
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM f_build_field_catalog_i
          USING:
          'DOCNUM'     'GT_ITEMS' space    10 lt_fieldcat,
          'OMS_ORD_NO' 'GT_ITEMS' TEXT-t17 10 lt_fieldcat,
          'POSEX'      'GT_ITEMS' TEXT-t07 10 lt_fieldcat,
          'IDNTR'      'GT_ITEMS' TEXT-t08 15 lt_fieldcat,
          'EAN'        'GT_ITEMS' TEXT-t09 10 lt_fieldcat,
          'MENGE'      'GT_ITEMS' TEXT-t10 10 lt_fieldcat,
          'DELETE'     'GT_ITEMS' TEXT-t11 10 lt_fieldcat
          .

    PERFORM f_prepare_layout_i CHANGING ls_layout.

    APPEND: cl_gui_alv_grid=>mc_fg_edit TO lt_excl.

    CALL METHOD go_alvgrid_i->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_excl
      CHANGING
        it_outtab                     = gt_items
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    CALL METHOD go_alvgrid_i->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM f_build_field_catalog_i USING p_fname p_tabname p_coltext p_len ct_fieldcat TYPE lvc_t_fcat.

  DATA: ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = p_fname.
  ls_fcat-tabname   = p_tabname.

  ls_fcat-outputlen = p_len.
  IF NOT p_coltext IS INITIAL.
    ls_fcat-coltext   = p_coltext.
  ELSE.
    ls_fcat-ref_table = 'EDIDC'.
    ls_fcat-ref_field  = p_fname.
  ENDIF.

  IF p_fname = 'DELETE'.
    ls_fcat-checkbox = 'X'.
  ENDIF.

  IF p_fname = 'IDNTR' OR p_fname = 'EAN' OR p_fname = 'MENGE' OR p_fname = 'DELETE'.
    ls_fcat-edit = 'X'.
  ENDIF.
  APPEND ls_fcat TO ct_fieldcat.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
FORM f_prepare_layout_i  CHANGING p_layout TYPE lvc_s_layo.

  p_layout-zebra = 'X'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZSTATUS_0100'.
  SET TITLEBAR 'Z0100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  PERFORM f_ucomm_0100.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ZSTATUS_0200'.
  SET TITLEBAR 'Z0200'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  PERFORM f_ucomm_0200.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'ZSTATUS_0300'.
  SET TITLEBAR 'Z0300'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  PERFORM f_ucomm_0300.
ENDMODULE.


*AEAS_37777 >>>>>>>>>>>>
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'ZSTATUS_0400'.
  SET TITLEBAR 'Z0400'.

ENDMODULE.

MODULE user_command_0400 INPUT.
  PERFORM f_ucomm_0400.
ENDMODULE.
*AEAS_37777 >>>>>>>>>>>>>>>>>



FORM f_modify_row.

  DATA: s_row     TYPE lvc_s_row,
        s_column  TYPE lvc_s_col,
        us_row_no TYPE lvc_s_roid.


  CALL METHOD go_alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_sel_idoc).

  IF lines( lt_sel_idoc ) > 1.
    MESSAGE TEXT-m02 TYPE 'S'.
  ELSEIF lines( lt_sel_idoc ) = 0.
    MESSAGE TEXT-m03 TYPE 'S'.
  ELSEIF lines( lt_sel_idoc ) = 1.
    READ TABLE lt_sel_idoc ASSIGNING FIELD-SYMBOL(<fs_sel_idoc>) INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_header INTO DATA(ls_header) INDEX <fs_sel_idoc>-index.

      IF sy-subrc = 0 .
        IF ls_header-idoc_status <> icon_green_light AND ls_header-idoc_status <> icon_yellow_light.
          gv_docnum  = ls_header-idoc_number.
          gv_ponum  = ls_header-po_number.
          PERFORM f_set_input_var USING ls_header s_column.
          CALL SCREEN 200 STARTING AT 10 5 ENDING AT 70 25.

        ELSE.
          MESSAGE TEXT-m05 TYPE 'S'.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*AEAS_37777 >>>>>>>>>>>>


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS handle_close FOR EVENT close OF cl_gui_dialogbox_container
      IMPORTING sender.

    CLASS-METHODS handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive sender.

    CLASS-METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_close.
    CALL METHOD sender->set_visible
      EXPORTING
        visible = space.
  ENDMETHOD.
  METHOD handle_user_command.
    DATA: ls_ag        TYPE e1edka1,
          ls_re        TYPE e1edka1,
          ls_we        TYPE e1edka1,
          ls_e1edka1   TYPE e1edka1,
          lt_cols      TYPE STANDARD TABLE OF fieldname,
          lt_edidd_adr TYPE edidd_tt,
          ls_edidd     TYPE edidd,
          ls_edidc     TYPE edidc,
          lt_edidc     TYPE STANDARD TABLE OF edidc.

    DATA lr_docnum TYPE RANGE OF edid4-docnum.
    DATA : lr_docnum_nm  TYPE RANGE OF edid4-docnum,
           lt_edidc_temp TYPE STANDARD TABLE OF edidc,
           lt_edidd_temp TYPE edidd_tt,
           lt_edid       TYPE STANDARD TABLE OF edidd.
    DATA: lt_edidd_display TYPE STANDARD TABLE OF edidd.

    DATA:
      lv_state            TYPE sysubrc,
      ls_inb_process_data TYPE tede2.

    CASE e_ucomm.
      WHEN 'FC_SAVE'.

        SELECT  *
               FROM edidc
               WHERE docnum = @gv_idoc_no_click
               INTO TABLE @lt_edidc_temp.
        IF sy-subrc IS INITIAL.

          SELECT *
             FROM edid4
             INTO TABLE @DATA(lt_edidd)
             WHERE docnum = @gv_idoc_no_click
             AND segnam = 'E1EDKA1' .
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.

          LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd>).

            ls_e1edka1 = <fs_edidd>-sdata.


            CASE ls_e1edka1-parvw.
              WHEN 'AG'.
                ls_ag = ls_e1edka1.
              WHEN 'RE'.
                ls_re = ls_e1edka1.
              WHEN 'WE'.
                ls_we = ls_e1edka1.
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.

          ENDLOOP.



          CALL METHOD go_alvgrid->check_changed_data.


          lt_cols = VALUE #( ( 'AG' ) ( 'RE' ) ( 'WE' ) ).

          LOOP AT gt_partn ASSIGNING FIELD-SYMBOL(<ls_partn>).

            LOOP AT lt_cols ASSIGNING FIELD-SYMBOL(<lv_col>).
              ASSIGN COMPONENT <lv_col> OF STRUCTURE <ls_partn> TO FIELD-SYMBOL(<lv_col_val>).
              DATA(lv_structure) = |LS_{ <lv_col> }|.
              ASSIGN (lv_structure) TO FIELD-SYMBOL(<ls_struct>).

              ASSIGN COMPONENT <ls_partn>-fieldname OF STRUCTURE <ls_struct> TO FIELD-SYMBOL(<lv_val>).
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.

              <lv_val> = <lv_col_val>.

            ENDLOOP.

          ENDLOOP.



          LOOP AT lt_edidd ASSIGNING <fs_edidd>.

            ls_e1edka1 = <fs_edidd>-sdata.
            CASE ls_e1edka1-parvw.
              WHEN 'AG'.
                MOVE-CORRESPONDING ls_ag TO ls_e1edka1.
              WHEN 'RE'.
                MOVE-CORRESPONDING ls_re TO ls_e1edka1.
              WHEN 'WE'.
                MOVE-CORRESPONDING ls_we TO ls_e1edka1.
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.
            <fs_edidd>-sdata = ls_e1edka1.



            APPEND INITIAL LINE TO lt_edidd_adr ASSIGNING FIELD-SYMBOL(<fs_edidd_adr>).
            <fs_edidd_adr>-segnam     =  <fs_edidd>-segnam.
            <fs_edidd_adr>-sdata      =  <fs_edidd>-sdata.
            <fs_edidd_adr>-docnum     =  <fs_edidd>-docnum.
            <fs_edidd_adr>-dtint2     =  <fs_edidd>-dtint2.
            <fs_edidd_adr>-hlevel     =  <fs_edidd>-hlevel.
            <fs_edidd_adr>-psgnum     =  <fs_edidd>-psgnum.
            <fs_edidd_adr>-segnum     =  <fs_edidd>-segnum.
          ENDLOOP.

          LOOP AT lt_edidc_temp ASSIGNING FIELD-SYMBOL(<fs_edidc>) WHERE docnum IN lr_docnum.

            ls_edidc-direct = '2'.
            ls_edidc-rcvpor = <fs_edidc>-rcvpor.
            ls_edidc-rcvprt = <fs_edidc>-rcvprt.
            ls_edidc-rcvprn = <fs_edidc>-rcvprn.
            ls_edidc-sndpor = <fs_edidc>-sndpor.
            ls_edidc-sndprt = <fs_edidc>-sndprt.
            ls_edidc-sndsad = <fs_edidc>-sndsad.
            ls_edidc-sndprn = <fs_edidc>-sndprn.
            ls_edidc-idoctp = <fs_edidc>-idoctp.
            ls_edidc-mescod = <fs_edidc>-mescod.
            ls_edidc-mesfct = <fs_edidc>-mesfct.


            ls_edidc-mestyp = <fs_edidc>-mestyp.






            PERFORM f_process_bp_data.

            CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
              EXPORTING
                pi_do_handle_error      = abap_true
                pi_no_dequeue           = space
                pi_return_data_flag     = abap_true
              IMPORTING
                pe_idoc_number          = gv_idoc_no_click
                pe_state_of_processing  = lv_state
                pe_inbound_process_data = ls_inb_process_data
              TABLES
                t_data_records          = lt_edidd_adr
              CHANGING
                pc_control_record       = ls_edidc
              EXCEPTIONS
                idoc_not_saved          = 1
                OTHERS                  = 2.

            IF sy-subrc = 0.
              ls_edidc-docnum = gv_idoc_no_click.

              SUBMIT rbdapp01
              WITH docnum = gv_idoc_no_click
              WITH p_output = space
              AND RETURN.

            ENDIF.



            CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_EDIT'
              EXPORTING
                document_number               = gv_idoc_no_click
              TABLES
                idoc_data                     = lt_edidd_display
              EXCEPTIONS
                document_foreign_lock         = 1
                document_not_exist            = 2
                document_not_open             = 3
                status_is_unable_for_changing = 4
                OTHERS                        = 5.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            lt_edidd_display = lt_edidd_adr. "table should have the data


*-- Editing the IDoc
            CALL FUNCTION 'EDI_CHANGE_DATA_SEGMENTS'
              TABLES
                idoc_changed_data_range = lt_edidd_display
              EXCEPTIONS
                idoc_not_open           = 1
                data_record_not_exist   = 2
                OTHERS                  = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
*-- Closing the IDoc after Edit
            CALL FUNCTION 'EDI_DOCUMENT_CLOSE_EDIT'
              EXPORTING
                document_number  = gv_idoc_no_click
                do_commit        = 'X'
                do_update        = 'X'
                write_all_status = 'X'
              EXCEPTIONS
                idoc_not_open    = 1
                db_error         = 2
                OTHERS           = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.


            SUBMIT rbdagaie
            WITH p_idoc = gv_idoc_no_click " This represent Edited IDoc numbers
            WITH p_direct = '2'  " Here 1 for Outbound 2 for Inbound
            WITH p_output = ' '
            AND RETURN.

            ls_edidc-docnum = gv_idoc_no_click.


            IF ls_edidc-docnum <> <fs_edidc>-docnum.
              PERFORM f_set_idoc_status USING <fs_edidc>-docnum abap_true.
            ENDIF.
            APPEND ls_edidc TO lt_edidc.
            CLEAR: lt_edidd, ls_edidc.

          ENDLOOP.
        ENDIF.
        MESSAGE : 'Salvato' TYPE 'S'.
        PERFORM f_refresh_screen USING '0100'.
    ENDCASE.

  ENDMETHOD.
  METHOD handle_toolbar.
    CLEAR: e_object->mt_toolbar, e_object->mt_btnmnu.

    APPEND VALUE #(
           function  = 'FC_SAVE'
           icon      = icon_system_save
           quickinfo = 'Salva'
     ) TO e_object->mt_toolbar.
  ENDMETHOD.
ENDCLASS.

*FORM f_idoc_create_adr  USING  ut_edidd      TYPE edidd_tt
*                           uv_docnum     TYPE edidc-docnum
*                  CHANGING cs_edidc      TYPE edidc .
*
*  DATA: lv_docnum           TYPE edidc-docnum,
*        lv_state            TYPE sysubrc,
*        ls_inb_process_data TYPE tede2.
*
*  DATA lr_idoc TYPE RANGE OF bdidocs-docnum.
*
*  CLEAR lr_idoc.
*  APPEND INITIAL LINE TO lr_idoc ASSIGNING FIELD-SYMBOL(<fs_idoc>).
*  <fs_idoc>-sign = 'I'.
*  <fs_idoc>-option = 'EQ'.
*  <fs_idoc>-low = uv_docnum.
*
*
*
*
*  CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
*    EXPORTING
*      pi_do_handle_error      = abap_true
*      pi_no_dequeue           = space
*      pi_return_data_flag     = abap_true
*    IMPORTING
*      pe_idoc_number          = lv_docnum
*      pe_state_of_processing  = lv_state
*      pe_inbound_process_data = ls_inb_process_data
*    TABLES
*      t_data_records          = ut_edidd
*    CHANGING
*      pc_control_record       = cs_edidc
*    EXCEPTIONS
*      idoc_not_saved          = 1
*      OTHERS                  = 2.
*
*
*
*  IF sy-subrc = 0.
*    cs_edidc-docnum = lv_docnum.
*
*    SUBMIT rbdapp01
*    WITH docnum = lv_docnum
*    WITH p_output = space
*    AND RETURN.
*
*
*  ENDIF.
*ENDFORM.

FORM f_modify_partner.


  DATA:
    ls_layout     TYPE lvc_s_layo,
    ls_fieldcat   TYPE slis_fieldcat_alv,
    lt_fieldcat   TYPE lvc_t_fcat,  " Field catalog
    ls_layout_alv TYPE slis_layout_alv,
    ls_e1edka1    TYPE e1edka1,
    lt_dfies      TYPE STANDARD TABLE OF dfies,
    lt_cols       TYPE STANDARD TABLE OF fieldname,
    lt_style      TYPE lvc_t_styl.

  CONSTANTS: gc_sold_to_party_partner TYPE string VALUE 'SP'.


  DATA: lv_popup TYPE REF TO cl_gui_dialogbox_container.


  CALL METHOD go_alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_sel_partn).

  IF lines( lt_sel_partn ) <> 1.
    MESSAGE TEXT-m03 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE lt_sel_partn ASSIGNING FIELD-SYMBOL(<fs_sel_idoc>) INDEX 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE gt_header ASSIGNING FIELD-SYMBOL(<fs_header>) INDEX <fs_sel_idoc>-index.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CASE <fs_header>-idoc_status.
    WHEN icon_green_light OR icon_yellow_light .
      MESSAGE TEXT-m05 TYPE 'S'.
    WHEN OTHERS.

      SELECT *
        FROM edid4
        INTO TABLE @DATA(lt_edidd)
      WHERE docnum = @<fs_header>-idoc_number
        AND segnam = 'E1EDKA1'.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      gv_idoc_no_click = <fs_header>-idoc_number.


      LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_adr>).

        ls_e1edka1 = <fs_edidd_adr>-sdata.

*        CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*          EXPORTING
*            input  = ls_e1edka1-parvw
*          IMPORTING
*            output = ls_e1edka1-parvw.

        CASE ls_e1edka1-parvw.
          WHEN 'AG'.
            DATA(ls_ag) = ls_e1edka1.
          WHEN 'RE'.
            DATA(ls_re) = ls_e1edka1.
          WHEN 'WE'.
            DATA(ls_we) = ls_e1edka1.

        ENDCASE.

      ENDLOOP.
      CLEAR lt_edidd.

      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = 'E1EDKA1'
          langu          = sy-langu
        TABLES
          dfies_tab      = lt_dfies
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      SORT lt_dfies BY fieldname.

      lt_style = VALUE #(
        ( fieldname = 'AG' style = go_alvgrid->mc_style_disabled  )
        ( fieldname = 'RE' style = go_alvgrid->mc_style_disabled  )
        ( fieldname = 'WE' style = go_alvgrid->mc_style_disabled  )
         ).
      gt_partn = VALUE #(
      ( fieldname = 'PARTN'  style = lt_style )
      ( fieldname = 'NAME1' )
      ( fieldname = 'STRAS' )
      ( fieldname = 'ORT01' )
      ( fieldname = 'PSTLZ' )
      ( fieldname = 'LAND1' )
      ( fieldname = 'TELF1' )
      ( fieldname = 'HAUSN' )
      ( fieldname = 'REGIO' )
      ).


      lt_cols = VALUE #( ( 'AG' ) ( 'RE' ) ( 'WE' ) ).

      LOOP AT gt_partn ASSIGNING FIELD-SYMBOL(<ls_partn>).
        READ TABLE lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>) WITH KEY fieldname = <ls_partn>-fieldname BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_partn>-descr = <ls_dfies>-scrtext_m.
        ENDIF.

        LOOP AT lt_cols ASSIGNING FIELD-SYMBOL(<lv_col>).
          ASSIGN COMPONENT <lv_col> OF STRUCTURE <ls_partn> TO FIELD-SYMBOL(<lv_col_val>).
          DATA(lv_structure) = |LS_{ <lv_col> }|.
          ASSIGN (lv_structure) TO FIELD-SYMBOL(<ls_struct>).

          ASSIGN COMPONENT <ls_partn>-fieldname OF STRUCTURE <ls_struct> TO FIELD-SYMBOL(<lv_val>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          <lv_col_val> = <lv_val>.

        ENDLOOP.

      ENDLOOP.


      IF go_alvgrid_i_partn IS NOT BOUND.

        CREATE OBJECT go_partn_container
          EXPORTING
            caption    = 'Address Details'
            width      = 800
            height     = 200
            top        = 70
            left       = 200
          EXCEPTIONS
            cntl_error = 1
            OTHERS     = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        SET HANDLER lcl_event_handler=>handle_close
         FOR go_partn_container.

        CREATE OBJECT go_alvgrid_i_partn
          EXPORTING
            i_parent          = go_partn_container
          EXCEPTIONS
            error_cntl_create = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        SET HANDLER lcl_event_handler=>handle_toolbar
                    lcl_event_handler=>handle_user_command
         FOR go_alvgrid_i_partn.


        lt_fieldcat = VALUE #(
          tabname = 'GT_PARTN'
          ( fieldname = 'DESCR' scrtext_m = 'Descrizione' key  = abap_true )
          ( fieldname = 'AG'    scrtext_m = 'AG'          edit = abap_true )
          ( fieldname = 'RE'    scrtext_m = 'RE'          edit = abap_true )
          ( fieldname = 'WE'    scrtext_m = 'WE'          edit = abap_true )
          ).

        ls_layout = VALUE #(
                  sel_mode    = 'A'
                  cwidth_opt  = 'X'
                  stylefname  = 'STYLE'
         ).

        go_alvgrid_i_partn->set_table_for_first_display(
         EXPORTING
           is_layout                     = ls_layout
         CHANGING
           it_outtab                     = gt_partn
           it_fieldcatalog               = lt_fieldcat
         EXCEPTIONS
           invalid_parameter_combination = 1
           program_error                 = 2
           too_many_lines                = 3
           OTHERS                        = 4 ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ELSE.
        CALL METHOD go_alvgrid_i_partn->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
      FREE go_alvgrid_i_partn.
  ENDCASE.
ENDFORM.




*AEAS_37777 <<<<<<<<<<<<<

FORM f_process_bp_data.

  DATA: ls_e1edka1 TYPE e1edka1,
        ls_e1edk02 TYPE e1edk02,
        lt_edidd   TYPE STANDARD TABLE OF edid4.

  SORT gt_header BY idoc_number.

  SELECT SINGLE *
   FROM edidc
   WHERE docnum = @gv_docnum
   INTO @DATA(ls_edidc).

  SORT gt_edidc BY docnum.

  READ TABLE gt_edidc TRANSPORTING NO FIELDS WITH KEY docnum = ls_edidc-docnum BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND ls_edidc TO gt_edidc.
  ENDIF.

  SORT gt_edidd BY docnum segnum.

  READ TABLE gt_edidd ASSIGNING FIELD-SYMBOL(<fs_gtedidd>) WITH KEY docnum = ls_edidc-docnum BINARY SEARCH.
  IF sy-subrc <> 0.

    CLEAR lt_edidd.
    SELECT *
      FROM edid4
      INTO CORRESPONDING FIELDS OF TABLE @lt_edidd
    WHERE docnum = @gv_docnum.

    IF lt_edidd IS NOT INITIAL.
      SORT lt_edidd BY docnum segnum.
      LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_adr>).
        IF <fs_edidd_adr>-segnam = 'E1EDKA1'.
          ls_e1edka1 = <fs_edidd_adr>-sdata.

          IF ls_e1edka1-parvw = gv_parvwag AND gv_parvwag IS NOT INITIAL.
            ls_e1edka1-partn = gv_sold_to.
            <fs_edidd_adr>-sdata = ls_e1edka1.
          ELSEIF ls_e1edka1-parvw = gv_parvwwe AND gv_parvwwe IS NOT INITIAL.
            ls_e1edka1-partn = gv_ship_to.
            <fs_edidd_adr>-sdata = ls_e1edka1.
          ELSEIF ls_e1edka1-parvw = gv_parvwjm AND gv_parvwjm IS NOT INITIAL.
            ls_e1edka1-partn = gv_store_code.
            <fs_edidd_adr>-sdata = ls_e1edka1.
*AEAS_37777 >>>>>>>>>>>>
          ELSEIF ls_e1edka1-parvw = gv_parvwre AND gv_parvwre IS NOT INITIAL.
            ls_e1edka1-partn = gv_bill_to.
            <fs_edidd_adr>-sdata = ls_e1edka1.
*AEAS_37777 <<<<<<<<<<<<<
          ENDIF.
        ELSEIF <fs_edidd_adr>-segnam = 'E1EDK02'.
          ls_e1edk02 = <fs_edidd_adr>-sdata.
          IF ls_e1edk02-qualf = '001' .
            ls_e1edk02-belnr = gv_ponum.
            <fs_edidd_adr>-sdata = ls_e1edk02.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF lt_edidd TO gt_edidd.
    ENDIF.

  ELSE.
    DATA(lv_index) = sy-tabix.
    LOOP AT gt_edidd ASSIGNING <fs_gtedidd> FROM lv_index.

      IF <fs_gtedidd>-docnum <> ls_edidc-docnum.
        EXIT.
      ENDIF.

      IF <fs_gtedidd>-segnam = 'E1EDKA1'.
        ls_e1edka1 = <fs_gtedidd>-sdata.

        IF ls_e1edka1-parvw = gv_parvwag AND gv_parvwag IS NOT INITIAL.
          ls_e1edka1-partn = gv_sold_to.
          <fs_gtedidd>-sdata = ls_e1edka1.
        ELSEIF ls_e1edka1-parvw = gv_parvwwe AND gv_parvwwe IS NOT INITIAL.
          ls_e1edka1-partn = gv_ship_to.
          <fs_gtedidd>-sdata = ls_e1edka1.
        ELSEIF ls_e1edka1-parvw = gv_parvwjm AND gv_parvwjm IS NOT INITIAL.
          ls_e1edka1-partn = gv_store_code.
          <fs_gtedidd>-sdata = ls_e1edka1.
*AEAS_37777 >>>>>>>>>>>>
        ELSEIF ls_e1edka1-parvw = gv_parvwre AND gv_parvwre IS NOT INITIAL.
          ls_e1edka1-partn = gv_bill_to.
          <fs_gtedidd>-sdata = ls_e1edka1.
*AEAS_37777 <<<<<<<<<<<<<
        ENDIF.
      ELSEIF <fs_gtedidd>-segnam = 'E1EDK02'.
        ls_e1edk02 = <fs_gtedidd>-sdata.
        IF ls_e1edk02-qualf = '001' .
          ls_e1edk02-belnr = gv_ponum.
          <fs_gtedidd>-sdata = ls_e1edk02.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  READ TABLE gt_header ASSIGNING FIELD-SYMBOL(<fs_header>) WITH KEY idoc_number = gv_docnum BINARY SEARCH.
  IF sy-subrc = 0.
    <fs_header>-sold_to_code = gv_sold_to.
    <fs_header>-ship_to_code = gv_ship_to.
    <fs_header>-bill_to_code = gv_bill_to.
    <fs_header>-store_code = gv_store_code.
    <fs_header>-po_number = gv_ponum.
  ENDIF.

ENDFORM.


FORM f_gen_idoc.

  DATA: ls_edidc      TYPE edidc,
        lt_edidd      TYPE edidd_tt,
        ls_edidd      TYPE edidd,
        lt_edidd_temp TYPE edidd_tt,
        lt_edidc      TYPE STANDARD TABLE OF edidc,
        lt_edidc_temp TYPE STANDARD TABLE OF edidc,
        lv_records    TYPE string,
        lv_message    TYPE char255.

  DATA: ls_e1edka1 TYPE e1edka1,
        ls_e1edk02 TYPE e1edk02,
        lv_edited  TYPE xfeld.


  DATA lr_docnum TYPE RANGE OF edid4-docnum.
  DATA lr_docnum_nm TYPE RANGE OF edid4-docnum.  "not modified.

  CLEAR lv_edited.

  IF sy-dynnr = '0100'.

    CALL METHOD go_alvgrid->get_selected_rows
      IMPORTING
        et_index_rows = DATA(lt_sel_idoc).

    IF lt_sel_idoc IS NOT INITIAL.
      LOOP AT lt_sel_idoc ASSIGNING FIELD-SYMBOL(<fs_sel_idoc>).
        READ TABLE gt_header ASSIGNING FIELD-SYMBOL(<fs_header>) INDEX <fs_sel_idoc>-index.
        IF sy-subrc = 0.
*          READ TABLE gt_header_temp ASSIGNING FIELD-SYMBOL(<fs_header_temp>) WITH KEY idoc_number = <fs_header>-idoc_number.
*          IF sy-subrc IS  INITIAL.
*            IF <fs_header> NE <fs_header_temp>.
          APPEND INITIAL LINE TO lr_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>).
          <fs_docnum>-sign = 'I'.
          <fs_docnum>-option = 'EQ'.
          <fs_docnum>-low = <fs_header>-idoc_number.
*            ELSE.
*              APPEND INITIAL LINE TO lr_docnum_nm ASSIGNING FIELD-SYMBOL(<fs_docnum_nm>).
*              <fs_docnum_nm>-sign = 'I'.
*              <fs_docnum_nm>-option = 'EQ'.
*              <fs_docnum_nm>-low = <fs_header>-idoc_number.
*            ENDIF.
*          ENDIF.

        ENDIF.
      ENDLOOP.
    ELSE.

*      MESSAGE TEXT-e03 TYPE 'E' DISPLAY LIKE 'I'.

    ENDIF.


  ELSEIF sy-dynnr = '0200'.

    READ TABLE gt_header_temp ASSIGNING FIELD-SYMBOL(<fs_header_temp>) WITH KEY idoc_number = gv_docnum.
    IF sy-subrc IS INITIAL.
*      IF <fs_header_temp>-po_number NE gv_ponum
*      OR <fs_header_temp>-sold_to_code NE gv_sold_to
*      OR <fs_header_temp>-ship_to_code NE gv_ship_to
*      OR <fs_header_temp>-store_code NE gv_store_code.
      APPEND INITIAL LINE TO lr_docnum ASSIGNING <fs_docnum>.
      <fs_docnum>-sign = 'I'.
      <fs_docnum>-option = 'EQ'.
      <fs_docnum>-low = <fs_header_temp>-idoc_number.
*      ELSE.
*        APPEND INITIAL LINE TO lr_docnum_nm ASSIGNING <fs_docnum_nm>.
*        <fs_docnum_nm>-sign = 'I'.
*        <fs_docnum_nm>-option = 'EQ'.
*        <fs_docnum_nm>-low = <fs_header_temp>-idoc_number.
*      ENDIF.
    ENDIF.

  ENDIF.


  DATA lv_documents TYPE char255.

*  IF lr_docnum_nm IS NOT INITIAL.
*    LOOP AT lr_docnum_nm ASSIGNING <fs_docnum_nm>.
*      CONCATENATE <fs_docnum_nm>-low lv_documents INTO lv_documents SEPARATED BY ';'.
*    ENDLOOP.
*    CONCATENATE lv_documents TEXT-e04 INTO DATA(lv_error) SEPARATED BY space.
*    MESSAGE lv_error TYPE 'I'.
*
*  ENDIF.

  IF lr_docnum IS NOT INITIAL.

    IF gt_edidc IS NOT INITIAL.

      SORT gt_edidc BY docnum.
      SORT gt_edidd BY docnum segnum segnam.

      DELETE gt_edidc WHERE docnum NOT IN lr_docnum.
      DELETE gt_edidd WHERE docnum NOT IN lr_docnum.

      SELECT  *
          FROM edidc
          WHERE docnum IN @lr_docnum
          INTO TABLE @lt_edidc_temp.
      IF sy-subrc IS INITIAL.

        SELECT *
       FROM edid4
       INTO CORRESPONDING FIELDS OF TABLE lt_edidd_temp
       WHERE docnum IN lr_docnum.
        IF sy-subrc IS INITIAL.

          SORT lt_edidc_temp BY docnum.
          SORT lt_edidd_temp BY docnum segnum segnam.

          LOOP AT lt_edidc_temp ASSIGNING FIELD-SYMBOL(<fs_edidc>) WHERE docnum IN lr_docnum.

            ls_edidc-direct = '2'.
            ls_edidc-rcvpor = <fs_edidc>-rcvpor.
            ls_edidc-rcvprt = <fs_edidc>-rcvprt.
            ls_edidc-rcvprn = <fs_edidc>-rcvprn.
            ls_edidc-sndpor = <fs_edidc>-sndpor.
            ls_edidc-sndprt = <fs_edidc>-sndprt.
            ls_edidc-sndsad = <fs_edidc>-sndsad.
            ls_edidc-sndprn = <fs_edidc>-sndprn.
            ls_edidc-idoctp = <fs_edidc>-idoctp.
            ls_edidc-mescod = <fs_edidc>-mescod.
            ls_edidc-mesfct = <fs_edidc>-mesfct.


            ls_edidc-mestyp = <fs_edidc>-mestyp.

            READ TABLE lt_edidd_temp TRANSPORTING NO FIELDS WITH KEY docnum = <fs_edidc>-docnum BINARY SEARCH.
            IF sy-subrc = 0.
              DATA(lv_index) = sy-tabix.
              LOOP AT lt_edidd_temp ASSIGNING FIELD-SYMBOL(<fs_edidd>) FROM lv_index.
                IF <fs_edidd>-docnum <> <fs_edidc>-docnum.
                  EXIT.
                ENDIF.
                lv_index = sy-tabix.
                IF lv_index IS INITIAL.
                  CONTINUE.
                ENDIF.
                CASE <fs_edidd>-segnam.
                  WHEN 'E1EDKA1'.
                    ls_e1edka1 = <fs_edidd>-sdata.

                    IF ls_e1edka1-parvw = 'AG'.

                      READ TABLE gt_edidd INTO DATA(ls_edidd_temp) INDEX lv_index.
                      IF sy-subrc IS INITIAL.
                        MOVE-CORRESPONDING ls_edidd_temp TO ls_edidd.
                        MODIFY lt_edidd_temp FROM ls_edidd INDEX lv_index.
                      ENDIF.

                    ELSEIF ls_e1edka1-parvw = 'WE'.
                      READ TABLE gt_edidd INTO ls_edidd_temp INDEX lv_index.
                      IF sy-subrc IS INITIAL.
                        MOVE-CORRESPONDING ls_edidd_temp TO ls_edidd.
                        MODIFY lt_edidd_temp FROM ls_edidd INDEX lv_index.
                      ENDIF.
                    ELSEIF ls_e1edka1-parvw = 'JM'.
                      READ TABLE gt_edidd INTO ls_edidd_temp INDEX lv_index.
*AEAS >>>>>>>>>>>>>>
                    ELSEIF ls_e1edka1-parvw = 'RE'.
                      READ TABLE gt_edidd INTO ls_edidd_temp INDEX lv_index.
*AEAS >>>>>>>>>>>>>>>

                      IF sy-subrc IS INITIAL.
                        MOVE-CORRESPONDING ls_edidd_temp TO ls_edidd.
                        MODIFY lt_edidd_temp FROM ls_edidd INDEX lv_index.
                      ENDIF.
                    ENDIF.

                  WHEN 'E1EDK02'.

                    ls_e1edk02 = <fs_edidd>-sdata.

                    IF ls_e1edk02-qualf = '001'.
                      READ TABLE gt_edidd INTO ls_edidd_temp INDEX lv_index.
                      IF sy-subrc IS INITIAL.
                        MOVE-CORRESPONDING ls_edidd_temp TO ls_edidd.
                        MODIFY lt_edidd_temp FROM ls_edidd INDEX lv_index.
                      ENDIF.
                    ENDIF.

                  WHEN OTHERS.
                ENDCASE.

                APPEND INITIAL LINE TO lt_edidd ASSIGNING FIELD-SYMBOL(<fs_append_eddid>).
                <fs_append_eddid>-segnam = <fs_edidd>-segnam.
                <fs_append_eddid>-sdata = <fs_edidd>-sdata.


                CLEAR :ls_edidd ,ls_edidd_temp.
              ENDLOOP.
            ENDIF.

            CLEAR lv_edited.
            READ TABLE gt_header ASSIGNING <fs_header> WITH KEY idoc_number = <fs_edidc>-docnum.
            IF sy-subrc = 0.
              READ TABLE gt_header_temp ASSIGNING <fs_header_temp> WITH KEY idoc_number = <fs_header>-idoc_number.
              IF sy-subrc IS  INITIAL.
                CASE sy-dynnr.
                  WHEN '0100'.
                    IF <fs_header> NE <fs_header_temp>.
                      lv_edited =  abap_true.
                    ENDIF.
                  WHEN '0200'.
                    IF <fs_header_temp>-po_number NE gv_ponum
                    OR <fs_header_temp>-sold_to_code NE gv_sold_to
                    OR <fs_header_temp>-ship_to_code NE gv_ship_to
                    OR <fs_header_temp>-store_code NE gv_store_code
*AEAS_37777 >>>>>>>>>>>>
                    OR <fs_header_temp>-bill_to_code NE gv_bill_to.
*AEAS_37777 >>>>>>>>>>>>
                      lv_edited =  abap_true.
                    ENDIF.
                ENDCASE.

              ENDIF.

            ENDIF.

            PERFORM f_idoc_create USING lt_edidd <fs_edidc>-docnum lv_edited CHANGING ls_edidc.
            IF ls_edidc-docnum <> <fs_edidc>-docnum.
              PERFORM f_set_idoc_status USING <fs_edidc>-docnum abap_true.
*            ELSE.
*              PERFORM f_set_idoc_status USING <fs_edidc>-docnum space.
            ENDIF.
            APPEND ls_edidc TO lt_edidc.
            CLEAR: lt_edidd, ls_edidc.

          ENDLOOP.
        ENDIF.
      ENDIF.

    ELSE.

      SELECT  *
        FROM edidc
        WHERE docnum IN @lr_docnum
        INTO TABLE @lt_edidc_temp.
      IF sy-subrc IS INITIAL.

        SELECT *
       FROM edid4
       INTO CORRESPONDING FIELDS OF TABLE lt_edidd_temp
       WHERE docnum IN lr_docnum.
        IF sy-subrc IS INITIAL.

          SORT lt_edidc_temp BY docnum.
          SORT lt_edidd_temp BY docnum segnum.

          LOOP AT lt_edidc_temp ASSIGNING <fs_edidc> WHERE docnum IN lr_docnum.

            ls_edidc-direct = '2'.
            ls_edidc-rcvpor = <fs_edidc>-rcvpor.
            ls_edidc-rcvprt = <fs_edidc>-rcvprt.
            ls_edidc-rcvprn = <fs_edidc>-rcvprn.
            ls_edidc-sndpor = <fs_edidc>-sndpor.
            ls_edidc-sndprt = <fs_edidc>-sndprt.
            ls_edidc-sndsad = <fs_edidc>-sndsad.
            ls_edidc-sndprn = <fs_edidc>-sndprn.
            ls_edidc-idoctp = <fs_edidc>-idoctp.
            ls_edidc-mescod = <fs_edidc>-mescod.
            ls_edidc-mesfct = <fs_edidc>-mesfct.


            ls_edidc-mestyp = <fs_edidc>-mestyp.

            READ TABLE lt_edidd_temp TRANSPORTING NO FIELDS WITH KEY docnum = <fs_edidc>-docnum BINARY SEARCH.
            IF sy-subrc = 0.
              lv_index = sy-tabix.
              LOOP AT lt_edidd_temp ASSIGNING <fs_edidd> FROM lv_index.
                IF <fs_edidd>-docnum <> <fs_edidc>-docnum.
                  EXIT.
                ENDIF.

                APPEND INITIAL LINE TO lt_edidd ASSIGNING <fs_append_eddid>.
                <fs_append_eddid>-segnam = <fs_edidd>-segnam.
                <fs_append_eddid>-sdata = <fs_edidd>-sdata.

              ENDLOOP.
            ENDIF.

            CLEAR lv_edited.

            READ TABLE gt_header ASSIGNING <fs_header> WITH KEY idoc_number = <fs_edidc>-docnum.
            IF sy-subrc = 0.
              READ TABLE gt_header_temp ASSIGNING <fs_header_temp> WITH KEY idoc_number = <fs_header>-idoc_number.
              IF sy-subrc IS  INITIAL.
                CASE sy-dynnr.
                  WHEN '0100'.
                    IF <fs_header> NE <fs_header_temp>.
                      lv_edited =  abap_true.
                    ENDIF.
                  WHEN '0200'.
                    IF <fs_header_temp>-po_number NE gv_ponum
                    OR <fs_header_temp>-sold_to_code NE gv_sold_to
                    OR <fs_header_temp>-ship_to_code NE gv_ship_to
                    OR <fs_header_temp>-store_code NE gv_store_code
*AEAS_37777 >>>>>>>>>>>>
                    OR <fs_header_temp>-bill_to_code NE gv_bill_to.
*AEAS_37777 >>>>>>>>>>>>
                      lv_edited =  abap_true.
                    ENDIF.
                ENDCASE.
              ENDIF.

            ENDIF.

            PERFORM f_idoc_create USING lt_edidd <fs_edidc>-docnum lv_edited CHANGING ls_edidc.
            IF ls_edidc-docnum <> <fs_edidc>-docnum.
              PERFORM f_set_idoc_status USING <fs_edidc>-docnum abap_true.
*            ELSE.
*              PERFORM f_set_idoc_status USING <fs_edidc>-docnum space.
            ENDIF.
            APPEND ls_edidc TO lt_edidc.
            CLEAR: lt_edidd, ls_edidc.

          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
    lv_message = TEXT-t20.
    lv_records = lines( lt_edidc ).
    CONDENSE lv_records.
    REPLACE ALL OCCURRENCES OF '&1' IN lv_message WITH lv_records.

    MESSAGE lv_message TYPE 'S'.


    CLEAR: gt_edidc, gt_edidd.


  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_idoc_create
*&---------------------------------------------------------------------*
FORM f_idoc_create  USING  ut_edidd      TYPE edidd_tt
                           uv_docnum     TYPE edidc-docnum
                           uv_edted      TYPE xfeld
                  CHANGING cs_edidc      TYPE edidc .

  DATA: lv_docnum           TYPE edidc-docnum,
        lv_state            TYPE sysubrc,
        ls_inb_process_data TYPE tede2.

  DATA lr_idoc TYPE RANGE OF bdidocs-docnum.

  CLEAR lr_idoc.
  APPEND INITIAL LINE TO lr_idoc ASSIGNING FIELD-SYMBOL(<fs_idoc>).
  <fs_idoc>-sign = 'I'.
  <fs_idoc>-option = 'EQ'.
  <fs_idoc>-low = uv_docnum.

  IF sy-dynnr EQ '0200'.

    CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
      EXPORTING
        pi_do_handle_error      = abap_true
        pi_no_dequeue           = space
        pi_return_data_flag     = abap_true
      IMPORTING
        pe_idoc_number          = lv_docnum
        pe_state_of_processing  = lv_state
        pe_inbound_process_data = ls_inb_process_data
      TABLES
        t_data_records          = ut_edidd
      CHANGING
        pc_control_record       = cs_edidc
      EXCEPTIONS
        idoc_not_saved          = 1
        OTHERS                  = 2.



    IF sy-subrc = 0.
      cs_edidc-docnum = lv_docnum.

      SUBMIT rbdapp01
      WITH docnum = lv_docnum
      WITH p_output = space
      AND RETURN.

    ENDIF.

  ELSEIF sy-dynnr EQ '0100'.
    IF uv_edted IS NOT INITIAL.

      DATA: lt_edidd TYPE STANDARD TABLE OF edidd."Local Table to Hold EDIDD
      lt_edidd = ut_edidd. "table should have the data
*-- Opening the IDoc for Edit
      CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_EDIT'
        EXPORTING
          document_number               = uv_docnum
        TABLES
          idoc_data                     = lt_edidd
        EXCEPTIONS
          document_foreign_lock         = 1
          document_not_exist            = 2
          document_not_open             = 3
          status_is_unable_for_changing = 4
          OTHERS                        = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
*-- Editing the IDoc
      CALL FUNCTION 'EDI_CHANGE_DATA_SEGMENTS'
        TABLES
          idoc_changed_data_range = lt_edidd
        EXCEPTIONS
          idoc_not_open           = 1
          data_record_not_exist   = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
*-- Closing the IDoc after Edit
      CALL FUNCTION 'EDI_DOCUMENT_CLOSE_EDIT'
        EXPORTING
          document_number  = uv_docnum
          do_commit        = 'X'
          do_update        = 'X'
          write_all_status = 'X'
        EXCEPTIONS
          idoc_not_open    = 1
          db_error         = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      SUBMIT rbdagaie
        WITH p_idoc IN lr_idoc " This represent Edited IDoc numbers
        WITH p_direct = '2'  " Here 1 for Outbound 2 for Inbound
        WITH p_output = ' '
        AND RETURN.

    ELSE.

      SUBMIT rbdmani2
         WITH so_docnu IN lr_idoc
         WITH p_output = space
         AND RETURN.

    ENDIF.

    cs_edidc-docnum = uv_docnum.

  ENDIF.

ENDFORM.

FORM f_set_input_var USING us_header TYPE ty_idoc_header
                           us_sname  TYPE lvc_s_col.
  DATA: ls_e1edka1  TYPE e1edka1.
  DATA: ls_e1edk02  TYPE e1edk02.
  DATA: lv_bp_type  TYPE char2.

  DATA lr_docnum TYPE RANGE OF edid4-docnum.



  IF gt_edidd IS INITIAL.
    LOOP AT gt_header ASSIGNING FIELD-SYMBOL(<fs_header>).
      APPEND INITIAL LINE TO lr_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>).
      <fs_docnum>-sign = 'I'.
      <fs_docnum>-option = 'EQ'.
      <fs_docnum>-low = <fs_header>-idoc_number.

    ENDLOOP.

    SELECT *
         FROM edid4
         INTO TABLE @DATA(lt_edidd)
       WHERE docnum IN @lr_docnum.
    IF sy-subrc IS INITIAL.
      gt_edidd = lt_edidd.
    ENDIF.
  ELSE.
    lt_edidd = gt_edidd.
  ENDIF.
  IF lt_edidd IS NOT INITIAL.
    SORT lt_edidd BY segnum.

    LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_adr>) WHERE docnum = us_header-idoc_number.
      IF <fs_edidd_adr>-segnam = 'E1EDKA1'.
        ls_e1edka1 = <fs_edidd_adr>-sdata.

        IF ls_e1edka1-parvw = 'AG'.
          gv_sold_to  = ls_e1edka1-partn.
          gv_parvwag   = 'AG'.
        ELSEIF ls_e1edka1-parvw = 'WE'.
          gv_ship_to  = ls_e1edka1-partn.
          gv_parvwwe   = 'WE'.
        ELSEIF ls_e1edka1-parvw = 'JM'.
          gv_store_code = ls_e1edka1-partn.
          gv_parvwjm   = 'JM'.
*AEAS_37777 >>>>>>>>>>>>
        ELSEIF ls_e1edka1-parvw = 'RE'.
          gv_bill_to = ls_e1edka1-partn.
          gv_parvwre   = 'RE'.
*AEAS_37777 <<<<<<<<<<<<<
        ENDIF.
      ELSEIF <fs_edidd_adr>-segnam = 'E1EDK02'.
        CLEAR ls_e1edk02.
        ls_e1edk02 = <fs_edidd_adr>-sdata.
        IF ls_e1edk02-qualf = '001' .
          gv_ponum = ls_e1edk02-belnr.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.


FORM f_clear_data.
  CLEAR: gv_bpn_in, gv_fn_in, gv_ln_in, gv_st_in,
         gv_pc_in, gv_city_in, gv_cc_in, gv_ph1_in,
         gv_ph2_in, gv_tlf_in, gv_tlb_in, gv_docnum, gv_parvwag, gv_parvwwe, gv_parvwjm,gv_parvwre,
         gv_store_code, gv_ship_to, gv_sold_to,gv_bill_to,gv_ponum.
ENDFORM.


FORM f_get_idoc_items.
  DATA: lt_sel_idoc TYPE lvc_t_row,
        ls_e1edp01  TYPE e1edp01,
        ls_e1edp19  TYPE e1edp19,
        ls_e1edka1  TYPE e1edka1.
  DATA: lv_sgt_rcat TYPE vbap-sgt_rcat.
  DATA: lv_counter TYPE i.
  CLEAR gt_items.

  CALL METHOD go_alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = lt_sel_idoc.

  IF lines( lt_sel_idoc ) > 1.
    MESSAGE TEXT-m02 TYPE 'S'.
  ELSEIF lines( lt_sel_idoc ) = 0.
    MESSAGE TEXT-m03 TYPE 'S'.
  ELSEIF lines( lt_sel_idoc ) = 1.
    READ TABLE lt_sel_idoc ASSIGNING FIELD-SYMBOL(<fs_sel_idoc>) INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_header ASSIGNING FIELD-SYMBOL(<fs_header>) INDEX <fs_sel_idoc>-index.
      IF sy-subrc = 0.
*        IF <fs_header>-mestyp <> 'INVOIC'.
        IF <fs_header>-status <> icon_green_light AND <fs_header>-status <> icon_yellow_light.
          SELECT SINGLE *
           FROM edidc
           WHERE docnum = @<fs_header>-idoc_number
           INTO @DATA(ls_edidc).

          SELECT *
            FROM edid4
            INTO TABLE @DATA(lt_edidd)
          WHERE docnum = @<fs_header>-idoc_number.

          IF lt_edidd IS NOT INITIAL.
            SORT lt_edidd BY docnum segnum.
            LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_adr>).
*
              IF <fs_edidd_adr>-segnam = 'E1EDKA1'.
                ls_e1edka1 = <fs_edidd_adr>-sdata.
                IF ls_e1edka1-parvw = 'AG'.
                  DATA(lv_oms_ord_no) = ls_e1edka1-ihrez.
                ENDIF.
              ENDIF.

              IF <fs_edidd_adr>-segnam = 'E1EDP01'.
                DATA(lv_index) = sy-tabix + 1.
                ls_e1edp01 = <fs_edidd_adr>-sdata.

                APPEND INITIAL LINE TO gt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
                lv_counter = lv_counter + 1.
                <fs_items>-docnum = <fs_header>-idoc_number.
                <fs_items>-oms_ord_no = lv_oms_ord_no.

                <fs_items>-posex = lv_counter.

                LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_n>) FROM lv_index.
                  IF <fs_edidd_n>-segnam = 'E1EDP01'.
                    EXIT.
                  ENDIF.
                  IF <fs_edidd_n>-segnam = 'E1EDP19'.
                    ls_e1edp19 = <fs_edidd_n>-sdata.

                    IF ls_e1edp19-idtnr IS NOT INITIAL OR ls_e1edp19-idtnr_long IS NOT INITIAL OR ls_e1edp19-idtnr_external IS NOT INITIAL.

                      IF ls_edidc-sndprn = 'WEBORDER'.

                        SELECT atwrt UP TO 1 ROWS
                         INTO  @DATA(lv_atwrt)
                         FROM wrf_charval
                         JOIN wrf_charval_head ON wrf_charval~atinn = wrf_charval_head~atinn
                         WHERE wrf_charval_head~atnam = @ls_e1edp19-idtnr_external
                           AND wrf_charval~srtpo = @ls_e1edp19-idtnr_version.
                        ENDSELECT.

                        REPLACE ALL OCCURRENCES OF '-' IN ls_e1edp19-idtnr_long WITH '_'.

                        <fs_items>-idntr = ls_e1edp19-idtnr_long && '_' && lv_atwrt.
                      ELSEIF ls_edidc-sndprn = 'NETEVEN'.
                        IF ls_e1edp19-idtnr IS NOT INITIAL.
                          <fs_items>-ean = ls_e1edp19-idtnr.
                        ELSEIF ls_e1edp19-idtnr_long IS NOT INITIAL.
                          <fs_items>-idntr = ls_e1edp19-idtnr_long.
                        ENDIF.
                      ELSE.
                        IF ls_e1edp19-idtnr IS NOT INITIAL.
                          <fs_items>-idntr = ls_e1edp19-idtnr.
                        ELSE.
                          <fs_items>-idntr = ls_e1edp19-idtnr_long.
                        ENDIF.
                      ENDIF.

                      SELECT  ean11 UP TO 1 ROWS
                        INTO @DATA(lv_ean11)
                        FROM mean
                        WHERE matnr = @<fs_items>-idntr.
                      ENDSELECT.

                      IF sy-subrc = 0.
                        <fs_items>-ean = lv_ean11.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
                <fs_items>-menge = ls_e1edp01-menge.
                <fs_items>-index = lv_index.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF gt_items IS NOT INITIAL.
            SORT gt_items BY docnum index.
            gv_docnum = <fs_header>-idoc_number.
          ELSE.
            MESSAGE TEXT-m07 TYPE 'S'.
          ENDIF.
        ELSE.
          MESSAGE TEXT-m05 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
*        ELSE.
*          MESSAGE TEXT-m09 TYPE 'S' DISPLAY LIKE 'E'.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.


FORM f_process_itm_data.

  DATA: ls_e1edp01 TYPE e1edp01,
        ls_e1edp19 TYPE e1edp19,
        lt_items   TYPE tty_idoc_items,
        lv_counter TYPE i,
        lv_idntr1  TYPE string,
        lv_idntr2  TYPE string,
        lv_idntr3  TYPE string.

  lt_items = gt_items.

  CALL METHOD go_alvgrid_i->check_changed_data.

  SELECT SINGLE *
   FROM edidc
   WHERE docnum = @gv_docnum
   INTO @DATA(ls_edidc).

  SORT gt_edidc BY docnum.

  READ TABLE gt_edidc TRANSPORTING NO FIELDS WITH KEY docnum = ls_edidc-docnum BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND ls_edidc TO gt_edidc.
  ENDIF.

  SORT gt_edidd BY docnum segnum.

  READ TABLE gt_edidd ASSIGNING FIELD-SYMBOL(<fs_gtedidd>) WITH KEY docnum = ls_edidc-docnum BINARY SEARCH.
  IF sy-subrc <> 0.

    SELECT *
     FROM edid4
     INTO TABLE @DATA(lt_edidd)
   WHERE docnum = @gv_docnum.

    IF lt_edidd IS NOT INITIAL.
      SORT lt_edidd BY docnum segnum.
      LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_adr>).
        IF <fs_edidd_adr>-segnam = 'E1EDP01'.
          DATA(lv_index) = sy-tabix + 1.
          ls_e1edp01 = <fs_edidd_adr>-sdata.
          READ TABLE gt_items ASSIGNING FIELD-SYMBOL(<fs_items>) WITH KEY docnum = <fs_edidd_adr>-docnum index = lv_index BINARY SEARCH.
          IF sy-subrc = 0.
            lv_counter = lv_counter + 1.
            ls_e1edp01-posex = lv_counter.
            ls_e1edp01-menge = <fs_items>-menge.
            <fs_edidd_adr>-sdata = ls_e1edp01.
            IF <fs_items>-delete = 'X'.
              CLEAR <fs_edidd_adr>.
              lv_counter = lv_counter - 1.
            ENDIF.
            LOOP AT lt_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd_n>) FROM lv_index.
              IF <fs_edidd_n>-segnam = 'E1EDP01'.
                EXIT.
              ENDIF.
              IF <fs_edidd_n>-segnam = 'E1EDP19'.
                ls_e1edp19 = <fs_edidd_n>-sdata.

                IF ls_edidc-sndprn = 'WEBORDER'.
                  SPLIT <fs_items>-idntr AT '_' INTO lv_idntr1 lv_idntr2 lv_idntr3.
                  ls_e1edp19-idtnr_long = lv_idntr1 && '_' && lv_idntr2.
                ELSE.
                  IF ls_e1edp19-idtnr IS NOT INITIAL.
                    ls_e1edp19-idtnr = <fs_items>-idntr.
                  ELSEIF ls_e1edp19-idtnr_long IS NOT INITIAL.
                    ls_e1edp19-idtnr_long = <fs_items>-idntr.
                  ENDIF.
                ENDIF.

                <fs_edidd_n>-sdata = ls_e1edp19.
                IF <fs_items>-delete = 'X'.
                  CLEAR <fs_edidd_n>.
                ENDIF.
              ELSE.
                IF <fs_items>-delete = 'X'.
                  CLEAR <fs_edidd_n>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDLOOP.
      DELETE lt_edidd WHERE docnum = space.
      APPEND LINES OF lt_edidd TO gt_edidd.
    ENDIF.

  ELSE.
    lv_index = sy-tabix.
    LOOP AT gt_edidd ASSIGNING <fs_gtedidd> FROM lv_index.
      IF <fs_gtedidd>-segnam = 'E1EDP01'.
        lv_counter = lv_counter + 1.
        lv_index = sy-tabix + 1.
        ls_e1edp01 = <fs_gtedidd>-sdata.
        READ TABLE gt_items ASSIGNING <fs_items> WITH KEY docnum = <fs_gtedidd>-docnum index = lv_index BINARY SEARCH.
        IF sy-subrc = 0.
          ls_e1edp01-posex = lv_counter.
          ls_e1edp01-menge = <fs_items>-menge.
          <fs_gtedidd>-sdata = ls_e1edp01.
          IF <fs_items>-delete = 'X'.
            CLEAR <fs_gtedidd>.
            lv_counter = lv_counter - 1.
          ENDIF.
          LOOP AT gt_edidd ASSIGNING FIELD-SYMBOL(<fs_gtedidd_n>) FROM lv_index.
            IF <fs_gtedidd_n>-segnam = 'E1EDP01'.
              EXIT.
            ENDIF.
            IF <fs_gtedidd_n>-segnam = 'E1EDP19'.
              ls_e1edp19 = <fs_gtedidd_n>-sdata.

              IF ls_edidc-sndprn = 'WEBORDER'.
                SPLIT <fs_items>-idntr AT '_' INTO lv_idntr1 lv_idntr2 lv_idntr3.
                ls_e1edp19-idtnr_long = lv_idntr1 && '_' && lv_idntr2.
              ELSE.
                IF ls_e1edp19-idtnr IS NOT INITIAL.
                  ls_e1edp19-idtnr = <fs_items>-idntr.
                ELSEIF ls_e1edp19-idtnr_long IS NOT INITIAL.
                  ls_e1edp19-idtnr_long = <fs_items>-idntr.
                ENDIF.
              ENDIF.

              IF <fs_items>-delete = 'X'.
                CLEAR <fs_gtedidd_n>.
              ENDIF.

              <fs_gtedidd_n>-sdata = ls_e1edp19.
            ENDIF.
            IF <fs_items>-delete = 'X'.
              CLEAR <fs_gtedidd_n>.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF lt_items <> gt_items.
    MESSAGE TEXT-m04 TYPE 'S'.
  ELSE.
    MESSAGE TEXT-m06 TYPE 'S'.
  ENDIF.

  CLEAR gv_docnum.
ENDFORM.


FORM f_set_idoc_status USING uv_docnum TYPE edidc-docnum
                             uv_archive TYPE xfeld.

  DATA: lt_status TYPE STANDARD TABLE OF bdidocstat.
  DATA: lt_sel_idoc   TYPE lvc_t_row,
        lv_stalight   TYPE edi_slight,
        lv_statustext   TYPE edi_text60,
        ls_status       TYPE edids,
        lv_message    TYPE string.



  IF uv_docnum IS NOT INITIAL.
    SORT gt_header BY idoc_number.
    READ TABLE gt_header ASSIGNING FIELD-SYMBOL(<fs_header>) WITH KEY idoc_number = uv_docnum BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      IF uv_archive IS NOT INITIAL.

        APPEND INITIAL LINE TO lt_status ASSIGNING FIELD-SYMBOL(<fs_status>).
        <fs_status>-docnum = uv_docnum.
        <fs_status>-status = '68'.

        CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
          EXPORTING
            idoc_number               = <fs_status>-docnum
          TABLES
            idoc_status               = lt_status
          EXCEPTIONS
            idoc_foreign_lock         = 1
            idoc_not_found            = 2
            idoc_status_records_empty = 3
            idoc_status_invalid       = 4
            db_error                  = 5
            OTHERS                    = 6.


*      IF sy-subrc = 0.

**      CALL FUNCTION 'EDI_DOCUMENT_READ_LAST_STATUS'
**        EXPORTING
**          document_number = uv_docnum
**        IMPORTING
**          status          = ls_status
**   EXCEPTIONS
**         DOCUMENT_NOT_OPEN            = 1
**         NO_STATUS_RECORD_FOUND       = 2
**         OTHERS          = 3
**        .
**      IF sy-subrc <> 0.
** Implement suitable error handling here
**      ENDIF.
**
**      IF ls_status-status = 53.
**
**      ENDIF.

        CALL FUNCTION 'ISU_IDOC_GET_STATUS_ICON'
          EXPORTING
            x_docnum   = uv_docnum
          IMPORTING
            y_stalight = lv_stalight.

        IF sy-subrc = 0.
          CASE lv_stalight.
            WHEN '1'.
              <fs_header>-idoc_status = icon_yellow_light.
            WHEN '2'.
              <fs_header>-idoc_status = icon_green_light.
            WHEN '3'.
              <fs_header>-idoc_status = icon_red_light.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

        SELECT docnum, countr, logdat, logtim, stamid, stamno, stapa1, stapa2, stapa3, stapa4
          FROM edids
          INTO TABLE @DATA(lt_edids)
          WHERE docnum = @uv_docnum.


        IF sy-subrc = 0.
          SORT lt_edids BY docnum countr  DESCENDING.
          DELETE ADJACENT DUPLICATES FROM lt_edids COMPARING docnum.

          READ TABLE lt_edids INTO DATA(ls_edids) INDEX 1.

          IF sy-subrc IS INITIAL.

            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = ls_edids-stamid
                msgnr               = ls_edids-stamno
                msgv1               = ls_edids-stapa1
                msgv2               = ls_edids-stapa2
                msgv3               = ls_edids-stapa3
                msgv4               = ls_edids-stapa4
              IMPORTING
                message_text_output = lv_message.

            <fs_header>-error_message_description = lv_message.
          ENDIF.

        ENDIF.


*      ENDIF.
      ELSE.
        CALL FUNCTION 'ISU_IDOC_GET_STATUS_ICON'
          EXPORTING
            x_docnum     = uv_docnum
          IMPORTING
            y_stalight   = lv_stalight
            y_statustext = lv_statustext.


        SELECT docnum, countr, logdat, logtim, stamid, stamno, stapa1, stapa2, stapa3, stapa4
               FROM edids
               INTO TABLE @lt_edids
               WHERE docnum = @uv_docnum.

        IF sy-subrc = 0.

          SORT lt_edids BY docnum countr  DESCENDING.
          DELETE ADJACENT DUPLICATES FROM lt_edids COMPARING docnum.

          READ TABLE lt_edids INTO  ls_edids INDEX 1.

          IF sy-subrc IS INITIAL.

            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = ls_edids-stamid
                msgnr               = ls_edids-stamno
                msgv1               = ls_edids-stapa1
                msgv2               = ls_edids-stapa2
                msgv3               = ls_edids-stapa3
                msgv4               = ls_edids-stapa4
              IMPORTING
                message_text_output = lv_message.

            CASE lv_stalight.
              WHEN '1'.
                CONCATENATE uv_docnum  lv_message INTO lv_message SEPARATED BY space.
                MESSAGE lv_message TYPE 'I'.
              WHEN '2'.
                CONCATENATE uv_docnum lv_message INTO lv_message SEPARATED BY space.
                MESSAGE lv_message TYPE 'I'.
              WHEN '3'.
                MESSAGE lv_message TYPE 'I'.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ELSE.

    CALL METHOD go_alvgrid->get_selected_rows
      IMPORTING
        et_index_rows = lt_sel_idoc.

    LOOP AT lt_sel_idoc ASSIGNING FIELD-SYMBOL(<fs_sel_idoc>).

      READ TABLE gt_header ASSIGNING <fs_header> INDEX <fs_sel_idoc>-index.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lt_status ASSIGNING <fs_status>.
        <fs_status>-docnum = <fs_header>-idoc_number.
        <fs_status>-status = '68'.

        CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
          EXPORTING
            idoc_number               = <fs_status>-docnum
          TABLES
            idoc_status               = lt_status
          EXCEPTIONS
            idoc_foreign_lock         = 1
            idoc_not_found            = 2
            idoc_status_records_empty = 3
            idoc_status_invalid       = 4
            db_error                  = 5
            OTHERS                    = 6.
*
        IF sy-subrc = 0.
          SELECT docnum, countr, logdat, logtim, stamid, stamno, stapa1, stapa2, stapa3, stapa4
         FROM edids
         INTO TABLE @lt_edids
         WHERE docnum = @uv_docnum.

          SORT lt_edids BY docnum countr DESCENDING.
          DELETE ADJACENT DUPLICATES FROM lt_edids COMPARING docnum.

          READ TABLE lt_edids INTO  ls_edids INDEX 1.

*
          IF sy-subrc = 0.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = ls_edids-stamid
                msgnr               = ls_edids-stamno
                msgv1               = ls_edids-stapa1
                msgv2               = ls_edids-stapa2
                msgv3               = ls_edids-stapa3
                msgv4               = ls_edids-stapa4
              IMPORTING
                message_text_output = lv_message.

            <fs_header>-error_message_description = lv_message.
            <fs_header>-status = '68'.

            CALL FUNCTION 'ISU_IDOC_GET_STATUS_ICON'
              EXPORTING
                x_docnum   = <fs_status>-docnum
              IMPORTING
                y_stalight = lv_stalight.

            IF sy-subrc = 0.
              CASE lv_stalight.
                WHEN '1'.
                  <fs_header>-idoc_status = icon_yellow_light.
                WHEN '2'.
                  <fs_header>-idoc_status = icon_green_light.
                WHEN '3'.
                  <fs_header>-idoc_status = icon_red_light.
                WHEN OTHERS.
              ENDCASE.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR lt_status.
      ENDIF.
    ENDLOOP.
    MESSAGE TEXT-m08 TYPE 'S'.

  ENDIF.

  IF sy-dynnr = '0100'.
    go_alvgrid->refresh_table_display( EXPORTING is_stable = gs_stbl ).
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_USER_CHECK
*&---------------------------------------------------------------------*
FORM f_user_check  CHANGING ct_header TYPE tty_idoc_header
                            cv_error TYPE xfeld.

  DATA(lt_header_temp) = ct_header.

  CLEAR ct_header.

  LOOP AT lt_header_temp ASSIGNING FIELD-SYMBOL(<fs_header_temp>).

    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
       ID 'VKORG' FIELD <fs_header_temp>-sales_organization.

    IF sy-subrc IS INITIAL.

      AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
       ID 'VKORG' FIELD <fs_header_temp>-sales_organization.

      IF sy-subrc IS INITIAL.
        APPEND INITIAL LINE TO ct_header ASSIGNING FIELD-SYMBOL(<fs_header>).
        MOVE-CORRESPONDING <fs_header_temp> TO <fs_header>.
      ENDIF.

    ENDIF.
  ENDLOOP.

  IF  ct_header IS INITIAL.
    cv_error  = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm  CHANGING cv_continue.

  DATA: lv_answer TYPE n LENGTH 1.
  DATA: lv_question TYPE char40,
        lv_lines    TYPE string.

  CALL METHOD go_alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_sel_idoc).


  lv_question = TEXT-t19.
  lv_lines = lines( lt_sel_idoc ).
  CONDENSE lv_lines.
  REPLACE ALL OCCURRENCES OF '&1' IN lv_question WITH lv_lines.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
*     TITLEBAR              = ' '
*     DIAGNOSE_OBJECT       = ' '
      text_question         = lv_question
      text_button_1         = TEXT-t21
*     ICON_BUTTON_1         = ' '
      text_button_2         = TEXT-t22
*     ICON_BUTTON_2         = ' '
      default_button        = '1'
      display_cancel_button = 'X'
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      answer                = lv_answer
*       TABLES
*     PARAMETER             =
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CASE lv_answer.
    WHEN '1'.
      cv_continue = abap_true.
    WHEN '2'.
      cv_continue = abap_false.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_SCREEN
*&---------------------------------------------------------------------*
FORM f_refresh_screen USING uv_screen TYPE screen-name.
  CLEAR gt_header.
  DATA(lv_temp) = abap_false.
  PERFORM get_idocs CHANGING lv_temp.          "CHANGING gt_header.
  IF gt_header IS NOT INITIAL.
    PERFORM f_display_header. "USING gt_header.
    LEAVE TO SCREEN uv_screen.
  ELSE.
    MESSAGE TEXT-m10 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.
