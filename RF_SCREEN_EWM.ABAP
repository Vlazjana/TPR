
FUNCTION zscwm_zpmat1_1_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZMATERIAL_LABEL) TYPE  ZMATERIAL_LABEL
*"----------------------------------------------------------------------


  DATA: lv_fcode   TYPE /scwm/de_fcode,
        lv_field   TYPE c LENGTH 60,
        lv_pmat    TYPE /scwm/de_matid,
        lt_uom     TYPE /scwm/tt_material_uom,
        lv_huident TYPE /scwm/de_huident,
        ls_huhdr   TYPE LINE OF /scwm/tt_huhdr_int,
        lt_huhdr   TYPE /scwm/tt_huhdr_int,
        lt_huitm   TYPE /scwm/tt_huitm_int,
        lt_hutree  TYPE /scwm/tt_hutree,
        lv_matid   TYPE /scwm/de_matid,
        gv_back1   TYPE char1,
        lt_high    TYPE /scwm/tt_huhdr_int,
        lv_max_rw  TYPE i,
        lr_lgort   TYPE RANGE OF mchb-lgort,
        lv_matnr   TYPE matnr.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  lv_field = /scwm/cl_rf_bll_srvc=>get_cursor_field( ).
  DATA(lv_inp_attr) = /scwm/cl_rf_bll_srvc=>get_screlm_inp_attrib_from_bll( 'ZMATERIAL_LABEL-MATNR' ).

  lv_matnr = zmaterial_label-matnr.


  CASE lv_fcode.
    WHEN 'ENTER'.
      CASE lv_field.
        WHEN 'ZMATERIAL_LABEL-MATNR'.
          IF zmaterial_label-matnr IS NOT INITIAL .
*
            CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
              EXPORTING
                input  = zmaterial_label-matnr
              IMPORTING
                output = lv_matid.

* enter with matnr, get oldest charg, check aqua, ignore blocked

            lr_lgort = VALUE #( sign   = 'I'
                                option = 'EQ'
                                ( low = 'BLU1' )
                                ( low = 'PRSH' )
                                ( low = 'QUAL' )
                                ( low = 'R200' )
                                ( low = 'R500' )
                                ( low = 'SORT' )
                                ( low = 'VIM1' )
                                ).
     


            SELECT m~charg,
                   m~lgort, "SLOC
                   m~clabs AS quan,
                   a~lgpla,
                   a~cat

              FROM mchb AS m

              LEFT JOIN /scwm/aqua AS a
                 ON m~charg = a~charg
                AND a~matid = @lv_matid
                AND m~lgort IN ( 'BLU1',
                                 'PRSH',
                                 'QUAL',
                                 'R200',
                                 'R500',
                                 'SORT',
                                 'VIM1' )

                WHERE m~matnr = @zmaterial_label-matnr
                  AND m~werks = 'IT10'
                  AND m~clabs <> 0

                INTO TABLE @DATA(lt_mchb).

            DELETE lt_mchb WHERE lgort IN lr_lgort AND cat NP 'F*'.

            IF lt_mchb IS NOT INITIAL.
              SELECT ms~charg, 
                     Ms~budat_mkpf  (gr_date
                     Ms~cpudt_mkpf as entry_date 
                     Ms~cputm_mkpf as entry_time
                FROM mseg AS ms
                FOR ALL ENTRIES IN @lt_mchb
                WHERE ms~matnr = @zmaterial_label-matnr
                  AND ms~charg = @lt_mchb-charg
                  AND ms~insmk IN ( @space, 'F' )
                  AND ms~werks = 'IT10'
AND ms~bwart IN ('101', '561', '712')  "GR movement types

  ORDER BY ms~charg, ms~budat_mkpf ASCENDING, ms~cpudt ASCENDING, ms~cputm ASCENDING

                INTO TABLE @DATA(lt_mseg).

              SORT lt_mseg BY charg budat_mkpf “entry_date entry_time
“.
              DELETE ADJACENT DUPLICATES FROM lt_mseg COMPARING charg budat_mkpf.

            ENDIF.

            LOOP AT lt_mchb ASSIGNING FIELD-SYMBOL(<ls_mchb>).
              APPEND INITIAL LINE TO zmaterial_t_label ASSIGNING FIELD-SYMBOL(<ls_mat_lbl>).
              <ls_mat_lbl> = CORRESPONDING #( <ls_mchb> ).

              READ TABLE lt_mseg ASSIGNING FIELD-SYMBOL(<ls_mseg>) WITH KEY charg = <ls_mchb>-charg BINARY SEARCH.
              IF sy-subrc = 0.
                <ls_mat_lbl>-budat_mkpf = <ls_mseg>-budat_mkpf.
            ls_mat_lbl>-entry_date = <ls_mseg>-entry_date.
            <ls_mat_lbl>-entry_time = <ls_mseg>-entry_time.

              ENDIF.

            ENDLOOP.
            CLEAR lt_mchb.

           SORT zmaterial_t_label BY budat_mkpf DESCENDING.
   "Sort by oldest GR date/time first
 
       SORT zmaterial_t_label BY budat_mkpf ASCENDING entry_date ASCENDING entry_time ASCENDING.

            READ TABLE zmaterial_t_label INTO gs_mat_detail INDEX 1.

            gv_max_lines = lines( zmaterial_t_label ).

            IF gv_max_lines > 0.
              gv_curr_line = 1.
            ELSE.
              gv_curr_line = 0.
            ENDIF.

          ENDIF.
      ENDCASE.

    WHEN 'ZPDN'.
      IF gv_curr_line < gv_max_lines.
        gv_curr_line = gv_curr_line + 1.

        CLEAR gs_mat_detail.
        " Read the next record into the structure based on the current index
        READ TABLE zmaterial_t_label INTO gs_mat_detail INDEX gv_curr_line.
      ENDIF.

    WHEN 'ZPUP'.
      IF gv_curr_line > 1.
        gv_curr_line = gv_curr_line - 1.
        " Read the previous record into the structure based on the current index
        CLEAR gs_mat_detail.
        READ TABLE zmaterial_t_label INTO gs_mat_detail INDEX gv_curr_line.
      ENDIF.
  ENDCASE.
ENDFUNCTION.


FUNCTION ZSCWM_ZPMAT1_1_PBO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZMATERIAL_LABEL) TYPE  ZMATERIAL_LABEL
*"     REFERENCE(ZMATERIAL_T_LABEL) TYPE  ZMATERIAL_T_LABEL
*"----------------------------------------------------------------------

  DATA ls_rsrc     TYPE /scwm/rsrc.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZMATERIAL_LABEL'.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZMATERIAL_T_LABEL'.

  CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
    EXPORTING
      iv_uname = sy-uname
    CHANGING
      cs_rsrc  = ls_rsrc.

*  ZMATERIAL_LABEL-lgnum = ls_rsrc-lgnum.
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode_off( 'CLEAR' ).



ENDFUNCTION.
PROCESS BEFORE OUTPUT.


  MODULE status_sscr.
PROCESS AFTER INPUT.
  MODULE user_comand.
  MODULE user_command_sscr.


*&---------------------------------------------------------------------*
*& Include ZSCWM_TAB_PAI
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TC 'ZMAT_TAB_CONTR'. DO NOT CHANGE THIS LIN
*&SPWIZARD: PROCESS USER COMMAND
*MODULE zmat_tab_contr_user_command INPUT.
*  ok_code = sy-ucomm.
*  PERFORM user_ok_tc USING    'ZMAT_TAB_CONTR'
*                              '<GT_SCR>'
*                              ' '
*                     CHANGING ok_code.
*  sy-ucomm = ok_code.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_comand INPUT.

  DATA lv_matid TYPE /scwm/de_matid.


  CASE sy-ucomm.

    WHEN 'CLEAR'.
      CLEAR: <gt_scr>, gs_mat_detail, gv_curr_line, gv_max_lines.

*      free zmaterial_label1.
      /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
      /scwm/cl_rf_bll_srvc=>set_fcode(
         /scwm/cl_rf_bll_srvc=>c_fcode_clear_all ).
*      CLEAR zmaterial_label1.
*      RETURN.

*      FREE <gt_scr>.
*      /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
*      /scwm/cl_rf_bll_srvc=>set_fcode(
*         /scwm/cl_rf_bll_srvc=>c_fcode_clear_all ).
*      RETURN.

*      FREE zmaterial_t_label.
*      CLEAR zmaterial_label1.
*      /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
*      /scwm/cl_rf_bll_srvc=>set_fcode(
*         /scwm/cl_rf_bll_srvc=>c_fcode_clear_all ).
*      RETURN.

    WHEN 'BACK'.
      /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
      /scwm/cl_rf_bll_srvc=>set_fcode(
         /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).

  ENDCASE.

ENDMODULE.


SECOND BUTTON



FUNCTION zscwm_zbinhu_zbinp2_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZBIN_TO_BIN_PSA) TYPE  ZBIN_TO_BIN_PSA
*"     REFERENCE(ZBIN_TO_BIN_PSA_T) TYPE  ZBIN_TO_BIN_PSA_T
*"     REFERENCE(ZBIN_TO_BIN_PSA_S) TYPE  ZBIN_TO_BIN_PSA_S
*"     REFERENCE(ZCUSTOM_MESSAGE) TYPE  ZCUSTOM_MESSAGE
*"----------------------------------------------------------------------
  "----------------------------------------------------------------------


  DATA: lv_fcode    TYPE /scwm/de_fcode,
        lv_field    TYPE c LENGTH 60,
        lv_pmat     TYPE /scwm/de_matid,
        lt_uom      TYPE /scwm/tt_material_uom,
        lv_huident  TYPE /scwm/de_huident,
        ls_huhdr    TYPE LINE OF /scwm/tt_huhdr_int,
        lt_huhdr    TYPE /scwm/tt_huhdr_int,
        lt_huitm    TYPE /scwm/tt_huitm_int,
        lt_hutree   TYPE /scwm/tt_hutree,
        lv_huident1 TYPE /scwm/de_huident,
        lt_high     TYPE /scwm/tt_huhdr_int.

  DATA(lv_inp_attr) = /scwm/cl_rf_bll_srvc=>get_screlm_inp_attrib_from_bll( 'ZBIN_TO_BIN_PSA-HUIDENT' ).
  DATA: go_obj   TYPE REF TO /scwm/cl_mon_stock, iv_lgnum TYPE /scwm/lgnum,
        lv_found TYPE abap_bool VALUE abap_false.
  DATA: lt_huident_r TYPE /scwm/tt_huident_r,
        lt_lgpla_r   TYPE /scwm/tt_lgpla_r,
        lt_stock_mon TYPE /scwm/tt_stock_mon,
        lv_matid     TYPE /scwm/de_matid,
        lv_error     TYPE abap_bool.


  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  lv_field = /scwm/cl_rf_bll_srvc=>get_cursor_field( ).

  CASE lv_fcode.

    WHEN 'CLEAR'.

      IF lv_inp_attr = '0'.
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on( 'ZBIN_TO_BIN_PSA-HUIDENT' ).
        CLEAR zbin_to_bin_psa-huident.
        CLEAR zbin_to_bin_psa-message1.
        CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode_off( 'CLEAR' ).
        RETURN.
      ENDIF.
    WHEN 'ENTER'.
      CASE lv_field.
        WHEN 'ZBIN_TO_BIN_PSA-HUIDENT'.

          IF lv_inp_attr = '0'.
            /scwm/cl_rf_bll_srvc=>set_prmod( '2' ).
            RETURN.
          ENDIF.

          IF zbin_to_bin_psa-huident IS NOT INITIAL.


            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = zbin_to_bin_psa-huident
              IMPORTING
                output = lv_huident1.

            SELECT SINGLE lgnum
             FROM /scwm/huhdr
             INTO @DATA(lv_lgnum)
             WHERE huident = @lv_huident1.

            CREATE OBJECT go_obj
              EXPORTING
                iv_lgnum = lv_lgnum.

            lt_huident_r = VALUE #( ( sign = 'I' option = 'EQ' low = zbin_to_bin_psa-huident ) ).

            go_obj->get_physical_stock_on_hu(

       EXPORTING

           iv_skip_bin       = abap_false
           iv_skip_resource  = abap_false
           iv_skip_tu        = abap_false
           it_huident_r      =  lt_huident_r

         IMPORTING
           et_stock_mon      =  lt_stock_mon
           ev_error          =  lv_error
            ).


            IF lt_stock_mon IS NOT INITIAL.
              READ TABLE lt_stock_mon ASSIGNING FIELD-SYMBOL(<ls_stock_mon>) INDEX 1.

              SORT lt_stock_mon BY ui_wdatu ASCENDING ui_wdatt ASCENDING.

              READ TABLE lt_stock_mon ASSIGNING FIELD-SYMBOL(<ls_batches>) INDEX 1.

              IF <ls_batches> IS ASSIGNED.
                IF <ls_stock_mon>-charg = <ls_batches>-charg.

                  CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
                    EXPORTING
                      input  = <ls_stock_mon>-matnr
                    IMPORTING
                      output = lv_matid.

                  SELECT SINGLE /scwm/aqua~cat
                     FROM /scwm/aqua
                    JOIN /scwm/tcat
                     ON /scwm/aqua~lgnum = /scwm/tcat~lgnum
                     AND /scwm/aqua~cat = /scwm/tcat~cat
                     WHERE /scwm/aqua~matid = @lv_matid
                    AND /scwm/aqua~charg = @<ls_batches>-charg
                     AND /scwm/tcat~catlocn = 'BB'
                     INTO @DATA(ls_catlocn).

                  IF sy-subrc = 0.

                    CALL METHOD /scwm/cl_rf_bll_srvc=>message
                      EXPORTING
                        iv_msgid    = 'ZSLOC'
                        iv_msgty    = 'E'
                        iv_msgno    = '000'
                        iv_msg_view = 'X'.                " Message view
                    RETURN.

                  ENDIF.

                  PERFORM add_hu1 CHANGING zbin_to_bin_psa.
                  /scwm/cl_rf_bll_srvc=>set_prmod( '2' ).
                ENDIF.
              ENDIF.

            ELSE .
              CLEAR lv_huident1.
              CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode_off( 'CLEAR' ).
              CALL METHOD /scwm/cl_rf_bll_srvc=>message
                EXPORTING
                  iv_msgid    = 'ZSLOC'
                  iv_msgty    = 'E'
                  iv_msgno    = '002'
                  iv_msg_view = 'X'.                " Message view
            ENDIF.
          ELSE.
               CLEAR lv_huident1.
              CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode_off( 'CLEAR' ).
            CALL METHOD /scwm/cl_rf_bll_srvc=>message
              EXPORTING
                iv_msgid    = 'ZSLOC'
                iv_msgty    = 'E'
                iv_msgno    = '001'
                iv_msg_view = 'X'.                " Message view
          ENDIF.
      ENDCASE.

    WHEN 'ZOK'.
      zcustom_message-text01 = TEXT-t01.
      zcustom_message-text02 = TEXT-t02.
      zcustom_message-text03 = TEXT-t03.
      zcustom_message-text04 = TEXT-t04.

    WHEN 'ZFINE'.

      PERFORM bin_to_bin_psa CHANGING zbin_to_bin_psa_t  zbin_to_bin_psa.

    WHEN 'ZBACK1'.
      CLEAR:  zbin_to_bin_psa-huident,
              zbin_to_bin_psa-message1,
              zbin_to_bin_psa_t,
              zbin_to_bin_psa_s,
              lt_stock_mon,
              zbin_to_bin_psa.

    WHEN 'BACKF'.
      /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
      /scwm/cl_rf_bll_srvc=>set_fcode(
        /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).

    WHEN OTHERS.
  ENDCASE.
ENDFUNCTION.


FORM bin_to_bin_psa CHANGING zbin_to_bin_psa_t TYPE zbin_to_bin_psa_t
                         zbin_to_bin_psa   TYPE zbin_to_bin_psa.

  DATA: adhoc         TYPE /scwm/s_rf_adhoc,
        lt_adhoc      TYPE STANDARD TABLE OF /scwm/s_rf_adhoc,
        ls_rsrc       TYPE /scwm/rsrc,
        lv_tanum      TYPE /scwm/tanum,
        ls_huhdr      TYPE /scwm/s_huhdr_int,
        lv_hu_on_rsrc TYPE xfeld,
        lt_ordim_o    TYPE /scwm/tt_ordim_o,
        ls_huheader   TYPE /scwm/s_huhdr_int,
        lt_create_hu  TYPE /scwm/tt_to_crea_hu,
        lt_ltap_vb    TYPE /scwm/tt_ltap_vb,
        lt_bapiret    TYPE bapiret2_t,
        lv_hazmat_ind TYPE char4,
        ls_t333       TYPE /scwm/t333,
        lt_text       TYPE tdtab_c132,
        ls_create_hu  TYPE /scwm/s_to_crea_hu,
        lv_id         TYPE /lime/line_item_id,
        lv_guid_hu    TYPE /lime/guid,
        lv_severity   TYPE bapi_mtype,
        lo_hu         TYPE REF TO  /scwm/cl_wm_packing,
        lv_wtcode     TYPE /scwm/de_wtcode,
        lt_huitm      TYPE  /scwm/tt_huitm_int,
        lv_vbeln      TYPE lips-vbeln.




  IF zbin_to_bin_psa-scanned_hu_list[]  IS INITIAL.
    MESSAGE 'HU list is empty' TYPE 'E'.
  ENDIF.

  zbin_to_bin_psa_t = zbin_to_bin_psa-scanned_hu_list.

  LOOP AT zbin_to_bin_psa_t ASSIGNING FIELD-SYMBOL(<fs_zbin_to_bin_t>).
    <fs_zbin_to_bin_t>-seqno = sy-tabix.
  ENDLOOP.



  CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
    EXPORTING
      iv_uname = sy-uname
    CHANGING
      cs_rsrc  = ls_rsrc.

  LOOP AT zbin_to_bin_psa_t INTO DATA(ls_bin_to_bin).

*    IF ls_bin_to_bin-huident = ls_bin_to_bin-huident_dest.
*      DATA(lv_huident_out) = |{ ls_bin_to_bin-huident_dest ALPHA = OUT }|.
*      MESSAGE e002(z_rf_mess) WITH lv_huident_out.
*    ENDIF.

*    IF ZBIN_TO_BIN_PSA-nlpla IS INITIAL and ls_bin_to_bin-huident_dest IS INITIAL.
    IF zbin_to_bin_psa-nlpla IS INITIAL.
      MESSAGE e003(z_rf_mess).
    ENDIF.

    CLEAR: ls_huhdr.
    adhoc-lgnum = ls_rsrc-lgnum.
    adhoc-drsrc = ls_rsrc-rsrc.
    adhoc-ident = ls_bin_to_bin-huident.
    adhoc-nlenr = ls_bin_to_bin-huident_dest.
    adhoc-procty = ls_bin_to_bin-procty.
    adhoc-nlpla = zbin_to_bin_psa-nlpla.

*   determine if id is hu or tu
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = adhoc-ident
      IMPORTING
        output = adhoc-ident.

    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_huident = adhoc-ident
        iv_lgnum   = adhoc-lgnum
      IMPORTING
        es_huhdr   = ls_huhdr
        et_huitm   = lt_huitm
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.


    LOOP AT lt_huitm INTO DATA(ls_huitm).
      SELECT SINGLE docno, itemno FROM /scdl/db_proci_i INTO @DATA(ls_db_proci_i)
        WHERE  docid = @ls_huitm-qdocid AND  itemid = @ls_huitm-qitmid.

      IF sy-subrc IS INITIAL.
        lv_vbeln = |{ ls_db_proci_i-docno ALPHA = OUT }|.
        lv_vbeln = |{ lv_vbeln ALPHA = IN }|.

        SELECT SINGLE wbsta FROM lips INTO @DATA(lv_wbsta)
          WHERE  vbeln = @lv_vbeln
          AND posnr = @ls_db_proci_i-itemno.
        IF sy-subrc IS INITIAL AND lv_wbsta <> 'C' AND lv_wbsta <> 'B'.
          MESSAGE e004(z_rf_mess).
        ENDIF.
      ENDIF.
    ENDLOOP.

    adhoc-huident = ls_huhdr-huident.
    adhoc-guid_hu = ls_huhdr-guid_hu.
    PERFORM another_wt_check "Check WT existance for this HU
      USING
        adhoc-lgnum
        adhoc-huident
        lv_hu_on_rsrc
      CHANGING
        adhoc-more
        lt_ordim_o.
    REFRESH lt_ordim_o.


*   determine source bin
    IF ls_huhdr-top IS NOT INITIAL.
      adhoc-vlpla = ls_huhdr-lgpla.
    ELSE.
      CALL FUNCTION '/SCWM/HUHEADER_READ'
        EXPORTING
          iv_guid_hu  = adhoc-guid_hu
        IMPORTING
          es_huheader = ls_huheader
        EXCEPTIONS
          not_found   = 1
          input       = 2
          error       = 3
          OTHERS      = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      adhoc-vlpla = ls_huheader-lgpla.
    ENDIF.

*   determine hazmat
    IF ls_huhdr-hzmt IS NOT INITIAL.
      CALL FUNCTION '/SCWM/RF_HAZMAT_IND_READ'
        EXPORTING
          iv_hazmat     = ls_huhdr-hzmt
        IMPORTING
          ev_hazmat_ind = lv_hazmat_ind.
      adhoc-hazmat_ind = lv_hazmat_ind.
    ENDIF.

    IF adhoc-procty IS INITIAL.
      MESSAGE e142(/scwm/rf_de).
    ENDIF.


*   determine activity type
    IF adhoc-procty IS NOT INITIAL.
      CALL FUNCTION '/SCWM/T333_READ_SINGLE'
        EXPORTING
          iv_lgnum    = adhoc-lgnum
          iv_procty   = adhoc-procty
        IMPORTING
          es_t333     = ls_t333
        EXCEPTIONS
          not_found   = 1
          wrong_input = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      adhoc-act_type = ls_t333-act_type.
      IF ls_t333-nlpla IS NOT INITIAL.
        adhoc-nlpla    = ls_t333-nlpla.
      ENDIF.
    ENDIF.


*   read texts for hazardous material
    IF adhoc-act_type IS NOT INITIAL.
      CALL FUNCTION '/SCWM/RF_TEXT_GET'
        EXPORTING
          iv_lgnum   = adhoc-lgnum
          iv_actty   = adhoc-act_type
          iv_huident = adhoc-huident
        IMPORTING
          et_text    = lt_text
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF lt_text IS NOT INITIAL.
        CALL METHOD /scwm/cl_rf_bll_srvc=>set_rf_text
          EXPORTING
            it_text = lt_text.

*     fill the text indicator with the text of data element
        CALL FUNCTION '/SCWM/RF_TEXT_IND_READ'
          EXPORTING
            iv_text     = 'X'
          IMPORTING
            ev_text_ind = adhoc-text_ind.
      ENDIF.
    ENDIF.


*   determine handling unit type
    adhoc-letyp = ls_huhdr-letyp.


*   fill TO create structure
    MOVE-CORRESPONDING adhoc TO ls_create_hu.
    CLEAR ls_create_hu-drsrc.
    lv_id = lv_id + 1.
    ls_create_hu-seqno = lv_id.
    ls_create_hu-norou = ls_t333-norou.
    ls_create_hu-whoseq = lv_id.

*   If a WPT is used with 'Propose Confirm. Upon WT Creation'
*     the WT will be created and confirmed immediately
    IF ls_t333-vquit IS NOT INITIAL.
      ls_create_hu-squit = 'X'.
    ENDIF.

    APPEND ls_create_hu TO lt_create_hu.
    APPEND adhoc TO lt_adhoc.
  ENDLOOP.

  LOOP AT lt_create_hu INTO ls_create_hu.
    DATA(lv_index) = sy-tabix.
    lv_guid_hu = ls_create_hu-guid_hu.
    CREATE OBJECT lo_hu.
    CALL METHOD lo_hu->get_hu
      EXPORTING
        iv_guid_hu = lv_guid_hu
        iv_lock    = 'X'
      IMPORTING
        es_huhdr   = ls_huhdr
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*     add consolidation group
    ls_create_hu-dstgrp = ls_huhdr-dstgrp.
    MODIFY lt_create_hu FROM ls_create_hu TRANSPORTING dstgrp.

  ENDLOOP.


*   wtcode
*  IF adhoc-tu_num IS NOT INITIAL.
  lv_wtcode = wmegc_wtcode_adhoc_yard.
*  ELSE.
*    lv_wtcode = wmegc_wtcode_adhoc_hu.
*  ENDIF.

*   setting up data for RF printing
  CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'.


*   create and save HU TO
  CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
    EXPORTING
      iv_lgnum       = adhoc-lgnum
      it_create_hu   = lt_create_hu
      iv_wtcode      = lv_wtcode
      iv_commit_work = ' '
    IMPORTING
      ev_tanum       = lv_tanum
      et_ltap_vb     = lt_ltap_vb
      et_bapiret     = lt_bapiret
      ev_severity    = lv_severity.

  IF lv_severity CA wmegc_severity_ea.
    ROLLBACK WORK.
    CALL FUNCTION 'DEQUEUE_ALL'.

    CALL METHOD /scwm/cl_tm=>cleanup( ).
    PERFORM error_analysis
      USING lv_severity
            lt_bapiret.
    EXIT.
  ENDIF.

  zbin_to_bin_psa-success = abap_true.

  COMMIT WORK AND WAIT.
  CALL METHOD /scwm/cl_tm=>cleanup( ).

*   importing data to printing cernel
  CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'.

*   go to entry screen / quit transaction and restart as defined
  /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
  /scwm/cl_rf_bll_srvc=>set_fcode(
    /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).

  CLEAR: zbin_to_bin_psa-scanned_hu_list, zbin_to_bin_psa_t.
ENDFORM.


FORM add_hu1 CHANGING zbin_to_bin_psa TYPE zbin_to_bin_psa.
  DATA: ls_huhdr TYPE /scwm/s_huhdr_int,
        lt_huitm TYPE /scwm/tt_huitm_int.

  DATA(lv_huident) = VALUE #( zbin_to_bin_psa-scanned_hu_list[ huident = zbin_to_bin_psa-huident ] OPTIONAL ).
  IF lv_huident IS NOT INITIAL.
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZBIN_TO_BIN_PSA-HUIDENT' ).
*    MESSAGE 'HU is already scanned' TYPE 'E'.
    zbin_to_bin_psa-message1 = 'HU is already scanned'.
    CONCATENATE 'E:' zbin_to_bin_psa-message1 INTO zbin_to_bin_psa-message1 SEPARATED BY space.
    CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode_on( 'CLEAR' ).
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCWM/HU_READ'
    EXPORTING
      iv_appl      = wmegc_huappl_wme
      iv_db_select = 'A'
      iv_huident   = zbin_to_bin_psa-huident
      iv_lgnum     = zbin_to_bin_psa-lgnum
    IMPORTING
      es_huhdr     = ls_huhdr
      et_huitm     = lt_huitm
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZBIN_TO_BIN_PSA-HUIDENT' ).
    CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode_on( 'CLEAR' ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO zbin_to_bin_psa-message1.
    CONCATENATE 'E:' zbin_to_bin_psa-message1 INTO zbin_to_bin_psa-message1 SEPARATED BY space.
    RETURN.
  ELSE.
    APPEND INITIAL LINE TO zbin_to_bin_psa-scanned_hu_list ASSIGNING FIELD-SYMBOL(<fs_hu>).
    <fs_hu>-huident = zbin_to_bin_psa-huident.
    <fs_hu>-procty = '3050'.
    <fs_hu>-huident_dest = <fs_hu>-huident.
    <fs_hu>-nlpla_src = ls_huhdr-lgpla.
    ADD 1 TO zbin_to_bin_psa-sumahu.

    READ TABLE lt_huitm INTO DATA(ls_huitm) INDEX 1.

    <fs_hu>-pmatid = ls_huitm-matid.
    IF ls_huitm-quan_t[] IS NOT INITIAL.
      <fs_hu>-vsola = ls_huitm-quan_t[ 1 ]-quan.
      <fs_hu>-altme = ls_huitm-quan_t[ 1 ]-unit.
    ENDIF.

  ENDIF.
  CLEAR zbin_to_bin_psa-huident.
ENDFORM.

FUNCTION ZSCWM_ZBINHU_ZBINP2_PBO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZBIN_TO_BIN_PSA) TYPE  ZBIN_TO_BIN_PSA
*"     REFERENCE(ZBIN_TO_BIN_PSA_T) TYPE  ZBIN_TO_BIN_PSA_T
*"     REFERENCE(ZBIN_TO_BIN_PSA_S) TYPE  ZBIN_TO_BIN_PSA_S
*"----------------------------------------------------------------------




 DATA ls_rsrc     TYPE /scwm/rsrc.

  IF zbin_to_bin_PSA-success = abap_true.
    DATA(lv_type) = 'S'.
    CALL METHOD /scwm/cl_rf_bll_srvc=>message
      EXPORTING
        iv_msgid = 'Z_RF_MESS'
        iv_msgty = lv_type
        iv_msgno = '001'
        iv_msgv1 = |{ zbin_to_bin_PSA-sumahu ALPHA = OUT }|.
  ENDIF.

  CLEAR: zbin_to_bin_PSA, zbin_to_bin_PSA_t.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZBIN_TO_BIN_PSA'.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZBIN_TO_BIN_PSA_T'.

  CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
    EXPORTING
      iv_uname = sy-uname
    CHANGING
      cs_rsrc  = ls_rsrc.

  zbin_to_bin_PSA-lgnum = ls_rsrc-lgnum.
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode_off( 'CLEAR' ).




ENDFUNCTION.

FUNCTION ZSCWM_ZBINHU_ZBINP_PAI.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZBIN_TO_BIN_PSA) TYPE  ZBIN_TO_BIN_PSA
*"     REFERENCE(ZBIN_TO_BIN_PSA_T) TYPE  ZBIN_TO_BIN_PSA_T
*"----------------------------------------------------------------------

  zbin_to_bin_PSA-nlpla = gv_chosen.





ENDFUNCTION.

FUNCTION ZSCWM_ZBINHU_ZBINP_PBO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZBIN_TO_BIN_PSA) TYPE  ZBIN_TO_BIN_PSA
*"     REFERENCE(ZBIN_TO_BIN_PSA_T) TYPE  ZBIN_TO_BIN_PSA_T
*"----------------------------------------------------------------------





ENDFUNCTION.

FUNCTION zscwm_zbinhu_zrfmsg_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCUSTOM_MESSAGE) TYPE  ZCUSTOM_MESSAGE
*"     REFERENCE(ZBIN_TO_BIN_PSA) TYPE  ZBIN_TO_BIN_PSA
*"----------------------------------------------------------------------

  DATA(lv_fcode) = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  DATA(lv_field) = /scwm/cl_rf_bll_srvc=>get_cursor_field( ).
*  DATA(LV_FCODE) = 'ZOK'.

  DATA: lv_msg1  TYPE symsgv.

  DATA:
    ls_ptwy  TYPE /scwm/s_rf_ptwy_att.



* Get actual function code
  CALL METHOD /scwm/cl_rf_bll_srvc=>get_fcode
    RECEIVING
      rv_fcode = lv_fcode.


  CASE lv_fcode.
    WHEN 'ZNO'.
      /scwm/cl_rf_bll_srvc=>set_fcode( 'CLEAR' ).
      /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
    WHEN 'ZSI'.
      PERFORM add_hu1 CHANGING zbin_to_bin_psa.
  ENDCASE.






ENDFUNCTION.

FUNCTION zscwm_zbinhu_zrfmsg_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCUSTOM_MESSAGE) TYPE  ZCUSTOM_MESSAGE
*"----------------------------------------------------------------------

  /scwm/cl_rf_bll_srvc=>set_screen_param( iv_param_name = 'ZCUSTOM_MESSAGE' ).

  zcustom_message-text01 = TEXT-t01.
  zcustom_message-text02 = TEXT-t02.
  zcustom_message-text03 = TEXT-t03.
  zcustom_message-text04 = TEXT-t04.

ENDFUNCTION.


*----------------------------------------------------------------------*
***INCLUDE LZSCWM_RF_TMPL14I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USERCOMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE usercommand INPUT.

  CASE sy-ucomm.
    WHEN '2A500' OR '3Z200' OR 'PROTO'.
      gv_chosen = sy-ucomm.
      /scwm/cl_rf_bll_srvc=>set_fcode( 'Z2A500' ).
  ENDCASE.

ENDMODULE.

*----------------------------------------------------------------------*
***INCLUDE LZSCWM_RF_TMPL14I02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_FINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_fine INPUT.
  CASE sy-ucomm.
    WHEN 'FINE'.
      gv_chosen = sy-ucomm.
      /scwm/cl_rf_bll_srvc=>set_fcode( 'ZFINE' ).
  ENDCASE.

ENDMODULE.


*----------------------------------------------------------------------*
***INCLUDE LZSCWM_RF_TMPL14I03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_comm INPUT.

  CASE sy-ucomm.
    WHEN 'SI'.
      gv_chosen = sy-ucomm.
      /scwm/cl_rf_bll_srvc=>set_fcode( 'ZSI' ).
    WHEN 'NO'.
      gv_chosen = sy-ucomm.
      /scwm/cl_rf_bll_srvc=>set_fcode( 'ZNO' ).
  ENDCASE.

ENDMODULE.
