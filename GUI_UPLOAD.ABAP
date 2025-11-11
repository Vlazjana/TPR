*&---------------------------------------------------------------------*
*&  Include           ZFI_INTERF_MOVCONT_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LEGGI_FILE
*&---------------------------------------------------------------------*

*FORM leggi_file .
*
*  DATA: tmp_rc     TYPE i,
*        tmp_ftable TYPE filetable,
*        wa_ftable  TYPE LINE OF filetable,
*        index      TYPE i, row TYPE i,
*        colno      TYPE i, indcol TYPE i,
*        v_len      TYPE i, v_len1 TYPE i,
*        v_filename LIKE rlgrap-filename.
*
*  FIELD-SYMBOLS <wa> TYPE any.
*  FIELD-SYMBOLS <str> TYPE any.
*  FIELD-SYMBOLS <strpos> TYPE any.
*
*  DATA: BEGIN OF iexcel OCCURS 0.
*          INCLUDE STRUCTURE alsmex_tabline.  "<- INS PD 07.09.11
*  DATA: END OF iexcel.
*
*  DATA v_rec.
*  DATA v_type  TYPE c LENGTH 1.
*  DATA wa_excel LIKE iexcel.
*
*  SELECT COUNT(*) INTO v_len
*    FROM dd03l
*    WHERE tabname = v_name.
*  CHECK sy-subrc EQ 0.
*
*  v_filename = p_file.
*  index = v_len.
*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*    EXPORTING
*      filename                = v_filename
*      i_begin_col             = 1
*      i_begin_row             = 2
*      i_end_col               = index
*      i_end_row               = 20000
*    TABLES
*      intern                  = iexcel
*    EXCEPTIONS
*      inconsistent_parameters = 1
*      upload_ole              = 2
*      OTHERS                  = 3.
*
*  CLEAR index.
**  LOOP AT iexcel INTO wa_excel.
**    AT LAST.
**      READ TABLE iexcel INTO wa_excel INDEX sy-tabix.
**      index = wa_excel-row.
**    ENDAT.
**  ENDLOOP.
*
*  index = COND #( WHEN lines( iexcel ) > 0
*                  THEN VALUE #( iexcel[ lines( iexcel ) ]-row )
*                 ).
*
**  ASSIGN w_header->* TO <str> CASTING TYPE (v_name).
**  ASSIGN w_position->* TO <strpos> CASTING TYPE (v_typep).
*
*  ASSIGN st_in TO <str>.
*
*  CHECK sy-subrc EQ 0.
*
*  row = 1.  " Indice per le righe
*  DO index TIMES.
*
*    READ TABLE iexcel INTO wa_excel WITH KEY row = row col = '0001'.
*    IF  sy-subrc NE 0. EXIT. ENDIF.
*
*    v_rec = wa_excel-value.
*    colno = v_len. "Lunghezza colonne di testata
*
*    indcol = 1.
*    DO colno TIMES.
*
*      ASSIGN COMPONENT indcol OF STRUCTURE <str> TO <wa>.
*      CHECK sy-subrc EQ 0.
*
*      READ TABLE iexcel INTO wa_excel WITH KEY row = row col = indcol.
*      IF sy-subrc EQ 0.
*        DESCRIBE FIELD <wa> TYPE v_type.
*        CASE v_type.
*          WHEN 'P'.
*            TRANSLATE wa_excel-value USING ',.'.
*          WHEN 'D'.
*
*        ENDCASE.
*        MOVE wa_excel-value TO <wa>.
*      ENDIF.
*
*      indcol = indcol + 1.
*    ENDDO.
*    MOVE-CORRESPONDING <str> TO st_in.
**    CLEAR st_in-belnr.
**    st_in-stela = 1.
*    APPEND st_in TO tb_in.
*    CLEAR <str>.
*    row = row + 1.
*  ENDDO.
*
*
*ENDFORM.
FORM leggi_file.
  DATA: tmp_rc     TYPE i,
        tmp_ftable TYPE filetable,
        wa_ftable  TYPE LINE OF filetable,
        index      TYPE i,
        row        TYPE i,
        colno      TYPE i,
        indcol     TYPE i,
        v_len      TYPE i,
        v_len1     TYPE i,
        v_filename LIKE rlgrap-filename.
  FIELD-SYMBOLS: <wa>     TYPE any,
                 <str>    TYPE any,
                 <strpos> TYPE any.
  DATA: BEGIN OF iexcel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF iexcel.
  DATA: v_rec    TYPE string,
        v_type   TYPE c LENGTH 1,
        wa_excel LIKE iexcel.
  DATA: lt_dd03l TYPE TABLE OF dd03l.


  DATA lt_Exc_bin TYPE solix_tab.

  FIELD-SYMBOLS: <lt_excel> TYPE STANDARD TABLE.

  SELECT COUNT(*) FROM dd03l
    INTO v_len
    WHERE tabname = v_name.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  v_filename = p_file.
  index = v_len.

  " Upload Excel file in binary mode
  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = |{ p_file }|
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_exc_bin
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19
  ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  " Convert binary data to xstring
  DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring(
                  it_solix = lt_exc_bin
                ).

  TRY.
      DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
          document_name = |{ p_file }|
          xdocument     = lv_bin_data
      ).

      lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
        IMPORTING
          worksheet_names = DATA(lt_worksheet)
      ).

      IF lt_worksheet IS INITIAL.
        MESSAGE 'No worksheets found in Excel file' TYPE 'E'.
        RETURN.
      ENDIF.

      DATA(lo_worksheet_table) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet[ 1 ] ).
      ASSIGN lo_worksheet_table->* TO <lt_excel>.

    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.


  LOOP AT <lt_excel> ASSIGNING FIELD-SYMBOL(<ls_excel>) FROM 2.

    DO v_len TIMES.
      DATA(lv_index) = sy-index.

      ASSIGN COMPONENT lv_index OF STRUCTURE st_in TO FIELD-SYMBOL(<lv_st_in>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " Read column value from Excel row
      ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel> TO FIELD-SYMBOL(<lv_exc_cell>).
      IF sy-subrc = 0 AND <lv_exc_cell> IS ASSIGNED.
        DESCRIBE FIELD <lv_st_in> TYPE v_type.

        CASE v_type.
          WHEN 'P'.
            TRANSLATE <lv_exc_cell> USING ',.'.
          WHEN 'D'.
            DATA(lv_user_date) = CONV char8( <lv_exc_cell> ).
            <lv_exc_cell> = |{ lv_user_date+4(4) }{ lv_user_date+2(2) }{ lv_user_date(2) }|.
        ENDCASE.

        TRY.
            <lv_st_in> = <lv_exc_cell>.
          CATCH cx_root.
        ENDTRY.
      ENDIF.

    ENDDO.

    APPEND st_in TO tb_in.
    CLEAR st_in.

  ENDLOOP.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  DESCRIBE_NUM_COL
*&---------------------------------------------------------------------*
FORM describe_num_col  USING    p_struct
                       CHANGING p_len.

  FIELD-SYMBOLS <w> TYPE any.

  p_len = 1.
  DO.
    ASSIGN COMPONENT p_len OF STRUCTURE p_struct TO <w>.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    p_len = p_len + 1.
  ENDDO.

ENDFORM.                    " DESCRIBE_NUM_COL
*&---------------------------------------------------------------------*
*&      Form  REGISTRA
*&---------------------------------------------------------------------*
FORM registra .

  DATA v_xmwno LIKE skb1-xmwno.
  DATA: lv_refnr(6)     TYPE c,
        lv_refnr_cha(9) TYPE c,
        l_tabix         TYPE i,
        l_rec           TYPE i.
  DATA v_err.
  FREE lt_return[].
  v_nrec = v_nrec + 1.

  CLEAR : g_flag.
  IF ls_documentheader-comp_code NE 'IN10' AND
     ls_documentheader-comp_code NE 'CN10'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_documentheader-ac_doc_no
      IMPORTING
        output = lv_refnr.
  ENDIF.

  "begin add docum no length is 8
  IF ls_documentheader-comp_code = 'CN10'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_documentheader-ac_doc_no
      IMPORTING
        output = lv_refnr_cha.
  ENDIF.
  "end add

  DATA(lt_tax) = lt_accounttax.
  SORT lt_tax BY gl_account.
  DELETE ADJACENT DUPLICATES FROM lt_tax COMPARING gl_account.
  DESCRIBE TABLE lt_tax LINES DATA(l_tax).
  LOOP AT lt_accounttax ASSIGNING FIELD-SYMBOL(<fs_tax>).
    IF l_tabix IS INITIAL.
      l_tabix = 1.
    ENDIF.
    READ TABLE lt_accountgl INTO ls_accountgl INDEX l_tabix.
    IF sy-subrc EQ 0.
      <fs_tax>-itemno_tax = ls_accountgl-itemno_tax.
      l_rec = l_rec + 1.
    ENDIF.
    IF l_rec EQ l_tax.
      l_tabix = l_tabix + 1.
      CLEAR l_rec.
    ENDIF.
  ENDLOOP.

  IF p_simu = 'X'.

    FREE MEMORY ID 'LDGRP'.
    IF ls_head-ldgrp IS NOT INITIAL.
      EXPORT ldgrp = ls_head-ldgrp TO MEMORY ID 'LDGRP'.
    ENDIF.

    CLEAR ls_head.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader    = ls_documentheader
*       CUSTOMERCPD       =
*       CONTRACTHEADER    =
      TABLES
        accountgl         = lt_accountgl
        accountreceivable = lt_accountrec
        accountpayable    = lt_accountvend
        accounttax        = lt_accounttax
        currencyamount    = lt_curramount
        criteria          = lt_criteria
*       VALUEFIELD        =
        extension1        = lt_extension1
        return            = lt_return
*       PAYMENTCARD       =
*       CONTRACTITEM      =
        extension2        = lt_extension2
*       REALESTATE        =
*       ACCOUNTWT         =
      .
*
    LOOP AT lt_return INTO ls_return.
      ls_err-belnr = '$TEMP'.
      MOVE-CORRESPONDING ls_return TO ls_err.
      IF ls_documentheader-comp_code = 'IN10'.
        ls_err-rec = g_refnr.
      ELSEIF ls_documentheader-comp_code = 'CN10'.
        ls_err-rec = lv_refnr_cha.
      ELSE.
        ls_err-rec = lv_refnr.
      ENDIF.
      APPEND ls_err .
    ENDLOOP.
*
    PERFORM verifica_documento.
*
  ELSE.
    FREE MEMORY ID 'LDGRP'.
    IF ls_head-ldgrp IS NOT INITIAL.
      EXPORT ldgrp = ls_head-ldgrp TO MEMORY ID 'LDGRP'.
    ENDIF.
    CLEAR ls_head.

    PERFORM verifica_documento.

    IF v_flag IS INITIAL.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = ls_documentheader
*         CUSTOMERCPD       =
*         CONTRACTHEADER    =
        IMPORTING
          obj_key           = ld_obj_key
        TABLES
          accountgl         = lt_accountgl
          accountreceivable = lt_accountrec
          accountpayable    = lt_accountvend
          accounttax        = lt_accounttax
          currencyamount    = lt_curramount
          criteria          = lt_criteria
*         VALUEFIELD        =
          extension1        = lt_extension1
          return            = lt_return
*         PAYMENTCARD       =
*         CONTRACTITEM      =
          extension2        = lt_extension2
*         REALESTATE        =
*     ACCOUNTWT
        .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "Set gsber
      IF p_bukrs = 'CN10' AND lt_gsber IS NOT INITIAL.
        PERFORM frm_set_ba USING ld_obj_key.
      ENDIF.

    ENDIF.

    LOOP AT lt_return INTO ls_return.
      ls_err-belnr = ld_obj_key.
      MOVE-CORRESPONDING ls_return TO ls_err.
*      ls_err-rec = v_nrec.
*      Begin of change 26/09
      IF ls_documentheader-comp_code = 'IN10'.
        ls_err-rec = g_refnr.
      ELSEIF ls_documentheader-comp_code = 'CN10'.
        ls_err-rec = lv_refnr_cha.
      ELSE.
        ls_err-rec = lv_refnr.
      ENDIF. "End of changes 26/09
      APPEND ls_err .
    ENDLOOP.
  ENDIF.



*  FREE: lt_accountgl[], lt_curramount[], lt_extension2[],
*      lt_accounttax[], ls_documentheader, lt_accountrec[],
*      lt_accountvend[],
*      g_refnr,g_ehsn, g_nonhsn.

  CLEAR:lt_accountgl[], lt_curramount[], lt_extension2[],
        lt_accounttax[], ls_documentheader, lt_accountrec[],
        lt_accountvend[],
        g_refnr,g_ehsn, g_nonhsn.

ENDFORM.                    " REGISTRA
*&---------------------------------------------------------------------*
*&      Form  VISUALIZZA_LOG
*&---------------------------------------------------------------------*
FORM visualizza_log .

  DATA: v_repid LIKE sy-repid,
        h_repid LIKE trdir-name.
  DATA t_field TYPE slis_t_fieldcat_alv.
  FIELD-SYMBOLS <wa> TYPE slis_fieldcat_alv.

  v_repid = sy-repid.
  h_repid = 'ZFI_INTERF_MOV_INT_TOP'.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = v_repid
      i_internal_tabname     = 'LS_ERR'
*     I_STRUCTURE_NAME       =
*     I_CLIENT_NEVER_DISPLAY = 'X'
      i_inclname             = h_repid
      i_bypassing_buffer     = 'X'
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat            = t_field
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  READ TABLE t_field ASSIGNING <wa> WITH KEY fieldname = 'REC'.
  IF sy-subrc EQ 0.
*    <wa>-seltext_l = <wa>-seltext_m = <wa>-seltext_l =  'N° Posizione'(L02).
    <wa>-seltext_l = <wa>-seltext_m = <wa>-seltext_l =  TEXT-004.
    <wa>-ddictxt = 'S'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat   = t_field
    TABLES
      t_outtab      = ls_err
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " VISUALIZZA_LOG
*&---------------------------------------------------------------------*
*&      Form  LEGGI_CSV
*&---------------------------------------------------------------------*
FORM leggi_csv .
  DATA: tmp_rc     TYPE i,
        tmp_ftable TYPE filetable,
        wa_ftable  TYPE LINE OF filetable,
        index      TYPE i, row TYPE i,
        colno      TYPE i, indcol TYPE i,
        v_file     TYPE string,
        v_len      TYPE i, v_len1 TYPE i .

  FIELD-SYMBOLS <wa> TYPE any.
  FIELD-SYMBOLS <str> TYPE any.
  FIELD-SYMBOLS <strpos> TYPE any.

  DATA: BEGIN OF t_data OCCURS 0,
          w_dati(256),
        END OF t_data,
        BEGIN OF t_cols OCCURS 0,
          w_colos(50),
        END OF t_cols.

  DATA v_rec.
  DATA v_type  TYPE c LENGTH 1.

  SELECT COUNT(*) INTO v_len
    FROM dd03l
    WHERE tabname = v_name.
  CHECK sy-subrc EQ 0.
  SELECT COUNT(*) INTO v_len1
      FROM dd03l
      WHERE tabname = v_typep.
  CHECK sy-subrc EQ 0.

  IF v_len LT v_len1.
    index = v_len1.
  ELSE.
    index = v_len.
  ENDIF.
  v_file = p_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = v_file
*     filetype                = 'ASC'
      has_field_separator     = ';'
    CHANGING
      data_tab                = t_data[]
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CLEAR index.
  DESCRIBE TABLE t_data LINES index.
*  index = index - 2.
  index = index - 1.

  ASSIGN w_header->* TO <str> CASTING TYPE (v_name).
  ASSIGN w_position->* TO <strpos> CASTING TYPE (v_typep).

  CHECK sy-subrc EQ 0.

*  row = 3.  " Indice per le righe
  row = 2.  " Indice per le righe
  DO index TIMES.

    READ TABLE t_data INDEX row.
    IF  sy-subrc NE 0. EXIT. ENDIF.


    v_rec = t_data(1).
    IF v_rec = '1'.
      colno = v_len. "Lunghezza colonne di testata
    ELSEIF v_rec = '2'.
      colno = v_len1. "Lunghezza colonne di posizioni
    ELSE.
      row = row + 1.
      CONTINUE.
    ENDIF.

    FREE t_cols[].
    SPLIT t_data AT ';' INTO TABLE t_cols.

    indcol = 1.
    DO colno TIMES.

      IF v_rec EQ '1'.
        ASSIGN COMPONENT indcol OF STRUCTURE <str> TO <wa>.
        CHECK sy-subrc EQ 0.
      ELSE.
        ASSIGN COMPONENT indcol OF STRUCTURE <strpos> TO <wa>.
        CHECK sy-subrc EQ 0.
      ENDIF.
      READ TABLE t_cols INDEX indcol.

      IF sy-subrc EQ 0.
        DESCRIBE FIELD <wa> TYPE v_type.
        CASE v_type.
          WHEN 'P'.
            TRANSLATE t_cols-w_colos USING ',.'.
          WHEN 'D'.

        ENDCASE.
        MOVE t_cols-w_colos TO <wa>.
      ENDIF.

      indcol = indcol + 1.
    ENDDO.

    IF v_rec = '1'.
*
      IF ls_documentheader IS NOT INITIAL.
        PERFORM registra.
      ENDIF.
      MOVE-CORRESPONDING <str> TO  ls_documentheader.
      MOVE sy-uname            TO  ls_documentheader-username.
      MOVE-CORRESPONDING <str> TO ls_head.
      CLEAR <str>.
    ELSE.

      CLEAR: ls_extension2. "ls_motorec_str_005_badi.

      MOVE-CORRESPONDING <strpos> TO ls_accountvend.
      IF ls_accountvend-vendor_no IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accountvend-vendor_no
          IMPORTING
            output = ls_accountvend-vendor_no.

        APPEND ls_accountvend TO lt_accountvend.
      ENDIF.

      MOVE-CORRESPONDING <strpos> TO ls_accountrec.
      IF ls_accountrec-customer IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accountrec-customer
          IMPORTING
            output = ls_accountrec-customer.

        APPEND ls_accountrec TO lt_accountrec.
      ENDIF.


      MOVE-CORRESPONDING <strpos> TO ls_accountgl.
      MOVE-CORRESPONDING <strpos> TO ls_accounttax.
      MOVE-CORRESPONDING <strpos> TO ls_curramount.

      IF ls_accountgl-acct_type = 'H'.
        ls_curramount-amt_doccur = ls_curramount-amt_doccur * ( -1 ).
      ELSEIF ls_accountgl-acct_type = 'S'.
        ls_curramount-amt_doccur = abs( ls_curramount-amt_doccur ).
      ENDIF.

      ls_accountgl-acct_type = 'S'.
*      ls_accountgl-comp_code = ls_documentheader-comp_code.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_accountgl-costcenter
        IMPORTING
          output = ls_accountgl-costcenter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_accountgl-gl_account
        IMPORTING
          output = ls_accountgl-gl_account.


      MOVE-CORRESPONDING <strpos> TO ls_sum.
      IF ls_sum-amt_doccur NE 0.
        ls_curramount-exch_rate = ls_sum-dmbtr / ls_sum-amt_doccur.
      ENDIF.
      APPEND ls_curramount TO lt_curramount.

      IF ls_accountgl-gl_account IS NOT INITIAL
        AND ls_sum-amt_base IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accountgl-tax_code
          IMPORTING
            output = ls_accountgl-tax_code.

        APPEND ls_accountgl TO lt_accountgl.
      ENDIF.

      IF ls_sum-amt_base IS NOT INITIAL AND ls_accounttax-tax_code IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accounttax-tax_code
          IMPORTING
            output = ls_accounttax-tax_code.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accounttax-gl_account
          IMPORTING
            output = ls_accounttax-gl_account.

        APPEND ls_accounttax TO lt_accounttax.
      ENDIF.

*** CAMPI CODING BLOCK


      CLEAR: ls_curramount, ls_accountgl, ls_accounttax.
      CLEAR <strpos>.

    ENDIF.

    row = row + 1.
  ENDDO.
  IF ls_documentheader IS NOT INITIAL.
    PERFORM registra.
  ENDIF.
ENDFORM.                    " LEGGI_CSV


*&---------------------------------------------------------------------*
*&      Form  MOVE_FIELDS
*&---------------------------------------------------------------------*
FORM move_fields   TABLES
*                          lt_zfi_newbs   STRUCTURE zfi_newbs
                           lt_t030k_buf   STRUCTURE t030k
                           lt_gst_buf     STRUCTURE zfi_gst
*                          lt_ska1_buf    STRUCTURE ska1
                           lt_setleaf_buf STRUCTURE setleaf
                   USING   fs_in          STRUCTURE st_in
                  CHANGING documentheader STRUCTURE ls_documentheader
                           head           STRUCTURE ls_head
                           accountvend    STRUCTURE ls_accountvend
                           accountrec     STRUCTURE ls_accountrec
                           accountgl      STRUCTURE ls_accountgl
                           accounttax     STRUCTURE ls_accounttax
                           curramount     STRUCTURE ls_curramount
                           sum            STRUCTURE ls_sum.
  DATA: lv_belnr LIKE st_in-belnr.
  DATA: ls_branch_gsber LIKE zfi_branch_gsber,
*        lv_refnr1(10)   TYPE c,
        lv_refnr1(6)    TYPE c,
        l_gsber         TYPE gsber,
        l_flag          TYPE c.

  IF fs_in-blart = 'CX'.
    CONCATENATE '190' fs_in-belnr INTO lv_belnr.
  ELSE.
    lv_belnr = fs_in-belnr.
  ENDIF.

  IF ls_documentheader-comp_code = 'IN10'.
    IF <fs_in>-sgtxt IS NOT INITIAL.
      IF fs_in-sgtxt+0(6) EQ '996531' OR
         fs_in-sgtxt+0(6) EQ '996812'.
        g_ehsn = g_ehsn + 1.
      ELSEIF fs_in-sgtxt+0(6) EQ '999999'.
      ELSE.
        IF <fs_in>-sgtxt+0(6) NA sy-abcde.
          g_nonhsn = g_nonhsn + 1..
        ENDIF.
      ENDIF.
    ENDIF.
    IF g_ehsn IS NOT INITIAL AND
       g_nonhsn IS NOT INITIAL.
      ls_err-type = 'E'.
      ls_err-rec = g_refnr.
      ls_err-belnr = ls_documentheader-ac_doc_no.
      ls_err-message = 'Invoice can not have SAC code other than 996531 or 996812 except 999999'.
      APPEND ls_err.
      CLEAR : documentheader,
              head          ,
              accountvend   ,
              accountrec    ,
              accountgl     ,
              accounttax    ,
              curramount    ,
              sum           ,
              g_ehsn,
              g_nonhsn.
      EXIT.

    ENDIF.
  ENDIF.
  IF fs_in-rectyp = 'T'.
    CLEAR gv_kunnr.
    CONDENSE fs_in-belnr NO-GAPS.
****ALI-Gestione Belnr in base a tipo documento----
*    Begin of change 26/09
    IF fs_in-bukrs EQ 'IN10' AND
       fs_in-budat IS NOT INITIAL.
      g_refnr = fs_in-belnr.
      CONCATENATE '1'
                  fs_in-budat+6(2)
                  fs_in-belnr
                  INTO documentheader-ac_doc_no..
      gv_xblnr = documentheader-ac_doc_no.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = documentheader-ac_doc_no
        IMPORTING
          output = documentheader-ac_doc_no.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
*         input  = fs_in-belnr
          input  = lv_belnr
        IMPORTING
          output = documentheader-ac_doc_no.
    ENDIF. "End of change 26/09
*    documentheader-ac_doc_no  = fs_in-belnr.
    documentheader-comp_code  = fs_in-bukrs.
*    documentheader-doc_date   = fs_in-bldat.

*    PERFORM conv_date USING fs_in-bldat
*                      CHANGING documentheader-doc_date. "VQ
    documentheader-doc_date = fs_in-bldat. "VQ

    documentheader-doc_type   = fs_in-blart.
*    documentheader-pstng_date = fs_in-budat.

*    PERFORM conv_date USING fs_in-budat
*                      CHANGING documentheader-pstng_date. "VQ
    documentheader-pstng_date = fs_in-budat. "VQ

***    Start modify AL - Ticket#10241462
*    documentheader-ref_doc_no = fs_in-xblnr.
    documentheader-ref_doc_no = gv_xblnr.
***    End modify AL - Ticket#10241462
    documentheader-header_txt = fs_in-bktxt.
*    PERFORM conv_date USING fs_in-bldat
*                      CHANGING documentheader-trans_date. "VQ
    documentheader-trans_date = fs_in-bldat.
    MOVE-CORRESPONDING documentheader TO head.
    CLEAR lv_posnr_acc.
    lv_waers = fs_in-waers.
*    lv_kursf = fs_in-kursf.

    CLEAR lv_mwskz.
*    IF fs_in-blart EQ 'MZ' OR
*       fs_in-blart EQ 'ME'.
*      lv_mwskz = '99'.
*    ENDIF.
  ELSE.

    lv_posnr_acc = lv_posnr_acc + 10.

    IF documentheader-comp_code = 'CN10'.
      CASE fs_in-vbund.
        WHEN 'CNSH'.
          documentheader-ac_doc_no = '1' && documentheader-ac_doc_no+2(8).
        WHEN 'CNSZ'.
          documentheader-ac_doc_no = '2' && documentheader-ac_doc_no+2(8).
        WHEN 'CNBJ'.
          documentheader-ac_doc_no = '3' && documentheader-ac_doc_no+2(8).
      ENDCASE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = documentheader-ac_doc_no
        IMPORTING
          output = documentheader-ac_doc_no.
    ENDIF.

* Determino Chiave contabile
    SELECT SINGLE newbs INTO fs_in-newbs
           FROM zfi_newbs
           WHERE blart    = documentheader-doc_type
             AND koart    = gv_koart
             AND newbs_in = fs_in-newbs.

*    READ TABLE lt_zfi_newbs ASSIGNING FIELD-SYMBOL(<ls_newbs_c>)
*      WITH KEY blart    = documentheader-doc_type
*               koart    = gv_koart
*               newbs_in = fs_in-newbs BINARY SEARCH.
*
*    IF sy-subrc = 0.
*      fs_in-newbs = <ls_newbs_c>-newbs.
*    ENDIF.

    IF documentheader-doc_type = 'CX'.
      CASE lv_posnr_acc.
        WHEN 10.
          IF fs_in-newbs = '40'.
            fs_in-newbs = '01'.
          ENDIF.
        WHEN OTHERS.
          IF fs_in-newbs = '11'.
            fs_in-newbs = '50'.
          ENDIF.
      ENDCASE.
    ENDIF.
    CASE fs_in-newbs.
      WHEN '01' OR '11'.
        CLEAR accountrec.
        accountrec-itemno_acc = lv_posnr_acc.
        accountrec-customer   = fs_in-newco.
        gv_kunnr   = fs_in-newco.
*        accountrec-ref_key_1  = fs_in-xref1.
        IF documentheader-comp_code = '5000'.
          accountrec-ref_key_2  = fs_in-sgtxt.
        ENDIF.
*        accountrec-ref_key_3  = fs_in-xref3.
        accountrec-comp_code  = fs_in-bukrs.
        accountrec-pmnttrms   = fs_in-zterm.
*        PERFORM conv_date USING fs_in-zfbdt
*                          CHANGING accountrec-bline_date.
        accountrec-pymt_meth = fs_in-zlsch.
        accountrec-alloc_nmbr = fs_in-zuonr.
        accountrec-ref_key_1 = fs_in-zuonr.
*        READ TABLE lt_in_bra ASSIGNING <fs_in_bra> WITH KEY belnr = fs_in-belnr.
*        IF sy-subrc EQ 0.
**          accountrec-bus_area = <fs_in_bra>-wwbrc.
*          CLEAR ls_branch_gsber.
*          SELECT SINGLE * INTO ls_branch_gsber FROM zfi_branch_gsber
*            WHERE bukrs = gv_bukrs
*              AND wwbrc = <fs_in_bra>-wwbrc.
*          IF sy-subrc EQ 0.
*            accountrec-bus_area = ls_branch_gsber-gsber.
*          ENDIF.
*        ENDIF.
        accountrec-bus_area = fs_in-vbund.
        accountrec-item_text  = fs_in-sgtxt.
        IF NOT lv_mwskz IS INITIAL.
          accountrec-tax_code   = lv_mwskz.
        ELSE.
          accountrec-tax_code   = fs_in-mwskz.
        ENDIF.
        accountrec-profit_ctr = fs_in-prctr.
***     Start modify AL Ticket#10241462
*        IF  fs_in-dmbtr NE fs_in-wrbtr.
        IF gr_burks[] IS NOT INITIAL AND p_bukrs IN gr_burks[].
        ELSE.
          IF  fs_in-dmbtr NE fs_in-wrbtr AND lv_kursf IS INITIAL AND fs_in-dmbtr IS NOT INITIAL.
***     End modify AL Ticket#10241462
            lv_kursf = fs_in-wrbtr / fs_in-dmbtr.
          ENDIF.
        ENDIF.
        IF fs_in-newbs = '11'.
*          IF NOT fs_in-dmbtr IS INITIAL.
*            lv_wrbtr = fs_in-dmbtr * -1.
*          ELSE.
*            lv_wrbtr = fs_in-wrbtr * -1.
*          ENDIF.
          lv_wrbtr = fs_in-wrbtr * -1.
        ELSE.
*          IF NOT fs_in-dmbtr IS INITIAL.
*            lv_wrbtr = fs_in-dmbtr.
*          ELSE.
*            lv_wrbtr = fs_in-wrbtr.
*          ENDIF.
          lv_wrbtr = fs_in-wrbtr.
        ENDIF.
      WHEN '31' OR '21' OR '34' OR '24'.
        CLEAR accountvend.
        accountvend-itemno_acc = lv_posnr_acc.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = fs_in-newco
          IMPORTING
            output = accountvend-vendor_no.
*        accountvend-vendor_no  = fs_in-newco.
*        accountvend-ref_key_1  = fs_in-xref1.
        IF documentheader-comp_code = '5000'.
          accountvend-ref_key_2  = fs_in-sgtxt.
        ENDIF.
*        accountvend-ref_key_3  = fs_in-xref3.
        accountvend-comp_code  = fs_in-bukrs.
        accountvend-pmnttrms   = fs_in-zterm.
*        PERFORM conv_date USING fs_in-zfbdt
*                          CHANGING accountvend-bline_date.
        accountvend-pymt_meth = fs_in-zlsch.
        accountvend-alloc_nmbr = fs_in-zuonr.
        accountvend-ref_key_1 = fs_in-zuonr.
        accountvend-item_text  = fs_in-sgtxt.
        accountvend-tax_code   = fs_in-mwskz.
        accountvend-profit_ctr = fs_in-prctr.
***     Start modify AL Ticket#10241462
*        IF  fs_in-dmbtr NE fs_in-wrbtr.
        IF gr_burks[] IS NOT INITIAL AND p_bukrs IN gr_burks[].
        ELSE.
          IF  fs_in-dmbtr NE fs_in-wrbtr AND lv_kursf IS INITIAL AND fs_in-dmbtr IS NOT INITIAL.
***     End modify AL Ticket#10241462
            lv_kursf = fs_in-wrbtr / fs_in-dmbtr.
          ENDIF.
        ENDIF.
        IF fs_in-newbs = '31' OR fs_in-newbs = '34'.
*          IF NOT fs_in-dmbtr IS INITIAL.
*            lv_wrbtr = fs_in-dmbtr * -1.
*          ELSE.
*            lv_wrbtr = fs_in-wrbtr * -1.
*          ENDIF.
          lv_wrbtr = fs_in-wrbtr * -1.
        ELSE.
*          IF NOT fs_in-dmbtr IS INITIAL.
*            lv_wrbtr = fs_in-dmbtr.
*          ELSE.
*            lv_wrbtr = fs_in-wrbtr.
*          ENDIF.
          lv_wrbtr = fs_in-wrbtr.
        ENDIF.
*        accountvend-ref_key_1 = fs_in-xref1.
        IF documentheader-comp_code = '5000'.
          accountvend-ref_key_2 = fs_in-sgtxt.
        ENDIF.
      WHEN '40' OR '50'.
        CLEAR: accountgl, accounttax.
        accountgl-itemno_acc = lv_posnr_acc.
        accountgl-gl_account = fs_in-newco.
        accountgl-item_text  = fs_in-sgtxt.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
**            input  = fs_in-belnr
*            input  = lv_belnr
*          IMPORTING
*            output = accountgl-ac_doc_no.
        accountgl-ac_doc_no = documentheader-ac_doc_no.
*        accountgl-ac_doc_no  = fs_in-belnr.
*        accountgl-ref_key_1  = fs_in-xref1.
        IF documentheader-comp_code = '5000'.
          accountgl-ref_key_2  = fs_in-sgtxt.
        ENDIF.
*        accountgl-ref_key_3  = fs_in-xref3.
        accountgl-doc_type   = fs_in-blart.
        accountgl-comp_code  = fs_in-bukrs.
        accountgl-tax_code   = fs_in-mwskz.
        accountgl-alloc_nmbr = fs_in-zuonr.
*        accountgl-costcenter = fs_in-kostl.
        accountgl-profit_ctr = fs_in-prctr.
*        CLEAR ls_branch_gsber.
*        SELECT SINGLE * INTO ls_branch_gsber FROM zfi_branch_gsber
*          WHERE bukrs = gv_bukrs
*            AND wwbrc = fs_in-wwbrc.
*        IF sy-subrc EQ 0.
*          accountgl-bus_area = ls_branch_gsber-gsber.
*        ENDIF.
        accountgl-bus_area = fs_in-vbund.
        l_gsber = fs_in-vbund.
        accounttax-itemno_acc = lv_posnr_acc.
        accounttax-gl_account = fs_in-newco.
        IF NOT lv_mwskz IS INITIAL.
          accounttax-tax_code   = lv_mwskz.
        ELSE.
          accounttax-tax_code   = fs_in-mwskz.
        ENDIF.
***     Start modify AL Ticket#10241462
        IF gr_burks[] IS NOT INITIAL AND p_bukrs IN gr_burks[].
        ELSE.
          IF  fs_in-dmbtr NE fs_in-wrbtr AND lv_kursf IS INITIAL AND fs_in-dmbtr IS NOT INITIAL .
            lv_kursf = fs_in-wrbtr / fs_in-dmbtr.
          ENDIF.
        ENDIF.
***     End modify AL Ticket#10241462
        IF fs_in-newbs = '50'.
*          IF NOT fs_in-dmbtr IS INITIAL.
*            lv_wrbtr = fs_in-dmbtr * -1.
*          ELSE.
*            lv_wrbtr = fs_in-wrbtr * -1.
*          ENDIF.
          lv_wrbtr = fs_in-wrbtr * -1.
        ELSE.
*          IF NOT fs_in-dmbtr IS INITIAL.
*            lv_wrbtr = fs_in-dmbtr.
*          ELSE.
*            lv_wrbtr = fs_in-wrbtr.
*          ENDIF.
          lv_wrbtr = fs_in-wrbtr.
        ENDIF.
    ENDCASE.
*** Tabella CO-PA - INIT
    PERFORM fill_criteria USING fs_in.
*** Tabella CO-PA -  END

    CLEAR: curramount, ls_sum.
    curramount-itemno_acc = lv_posnr_acc.
    curramount-curr_type = '00'.
    curramount-currency = lv_waers.
    curramount-amt_doccur = lv_wrbtr.
*    IF fs_in-dmbtr IS INITIAL.
*      fs_in-dmbtr = fs_in-wrbtr.
*    ENDIF.
*    IF  fs_in-dmbtr NE fs_in-wrbtr.
*      lv_kursf = fs_in-wrbtr / fs_in-dmbtr.
*    ENDIF.
    IF NOT lv_kursf IS INITIAL.
      curramount-exch_rate = lv_kursf.
      curramount-exch_rate_v = lv_kursf.
    ENDIF.

    curramount-amt_base = fs_in-hwbas.
    curramount-disc_amt = fs_in-fwbas.
    IF lv_wrbtr < 0.
      curramount-amt_base = curramount-amt_base * -1.
      curramount-disc_amt = curramount-disc_amt * -1.
    ENDIF.
    curramount-tax_amt = lv_wrbtr.
    MOVE-CORRESPONDING curramount TO ls_sum.
  ENDIF.

*Begin of changes

  IF ls_documentheader-comp_code = 'IN10'.

    DATA : l_glacc TYPE hkont.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accounttax-gl_account
      IMPORTING
        output = ls_accounttax-gl_account.


*    SELECT SINGLE ktosl
*          FROM t030k
*          INTO @DATA(l_ktosl)
*          WHERE ktopl = 'Z001'
*          AND mwskz = @ls_accounttax-tax_code
*          AND konts = @ls_accounttax-gl_account.
*
*
*    IF sy-subrc EQ 0.
*
*      SELECT SINGLE kschl
*             FROM zfi_gst
*             INTO @DATA(l_kschl)
*             WHERE supplytype = 'O'
*             AND   ktosl = @l_ktosl.


    READ TABLE lt_t030k_buf ASSIGNING FIELD-SYMBOL(<ls_t030k_buf>)
          WITH KEY ktopl = 'Z001'
                   mwskz = ls_accounttax-tax_code
                   konts = ls_accounttax-gl_account
          BINARY SEARCH.

    IF sy-subrc EQ 0.
      READ TABLE lt_gst_buf ASSIGNING FIELD-SYMBOL(<ls_gst_buf>)
        WITH KEY supplytype = 'O'
                 ktosl = <ls_t030k_buf>-ktosl
        BINARY SEARCH.


      ls_accounttax-cond_key = <ls_gst_buf>-kschl.
      ls_accounttax-acct_key = <ls_t030k_buf>-ktosl.

*     ls_accounttax-cond_key = l_kschl.
*     ls_accounttax-acct_key = l_ktosl.

      IF ls_accounttax-tax_code = '9C'.
        ls_accounttax-tax_rate = '9'.
      ELSEIF ls_accounttax-tax_code = '18'.
        ls_accounttax-tax_rate = '18'.
      ELSEIF ls_accounttax-tax_code = '00'.
        ls_accounttax-tax_rate = '0'.
      ENDIF.
      ls_accounttax-direct_tax = 'X'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = fs_in-newco
      IMPORTING
        output = l_glacc.

    SELECT SINGLE gvtyp
       FROM ska1
       INTO @g_gvtyp
       WHERE ktopl = 'Z001'
       AND  saknr = @l_glacc.

    CLEAR : l_flag.
    IF g_gvtyp IS NOT INITIAL.
      l_flag = abap_true.
    ELSE.

      READ TABLE lt_setleaf_buf ASSIGNING FIELD-SYMBOL(<ls_setleaf_buf>)
           WITH KEY setname = 'ZSALEREG'
                    valfrom = l_glacc
           BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_flag = abap_true.
      ENDIF.
    ENDIF.

    IF l_flag IS NOT INITIAL.
      IF ls_accounttax-tax_code IS INITIAL.
        LOOP AT tb_in INTO DATA(wa_tb)
                      WHERE mwskz IS NOT INITIAL.
          ls_accounttax-tax_code = wa_tb-mwskz.
          EXIT.
        ENDLOOP.
      ENDIF.

      CALL FUNCTION 'ZFI_CUST_INV_IND_LOC'
        EXPORTING
          i_bukrs    = ls_documentheader-comp_code
          i_kunnr    = ls_kna1-kunnr
          i_mwskz    = ls_accounttax-tax_code
          i_sgtxt    = fs_in-sgtxt
          i_gsber    = l_gsber
        IMPORTING
          o_gst_part = g_partner
          o_bupla    = g_bupla
          o_plc_sup  = g_plc_sup
          o_mwskz    = g_mwskz
          o_hsn_sac  = g_hsn
        TABLES
          e_return   = it_return.

      IF it_return IS INITIAL .

*        IF  g_flag IS INITIAL.
        LOOP AT lt_accountrec ASSIGNING FIELD-SYMBOL(<fs>).
          IF g_bupla IS NOT INITIAL.
            <fs>-businessplace = g_bupla.
            MODIFY lt_accountrec FROM <fs>.
          ENDIF.
        ENDLOOP.

*          g_flag = abap_true.
        IF <fs_in>-zuonr IS NOT INITIAL AND
         g_gvtyp IS NOT INITIAL.

          PERFORM update_extension_field USING <fs_in>
                                         CHANGING ls_extension2.
        ENDIF.
        IF g_partner IS NOT INITIAL OR
           g_plc_sup IS NOT INITIAL.
          ls_extension2-structure = 'IN_LOCAL_FIELD_CUST_UPDATE'.
          ls_extension2-valuepart1 = g_partner.
          ls_extension2-valuepart2 = g_plc_sup.
          ls_extension2-valuepart3 = ls_extension2-valuepart3.
          APPEND ls_extension2 TO lt_extension2.
          CLEAR : ls_extension2,ls_extension1.
        ENDIF.

*        ENDIF.
        IF g_hsn IS NOT INITIAL.
          ls_extension2-structure = 'IN_LOCAL_FIELD_GL_UPDATE'.
          ls_extension2-valuepart1 = lv_posnr_acc.
          ls_extension2-valuepart2 = g_hsn.
          APPEND ls_extension2 TO lt_extension2.
          CLEAR : ls_extension2.
        ENDIF.

      ELSE.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_documentheader-ac_doc_no
*          IMPORTING
*            output = lv_refnr1.

        LOOP AT it_return INTO DATA(wa_return).
          ls_err-type = 'E'.
          ls_err-rec = g_refnr.
          ls_err-belnr = ls_documentheader-ac_doc_no.
          ls_err-message = wa_return-error.
          APPEND ls_err.
        ENDLOOP.
        CLEAR : documentheader,
                head          ,
                accountvend   ,
                accountrec    ,
                accountgl     ,
                accounttax    ,
                curramount    ,
                sum           .

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " MOVE_FIELDS
*&---------------------------------------------------------------------*
*&      Form  CONV_DATE
*&---------------------------------------------------------------------*
FORM conv_date  USING    in_date
                CHANGING out_date.

*  DATA: aaaa(4),
*        mm(2),
*        gg(2).
*
*  gg = in_date(2).
*  mm = in_date+2(2).
*  aaaa = in_date+4(4).
*
*  CONCATENATE aaaa mm gg INTO out_date.


ENDFORM.                    " CONV_DATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_IVA
*&---------------------------------------------------------------------*
FORM check_iva .

*  LOOP AT tb_in ASSIGNING <fs_in>.
*    IF <fs_in>-rectyp = 'P'.
*      IF ( <fs_in>-newco EQ '13241003' AND <fs_in>-wrbtr EQ 0 ) OR
*         ( <fs_in>-newco EQ '13241002' AND <fs_in>-wrbtr EQ 0 ).
*        MOVE-CORRESPONDING <fs_in> TO st_in.
**        st_in-wrbtr = <fs_in>-hwbas * -1.
*        st_in-wrbtr = <fs_in>-hwbas.
*        CLEAR st_in-hwbas.
*        <fs_in>-wrbtr = <fs_in>-hwbas.
*        CLEAR <fs_in>-hwbas.
*        IF <fs_in>-newbs EQ '50'.
*          st_in-newbs = '40'.
*        ELSEIF <fs_in>-newbs = '40'.
*          st_in-newbs = '50'.
*        ENDIF.
*        st_in-newco = '24140050'.
*        <fs_in>-newco = '24140050'.
*READ TABLE tb_t007a ASSIGNING <fs_t007a> WITH KEY mwskz = <fs_in>-mwskz.
*        IF sy-subrc EQ 0.
*          st_in-mwskz = 'ZZ'.
*        ELSE.
*          st_in-mwskz = '99'.
*        ENDIF.
*        APPEND st_in TO tb_in.
*        CLEAR st_in.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

  SORT tb_in BY belnr rectyp DESCENDING.

ENDFORM.                    " CHECK_IVA
*&---------------------------------------------------------------------*
*&      Form  INIT_TABLE
*&---------------------------------------------------------------------*
FORM init_table .

  SELECT 'I'    AS sign,
         'EQ'   AS option,
          bukrs AS low
    FROM zfi_inv_ex
    INTO TABLE @gr_burks.

ENDFORM.                    " INIT_TABLE
*&---------------------------------------------------------------------*
*&      Form  FILL_CRITERIA
*&---------------------------------------------------------------------*

FORM fill_criteria  USING    p_in STRUCTURE st_in.
*
  IF NOT p_in-kndnr IS INITIAL.
    CLEAR ls_criteria.
    ls_criteria-itemno_acc = lv_posnr_acc.
    ls_criteria-fieldname     = 'KNDNR'.

*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = p_in-kndnr
*      IMPORTING
*        output = p_in-kndnr. "VQ

    ls_criteria-character     = p_in-kndnr.

    APPEND ls_criteria TO lt_criteria.
**
    CLEAR ls_criteria.
    IF NOT p_in-prctr IS INITIAL.
      ls_criteria-itemno_acc = lv_posnr_acc..
      ls_criteria-fieldname     = 'PRCTR'.
      ls_criteria-character     = p_in-prctr.
      APPEND ls_criteria TO lt_criteria.
    ENDIF.
**
    CLEAR ls_criteria.
    IF NOT p_in-wwbrc IS INITIAL.
      ls_criteria-itemno_acc = lv_posnr_acc..
      ls_criteria-fieldname     = 'WWBRC'.
      ls_criteria-character     = p_in-wwbrc.
      APPEND ls_criteria TO lt_criteria.
    ENDIF.
**
    CLEAR ls_criteria.
    IF NOT p_in-wwinc IS INITIAL.
      ls_criteria-itemno_acc = lv_posnr_acc..
      ls_criteria-fieldname     = 'WWINC'.
      ls_criteria-character     = p_in-wwinc .
      APPEND ls_criteria TO lt_criteria.
    ENDIF.

    CLEAR ls_criteria.
    IF NOT p_in-wwpra IS INITIAL.
      ls_criteria-itemno_acc = lv_posnr_acc..
      ls_criteria-fieldname     = 'WWPRA'.
      ls_criteria-character     = p_in-wwpra .
      APPEND ls_criteria TO lt_criteria.
    ENDIF.

    CLEAR ls_criteria.
    IF NOT p_in-wwser IS INITIAL.
      ls_criteria-itemno_acc = lv_posnr_acc..
      ls_criteria-fieldname     = 'WWSER'.
      ls_criteria-character     = p_in-wwser .
      APPEND ls_criteria TO lt_criteria.
    ENDIF.

    CLEAR ls_criteria.
    IF NOT p_in-wworl IS INITIAL.
      ls_criteria-itemno_acc = lv_posnr_acc..
      ls_criteria-fieldname     = 'WWORL'.
      ls_criteria-character     = p_in-wworl .
      APPEND ls_criteria TO lt_criteria.
    ENDIF.

    CLEAR ls_criteria.
    IF NOT p_in-wwdel IS INITIAL.
      ls_criteria-itemno_acc = lv_posnr_acc..
      ls_criteria-fieldname     = 'WWDEL'.
      ls_criteria-character     = p_in-wwdel .
      APPEND ls_criteria TO lt_criteria.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  OPEN_DIALOG
*&---------------------------------------------------------------------*
FORM open_dialog  USING    p_flsrv.
*
*  DATA: file_svr(255).
*  file_svr =  p_flsrv.
*
*  CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
*    EXPORTING
*      i_location_flag = 'A'
*      i_server        = ''
*      i_path          = './'  "'/adm/trasf_sap' "?????????
*      filemask        = '*.*'
*      fileoperation   = 'R'
*    IMPORTING
*      o_path          = file_svr
*    EXCEPTIONS
*      rfc_error       = 1.
**            OTHERS          = 2.
*  IF sy-subrc NE 0.
*  ENDIF.
*   p_flsrv =   file_svr.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LEGGI_FILE_SERVER
*&---------------------------------------------------------------------*
FORM leggi_file_server .
*
  DATA: lv_record(5000).
  DATA lt_split(2000)     TYPE c OCCURS 0.
  DATA v_type  TYPE c LENGTH 1.

  FIELD-SYMBOLS <fs_split> LIKE LINE OF lt_split.
  FIELD-SYMBOLS <fs_field> TYPE any.
*
  OPEN DATASET p_flsrv FOR INPUT IN  TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.
    DO.
      READ DATASET p_flsrv INTO lv_record.
      IF sy-subrc = 0.
        CHECK lv_record(1) CO '0123456789'.
        FREE lt_split[].
        SPLIT lv_record AT ';' INTO TABLE lt_split.

        LOOP AT lt_split ASSIGNING <fs_split>.
          ASSIGN COMPONENT sy-tabix OF STRUCTURE st_in TO <fs_field>.

          DESCRIBE FIELD <fs_field> TYPE v_type.
          CASE v_type.
            WHEN 'P'.
              TRANSLATE <fs_split> USING ',.'.
            WHEN 'D'.

          ENDCASE.
          MOVE <fs_split> TO <fs_field>.

        ENDLOOP.
        IF NOT st_in IS INITIAL.
          APPEND st_in   TO tb_in.
          CLEAR st_in.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_documento .

  IF ls_documentheader-comp_code = '5000'.
    CLEAR: bkpf, v_year, v_belnr, v_flag.

    CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
      EXPORTING
        i_date = ls_documentheader-pstng_date
      IMPORTING
*       E_MONTH       =
        e_year = v_year.

    bkpf-belnr = ls_documentheader-ref_doc_no.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = bkpf-belnr
      IMPORTING
        output = bkpf-belnr.
*
    v_belnr = bkpf-belnr.

* verifica che la fattura sia già registrata
    "VQ
*    SELECT SINGLE * FROM bkpf WHERE bukrs = '1000' AND belnr = v_belnr AND gjahr = v_year. " and BLART = 'CB'.

    SELECT SINGLE @abap_true
      FROM bkpf
      INTO @DATA(lv_exists)
      WHERE bukrs = '1000'
        AND belnr = @v_belnr
        AND gjahr = @v_year.

*    IF sy-subrc IS NOT INITIAL.
    IF lv_exists IS INITIAL.
      ls_err-type = 'E'.
      ls_err-message = |The document { ls_documentheader-ref_doc_no } is not posted yet in Brugola Italy|.
      "VQ
*      CONCATENATE 'The document' ls_documentheader-ref_doc_no 'is not posted yet in Brugola Italy' INTO ls_err-message SEPARATED BY space.
      APPEND ls_err .
      v_flag = 'X'.
    ENDIF.
* verifica che la fattura passiva non sia già registrata
    "VQ
    CLEAR: bkpf, ls_err-message.
*    SELECT SINGLE * FROM bkpf WHERE bukrs = ls_documentheader-comp_code AND xblnr = ls_documentheader-ref_doc_no AND gjahr = v_year." and BLART = 'FX'.

    SELECT SINGLE @abap_true
     FROM bkpf
    INTO @DATA(lv_duplicate)
    WHERE bukrs = @ls_documentheader-comp_code
    AND xblnr = @ls_documentheader-ref_doc_no
    AND gjahr = @v_year.

    IF lv_duplicate IS NOT INITIAL.
      ls_err-type = 'E'.
      ls_err-message = |Attention!!! The document { ls_documentheader-ref_doc_no } is already posted in Brugola USA|.
      APPEND ls_err.
      v_flag = 'X'.
    ENDIF.

    "VQ
*    IF sy-subrc IS INITIAL.
*      ls_err-type = 'E'.
*      CONCATENATE 'Attention!!! The document' ls_documentheader-ref_doc_no 'is already posted in Brugola USA' INTO ls_err-message SEPARATED BY space.
*      APPEND ls_err .
*      v_flag = 'X'.
*    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXTENSION_FIELD
*&---------------------------------------------------------------------*
FORM extension_field  USING    fs_in          STRUCTURE st_in
                      CHANGING p_extension1   STRUCTURE ls_extension2.

  DATA : l_rebzg TYPE rebzg,
         l_rebzj TYPE rebzj.

  p_extension1-structure = 'IN_LOCAL_FIELD_CUST_UPDATE'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = fs_in-rebzg
    IMPORTING
      output = l_rebzg.
  CALL FUNCTION 'CONVERSION_EXIT_GJAHR_INPUT'
    EXPORTING
      input       = fs_in-rebzj
    IMPORTING
      output      = l_rebzj
    EXCEPTIONS
      wrong_input = 1
      OTHERS      = 2.

  CONCATENATE               l_rebzg
                            l_rebzj
                            fs_in-rebzz
         INTO p_extension1-valuepart3.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CREATE_REFERENCE
*&---------------------------------------------------------------------*
FORM check_create_reference .

  DATA: ls_t2502 LIKE t2502.
  DATA: lt_t2502 LIKE t2502 OCCURS 0.
  DATA: lv_copachar TYPE rke_characteristic VALUE 'WWPRA'.
  DATA: lt_copacharval TYPE TABLE OF bapi1161_charact_values.
  DATA: ls_copacharval LIKE LINE OF lt_copacharval.
  DATA: lt_bapiret2 TYPE TABLE OF bapiret2.
  DATA: ls_bapiret2 LIKE  bapiret2.


  SELECT wwpra
       INTO TABLE lt_t2502 FROM t2502
      FOR ALL ENTRIES IN tb_in
      WHERE wwpra = tb_in-wwpra.
  SORT lt_t2502 BY wwpra.

  LOOP AT tb_in ASSIGNING <fs_in>.

    <fs_in>-kndnr = |{ <fs_in>-kndnr ALPHA = IN }|.
    READ TABLE lt_t2502 INTO ls_t2502
      WITH KEY wwpra = <fs_in>-wwpra BINARY SEARCH.
    IF sy-subrc NE 0.
      ls_copacharval-value = <fs_in>-wwpra.
      ls_copacharval-name = <fs_in>-wwpra.
      APPEND ls_copacharval TO lt_copacharval.
      CLEAR ls_copacharval.
    ENDIF.
  ENDLOOP.
  SORT lt_copacharval BY value.
  DELETE lt_copacharval WHERE value EQ space.
  DELETE ADJACENT DUPLICATES FROM lt_copacharval COMPARING value.

  IF NOT lt_copacharval[] IS INITIAL.
    CALL FUNCTION 'BAPI_COPACHARUDEF_ADDVALUES'
      EXPORTING
        copacharacteristic    = lv_copachar
      IMPORTING
        return                = ls_bapiret2
      TABLES
        characteristic_values = lt_copacharval.
*    IF lt_bapiret2[] IS INITIAL.
*    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PARAMETERS
*&---------------------------------------------------------------------*
*FORM check_parameters .
*  IF NOT p_bukrs IS INITIAL.
****  Start modify AL Ticket#10241462
*    DATA: lv_bukrs      TYPE bukrs,
*          lv_invkey(19).
*    CLEAR: lv_bukrs, lv_invkey.
*    IF p_bukrs EQ 'IN10'.
**    SORT tb_in BY belnr ASCENDING rectyp DESCENDING invkey ASCENDING. "Change on 13/09/2023
**    SORT tb_in BY belnr ASCENDING rectyp DESCENDING invkey ASCENDING. "Change on 02/04/2025  uncommenting this code..  Jira-ITSUPP-54479 Shekhar
*      SORT tb_in STABLE BY belnr ASCENDING rectyp DESCENDING invkey ASCENDING.
*    ELSE.
*      SORT tb_in BY belnr ASCENDING rectyp DESCENDING invkey ASCENDING.
*    ENDIF.
*    LOOP AT tb_in ASSIGNING <fs_in>.
**      IF sy-tabix = 1.
**        lv_bukrs = <fs_in>-bukrs.
**        lv_invkey = <fs_in>-invkey.
**      ELSE.
**        IF <fs_in>-bukrs IS INITIAL AND <fs_in>-invkey EQ lv_invkey.
**          <fs_in>-bukrs = lv_bukrs.
**          MODIFY tb_in FROM <fs_in> INDEX sy-tabix.
**        ELSEIF <fs_in>-bukrs IS INITIAL AND <fs_in>-invkey NE lv_invkey.
**          DELETE tb_in INDEX sy-tabix.
**        ELSE.
**          CLEAR: lv_bukrs, lv_invkey.
**          lv_bukrs = <fs_in>-bukrs.
**          lv_invkey = <fs_in>-invkey.
**        ENDIF.
**      ENDIF.
*      DO.
*        IF <fs_in>-bukrs IS NOT INITIAL.
*          CLEAR: st_in.
*          READ TABLE tb_in INTO st_in WITH  KEY invkey = <fs_in>-invkey
*                                                bukrs = ''.
*          IF sy-subrc IS INITIAL.
*            st_in-bukrs = <fs_in>-bukrs.
*            MODIFY tb_in FROM st_in INDEX sy-tabix.
*          ELSE.
*            EXIT.
*          ENDIF.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDDO.
*    ENDLOOP.
****  End modify AL Ticket#10241462
****    Start modify AL Ticket#10241462
*****    lt_in_buk[] = tb_in[].
*****    DELETE lt_in_buk WHERE bukrs IS INITIAL.
*****    DELETE lt_in_buk WHERE vbund IS INITIAL.  " AL Ticket#10241462
*****    DELETE lt_in_buk WHERE bukrs NE p_bukrs.
*****    IF NOT p_vbund IS INITIAL.                " AL Ticket#10241462
*****      DELETE lt_in_buk WHERE vbund NE p_vbund." AL Ticket#10241462
*****    ENDIF.                                    " AL Ticket#10241462
*****    REFRESH tb_in.                            " AL Ticket#10241462
*****    tb_in[] = lt_in_buk[].                    " AL Ticket#10241462
*
****  End modify AL Ticket#10241462
*
*    lt_in_buk[] = tb_in[].
*    DELETE lt_in_buk WHERE bukrs IS INITIAL.
*    DELETE lt_in_buk WHERE bukrs NE p_bukrs.
*    LOOP AT tb_in ASSIGNING <fs_in>.
*      READ TABLE lt_in_buk ASSIGNING <fs_in_buk>
*                           WITH KEY belnr = <fs_in>-belnr
*                                    bukrs = <fs_in>-bukrs. " AL Ticket#10241462
*      IF sy-subrc NE 0.
*        DELETE tb_in.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
**  lt_in_bra[] = tb_in[].
**  DELETE lt_in_bra WHERE wwbrc IS INITIAL.
**  IF NOT p_wwbrc IS INITIAL.
**    DELETE lt_in_bra WHERE wwbrc NE p_wwbrc.
**    LOOP AT tb_in ASSIGNING <fs_in>.
**      READ TABLE lt_in_bra ASSIGNING <fs_in_bra>
**                           WITH KEY belnr = <fs_in>-belnr.
**      IF sy-subrc NE 0.
**        DELETE tb_in.
**      ELSE.
**      ENDIF.
**    ENDLOOP.
**  ENDIF.
*
*  lt_in_vbu[] = tb_in[].
*  DELETE lt_in_vbu WHERE vbund IS INITIAL.
*  IF NOT p_vbund IS INITIAL.
*    DELETE lt_in_vbu WHERE vbund NE p_vbund.
*    LOOP AT tb_in ASSIGNING <fs_in>.
*      READ TABLE lt_in_vbu ASSIGNING <fs_in_vbu>
*                           WITH KEY belnr = <fs_in>-belnr.
*      IF sy-subrc NE 0.
*        DELETE tb_in.
*      ELSE.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*ENDFORM.


FORM check_parameters.
  DATA: lt_bukrs_map TYPE HASHED TABLE OF zfi_movcont_rec
                   WITH UNIQUE KEY invkey.

  IF NOT p_bukrs IS INITIAL.

    IF p_bukrs EQ 'IN10'.
      SORT tb_in STABLE BY belnr ASCENDING rectyp DESCENDING invkey ASCENDING.
    ELSE.
      SORT tb_in BY belnr ASCENDING rectyp DESCENDING invkey ASCENDING.
    ENDIF.

    LOOP AT tb_in ASSIGNING <fs_in> WHERE bukrs IS NOT INITIAL.
      READ TABLE lt_bukrs_map TRANSPORTING NO FIELDS
           WITH TABLE KEY invkey = <fs_in>-invkey.
      IF sy-subrc <> 0.
        INSERT <fs_in> INTO TABLE lt_bukrs_map.
      ENDIF.
    ENDLOOP.


    LOOP AT tb_in ASSIGNING <fs_in> WHERE bukrs IS INITIAL.

      READ TABLE lt_bukrs_map ASSIGNING FIELD-SYMBOL(<fs_map>)
           WITH TABLE KEY invkey = <fs_in>-invkey.
      IF sy-subrc = 0.
        <fs_in>-bukrs = <fs_map>-bukrs.
      ENDIF.

    ENDLOOP.

    DELETE tb_in WHERE bukrs IS INITIAL OR bukrs NE p_bukrs.

  ENDIF.


  IF NOT p_vbund IS INITIAL.
    DELETE tb_in WHERE vbund NE p_vbund.
  ENDIF.
ENDFORM.

**&---------------------------------------------------------------------*
*& Form READ_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM read_data .
  v_nrec = 0.

  IF p_file IS INITIAL AND
     p_flsrv IS INITIAL.

    CREATE OBJECT o_conn.
    IF sy-subrc EQ 0.
      CALL METHOD o_conn->connect
        EXCEPTIONS
          conn_failed = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
          EXPORTING
            titel = 'DBLink'(l01)
            msgid = sy-msgid
            msgty = sy-msgty
            msgno = sy-msgno
            msgv1 = sy-msgv1
            msgv2 = sy-msgv2
            msgv3 = sy-msgv3
            msgv4 = sy-msgv4.
        EXIT.
      ENDIF.

      CALL METHOD o_conn->read_table
        EXPORTING
          i_test  = p_simu
          tbname  = 'MOVCO00F'
        IMPORTING
          intable = tb_in[].
    ENDIF.

  ELSEIF NOT p_file IS INITIAL.
    PERFORM leggi_file.

  ELSE.
    PERFORM leggi_file_server.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ELABORATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM elaborate .

  IF tb_in[] IS INITIAL.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM check_parameters.
  PERFORM check_create_reference.

***    Ticket#10282820
  DATA: lt_t030k TYPE TABLE OF t030k,
        ls_t030k TYPE t030k,
        ls_tax   TYPE bapiactx09,
        lv_lines TYPE i,
        lv_key.

  DATA:lt_t030k_buf   TYPE TABLE OF t030k,
       lt_gst_buf     TYPE TABLE OF zfi_gst,
       lt_setleaf_buf TYPE TABLE OF setleaf,
       lt_knb1_buf    TYPE TABLE OF knb1,
       lt_T030_TEMP   TYPE TABLE OF t030k.
*      lt_zfi_newbs   TYPE STANDARD TABLE OF zfi_newbs,
*      lt_ska1_buf    TYPE TABLE OF ska1,

**  SELECT blart koart newbs_in newbs
**    FROM zfi_newbs
**    INTO CORRESPONDING FIELDS OF TABLE lt_zfi_newbs
**    FOR ALL ENTRIES IN tb_in
*    WHERE blart     = tb_in-blart
*       AND newbs_in = tb_in-newbs.
*  SORT lt_zfi_newbs BY blart koart newbs_in.


  SELECT ktopl mwskz konts ktosl
    FROM t030k
    INTO TABLE lt_t030k_buf
    FOR ALL ENTRIES IN tb_in
    WHERE ktopl = 'Z001'
      AND mwskz = tb_in-mwskz.

  IF lt_t030k_buf IS NOT INITIAL.

    SORT lt_t030k_buf BY ktopl mwskz konts.

    SELECT supplytype ktosl kschl
      FROM zfi_gst
      INTO CORRESPONDING FIELDS OF TABLE lt_gst_buf
      FOR ALL ENTRIES IN lt_t030k_buf
      WHERE supplytype = 'O'
      AND   ktosl = lt_t030k_buf-ktosl.
    SORT lt_gst_buf BY supplytype ktosl.
  ENDIF.

*
*  SELECT ktopl, saknr, gvtyp
*    FROM ska1
*    INTO CORRESPONDING FIELDS OF TABLE @lt_ska1_buf
*    WHERE ktopl = 'Z001'.
*  SORT lt_ska1_buf BY ktopl saknr.

  SELECT setname, valfrom
    FROM setleaf
    INTO CORRESPONDING FIELDS OF TABLE @lt_setleaf_buf
    WHERE setname = 'ZSALEREG'.
  SORT lt_setleaf_buf BY setname valfrom.

  SELECT kunnr, bukrs, zwels
  FROM knb1
  INTO CORRESPONDING FIELDS OF TABLE @lt_knb1_buf
  FOR ALL ENTRIES IN @tb_in
  WHERE kunnr = @tb_in-kndnr
  AND   bukrs = @tb_in-bukrs.
  SORT lt_knb1_buf BY kunnr bukrs.


  LOOP AT tb_in ASSIGNING <fs_in>.
***    Start modify AL - Ticket#10241462
    CLEAR: gv_xblnr.
    gv_xblnr = <fs_in>-xblnr.
    CLEAR: <fs_in>-xblnr.
    CONCATENATE <fs_in>-bukrs <fs_in>-belnr INTO <fs_in>-xblnr.
***    End modify AL - Ticket#10241462
    IF <fs_in>-rectyp = 'T'.
*        REFRESH: lt_t030k. " Ticket#10282820   VQ
      CLEAR gv_bukrs.
      gv_bukrs = <fs_in>-bukrs.
      IF ls_documentheader IS NOT INITIAL.
        PERFORM registra.
        CLEAR: ls_documentheader, lv_kursf, g_taxno.
      ENDIF.

      CLEAR: lt_accountgl[], lt_curramount[], lt_extension2[],
             lt_accounttax[], ls_documentheader, lt_accountrec[],
             lt_criteria[], ls_criteria,lt_gsber,ls_gsber,
             lt_accountvend[], ls_accountvend, ls_accountrec,
             ls_accountgl, ls_accounttax, ls_curramount, ls_sum,
             lv_posnr_acc, g_refnr, g_ehsn, g_nonhsn.
      PERFORM move_fields  TABLES
*                                    lt_zfi_newbs
                                     lt_t030k_buf
                                     lt_gst_buf
*                                     lt_ska1_buf
                                     lt_setleaf_buf
                           USING    <fs_in>
                          CHANGING ls_documentheader
                                   ls_head
                                   ls_accountvend
                                   ls_accountrec
                                   ls_accountgl
                                   ls_accounttax
                                   ls_curramount
                                   ls_sum.
      MOVE sy-uname            TO ls_documentheader-username.
    ELSE.
      CLEAR gv_kunnr_app.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_in>-newco
        IMPORTING
          output = gv_kunnr_app.
      "VQ
*        SELECT SINGLE * INTO ls_kna1 FROM kna1
*          WHERE kunnr = gv_kunnr_app. VQ

      SELECT SINGLE @abap_true
      FROM kna1
      INTO @DATA(lv_exists)
      WHERE kunnr = @gv_kunnr_app.

      IF sy-subrc EQ 0.
        gv_koart = 'D'.
      ELSE.
        gv_koart = 'S'.
      ENDIF.

      CLEAR: ls_accountvend, ls_accountrec,
             ls_accountgl, ls_accounttax, ls_curramount, ls_sum.
      PERFORM move_fields  TABLES
*                                    lt_zfi_newbs
                                     lt_t030k_buf
                                     lt_gst_buf
*                                    lt_ska1_buf
                                     lt_setleaf_buf
                           USING <fs_in>
                           CHANGING ls_documentheader
                                   ls_head
                                   ls_accountvend
                                   ls_accountrec
                                   ls_accountgl
                                   ls_accounttax
                                   ls_curramount
                                   ls_sum.

*        CLEAR: ls_extension2, ls_extension1. " ls_motorec_str_005_badi.
***    Start modify AL Ticket#10241462
**        Questa parte va tolta perche il PERFORM e' vuoto e cosi anche
**        la tabella lt_extension1 sara vuota
      IF <fs_in>-rebzg IS NOT INITIAL.
        PERFORM extension_field USING <fs_in>
                                CHANGING ls_extension2.
*          APPEND ls_extension1 TO lt_extension2.
      ENDIF.
***    End modify AL Ticket#10241462

      CLEAR : g_gvtyp.

      IF ls_accountvend-vendor_no IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accountvend-vendor_no
          IMPORTING
            output = ls_accountvend-vendor_no.

        APPEND ls_accountvend TO lt_accountvend.
      ENDIF.

      IF ls_accountrec-customer IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accountrec-customer
          IMPORTING
            output = ls_accountrec-customer.

        " Get payment method if not provided
        IF ls_accountrec-pymt_meth IS INITIAL.

*          SELECT SINGLE zwels INTO ls_accountrec-pymt_meth
*            FROM knb1
*            WHERE kunnr = ls_accountrec-customer
*              AND bukrs = ls_head-comp_code.

          READ TABLE lt_knb1_buf ASSIGNING FIELD-SYMBOL(<ls_knb1_buf>)
             WITH KEY kunnr = ls_accountrec-customer
                      bukrs = ls_head-comp_code
             BINARY SEARCH.

          IF sy-subrc EQ 0.
            ls_accountrec-pymt_meth = <ls_knb1_buf>-zwels.
*            ls_accountrec-pymt_meth = ls_accountrec-pymt_meth.
          ENDIF.
        ENDIF.
        APPEND ls_accountrec TO lt_accountrec.
      ENDIF.

      ls_accountgl-acct_type = 'S'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_accountgl-costcenter
        IMPORTING
          output = ls_accountgl-costcenter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_accountgl-gl_account
        IMPORTING
          output = ls_accountgl-gl_account.

      APPEND ls_curramount TO lt_curramount.

*20240226 hq
      IF p_bukrs = 'CN10' AND <fs_in>-dmbtr IS NOT INITIAL.
        ls_curramount-currency = 'CNY'.
        ls_curramount-curr_type = '10'.
        IF ls_curramount-amt_doccur <= 0 .
          ls_curramount-amt_doccur = <fs_in>-dmbtr * -1.
        ELSE.
          ls_curramount-amt_doccur = <fs_in>-dmbtr.
        ENDIF.
        APPEND ls_curramount TO lt_curramount.
      ENDIF.

      IF ls_accountgl-gl_account IS NOT INITIAL
        AND ls_sum-amt_base IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accountgl-tax_code
          IMPORTING
            output = ls_accountgl-tax_code.

        IF ls_documentheader-comp_code = 'IN10'.

          g_taxno = g_taxno + 1.
          ls_accountgl-itemno_tax =  g_taxno.
        ENDIF.

        APPEND ls_accountgl TO lt_accountgl.
      ENDIF.

      IF ls_sum-amt_base IS NOT INITIAL AND ls_accounttax-tax_code IS
      NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accounttax-tax_code
          IMPORTING
            output = ls_accounttax-tax_code.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_accounttax-gl_account
          IMPORTING
            output = ls_accounttax-gl_account.

* Start modify AL - Ticket#10282820
***        Insert ACCT_KEY value
*          SELECT * FROM t030k INTO TABLE lt_t030k
*                  WHERE mwskz = ls_accounttax-tax_code
*                    AND konts = ls_accounttax-gl_account.

*lt_t030k = FILTER #( lt_t030k_buf
*                     WHERE mwskz = ls_accounttax-tax_code
*                     AND konts = ls_accounttax-gl_account ). VQ

        SELECT ktosl,konts
                FROM t030k INTO TABLE @lt_t030k
                WHERE mwskz = @ls_accounttax-tax_code
                AND konts = @ls_accounttax-gl_account.

        IF sy-subrc IS INITIAL.
          CLEAR ls_tax.
          READ TABLE lt_accounttax INTO ls_tax  INDEX 1.
          IF sy-subrc IS NOT INITIAL.
***  non e' scritto ancora il primo record
            READ TABLE lt_t030k INTO ls_t030k INDEX 1.
            ls_accounttax-acct_key = ls_t030k-ktosl.
          ELSE.
            CLEAR: lv_lines, ls_t030k, ls_tax, lv_key.
            DESCRIBE TABLE lt_accounttax LINES lv_lines.
            READ TABLE lt_accounttax INTO ls_tax  INDEX lv_lines.
            LOOP AT lt_t030k INTO ls_t030k WHERE ktosl NE ls_tax-acct_key
                                             AND konts = ls_accounttax-gl_account.
              ls_accounttax-acct_key = ls_t030k-ktosl.
              lv_key = 'X'.
              EXIT.
            ENDLOOP.
            IF lv_key IS INITIAL.
              READ TABLE lt_t030k INTO ls_t030k INDEX 1.
              ls_accounttax-acct_key = ls_t030k-ktosl.
            ENDIF.
            CLEAR: lv_key.
          ENDIF.
        ENDIF.
***  End modify AL - Ticket#10282820
        APPEND ls_accounttax TO lt_accounttax.

        CLEAR:ls_gsber.
        ls_gsber-docln = ls_accounttax-itemno_acc+3(6).
        ls_gsber-gsber = <fs_in>-vbund.
        APPEND ls_gsber TO lt_gsber.
      ENDIF.
      CLEAR: ls_curramount, ls_accountgl, ls_accounttax.
    ENDIF.
  ENDLOOP.

  IF ls_documentheader IS NOT INITIAL.
    PERFORM registra.
  ENDIF.

*    LOOP AT ls_err.
*      IF ls_err-type = 'S'.
*        LOOP AT tb_in ASSIGNING <fs_in> WHERE belnr = ls_err-rec.
*          <fs_in>-msger = TEXT-s01.
*        ENDLOOP.
*      ELSEIF ls_err-type = 'E'.
*        LOOP AT tb_in ASSIGNING <fs_in> WHERE belnr = ls_err-rec.
*          <fs_in>-msger = TEXT-s02.
*        ENDLOOP.
*      ENDIF.
*    ENDLOOP.
*  ENDIF. VQ'

  SORT ls_err BY rec.
  LOOP AT tb_in ASSIGNING FIELD-SYMBOL(<ls_tbin>).
    READ TABLE ls_err ASSIGNING FIELD-SYMBOL(<ls_serr>) WITH KEY rec = <ls_tbin>-belnr BINARY SEARCH.
    IF sy-subrc = 0.
      CASE <ls_serr>-type.
        WHEN 'S'.
          <ls_tbin>-msger = TEXT-s01.
        WHEN 'E'.
          <ls_tbin>-msger = TEXT-s02.
      ENDCASE.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&--------------------------------
*&---------------------------------------------------------------------*
*& Form UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update .
  IF p_file IS INITIAL AND
     p_flsrv IS INITIAL.
    IF p_simu EQ ' '.
      CALL METHOD o_conn->update_stato
        EXPORTING
          i_tabname = 'MOVCO00F'
          intable   = tb_in
        IMPORTING
          errmes    = gv_errmess.
    ENDIF.
    CALL METHOD o_conn->disconnect.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_extension_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <FS_IN>
*&      <-- LS_EXTENSION2
*&---------------------------------------------------------------------*
FORM update_extension_field  USING fs_in          STRUCTURE st_in
                      CHANGING p_extension1   STRUCTURE ls_extension2.

  DATA : l_rebzg TYPE rebzg,
         l_rebzj TYPE rebzj.

  p_extension1-structure = 'IN_LOCAL_FIELD_CUST_UPDATE'.

  CALL FUNCTION 'CONVERSION_EXIT_GJAHR_INPUT'
    EXPORTING
      input       = fs_in-zuonr+0(4)
    IMPORTING
      output      = l_rebzj
    EXCEPTIONS
      wrong_input = 1
      OTHERS      = 2.

  l_rebzg = fs_in-zuonr+4(10).

  CONCATENATE               l_rebzg
                            l_rebzj
                            '001'
         INTO p_extension1-valuepart3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_ba
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LD_OBJ_KEY
*&---------------------------------------------------------------------*
FORM frm_set_ba  USING   pv_obj_key.

  DATA: lt_bkpf       LIKE bkpf OCCURS 0,
        ls_bkpf       LIKE bkpf,
        lt_bseg       LIKE bseg OCCURS 0,
        ls_bseg       LIKE bseg,
        lt_dummy_bkdf LIKE bkdf OCCURS 0,
        lt_dummy_bsec LIKE bsec OCCURS 0,
        lt_dummy_bsed LIKE bsed OCCURS 0,
        lt_dummy_bset LIKE bset OCCURS 0.

  DATA: lo_acdoc_change  TYPE REF TO cl_fins_acdoc_change,
        lt_change_fields TYPE finst_fieldname,
        ls_change_fields LIKE LINE OF lt_change_fields,
        lt_acdoca_upd    TYPE finst_acdoca,
        lt_acdoca_sort   TYPE if_fins_acdoc_util_types=>tt_acdoca_sorted WITH HEADER LINE.

  CLEAR:lt_acdoca_sort,lt_acdoca_upd.


  DO 10 TIMES.
    SELECT rldnr,
           rbukrs,
           gjahr,
           belnr,
           docln,
           rbusa
      FROM acdoca
      INTO TABLE @DATA(lt_acdoca)
     WHERE belnr = @pv_obj_key(10)
       AND rbukrs = @pv_obj_key+10(4)
       AND gjahr = @pv_obj_key+14(4)
       AND rbusa = ''.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO '0.3' SECONDS.
    ENDIF.

  ENDDO.

  " 需要更新字段
  ls_change_fields = 'RBUSA'.
  COLLECT ls_change_fields INTO lt_change_fields.

  CREATE OBJECT lo_acdoc_change.

  LOOP AT lt_acdoca ASSIGNING FIELD-SYMBOL(<fs_acdoca>).
*    CLEAR:lt_acdoca_sort,lt_acdoca_upd.

    READ TABLE lt_gsber INTO DATA(ls_gsber) WITH KEY docln = <fs_acdoca>-docln.
    IF sy-subrc = 0.
      APPEND VALUE #( rldnr   = <fs_acdoca>-rldnr
                      rbukrs  = <fs_acdoca>-rbukrs
                      gjahr   = <fs_acdoca>-gjahr
                      belnr   = <fs_acdoca>-belnr
                      docln   = <fs_acdoca>-docln
                      rbusa   = ls_gsber-gsber )
          TO lt_acdoca_sort.
    ENDIF.
  ENDLOOP.

  IF lt_acdoca_sort[] IS NOT INITIAL.

    lt_acdoca_upd = lt_acdoca_sort[].

    lo_acdoc_change->set_support_mode(
       EXPORTING
         it_change_fields = lt_change_fields ).

    lo_acdoc_change->change_acdoca(
       EXPORTING
         it_change_fields     = lt_change_fields
         it_acdoca_upd        = lt_acdoca_upd
         iv_write_change_docs = abap_false
         iv_direct_update     = abap_true ).

    COMMIT WORK.
  ENDIF.

*  "set bseg
*  MOVE-CORRESPONDING <fs_bkpf> TO s_bkpf.
*  APPEND s_bkpf TO t_bkpf.
*
*  MOVE-CORRESPONDING <fs_bseg> TO s_bseg.
*  s_bseg-gsber = p_gsber.  "change to screen bus area
*  APPEND s_bseg TO t_bseg.
*
*  CLEAR:   t_dummy_bkdf, t_dummy_bsec, t_dummy_bsed, t_dummy_bset.
*  REFRESH: t_dummy_bkdf, t_dummy_bsec, t_dummy_bsed, t_dummy_bset.
*
*  CALL FUNCTION 'CHANGE_DOCUMENT'
*    TABLES
*      t_bkdf = t_dummy_bkdf
*      t_bkpf = t_bkpf
*      t_bsec = t_dummy_bsec
*      t_bsed = t_dummy_bsed
*      t_bseg = t_bseg
*      t_bset = t_dummy_bset
*    EXCEPTIONS
*      OTHERS = 1.
*  IF sy-subrc EQ 0.
*    COMMIT WORK AND WAIT.
*  ELSE.


ENDFORM.
