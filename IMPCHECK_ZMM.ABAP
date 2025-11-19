*&---------------------------------------------------------------------*
*& Report ZMM_IMPCHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_impcheck.

TABLES: sscrfields.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_file TYPE string,
              p_test AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_IMPCHECK DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_impcheck DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS: constructor,
      execute,
      at_selection_screen ,
      get_filename,
      user_command_0100,
      status_0100.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_excel_data,
              werks        TYPE zmm_controlli-werks,
              sernr        TYPE zmm_controlli-sernr,
              zzodp        TYPE zmm_controlli-zzodp,
              zzmodmat     TYPE zmm_controlli-zzmodmat,
              zzesito      TYPE zmm_controlli-zzesito,
              zzcasaprod   TYPE zmm_controlli-zzcasaprod,
              zznpietre    TYPE zmm_controlli-zznpietre,
              zzdatacheck  TYPE zmm_controlli-zzdatacheck,
              zzartforn    TYPE zmm_controlli-zzartforn,
              pernr        TYPE zmm_controlli-pernr,
              zznddt       TYPE zmm_controlli-zznddt,
              zzdataddt    TYPE zmm_controlli-zzdataddt,
              zzaltricheck TYPE zmm_controlli-zzaltricheck,
              zzcheckigi   TYPE zmm_controlli-zzcheckigi,
              zznote       TYPE zmm_controlli-zznote,
            END OF ty_excel_data,
            tt_excel_data TYPE STANDARD TABLE OF ty_excel_data.

    TYPES: BEGIN OF ty_output,
*             Technical fields
             status        TYPE char1,
             excel_line_no TYPE i.
*             Excel fields
             INCLUDE TYPE ty_excel_data.
    TYPES:   sname         TYPE pa0001-sname,
             errori        TYPE char255,
             t_color       TYPE lvc_t_scol,
           END OF ty_output,
           tt_output TYPE STANDARD TABLE OF ty_output.

    DATA: mt_output   TYPE tt_output,
          mo_alv_grid TYPE REF TO cl_gui_alv_grid,
          mv_error    TYPE abap_bool.

    METHODS: read_excel_data,
      download_template,
      set_first_display,
      save_data,
      check_data.

ENDCLASS.

DATA :go_impcheck TYPE REF TO lcl_impcheck,

      BEGIN OF gs_screen100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen100.

*----------------------------------------------------------------------*
*       CLASS lcl_IMPCHECK IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_impcheck IMPLEMENTATION.

  METHOD execute.

    read_excel_data( ).
    check_data( ).
    set_first_display( ).

  ENDMETHOD.

  METHOD constructor.

    DATA: ls_button TYPE smp_dyntxt.

    ls_button = VALUE #( icon_id =  icon_export
                         icon_text = 'Download Template'(002) ).

    sscrfields-functxt_01 = ls_button.

  ENDMETHOD.

  METHOD at_selection_screen.
    CASE sscrfields-ucomm.
      WHEN 'FC01'.
        download_template( ).
    ENDCASE.
  ENDMETHOD.

  METHOD save_data.

    DATA: lt_zmm_controlli TYPE STANDARD TABLE OF zmm_controlli.

    LOOP AT mt_output ASSIGNING FIELD-SYMBOL(<ls_output>) WHERE status <> 1.

      APPEND VALUE #(
          mandt         = sy-mandt
          werks         = <ls_output>-werks
          zzdatacheck   = <ls_output>-zzdatacheck
          sernr         = <ls_output>-sernr
          zzodp         = <ls_output>-zzodp
          zzmodmat      = <ls_output>-zzmodmat
          zzcheckigi    = <ls_output>-zzcheckigi
          zzesito       = <ls_output>-zzesito
          zzcasaprod    = <ls_output>-zzcasaprod
          bname         = sy-uname
          zzdataimp     = sy-datum
          zznpietre     = <ls_output>-zznpietre
          zzartforn     = <ls_output>-zzartforn
          pernr         = <ls_output>-pernr
          sname         = <ls_output>-sname
          zznddt        = <ls_output>-zznddt
          zzdataddt     = <ls_output>-zzdataddt
          zzaltricheck  = <ls_output>-zzaltricheck
          zznote        = <ls_output>-zznote   ) TO lt_zmm_controlli.
    ENDLOOP.

    IF sy-subrc <> 0.
      MESSAGE 'All records are in error. Data was not saved'(003) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    MODIFY zmm_controlli FROM TABLE lt_zmm_controlli.
    COMMIT WORK.
    MESSAGE 'Data saved successfully'(004) TYPE 'S'.

  ENDMETHOD.

  METHOD check_data.

    DATA lt_errors TYPE TABLE OF string.

    SELECT mandt,
           werks,
           zzdatacheck,
           sernr,
           zzodp,
           zzmodmat,
           zzcheckigi,
           zzesito

     FROM zmm_controlli
     INTO TABLE @DATA(lt_existing)
     FOR ALL ENTRIES IN @mt_output
     WHERE werks        = @mt_output-werks
     AND   zzdatacheck  = @mt_output-zzdatacheck
     AND   sernr        = @mt_output-sernr
     AND   zzodp        = @mt_output-zzodp
     AND   zzmodmat     = @mt_output-zzmodmat
     AND   zzcheckigi   = @mt_output-zzcheckigi
     AND   zzesito      = @mt_output-zzesito
      ORDER BY PRIMARY KEY.

    SELECT werks
      FROM t001w
      INTO TABLE @DATA(lt_t001w)
      FOR ALL ENTRIES IN @mt_output
      WHERE werks = @mt_output-werks.
    SORT lt_t001w BY werks.

    SELECT ser05~ppaufnr,
           objk~sernr

      FROM objk
      LEFT JOIN ser05

      ON objk~obknr = ser05~obknr

      INTO TABLE @DATA(lt_ser05)
      FOR ALL ENTRIES IN @mt_output
      WHERE ser05~ppaufnr = @mt_output-zzodp
      AND objk~sernr = @mt_output-sernr.
    SORT lt_ser05 BY ppaufnr sernr.

    SELECT aufnr,
           werks
      FROM aufk
      INTO TABLE @DATA(lt_aufnr)
      FOR ALL ENTRIES IN @mt_output
      WHERE aufnr = @mt_output-zzodp
      AND werks = @mt_output-werks.
    SORT lt_aufnr BY aufnr werks.

    SELECT sernr,
           kmatn
      FROM equi
      INTO TABLE @DATA(lt_equi)
      FOR ALL ENTRIES IN @mt_output
      WHERE sernr = @mt_output-sernr.
    SORT lt_equi BY sernr.

    SELECT pernr,
           sname
      FROM pa0001
      INTO TABLE @DATA(lt_pa0001)
      FOR ALL ENTRIES IN @mt_output
      WHERE pernr = @mt_output-pernr.
    SORT lt_pa0001 BY pernr.

    LOOP AT mt_output ASSIGNING FIELD-SYMBOL(<ls_output>).

      CLEAR lt_errors.
      <ls_output>-excel_line_no = sy-tabix + 1.

*     date check - ZZDATACHECK
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = <ls_output>-zzdatacheck
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
      IF sy-subrc <> 0.
        APPEND 'Check Date'(005) TO lt_errors.
        APPEND VALUE #( fname = 'ZZDATACHECK' color-col = 6 ) TO <ls_output>-t_color.
      ENDIF.

*      date check - ZZDATADDT
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = <ls_output>-zzdataddt
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
      IF sy-subrc <> 0.
        APPEND  'Document Date'(006) TO lt_errors.
        APPEND VALUE #( fname = 'ZZDATADDT' color-col = 6 ) TO <ls_output>-t_color.
      ENDIF.

*     WERKS check
      IF <ls_output>-werks IS NOT INITIAL.

        READ TABLE lt_t001w WITH KEY werks = <ls_output>-werks TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc <> 0.
          APPEND 'Plant'(007) TO lt_errors.
          APPEND VALUE #( fname = 'WERKS' color-col = 6 ) TO <ls_output>-t_color.
        ENDIF.

      ELSEIF <ls_output>-zzcasaprod IS INITIAL.

        APPEND 'Plant'(007) TO lt_errors.
        APPEND VALUE #( fname = 'WERKS' color-col = 6 ) TO <ls_output>-t_color.

        APPEND 'Manufacturer'(008) TO lt_errors.
        APPEND VALUE #( fname = 'ZZCASAPROD' color-col = 6 ) TO <ls_output>-t_color.

      ENDIF.

*     SERNR check
      READ TABLE lt_ser05 WITH KEY ppaufnr = <ls_output>-zzodp
                                   sernr = <ls_output>-sernr TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc <> 0.
        APPEND 'Serial Number'(009) TO lt_errors.
        APPEND VALUE #( fname = 'SERNR' color-col = 6 ) TO <ls_output>-t_color.
      ENDIF.

*     ZZODP check
      READ TABLE lt_aufnr WITH KEY aufnr = <ls_output>-zzodp
                                   werks = <ls_output>-werks TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc <> 0.
        APPEND 'ODP' TO lt_errors.
        APPEND VALUE #( fname = 'ZZODP' color-col = 6 ) TO <ls_output>-t_color.
      ENDIF.

*   ZZMODMAT check
      IF <ls_output>-sernr IS NOT INITIAL.

        READ TABLE lt_equi ASSIGNING FIELD-SYMBOL(<ls_kmatn>) WITH KEY sernr = <ls_output>-sernr BINARY SEARCH.
        IF sy-subrc = 0.

          IF <ls_output>-zzmodmat IS NOT INITIAL AND <ls_output>-zzmodmat <> <ls_kmatn>-kmatn.
            APPEND 'Model/Material'(010) TO lt_errors.
            APPEND VALUE #( fname = 'ZZMODMAT' color-col = 6 ) TO <ls_output>-t_color.
          ELSE.
            <ls_output>-zzmodmat = <ls_kmatn>-kmatn.
          ENDIF.

        ENDIF.
      ENDIF.

*   ZZESITO check
      IF NOT ( <ls_output>-zzesito = 'OK'
           OR <ls_output>-zzesito = 'KO'
           OR <ls_output>-zzesito = 'DR' ).
        APPEND 'Result'(011) TO lt_errors.
        APPEND VALUE #( fname = 'ZZESITO' color-col = 6 ) TO <ls_output>-t_color.
      ENDIF.

*   ZZNPIETRE check
      IF <ls_output>-zznpietre <= 0.
        APPEND 'No. of Stones'(012) TO lt_errors.
        APPEND VALUE #( fname = 'ZZNPIETRE' color-col = 6 ) TO <ls_output>-t_color.
      ENDIF.

*   PERNR check to get the SNAME
      IF <ls_output>-pernr IS NOT INITIAL.
        READ TABLE lt_pa0001 ASSIGNING FIELD-SYMBOL(<ls_pa0001>)
                             WITH KEY pernr = <ls_output>-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_output>-sname = <ls_pa0001>-sname.
        ELSE.
          APPEND 'Personnel Number'(013) TO lt_errors.
          APPEND VALUE #( fname = 'PERNR' color-col = 6 ) TO <ls_output>-t_color.
        ENDIF.

      ENDIF.

      IF lt_errors IS INITIAL.
        READ TABLE lt_existing TRANSPORTING NO FIELDS
                       WITH KEY werks        = <ls_output>-werks
                                zzdatacheck  = <ls_output>-zzdatacheck
                                sernr        = <ls_output>-sernr
                                zzodp        = <ls_output>-zzodp
                                zzmodmat     = <ls_output>-zzmodmat
                                zzcheckigi   = <ls_output>-zzcheckigi
                                zzesito      = <ls_output>-zzesito  BINARY SEARCH.
        IF sy-subrc <> 0.
          <ls_output>-status = 3.
        ELSE.
          <ls_output>-status = 2.
        ENDIF.

      ELSE.
        mv_error = abap_true.
        <ls_output>-status = 1.
        CONCATENATE LINES OF lt_errors INTO <ls_output>-errori SEPARATED BY ', '.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD set_first_display.

    DATA: lo_cust_grid TYPE REF TO cl_gui_custom_container,
          lt_gridfcat  TYPE lvc_t_fcat.

    lo_cust_grid = NEW cl_gui_custom_container( container_name = 'CONT' ).
    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = lo_cust_grid ).

    lt_gridfcat = VALUE #(
        ( fieldname = 'STATUS'         coltext   = 'Status'(014)  )
        ( fieldname = 'EXCEL_LINE_NO'  coltext   = 'Excel line number'(015) )
        ( fieldname = 'WERKS'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'SERNR'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZODP'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZMODMAT'       ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZCHECKIGI'     ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZCASAPROD'     ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZESITO'        ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZDATACHECK'    ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZNPIETRE'      ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZARTFORN'      ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'PERNR'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'SNAME'          ref_table = 'PA0001' )
        ( fieldname = 'ZZNDDT'         ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZDATADDT'      ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZALTRICHECK'   ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZNOTE'         ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ERRORI'         coltext   = 'Errors found'(016) tech = xsdbool( mv_error = abap_false ) )
        ).

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt  = abap_true
                                        ctab_fname  = 'T_COLOR'
                                        excp_fname  = 'STATUS' ).

    mo_alv_grid->set_table_for_first_display(
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = mt_output
        it_fieldcatalog = lt_gridfcat
      EXCEPTIONS
        invalid_parameter_combination = 1                " Wrong Parameter
        program_error                 = 2                " Program Errors
        too_many_lines                = 3                " Too many Rows in Ready for Input Grid
        OTHERS                        = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.

  METHOD download_template.

    TYPES : BEGIN OF ty_excel_template,
              werks        TYPE text50,
              sernr        TYPE text50,
              zzodp        TYPE text50,
              zzmodmat     TYPE text50,
              zzesito      TYPE text50,
              zzcasaprod   TYPE text50,
              zznpietre    TYPE text50,
              zzdatacheck  TYPE text50,
              zzartforn    TYPE text50,
              pernr        TYPE text50,
              zznddt       TYPE text50,
              zzdataddt    TYPE text50,
              zzaltricheck TYPE text50,
              zzcheckigi   TYPE text50,
              zznote       TYPE text50,
            END OF ty_excel_template,

            BEGIN OF ty_map_excel,
              field    TYPE lvc_fname,
              descr    TYPE text50,
              datatype TYPE text20,
            END OF ty_map_excel.

    DATA: lt_template  TYPE STANDARD TABLE OF ty_excel_template,
          lt_map_excel TYPE STANDARD TABLE OF ty_map_excel,
          lv_filename  TYPE string,
          lt_raw_data  TYPE xml_rawdata,
          lv_size      TYPE i,
          lv_path      TYPE string,
          lv_fullpath  TYPE string,
          lv_action    TYPE i.

    lt_map_excel = VALUE #(
    ( field = 'WERKS'         descr = 'Plant'(007)                datatype = 'Characters 4'(030)   )
    ( field = 'SERNR'         descr = 'SERIAL NUMBER'(017)        datatype = 'Characters 18'(031)  )
    ( field = 'ZZODP'         descr = 'ODP'                       datatype = 'Characters 12'(032)  )
    ( field = 'ZZMODMAT'      descr = 'MODEL/MATERIAL'(018)       datatype = 'Characters 40'(033)  )
    ( field = 'ZZESITO'       descr = 'RESULT OK/KO/DR'(019)      datatype = 'Characters 2'(034)   )
    ( field = 'ZZCASAPROD'    descr = 'Manufacturer'(020)         datatype = 'Characters 30'(035)  )
    ( field = 'ZZNPIETRE'     descr = 'NO. OF STONES'(021)        datatype = 'Integer'(036)        )
    ( field = 'ZZDATACHECK'   descr = 'CHECK DATE'(022)           datatype = 'Date'(037)     )
    ( field = 'ZZARTFORN'     descr = 'CRAFTSMAN/SUPPLIER'(023)   datatype = 'Characters 30'(035)  )
    ( field = 'PERNR'         descr = 'PERSONNEL NUMBER'(024)     datatype = 'Characters 8'(038)   )
    ( field = 'ZZNDDT'        descr = 'Document (DDT/FT)'(025)    datatype = 'Characters 20'(039)  )
    ( field = 'ZZDATADDT'     descr = 'DOCUMENT DATE'(026)        datatype = 'Date'(037)     )
    ( field = 'ZZALTRICHECK'  descr = 'OTHER CHECKS'(027)         datatype = 'Characters 40'(033)  )
    ( field = 'ZZCHECKIGI'    descr = 'IGI CHECK'(028)            datatype = 'Characters 1'(040)   )
    ( field = 'ZZNOTE'        descr = 'COMMENTS'(029)             datatype = 'Characters 40'(033)  )
    ).

    APPEND INITIAL LINE TO lt_template ASSIGNING FIELD-SYMBOL(<ls_temp>).
    LOOP AT lt_map_excel ASSIGNING FIELD-SYMBOL(<ls_map_exc>).
      ASSIGN COMPONENT <ls_map_exc>-field OF STRUCTURE <ls_temp> TO FIELD-SYMBOL(<lv_cell>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      <lv_cell> = <ls_map_exc>-datatype.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
         IMPORTING
           r_salv_table = DATA(lo_salv)
         CHANGING
           t_table      = lt_template ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

    DATA(lo_cols) = lo_salv->get_columns( ).

    LOOP AT lt_map_excel ASSIGNING <ls_map_exc>.

      TRY.
          DATA(lo_col) = lo_cols->get_column( columnname = <ls_map_exc>-field ).
          lo_col->set_short_text( value = CONV #( <ls_map_exc>-descr ) ).
          lo_col->set_medium_text( value = CONV #( <ls_map_exc>-descr ) ).
          lo_col->set_long_text( value = CONV #( <ls_map_exc>-descr ) ).
        CATCH cx_salv_not_found. " ALV: General Error Class (Checked in Syntax Check)
      ENDTRY.

    ENDLOOP.

    DATA(lv_xml) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).

    IF xstrlen( lv_xml ) = 0.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension         = 'xlsx'    " Default Extension
        default_file_name         = 'Template.xlsx'   " Default File Name
        file_filter               = cl_gui_frontend_services=>filetype_excel  " File Type Filter Table
        prompt_on_overwrite       = 'X'
      CHANGING
        filename                  = lv_filename     " File Name to Save
        path                      = lv_path   " Path to File
        fullpath                  = lv_fullpath   " Path + File Name
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lv_action <> cl_gui_frontend_services=>action_ok.
      RETURN.
    ENDIF.

    cl_scp_change_db=>xstr_to_xtab(
      EXPORTING
        im_xstring = lv_xml    " XSTRING
      IMPORTING
        ex_xtab    = lt_raw_data   " Table of Type X
        ex_size    = lv_size    " Size of XSTRING
    ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  =  lv_filename   " Name of file
        filetype                  = 'BIN'    " File type (ASCII, binary ...)
        bin_filesize              =  lv_size
      CHANGING
        data_tab                  =  lt_raw_data  " Transfer table
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD read_excel_data.

    DATA: lt_data       TYPE solix_tab,
          lv_day        TYPE n LENGTH 2,
          lv_month      TYPE n LENGTH 2,
          lt_excel_data TYPE tt_excel_data.

    FIELD-SYMBOLS: <lt_excel_upload> TYPE STANDARD TABLE.

    IF p_file IS INITIAL.
      MESSAGE 'Enter a file first'(043) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename                = p_file
            filetype                = 'BIN'
          CHANGING
            data_tab                = lt_data
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
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet( document_name = p_file
                                                xdocument     = lv_bin_data ).


    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheet_names) ).

    IF lt_worksheet_names IS INITIAL.

      MESSAGE 'Problem with excel file'(041) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_itab) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet_names[ 1 ] ).
    ASSIGN lo_worksheet_itab->* TO <lt_excel_upload>.
    DELETE <lt_excel_upload> INDEX 1.

    IF <lt_excel_upload> IS INITIAL.
      MESSAGE 'The excel file does not have any data'(042) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT <lt_excel_upload> ASSIGNING FIELD-SYMBOL(<ls_excel_upload>).

      APPEND INITIAL LINE TO lt_excel_data ASSIGNING FIELD-SYMBOL(<ls_excel_new>).
      DO.

        DATA(lv_index) =  sy-index.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_upload> TO FIELD-SYMBOL(<lv_input_field>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_new> TO FIELD-SYMBOL(<lv_output_field>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CASE lv_index .

          WHEN 2. " SERNR

            CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
              EXPORTING
                input  = <lv_input_field>
              IMPORTING
                output = <lv_output_field>.

          WHEN 3. " ZZODP
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lv_input_field>
              IMPORTING
                output = <lv_output_field>.

          WHEN 4. " ZZMODMAT

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <lv_input_field>
              IMPORTING
                output       = <lv_output_field>
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

          WHEN 8 OR 12. " ZDATACHECK , ZDATADDT

            SPLIT <lv_input_field> AT '-' INTO DATA(lv_year) lv_month lv_day .

            <lv_output_field> = |{ lv_year }{ lv_month ALPHA = IN }{ lv_day ALPHA = IN }|.

          WHEN OTHERS.

            <lv_output_field> = <lv_input_field> .
        ENDCASE.
      ENDDO.
    ENDLOOP.

    mt_output = CORRESPONDING #( lt_excel_data ).

  ENDMETHOD.                    "extract_data

  METHOD get_filename.

    DATA: lt_filetable TYPE STANDARD TABLE OF file_table,
          lv_rc        TYPE i,
          lv_user      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             =  cl_gui_frontend_services=>filetype_excel
      CHANGING
        file_table              =  lt_filetable
        rc                      =  lv_rc
        user_action             =  lv_user
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lt_filetable IS NOT INITIAL.
      p_file = lt_filetable[ 1 ]-filename.
    ENDIF.

  ENDMETHOD.

  METHOD user_command_0100.

    CASE gs_screen100-ok_code.

      WHEN 'FC_BACK'.
        LEAVE TO SCREEN 0.

      WHEN 'FC_SAVE'.
        save_data( ).
    ENDCASE.

  ENDMETHOD.

  METHOD status_0100.
    DATA: lt_exclude TYPE TABLE OF syucomm.

    IF p_test IS NOT INITIAL.
      APPEND 'FC_SAVE' TO lt_exclude.
    ENDIF.

    SET TITLEBAR 'Z_MM_IMPCHECK_TB'.
    SET PF-STATUS 'Z_MM_IMPCHECK_PS' EXCLUDING lt_exclude.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  CREATE OBJECT go_impcheck.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  go_impcheck->get_filename( ).

AT SELECTION-SCREEN.
  go_impcheck->at_selection_screen( ).

START-OF-SELECTION.
  go_impcheck->execute( ).

MODULE status_0100 OUTPUT.
  go_impcheck->status_0100( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  go_impcheck->user_command_0100( ).
ENDMODULE.
