REPORT zvq_updatemm02.

TABLES : sscrfields.
SELECTION-SCREEN : FUNCTION KEY 1,
                   FUNCTION KEY 3,
                   FUNCTION KEY 2.
PARAMETERS: p_file TYPE string.

CLASS lcl_update_material DEFINITION.

  PUBLIC SECTION.
    METHODS execute.
    METHODS display_salv.
    METHODS at_selection_screen.
    METHODS:  get_filepath.
    DATA: mo_salv TYPE REF TO cl_salv_table.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_excel,
             a TYPE marc-matnr,
             b TYPE mara-mstae,
             c TYPE marc-dispo,
             d TYPE marc-werks,
             e TYPE marc-dismm,
             f TYPE marc-disls,
             g TYPE marc-ekgrp,
             h TYPE marc-beskz,
             i TYPE marc-fhori,
           END OF ty_excel,
           tt_excel TYPE STANDARD TABLE OF ty_excel WITH EMPTY KEY.


    TYPES: BEGIN OF ty_material,
             matnr TYPE marc-matnr,
             mstae TYPE mara-mstae,
             dispo TYPE marc-dispo,
             werks TYPE marc-werks,
             dismm TYPE marc-dismm,
             disls TYPE marc-disls,
             ekgrp TYPE marc-ekgrp,
             beskz TYPE marc-beskz,
             fhori TYPE marc-fhori,
           END OF ty_material,
           tt_material TYPE STANDARD TABLE OF ty_material WITH EMPTY KEY.

    TYPES: BEGIN OF ty_message,
             matnr   TYPE marc-matnr,
             icon    TYPE icon-name,
             id      TYPE i,
             message TYPE bapi_msg,
             type    TYPE string,
           END OF ty_message.

    DATA: mt_message  TYPE STANDARD TABLE OF ty_message.
    DATA: lv_test         TYPE abap_bool,
          lv_test_success TYPE abap_bool.
    DATA: gs_msg TYPE ty_message.
    DATA: mt_material TYPE tt_material.
    METHODS upload_excel.
    METHODS material_change .
    METHODS download.
    METHODS modify.
    METHODS check_matnr IMPORTING it_excel TYPE tt_excel.


ENDCLASS.

DATA go_bapi TYPE REF TO lcl_update_material.

CLASS lcl_update_material IMPLEMENTATION.
  METHOD execute.
    upload_excel( ).
  ENDMETHOD.

  METHOD get_filepath.

    DATA: lt_files  TYPE STANDARD TABLE OF file_table,
          lv_rc     TYPE i,
          lv_action TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             =  cl_gui_frontend_services=>filetype_excel
      CHANGING
        file_table              =  lt_files
        rc                      =  lv_rc
        user_action             =  lv_action
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

    IF lines( lt_files ) > 0.
      p_file = lt_files[ 1 ]-filename.
    ENDIF.

  ENDMETHOD.
  METHOD upload_excel.

    DATA: lv_filelength    TYPE i,
          lv_headerxstring TYPE xstring,
          lt_records       TYPE solix_tab,
          lo_excel_ref     TYPE REF TO cl_fdt_xl_spreadsheet,
          lv_day           TYPE c LENGTH 2,
          lv_month         TYPE c LENGTH 2.
    DATA lt_excel         TYPE  tt_excel.


    FIELD-SYMBOLS <ft_excel_upload> TYPE STANDARD TABLE.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = p_file
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_filelength
        header                  = lv_headerxstring
      TABLES
        data_tab                = lt_records
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
        OTHERS                  = 17.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring(
                           it_solix   = lt_records
                       ).


    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = lv_headerxstring
      TABLES
        binary_tab   = lt_records
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

    TRY.
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet( document_name = p_file
                                                  xdocument     = lv_headerxstring ).
      CATCH cx_fdt_excel_core.

        IF lo_excel_ref IS NOT BOUND.
          MESSAGE 'This file could not be found in your device' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
    ENDTRY.

    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_worksheets) ).
    IF lt_worksheets IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE lt_worksheets ASSIGNING FIELD-SYMBOL(<fv_woksheet_name>) INDEX 1.
    IF <fv_woksheet_name> IS NOT ASSIGNED.
      MESSAGE 'There is no workshsheet to be displayed' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <fv_woksheet_name> ).
    ASSIGN lo_data_ref->* TO <ft_excel_upload>.
    DELETE <ft_excel_upload> INDEX 1.
    MOVE-CORRESPONDING <ft_excel_upload> TO lt_excel.

    check_matnr( EXPORTING it_excel = lt_excel ).

    MOVE lt_excel TO mt_material.

  ENDMETHOD.

  METHOD check_matnr.

    LOOP AT it_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).

      DATA(lv_exist) = EXACT abap_bool( ' ' ).

      SELECT SINGLE @abap_true
        FROM marc
        INTO @lv_exist
        WHERE matnr = @<fs_excel>-a.

      IF lv_exist = abap_false.
        MESSAGE |Purchasing Document Number { <fs_excel>-a } doesnt't exist | TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD material_change.

    DATA: ls_headdata   TYPE bapimathead,
          ls_message    TYPE ty_message,
          ls_bapi_marc  TYPE bapi_marc,
          ls_bapi_marcx TYPE bapi_marcx,
          ls_bapi_mara  TYPE bapi_mara,
          ls_bapi_marax TYPE bapi_marax,
          ls_return     TYPE bapiret2,
          lt_return     TYPE STANDARD TABLE OF bapiret2.
    DATA: it_returnmessages TYPE STANDARD TABLE OF bapi_matreturn2 WITH DEFAULT KEY.

    CLEAR: mt_message,
           ls_return,
           it_returnmessages.
    LOOP AT mt_material ASSIGNING FIELD-SYMBOL(<fs_material>) .
      DATA(lv_row) = sy-tabix.

      ls_headdata = VALUE #( material = |{ <fs_material>-matnr ALPHA = IN }| ) .

      ls_bapi_marc = VALUE #( mrp_ctrler = <fs_material>-dispo
                              plant = <fs_material>-werks
                              mrp_type = <fs_material>-dismm
                              lotsizekey = <fs_material>-disls
                              pur_group = |{ <fs_material>-ekgrp ALPHA = IN }|
                              proc_type = <fs_material>-beskz
                              sm_key = <fs_material>-fhori
       ).

      ls_bapi_marcx = VALUE #( mrp_ctrler = abap_true
                               plant = <fs_material>-werks
                               mrp_type = abap_true
                               lotsizekey = abap_true
                               pur_group = abap_true
                               proc_type = abap_true
                               sm_key = abap_true
       ).

      ls_bapi_mara = VALUE #( pur_status = <fs_material>-mstae ).

      ls_bapi_marax = VALUE #( pur_status = abap_true ).

      DATA: mv_bapi_error TYPE abap_bool.

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata       = ls_headdata
          clientdata     = ls_bapi_mara
          clientdatax    = ls_bapi_marax
          plantdata      = ls_bapi_marc
          plantdatax     = ls_bapi_marcx
        IMPORTING
          return         = ls_return
        TABLES
          returnmessages = it_returnmessages.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT it_returnmessages ASSIGNING FIELD-SYMBOL(<fs_return>) .

        IF <fs_return>-type CA 'EAX'.
          DATA(lv_error) = abap_true.
        ENDIF.

        CASE <fs_return>-type.
          WHEN 'S'.
            DATA(lv_icon) = icon_green_light.
          WHEN 'W'.
            lv_icon = icon_yellow_light.
          WHEN 'E'.
            lv_icon = icon_red_light.
            lv_test_success = abap_false.
        ENDCASE.

        ls_message-message = <fs_return>-message.
        ls_message-icon = lv_icon.
        ls_message-id = lv_row.
        ls_message-matnr = <fs_material>-matnr.
        ls_message-type = <fs_return>-type.

        IF <fs_return>-type = 'E'.
          lv_test_success = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
      APPEND ls_message TO mt_message.
    ENDLOOP.

    display_salv( ).
    lv_test = abap_true.
  ENDMETHOD.

  METHOD modify.
    DATA: ls_headdata   TYPE bapimathead,
          ls_message    TYPE ty_message,
          ls_bapi_marc  TYPE bapi_marc,
          ls_bapi_marcx TYPE bapi_marcx,
          ls_bapi_mara  TYPE bapi_mara,
          ls_bapi_marax TYPE bapi_marax,
          ls_return     TYPE bapiret2,
          lt_return     TYPE STANDARD TABLE OF bapiret2.
    DATA: it_returnmessages TYPE STANDARD TABLE OF bapi_matreturn2 WITH DEFAULT KEY.

    IF lv_test IS INITIAL.
      " If not, raise an exception or return an error message
      MESSAGE 'You have to run a test before modifying'(004) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF lv_test_success IS INITIAL.
      MESSAGE 'Every test must be a success in order to proceed'(005) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT mt_material ASSIGNING FIELD-SYMBOL(<fs_material>) .
      DATA(lv_row) = sy-tabix.

      ls_headdata = VALUE #( material = |{ <fs_material>-matnr ALPHA = IN }| ) .

      ls_bapi_marc = VALUE #( mrp_ctrler = <fs_material>-dispo
                              plant = <fs_material>-werks
                              mrp_type = <fs_material>-dismm
                              lotsizekey = <fs_material>-disls
                              pur_group = |{ <fs_material>-ekgrp ALPHA = IN }|
                              proc_type = <fs_material>-beskz
                              sm_key = <fs_material>-fhori
       ).

      ls_bapi_marcx = VALUE #( mrp_ctrler = abap_true
                               plant = <fs_material>-werks
                               mrp_type = abap_true
                               lotsizekey = abap_true
                               pur_group = abap_true
                               proc_type = abap_true
                               sm_key = abap_true
       ).

      ls_bapi_mara = VALUE #( pur_status = <fs_material>-mstae ).

      ls_bapi_marax = VALUE #( pur_status = abap_true ).

      DATA: mv_bapi_error TYPE abap_bool.

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata       = ls_headdata
          clientdata     = ls_bapi_mara
          clientdatax    = ls_bapi_marax
          plantdata      = ls_bapi_marc
          plantdatax     = ls_bapi_marcx
        IMPORTING
          return         = ls_return
        TABLES
          returnmessages = it_returnmessages.


      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = ' '.
    ENDLOOP.

    MESSAGE |Material's modified succsesfully| TYPE 'S'.
  ENDMETHOD.

  METHOD display_salv.

    DATA: lo_column   TYPE REF TO cl_salv_column_table,
          lo_tooltips TYPE REF TO cl_salv_tooltips,
          lv_value    TYPE lvc_value.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = mt_message ).


        mo_salv->get_columns( )->set_optimize( ).
        mo_salv->get_functions( )->set_all( ).


        lo_column ?= mo_salv->get_columns( )->get_column( 'MATNR' ).
        lo_column->set_medium_text( 'Material' ).
        lo_column->set_key( ).

        lo_column ?= mo_salv->get_columns( )->get_column( 'ID' ).
        lo_column->set_medium_text( 'Line' ).
        lo_column->set_key( ).

        lo_column ?= mo_salv->get_columns( )->get_column( 'ICON' ).
        lo_column->set_medium_text( 'Status' ).
        lo_column->set_icon( if_salv_c_bool_sap=>true ).

        lo_column ?= mo_salv->get_columns( )->get_column( 'MESSAGE' ).
        lo_column->set_medium_text( 'Message' ).


        lo_column ?= mo_salv->get_columns( )->get_column( 'TYPE' ).
        lo_column->set_medium_text( 'TYPE' ).

        lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

*    Tooltips

        lo_tooltips = mo_salv->get_functional_settings( )->get_tooltips( ).

        lv_value = icon_green_light.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = lv_value
          tooltip = 'Everything is Processed' ).

        lv_value = icon_yellow_light.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = lv_value
          tooltip = 'Partially processed' ).

        lv_value = icon_red_light.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = lv_value
          tooltip = 'Nothing Yet processed' ).

      CATCH cx_salv_msg
            cx_salv_not_found
            cx_salv_existing.

    ENDTRY.

    mo_salv->display( ).

  ENDMETHOD.

  METHOD download.

    DATA: lt_template TYPE STANDARD TABLE OF  ty_material.
    DATA: lo_salv   TYPE REF TO cl_salv_table,
          lo_column TYPE REF TO cl_salv_column_table.


    TRY.
        cl_salv_table=>factory( IMPORTING
                                 r_salv_table   = lo_salv
                               CHANGING
                                 t_table        = lt_template ).





        lo_column ?= lo_salv->get_columns( )->get_column( 'MATNR' ).

        lo_column->set_short_text( 'Materiale' ).
        lo_column->set_medium_text( 'Materiale' ).
        lo_column->set_long_text( 'Materiale' ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'MSTAE' ).

        lo_column->set_short_text( 'Material' ).
        lo_column->set_medium_text( 'Material Status' ).
        lo_column->set_long_text( 'Material Status' ).



        lo_column ?= lo_salv->get_columns( )->get_column( 'DISPO' ).

        lo_column->set_short_text( 'Resp. MRP' ).
        lo_column->set_medium_text( 'Resp. MRP' ).
        lo_column->set_long_text( 'Resp. MRP' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'WERKS' ).

        lo_column->set_short_text( 'Divisione' ).
        lo_column->set_medium_text( 'Divisione' ).
        lo_column->set_long_text( 'Divisione' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'DISMM' ).

        lo_column->set_short_text( 'Car. MRP' ).
        lo_column->set_medium_text( 'Car. MRP' ).
        lo_column->set_long_text( 'Car. MRP' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'DISLS' ).

        lo_column->set_short_text( 'Proc' ).
        lo_column->set_medium_text( 'Proc. Dimens. Lotto' ).
        lo_column->set_long_text( 'Proc. Dimens. Lotto' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'EKGRP' ).

        lo_column->set_short_text( 'Purchasing' ).
        lo_column->set_medium_text( 'PurchasinF' ).
        lo_column->set_long_text( 'Purchasing Group' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'BESKZ' ).

        lo_column->set_short_text( 'Tipo.' ).
        lo_column->set_medium_text( 'Tipo.' ).
        lo_column->set_long_text( 'Tipo Approvv.' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'FHORI' ).

        lo_column->set_short_text( 'Chiave' ).
        lo_column->set_medium_text( 'Chiave Orizzonte' ).
        lo_column->set_long_text( 'Chiave Orizzonte' ).

      CATCH cx_salv_msg
                cx_salv_not_found
                cx_salv_existing.

    ENDTRY.

    DATA(lv_xml_bytes) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
    DATA: lv_size TYPE i.
    DATA: it_raw_data TYPE xml_rawdata.

    cl_scp_change_db=>xstr_to_xtab( EXPORTING
                                      im_xstring = lv_xml_bytes
                                    IMPORTING
                                      ex_size    = lv_size
                                      ex_xtab    = it_raw_data ).
    IF lines( it_raw_data ) > 0.
      DATA: lv_action TYPE i.
      DATA: lv_filename TYPE string.
      DATA: lv_fullpath TYPE string.
      DATA: lv_path TYPE string.
      TRY.
          cl_gui_frontend_services=>file_save_dialog( EXPORTING
                                                        default_extension   = 'xlsx'
                                                        default_file_name   = 'export.xlsx'
                                                        file_filter         = cl_gui_frontend_services=>filetype_excel
                                                        prompt_on_overwrite = abap_true
                                                      CHANGING
                                                        filename            = lv_filename
                                                        path                = lv_path
                                                        fullpath            = lv_fullpath
                                                        user_action         = lv_action ).

          IF lv_action EQ cl_gui_frontend_services=>action_ok.

            cl_gui_frontend_services=>gui_download( EXPORTING
                                                      filename                  = lv_fullpath
                                                      filetype                  = 'BIN'
                                                      bin_filesize              = lv_size " Size ist wichtig für das korrekte Schreiben der Excel-Datei
                                                    CHANGING
                                                      data_tab                  = it_raw_data ).

            cl_gui_frontend_services=>execute( application = 'excel.exe'
                                               parameter   = |"{ lv_fullpath }"| ).
          ENDIF.

        CATCH cx_root INTO DATA(e_txt).
          MESSAGE e_txt->get_text( ) TYPE 'S'.
      ENDTRY.
    ENDIF.


  ENDMETHOD.
  METHOD at_selection_screen.

    execute( ).
    CASE sy-ucomm.
      WHEN 'FC01'.
        material_change( ).
      WHEN 'FC02'.
        modify( ).
      WHEN 'FC03'.
        download( ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
*
  DATA ls_function TYPE smp_dyntxt.
  ls_function-icon_id = icon_display.
  ls_function-quickinfo = 'RUN'.
  ls_function-icon_text = 'Run Test'.
  sscrfields-functxt_01 = ls_function.

  ls_function-icon_id = icon_display.
  ls_function-quickinfo = 'ACTUAL'.
  ls_function-icon_text = 'Actual Test'.
  sscrfields-functxt_02 = ls_function.

  ls_function-icon_id = '@49@'.
  ls_function-quickinfo = 'Download'.
  ls_function-icon_text = 'Download'.
  sscrfields-functxt_03 = ls_function.

  CREATE OBJECT go_bapi.
  go_bapi = NEW lcl_update_material( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  go_bapi->get_filepath( ).

AT SELECTION-SCREEN .
  go_bapi->at_selection_screen( ).

START-OF-SELECTION.
  go_bapi->execute( ).
