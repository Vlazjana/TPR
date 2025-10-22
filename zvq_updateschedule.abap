REPORT zvq_updateschedule.

TABLES: sscrfields.
SELECTION-SCREEN : FUNCTION KEY 1.

PARAMETERS: p_file TYPE string .

CLASS lcl_update_schedule DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS: constructor,
      get_filepath,
      at_selection_screen,
      execute.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_schedule,
             sales_order    TYPE vbep-vbeln,
             itm_number     TYPE vbep-posnr,
             sched_line     TYPE vbep-etenr,
             req_date       TYPE vbep-edatu,
             date_type      TYPE vbep-prgrs,
             req_qty        TYPE vbep-wmeng,
             schedule_type  TYPE vbep-etart,
             div_schedule   TYPE vblb-labnk,
             dic_sched_date TYPE vblb-abrdt,
           END OF ty_schedule,
           tt_schedule TYPE STANDARD TABLE OF ty_schedule WITH EMPTY KEY.

    TYPES : BEGIN OF ty_buffer,
              vbeln   TYPE vbep-vbeln,
              posnr   TYPE vbep-posnr,
              max_pos TYPE vbep-etenr,
            END OF ty_buffer.

    TYPES: BEGIN OF ty_message,
             sales_order TYPE vbak-vbeln,
             icon        TYPE icon-name,
             id          TYPE i,
             message     TYPE bapi_msg,
             type        TYPE string,
           END OF ty_message.

    DATA: mt_schedule TYPE tt_schedule,
          mt_return   TYPE  bapiret2_tab,
          mt_message  TYPE STANDARD TABLE OF ty_message.

    METHODS upload_excel.
    METHODS sales_order_change.
    METHODS display_logs.
    METHODS download_template.

ENDCLASS.

CLASS lcl_update_schedule IMPLEMENTATION.
  METHOD constructor.

    DATA ls_function TYPE smp_dyntxt.
    ls_function-icon_id = '@49@'.
    ls_function-quickinfo = 'Download Template'.
    ls_function-icon_text = 'Download Template'.
    sscrfields-functxt_01 = ls_function.


  ENDMETHOD.

  METHOD upload_excel.

    DATA: lv_filelength    TYPE i,
          lv_headerxstring TYPE xstring,
          lt_records       TYPE solix_tab,
          lo_excel_ref     TYPE REF TO cl_fdt_xl_spreadsheet,
          lv_day           TYPE c LENGTH 2,
          lv_month         TYPE c LENGTH 2.
    DATA: lt_modified_excel TYPE TABLE OF ty_schedule.

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

    TRY.
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet( document_name = p_file
                                                  xdocument     = lv_bin_data ).
      CATCH cx_fdt_excel_core.

        IF lo_excel_ref IS NOT BOUND.
          MESSAGE 'This file could not be found in your device' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
    ENDTRY.

    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheet)
  ).

    IF lt_worksheet IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_worksheet_table) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet[ 1 ] ).
    ASSIGN lo_worksheet_table->* TO <ft_excel_upload>.

    DELETE <ft_excel_upload> INDEX 1.

    LOOP AT <ft_excel_upload> ASSIGNING FIELD-SYMBOL(<fs_excel_upload>)  .
      APPEND INITIAL LINE TO mt_schedule ASSIGNING FIELD-SYMBOL(<fs_new_excel>).
      DO.

        DATA(lv_index) = sy-index.

        ASSIGN COMPONENT lv_index OF STRUCTURE <fs_excel_upload> TO FIELD-SYMBOL(<fs>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT lv_index OF STRUCTURE <fs_new_excel> TO FIELD-SYMBOL(<fs_new>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CASE lv_index.
          WHEN 4 OR 9.
            SPLIT <fs> AT '.' INTO lv_day lv_month DATA(lv_year).

            UNPACK lv_day TO lv_day.
            UNPACK lv_month TO lv_month.

            <fs_new> = lv_year && lv_month && lv_day.

            CONTINUE.
          WHEN 1.
            <fs_new> = CONV vbeln( |{ <fs> ALPHA = IN }| ).
          WHEN OTHERS.
            <fs_new> = <fs>  .
        ENDCASE.
      ENDDO.
    ENDLOOP.

    DELETE mt_schedule WHERE sales_order IS INITIAL.
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

  METHOD sales_order_change.

    DATA:
      ls_message          TYPE ty_message,
      lv_sched_line       TYPE vbep-etenr,
      lt_schedule_in      TYPE TABLE OF bapischdl,
      ls_schedule_in      TYPE  bapischdl,
      lt_schedule_inx     TYPE TABLE OF bapischdlx,
      ls_schedule_inx     TYPE bapischdlx,
      lt_del_schedule_in  TYPE TABLE OF bapisddelsched_in,
      ls_del_schedule_in  TYPE  bapisddelsched_in,
      lt_del_schedule_inx TYPE TABLE OF bapisddelsched_inx,
      ls_del_schedule_inx TYPE  bapisddelsched_inx,
      ls_order_header_inx TYPE bapisdhd1x.
    DATA : lt_buffer TYPE SORTED TABLE OF ty_buffer WITH UNIQUE KEY vbeln posnr.

    ls_order_header_inx-updateflag = 'U'.

    LOOP AT mt_schedule ASSIGNING FIELD-SYMBOL(<fs_schedule>) GROUP BY ( sales_order = <fs_schedule>-sales_order ) ASSIGNING FIELD-SYMBOL(<fg_schedule>).

      LOOP AT GROUP <fg_schedule> ASSIGNING FIELD-SYMBOL(<fs_vbeln>).

        IF <fs_vbeln>-sched_line IS INITIAL.

          READ TABLE lt_buffer ASSIGNING FIELD-SYMBOL(<fs_buffer>) WITH KEY vbeln = <fs_vbeln>-sales_order  posnr = <fs_vbeln>-itm_number  BINARY SEARCH.

          IF sy-subrc <> 0.

            INSERT VALUE #( vbeln  = <fs_vbeln>-sales_order
                            posnr  = <fs_vbeln>-itm_number )
                            INTO TABLE lt_buffer ASSIGNING <fs_buffer>.


            SELECT MAX( etenr )
               FROM vbep
               INTO <fs_buffer>-max_pos
               WHERE vbeln = <fs_vbeln>-sales_order
               AND posnr = <fs_vbeln>-itm_number.

          ENDIF.

          <fs_buffer>-max_pos = <fs_buffer>-max_pos + 1.
          <fs_vbeln>-sched_line = <fs_buffer>-max_pos.

        ENDIF.



        ls_schedule_in =  VALUE #(  itm_number = <fs_vbeln>-itm_number
                                     sched_line = <fs_vbeln>-sched_line
                                     req_date   = <fs_vbeln>-req_date
                                     date_type  = <fs_vbeln>-date_type
                                     req_qty    = <fs_vbeln>-req_qty
                                     sched_type = 'L1'
                                     rel_type   =  '1'
                                     plan_sched_type = <fs_vbeln>-schedule_type
                                                                          ) .
        IF <fs_vbeln>-sched_line IS NOT INITIAL.
          DATA(lv_updateflag) = 'U'.
        ELSE.
          lv_updateflag = 'I'.
        ENDIF.

        ls_schedule_inx = VALUE #(  itm_number = <fs_vbeln>-itm_number
                                      sched_line =  <fs_vbeln>-sched_line
                                      updateflag = lv_updateflag
                                      req_date   = 'X'
                                      date_type  = 'X'
                                      req_qty    = 'X'
                                      sched_type = 'X'
                                      rel_type   = 'X'
                                      plan_sched_type = 'X' ) .



        IF <fs_vbeln>-div_schedule IS INITIAL AND <fs_vbeln>-dic_sched_date IS  INITIAL.
          CONTINUE.
        ENDIF.

        ls_del_schedule_in = VALUE #(   itm_number = <fs_vbeln>-itm_number
                                        rel_type   =  '1'
                                        dlvschedno = <fs_vbeln>-div_schedule
                                        dlvscheddate = <fs_vbeln>-dic_sched_date )  .


        ls_del_schedule_inx = VALUE #( itm_number = <fs_vbeln>-itm_number
                                         rel_type   =  '1'
                                         dlvschedno = 'X'
                                         dlvscheddate = 'X'
                                         updateflag = 'U'                            ) .



      ENDLOOP.

      CALL FUNCTION 'SD_SALESDOCUMENT_CHANGE'
        EXPORTING
          salesdocument    = <fg_schedule>-sales_order
          order_header_inx = ls_order_header_inx
        TABLES
          return           = mt_return
          schedule_in      = lt_schedule_in
          schedule_inx     = lt_schedule_inx
          del_schedule_in  = lt_del_schedule_in
          del_schedule_inx = lt_del_schedule_inx.

      CLEAR : ls_schedule_in , ls_schedule_inx , ls_del_schedule_in , ls_del_schedule_inx.

      LOOP AT mt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
        CLEAR ls_message.
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
        ENDCASE.
        ls_message-message = <fs_return>-message.
        ls_message-icon = lv_icon.
        ls_message-id = sy-tabix .
        ls_message-sales_order = <fs_vbeln>-sales_order.
        ls_message-type = <fs_return>-type.

        APPEND ls_message TO mt_message.
      ENDLOOP.


      IF lv_error IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDLOOP.

    display_logs( ).

  ENDMETHOD.

  METHOD display_logs.
    DATA: lo_salv     TYPE REF TO cl_salv_table,
          lo_column   TYPE REF TO cl_salv_column_table,
          lo_tooltips TYPE REF TO cl_salv_tooltips,
          lv_value    TYPE lvc_value.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = mt_message ).


        lo_salv->get_columns( )->set_optimize( ).
        lo_salv->get_functions( )->set_all( ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'SALES_ORDER' ).
        lo_column->set_medium_text( 'Sales Order' ).
        lo_column->set_key( ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'ID' ).
        lo_column->set_medium_text( 'Line' ).
        lo_column->set_key( ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'ICON' ).
        lo_column->set_medium_text( 'Status' ).
        lo_column->set_icon( if_salv_c_bool_sap=>true ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'MESSAGE' ).
        lo_column->set_medium_text( 'Message' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'TYPE' ).
        lo_column->set_medium_text( 'TYPE' ).

        lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

*    Tooltips

        lo_tooltips = lo_salv->get_functional_settings( )->get_tooltips( ).

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

* display the table
    lo_salv->display( ).

  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'FC01'.
        download_template( ).
    ENDCASE.

  ENDMETHOD.

  METHOD download_template.


    DATA: lt_template TYPE STANDARD TABLE OF  ty_schedule.
    DATA: lo_salv   TYPE REF TO cl_salv_table,
          lo_column TYPE REF TO cl_salv_column_table.


    TRY.
        cl_salv_table=>factory( IMPORTING
                                 r_salv_table   = lo_salv
                               CHANGING
                                 t_table        = lt_template ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'D_DATE' ).

        lo_column->set_short_text( 'Dlv Date' ).
        lo_column->set_medium_text( 'Delivery Date' ).
        lo_column->set_long_text( 'Delivery Date' ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'DLV_SCHED_DATE' ).

        lo_column->set_short_text( 'Dlv Sched' ).
        lo_column->set_medium_text( 'Delivery Sched' ).
        lo_column->set_long_text( 'Delivery S' ).

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

  METHOD execute.
    upload_excel( ).
    sales_order_change( ).

  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

  DATA(go_bapi) = NEW lcl_update_schedule( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  go_bapi->get_filepath( ).

AT SELECTION-SCREEN .
  go_bapi->at_selection_screen( ).

START-OF-SELECTION.

  go_bapi->execute( ).
