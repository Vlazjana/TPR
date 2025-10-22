    TYPES:
      BEGIN OF ty_excel,
        icon_descr TYPE text15.
        INCLUDE STRUCTURE z2fi_riscat_riep_alv.
        TYPES color      TYPE lvc_t_scol.
    TYPES: END OF ty_excel.

    DATA: lt_sheet_data TYPE zcl_fdt_xl_spreadsheet_v2=>tt_sheet_data,
          lt_excel      TYPE STANDARD TABLE OF ty_excel,
          lt_field_desc TYPE zcl_fdt_xl_spreadsheet_v2=>tt_desc.

    LOOP AT tb_out ASSIGNING FIELD-SYMBOL(<ls_out>).
      APPEND CORRESPONDING #( <ls_out> ) TO lt_excel ASSIGNING FIELD-SYMBOL(<ls_excel>).
      CASE <ls_out>-stato_oda.
        WHEN icon_green_light.
          <ls_excel>-icon_descr = 'Completo'.
          APPEND VALUE #( fname = 'ICON_DESCR' color-col = 5 ) TO <ls_excel>-color.
        WHEN icon_yellow_light.
          <ls_excel>-icon_descr = 'Incompleto'.
          APPEND VALUE #( fname = 'ICON_DESCR' color-col = 3 ) TO <ls_excel>-color.
      ENDCASE.
    ENDLOOP.

    lt_field_desc = VALUE #( FOR <ls_fcat> IN tb_fc
                             ( fieldname  = COND #( WHEN <ls_fcat>-fieldname = 'STATO_ODA' THEN 'ICON_DESCR' ELSE <ls_fcat>-fieldname )
                               descr      = <ls_fcat>-seltext ) ).

    lt_sheet_data = VALUE #(
  ( sheet_name  = 'Sheet1'
    tab_data    = REF #( lt_excel )
    field_desc  = lt_field_desc
    color_field = 'COLOR'
  ) ).

    DATA(lv_bin_data) = zcl_fdt_xl_spreadsheet_v2=>create_document(
                        name              = lv_filename
                        it_sheet_data     = lt_sheet_data ).

    DATA(lt_raw_data) = cl_bcs_convert=>xstring_to_solix( EXPORTING iv_xstring = lv_bin_data ).

    cl_gui_frontend_services=>gui_download( EXPORTING
                                              filename     = lv_fullpath
                                              filetype     = 'BIN'
                                              bin_filesize = xstrlen( lv_bin_data )
                                            CHANGING
                                              data_tab     = lt_raw_data ).

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document               = lv_fullpath
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10.
