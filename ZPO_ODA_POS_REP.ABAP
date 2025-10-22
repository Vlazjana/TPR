*&---------------------------------------------------------------------*
*& Report ZPO_ODA_POS_REP
*&---------------------------------------------------------------------*
REPORT zpo_oda_pos_rep.

TABLES: ekko, ekpo, sscrfields.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_ebeln FOR ekpo-ebeln,
                  s_ebelp FOR ekpo-ebelp,
                  s_aedat FOR ekko-aedat ,
                  s_ernam FOR ekko-ernam NO INTERVALS.

  PARAMETERS: p_elikz TYPE ekpo-elikz,
              p_loekz TYPE ekpo-loekz AS CHECKBOX.

  SELECT-OPTIONS: s_bukrs FOR ekko-bukrs NO INTERVALS,
                  s_lifnr FOR ekko-lifnr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_delim TYPE c LENGTH 1 DEFAULT ',' .
SELECTION-SCREEN END OF BLOCK b2.

CLASS lcl_report DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      at_selection_screen,
      init.

    METHODS:  execute IMPORTING iv_upload TYPE abap_bool OPTIONAL,
      status_0100,
      user_command_0100.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_csv_data,
             ebeln       TYPE ebeln,
             ebelp       TYPE ebelp,
             elikz       TYPE ekpo-elikz,
             loekz       TYPE ekpo-loekz,
             motivazione TYPE string,
           END OF ty_csv_data,
           tt_csv_data TYPE STANDARD TABLE OF ty_csv_data WITH EMPTY KEY.

    TYPES: BEGIN OF ty_alv_data,
             status  TYPE icon_d,
             ebeln   TYPE ekpo-ebeln,
             ebelp   TYPE ekpo-ebelp,
             bsart   TYPE ekko-bsart,
             aedat   TYPE ekko-aedat,
             ernam   TYPE ekko-ernam,
             elikz   TYPE ekpo-elikz,
             loekz   TYPE ekpo-loekz,
             ekgrp   TYPE ekko-ekgrp,
             netpr   TYPE ekpo-netpr,
             waers   TYPE ekko-waers,
             bukrs   TYPE ekko-bukrs,
             lifnr   TYPE ekko-lifnr,
             zzmatu  TYPE ekpo-zzmatu,
             changed TYPE abap_bool,
           END OF ty_alv_data.

    TYPES: BEGIN OF ty_msg,
             time     TYPE sy-uzeit,
             test     TYPE icon_d,
             ebeln    TYPE ekkn-ebeln,
             status   TYPE icon_d,
             msg(220) TYPE c,
           END OF ty_msg.

    DATA: mt_msg        TYPE STANDARD TABLE OF ty_msg,
          mt_alv_data   TYPE STANDARD TABLE OF ty_alv_data,
          mt_first_data TYPE STANDARD TABLE OF ty_alv_data.

    DATA: mo_grid     TYPE REF TO cl_gui_alv_grid,
          mo_cont     TYPE REF TO cl_gui_custom_container,
          mo_salv_msg TYPE REF TO cl_salv_table,
          mo_dock_msg TYPE REF TO cl_gui_docking_container.

    CLASS-METHODS: download_template.

    METHODS: extract_data,
      csv_to_table CHANGING cv_filename TYPE string OPTIONAL,
      display_alv,
      value_request RETURNING VALUE(rv_filename) TYPE string.


    METHODS: handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object,
      handle_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_logs_ucomm FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.


    METHODS: mass_update IMPORTING iv_test TYPE bapiflag-bapiflag,
      bapi_change IMPORTING iv_ebeln    TYPE ebeln
                            iv_test     TYPE bapiflag-bapiflag
                            iv_time     TYPE sy-uzeit
                            iv_versions TYPE  bapimedcm
                  CHANGING  cs_out      TYPE ty_alv_data
                            ct_poitem   TYPE bapimepoitem_tp
                            ct_poitemx  TYPE bapimepoitemx_tp ,

      get_status_icon IMPORTING iv_type        TYPE sy-msgty
                      RETURNING VALUE(rv_icon) TYPE icon_d.


    METHODS: display_docking ,
      sort_cols_msg,
      adjust_columns,
      set_tooltips,
      set_tooltips_msg,
      set_default_tooltips CHANGING co_tooltips TYPE REF TO cl_salv_tooltips.

ENDCLASS.

DATA go_report TYPE REF TO lcl_report.
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.

    DATA lv_filename TYPE string.
    CLEAR: mt_alv_data, lv_filename.

    IF iv_upload = abap_true.


      IF lv_filename IS INITIAL.
        lv_filename = value_request( ).

        IF lv_filename IS INITIAL.
          MESSAGE 'Nessun file selezionato' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDIF.

      csv_to_table( CHANGING cv_filename = lv_filename ).

    ELSE.

      IF s_aedat[] IS INITIAL.
        MESSAGE 'Il campo Data Creazione è obbligatorio' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      extract_data( ).

    ENDIF.
    IF mt_alv_data IS INITIAL.
      MESSAGE 'Nessun dato trovato nel database' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    display_alv( ).
  ENDMETHOD.

  METHOD init.

    DATA: lt_buttons TYPE STANDARD TABLE OF smp_dyntxt.

    lt_buttons = VALUE #(
      ( icon_id = icon_export icon_text = 'Download Template' )
      ( icon_id = icon_import icon_text = 'Upload File' ) ).

    LOOP AT lt_buttons ASSIGNING FIELD-SYMBOL(<ls_button>).
      CASE sy-tabix.
        WHEN 1.
          sscrfields-functxt_01 = <ls_button>.
        WHEN 2.
          sscrfields-functxt_02 = <ls_button>.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.
      WHEN 'FC01'.
        download_template( ).
      WHEN 'FC02'.
        CREATE OBJECT go_report.
        go_report->execute(
          iv_upload = abap_true
        ).
    ENDCASE.
  ENDMETHOD.

  METHOD extract_data.

    CLEAR mt_alv_data.
    DATA(lv_loekz) = COND ekpo-loekz( WHEN p_loekz = abap_true THEN 'L' ).

    SELECT ekpo~ebeln, ekpo~ebelp, ekko~bsart, ekko~aedat, ekko~ernam,
           ekpo~elikz,
       CASE WHEN ekpo~loekz = 'L' THEN 'X' END AS loekz,
        ekko~ekgrp, ekpo~netpr, ekko~waers,
           ekko~bukrs, ekko~lifnr , ekpo~zzmatu
      FROM ekpo
      INNER JOIN ekko ON ekko~ebeln = ekpo~ebeln
      WHERE ekpo~ebeln IN @s_ebeln
        AND ekpo~ebelp IN @s_ebelp
        AND ekko~aedat IN @s_aedat
        AND ekko~ernam IN @s_ernam
        AND ekpo~elikz =  @p_elikz
        AND ekpo~loekz =  @lv_loekz
        AND ekko~bukrs IN @s_bukrs
        AND ekko~lifnr IN @s_lifnr
      INTO CORRESPONDING FIELDS OF TABLE @mt_alv_data.


  ENDMETHOD.

  METHOD value_request.

    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_filename = '*.CSV'
        file_filter      = '*.CSV'
      CHANGING
        file_table       = lt_filetable
        rc               = lv_rc
      EXCEPTIONS
        OTHERS           = 5 ).

    IF sy-subrc = 0 AND lv_rc > 0.
      READ TABLE lt_filetable INDEX 1 INTO DATA(ls_file).
      rv_filename = ls_file-filename.
    ENDIF.

  ENDMETHOD.

  METHOD csv_to_table.

    TYPES: BEGIN OF ty_split,
             ebeln       TYPE string,
             ebelp       TYPE string,
             elikz       TYPE string,
             loekz       TYPE string,
             motivazione TYPE string,
           END OF ty_split.

    DATA: lt_strings   TYPE STANDARD TABLE OF string,
          ls_split     TYPE ty_split,
          lv_line      TYPE string,
          lv_delim     TYPE c LENGTH 1,
          lt_csv_tab   TYPE tt_csv_data,
          lt_filetable TYPE TABLE OF string.


    DATA: lt_excel_check TYPE TABLE OF ty_csv_data.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = cv_filename
                                                     filetype                = 'ASC'
                                          CHANGING   data_tab                = lt_strings
                                          EXCEPTIONS file_open_error         = 1
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
                                                     OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      MESSAGE 'Errore nel caricamento del file' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_strings ASSIGNING FIELD-SYMBOL(<ls_strings>) FROM 2.
      DATA(lv_excel_line) = sy-tabix.
      IF <ls_strings> IS INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR ls_split.
      SPLIT <ls_strings> AT p_delim INTO
            ls_split-ebeln
            ls_split-ebelp
            ls_split-elikz
            ls_split-loekz
            ls_split-motivazione.

      ls_split-ebeln = |{ ls_split-ebeln ALPHA = OUT }|.

      APPEND VALUE #(
            ebeln       =  ls_split-ebeln
            ebelp       =  ls_split-ebelp
            elikz       =  ls_split-elikz
            loekz       =  ls_split-loekz
            motivazione =  ls_split-motivazione

       ) TO lt_csv_tab.

    ENDLOOP.

    IF lt_csv_tab IS INITIAL .
      RETURN.
    ENDIF.
    SORT lt_csv_tab BY ebeln ebelp.
    SELECT a~ebeln,
           a~ebelp,
           b~bsart,
           b~aedat,
           b~ernam,
           b~ekgrp,
           a~netpr,
           b~waers,
           b~bukrs,
           b~lifnr,
           a~zzmatu
        FROM ekpo AS a
         JOIN ekko AS b
         ON a~ebeln = b~ebeln
        INTO CORRESPONDING FIELDS OF TABLE @mt_alv_data
        FOR ALL ENTRIES IN @lt_csv_tab
          WHERE a~ebeln = @lt_csv_tab-ebeln
          AND a~ebelp = @lt_csv_tab-ebelp.

    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_alv_data>) .
      READ TABLE lt_csv_tab ASSIGNING FIELD-SYMBOL(<ls_excel>)
             WITH KEY ebeln = <ls_alv_data>-ebeln
                      ebelp = <ls_alv_data>-ebelp BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_alv_data>-elikz = COND #( WHEN <ls_excel>-elikz = 'X' THEN abap_true ELSE '' ).
        <ls_alv_data>-loekz = COND #( WHEN <ls_excel>-loekz = 'X' OR <ls_excel>-loekz = 'L' THEN abap_true ELSE '' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD display_alv.

    DATA:lt_gridfcat TYPE lvc_t_fcat.


    mo_cont = NEW cl_gui_custom_container( container_name = 'CUST_CONT' ).
    mo_grid = NEW cl_gui_alv_grid( i_parent = mo_cont ).

    SET HANDLER:    handle_toolbar
                    handle_ucomm FOR mo_grid.


    lt_gridfcat = VALUE #( tabname = 'MT_ALV_DATA'
                    ( fieldname = 'STATUS' coltext = 'Status'   )
                    ( fieldname = 'EBELN'  ref_table = 'EKPO'  )
                    ( fieldname = 'EBELP'  ref_table = 'EKPO'  )
                    ( fieldname = 'BSART'  ref_table = 'EKKO'    )
                    ( fieldname = 'AEDAT'  ref_table = 'EKKO'  )
                    ( fieldname = 'ERNAM'  ref_table = 'EKKO'  )
                    ( fieldname = 'ELIKZ'  ref_table = 'EKPO' edit = 'X' checkbox = 'X' )
                    ( fieldname = 'LOEKZ'  ref_table = 'EKPO' edit = 'X' checkbox = 'X')
                    ( fieldname = 'EKGRP'  ref_table = 'EKKO'  )
                    ( fieldname = 'NETPR'  ref_table = 'EKPO'  )
                    ( fieldname = 'WAERS'  ref_table = 'EKKO'  )
                    ( fieldname = 'BUKRS'  ref_table = 'EKKO'  )
                    ( fieldname = 'LIFNR'  ref_table = 'EKKO'  )
                    ( fieldname = 'ZZMATU' ref_table = 'EKPO' coltext = 'Maturato' checkbox = 'X'  )
                     ).


    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt  = abap_true
                                        no_rowmark = 'X'
                                          ).


    DATA(lt_exclude) = VALUE ui_functions(
      ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
      ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
      ( cl_gui_alv_grid=>mc_fc_loc_append_row )
      ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
      ( cl_gui_alv_grid=>mc_fc_loc_move_row )
      ( cl_gui_alv_grid=>mc_fc_loc_copy )
      ( cl_gui_alv_grid=>mc_fc_loc_cut )
      ( cl_gui_alv_grid=>mc_fc_loc_paste )
      ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
      ( cl_gui_alv_grid=>mc_fc_loc_undo )
      ( cl_gui_alv_grid=>mc_fc_check )
    ).
    mo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    mo_grid->set_table_for_first_display(
      EXPORTING
        is_layout            = ls_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = mt_alv_data
        it_fieldcatalog      = lt_gridfcat ).

    CALL SCREEN 100.


  ENDMETHOD.


  METHOD handle_toolbar.

    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
      ( function = 'SIMULATE' icon = icon_test text = 'Test Run'
        quickinfo = 'Simulate Changes' )
      ( function = 'POST' icon = icon_execute_object text = 'Post'
        quickinfo = 'Post Changes' ) ).

  ENDMETHOD.

  METHOD handle_ucomm.
    DATA: lv_answer TYPE c LENGTH 1.

    CASE e_ucomm.
      WHEN 'SIMULATE'.
        mass_update( iv_test = abap_true ).

      WHEN 'POST'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confermare l esecuzione'
            text_question         = 'Eseguire le modifiche all PO?'
            text_button_1         = 'Si'
            text_button_2         = 'No'
            default_button        = '2'
            display_cancel_button = 'X'
          IMPORTING
            answer                = lv_answer.

        IF lv_answer = '1'.
          mass_update( iv_test = abap_false ).
        ENDIF.

    ENDCASE.
    display_docking( ).
  ENDMETHOD.

  METHOD mass_update.

    DATA: lt_poitem          TYPE STANDARD TABLE OF bapimepoitem,
          lt_poitemx         TYPE STANDARD TABLE OF bapimepoitemx,
          lt_data_to_process TYPE STANDARD TABLE OF ty_alv_data.

    IF mt_alv_data IS INITIAL.
      MESSAGE 'Nessun dato da elaborare' TYPE 'I'.
      RETURN.
    ENDIF.

    CLEAR mt_msg.
    GET TIME FIELD DATA(lv_time).

    DATA(ls_versions) = VALUE bapimedcm( reason = '05'
                                         completed = 'X' ).


    SELECT  ebeln,
            ebelp,
            loekz,
            elikz
          FROM ekpo
          FOR ALL ENTRIES IN @mt_alv_data
          WHERE ebeln = @mt_alv_data-ebeln
            AND ebelp = @mt_alv_data-ebelp
          INTO TABLE @DATA(lt_ekpo)
      .
    SORT lt_ekpo BY ebeln ebelp.

    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>) .
      READ TABLE lt_ekpo ASSIGNING FIELD-SYMBOL(<ls_ekpo>) WITH KEY ebeln = <ls_data>-ebeln ebelp = <ls_data>-ebelp BINARY SEARCH.

      IF sy-subrc = 0.
        DATA(lv_loekz) = COND ekpo-loekz( WHEN <ls_ekpo>-loekz = 'X' OR <ls_ekpo>-loekz = 'L' THEN abap_true ELSE '' ).
        IF  (  <ls_data>-loekz <> lv_loekz  OR <ls_data>-elikz <> <ls_ekpo>-elikz ) .
          APPEND <ls_data> TO lt_data_to_process.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_data_to_process IS INITIAL.
      MESSAGE 'Nessuna riga modificata da elaborare' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_data_to_process ASSIGNING FIELD-SYMBOL(<ls_alv_data>)
     GROUP BY <ls_alv_data>-ebeln ASSIGNING FIELD-SYMBOL(<ls_group>).

      CLEAR: lt_poitem, lt_poitemx.

      LOOP AT GROUP <ls_group> ASSIGNING FIELD-SYMBOL(<ls_po_item>).
        APPEND VALUE #(
          po_item    = <ls_po_item>-ebelp
          delete_ind = <ls_po_item>-loekz
          no_more_gr = <ls_po_item>-elikz
          net_price  = <ls_po_item>-netpr
        ) TO lt_poitem.

        APPEND VALUE #(
          po_item    = <ls_po_item>-ebelp
          po_itemx   = abap_true
          delete_ind = abap_true
          no_more_gr = abap_true
          net_price  = abap_true
        ) TO lt_poitemx.
      ENDLOOP.

      bapi_change( EXPORTING iv_ebeln    = <ls_po_item>-ebeln
                             iv_test     = iv_test
                             iv_time     = lv_time
                             iv_versions = ls_versions
                   CHANGING  cs_out      = <ls_po_item>
                             ct_poitem   = lt_poitem
                             ct_poitemx  = lt_poitemx ).


    ENDLOOP.

    mo_grid->refresh_table_display( ).


  ENDMETHOD.

  METHOD bapi_change.

    DATA: lt_return  TYPE STANDARD TABLE OF bapiret2.
    DATA: lv_has_error   TYPE abap_bool,
          lv_has_success TYPE abap_bool.

    CLEAR: lt_return.


    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = iv_ebeln
        testrun       = iv_test
        versions      = iv_versions
      TABLES
        return        = lt_return
        poitem        = ct_poitem
        poitemx       = ct_poitemx.


    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      IF <ls_return>-type CA 'EAX'.
        lv_has_error = abap_true.
      ELSEIF <ls_return>-type = 'S'.
        lv_has_success = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_has_error = abap_true.
      cs_out-status = icon_red_light.
    ELSEIF lv_has_success = abap_true.
      cs_out-status = icon_green_light.
    ELSE.
      cs_out-status = icon_yellow_light.
    ENDIF.
    MODIFY mt_alv_data FROM cs_out TRANSPORTING status WHERE  ebeln = iv_ebeln.

    LOOP AT lt_return ASSIGNING <ls_return>.

      APPEND VALUE #(
        time   = iv_time
        test   = COND #( WHEN iv_test = abap_true THEN icon_test ELSE icon_execute_object )
        ebeln  = iv_ebeln
        status = get_status_icon( <ls_return>-type )
        msg    = <ls_return>-message
      ) TO mt_msg.
    ENDLOOP.


    IF lines( lt_return ) = 0 AND cs_out-status IS INITIAL.
      cs_out-status = icon_green_light.
      APPEND VALUE #(
        time   = iv_time
        test   = COND #( WHEN iv_test = abap_true THEN icon_test ELSE icon_execute_object )
        ebeln  = iv_ebeln
        status = icon_green_light
        msg    = COND #( WHEN iv_test = abap_true
                        THEN 'Esecuzione del test riuscita'
                        ELSE 'PO item aggiornato con successo' )
      ) TO mt_msg.
    ENDIF.

    IF iv_test = abap_false AND NOT line_exists( lt_return[ type = 'E' ] ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSEIF line_exists( lt_return[ type = 'E' ] ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.

  METHOD get_status_icon.

    rv_icon = SWITCH #( iv_type
                        WHEN 'E' OR 'X' OR 'A' THEN icon_red_light
                        WHEN 'W' THEN icon_yellow_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'I' THEN icon_information ).
  ENDMETHOD.
  METHOD handle_logs_ucomm.

    CASE e_salv_function.

      WHEN 'FC_CLOSE'.

        IF mo_salv_msg IS NOT INITIAL.
          FREE mo_salv_msg.
        ENDIF.

        mo_dock_msg->set_visible(
          EXPORTING
            visible           = abap_false
          EXCEPTIONS
            cntl_error        = 1                " CNTL_ERROR
            cntl_system_error = 2                " CNTL_SYSTEM_ERROR
            OTHERS            = 3
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD display_docking.
*
    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lo_events    TYPE REF TO cl_salv_events_table.

    IF mo_salv_msg IS NOT INITIAL.
      mo_salv_msg->refresh( ).
      RETURN.
    ENDIF.

    IF mt_msg IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT mo_dock_msg
      EXPORTING
        repid = sy-repid
        dynnr = sy-dynnr
        ratio = 50
        side  = cl_gui_docking_container=>dock_at_right.
    cl_gui_cfw=>set_new_ok_code( '/00' ).
    mo_dock_msg->set_visible(
      EXPORTING
        visible           = abap_true        " Visible
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_dock_msg
          IMPORTING
            r_salv_table = mo_salv_msg
          CHANGING
            t_table      = mt_msg ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    lo_functions = mo_salv_msg->get_functions( ).
    lo_functions->set_all( 'X' ).

    TRY.
        DATA(lv_icon) = icon_close.
        lo_functions->add_function(
          EXPORTING
            name     = 'FC_CLOSE'
            icon     = CONV #( lv_icon )
            tooltip  = 'Close'
            position = if_salv_c_function_position=>right_of_salv_functions
        ).
      CATCH cx_salv_existing.
      CATCH cx_salv_wrong_call.
    ENDTRY.

    adjust_columns( ).
    sort_cols_msg( ).
    set_tooltips( ).
    set_tooltips_msg( ).

    lo_events = mo_salv_msg->get_event( ).
    SET HANDLER handle_logs_ucomm
           FOR lo_events.

    mo_salv_msg->display( ).
  ENDMETHOD.

  METHOD set_tooltips.

    DATA(lo_tooltips) = mo_salv_msg->get_functional_settings( )->get_tooltips( ).

    set_default_tooltips( CHANGING co_tooltips = lo_tooltips ).
  ENDMETHOD.

  METHOD set_tooltips_msg.
    DATA(lo_tooltips) = mo_salv_msg->get_functional_settings( )->get_tooltips( ).

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_test )
          tooltip = 'Modalità Test' ).                      "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.


    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_check )
          tooltip = 'Controllo del sistema' ).              "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    set_default_tooltips( CHANGING co_tooltips = lo_tooltips ).

  ENDMETHOD.

  METHOD set_default_tooltips.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_green_light )
          tooltip = 'Successo' ).                           "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_yellow_light )
          tooltip = 'Avvertimento' ).                       "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_red_light )
          tooltip = 'Errore' ).                             "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_information )
          tooltip = 'Informazione' ).                       "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


  METHOD sort_cols_msg.
    DATA: lo_sort TYPE REF TO cl_salv_sorts.

    " get Sort object
    lo_sort = mo_salv_msg->get_sorts( ).
    lo_sort->clear( ).
    TRY.
        lo_sort->add_sort( columnname = 'TIME'  sequence = if_salv_c_sort=>sort_down ).
        lo_sort->add_sort( columnname = 'EBELN' ).
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.

    mo_salv_msg->get_columns( )->set_optimize( ).
    mo_salv_msg->get_filters( )->clear( ).

  ENDMETHOD.

  METHOD adjust_columns.

    IF mo_salv_msg IS INITIAL.
      RETURN.
    ENDIF.

    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv_msg->get_columns( ).
    lo_cols->set_optimize( ).

    TRY.
        lo_column ?= lo_cols->get_column( 'TIME' ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'TEST' ).
        DATA(lv_text) = CONV scrtext_s( 'Test').
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'EBELN' ).
        lo_column->set_key( ).

        lv_text = CONV scrtext_s( 'Stato' ).
        lo_column ?= lo_cols->get_column( 'STATUS' ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Messaggi'.
        lo_column ?= lo_cols->get_column( 'MSG' ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lo_cols->set_column_position( columnname = 'STATUS'
                                  position   = 1 ).


  ENDMETHOD.

  METHOD download_template.

    DATA: lv_filename  TYPE string,
          lv_path      TYPE string,
          lv_fullpath  TYPE string,
          lv_action    TYPE i,
          lv_delimiter TYPE c LENGTH 1,
          lt_csv_data  TYPE STANDARD TABLE OF string.



    CONCATENATE 'Ordine Dacquisto'
                'Posizione Ordine'
                'Consegna finale'
                'Indice di Cancellazione'
                'Motivazione'
                 INTO DATA(lv_header) SEPARATED BY p_delim.

    APPEND lv_header TO lt_csv_data.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension         = 'csv'
        default_file_name         = 'PO_Template.csv'
        file_filter               = 'CSV Files (*.csv)|*.csv'
      CHANGING
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
        user_action               = lv_action
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
      RETURN.
    ENDIF.

    IF lv_action = cl_gui_frontend_services=>action_ok AND lv_fullpath IS NOT INITIAL.

      " Download CSV file
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename                = lv_fullpath
          filetype                = 'ASC'
          write_field_separator   = 'X'
        CHANGING
          data_tab                = lt_csv_data
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24
      ).

      IF sy-subrc = 0.
        MESSAGE |Template CSV scaricato con successo in { lv_fullpath }| TYPE 'S'.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD status_0100.
    SET PF-STATUS 'STATUS_0100'.
    SET TITLEBAR 'TITLE_0100'.
  ENDMETHOD.

  METHOD user_command_0100.
    CASE sy-ucomm.
      WHEN 'FC_BACK' OR 'FC_EXIT'.

        IF mo_dock_msg IS NOT INITIAL.
          mo_dock_msg->set_visible( abap_false ).
          mo_dock_msg->free( ).
        ENDIF.

        IF mo_grid IS BOUND.
          mo_grid->free( ).
*          FREE mo_grid.
          CLEAR mo_grid.
        ENDIF.

        LEAVE TO SCREEN 0.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  lcl_report=>init( ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN INTO DATA(ls_screen).
    IF ls_screen-name = 'S_AEDAT-LOW' OR ls_screen-name = 'P_DELIM'.
      ls_screen-required = '2'.
      MODIFY SCREEN FROM ls_screen.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF ( sscrfields-ucomm = 'FC01' OR sscrfields-ucomm = 'FC02' ) AND
     p_delim IS INITIAL.
    MESSAGE 'Il delimitatore è obbligatorio per questa operazione' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  lcl_report=>at_selection_screen( ).

START-OF-SELECTION.
  CREATE OBJECT go_report.
  go_report->execute( ).

MODULE status_0100 OUTPUT.
  go_report->status_0100( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  go_report->user_command_0100( ).
ENDMODULE.
