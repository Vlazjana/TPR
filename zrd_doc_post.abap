REPORT zrd_doc_post.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS: p_file TYPE string .

SELECTION-SCREEN END OF BLOCK b01.


*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class_name DEFINITION FINAL.
  PUBLIC SECTION.


    CLASS-METHODS value_request.

    METHODS execute.
    METHODS display_msg IMPORTING iv_no_dock TYPE abap_bool OPTIONAL.
    METHODS adjust_cols_msg.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_incasso,
             data_incasso  TYPE sy-datum,
             importing_qty TYPE bstmg,
             gl_account    TYPE hkont,
             cdnuc         TYPE char2,
             xblnr         TYPE xblnr,
             budat         TYPE budat,
             kunnr         TYPE kunnr,
             bukrs         TYPE bukrs,
             belnr         TYPE belnr,
             gjahr         TYPE gjahr,
             blocked       TYPE char1,
           END OF ty_incasso,
           tt_incasso TYPE STANDARD TABLE OF ty_incasso.

    TYPES: BEGIN OF ty_messages,
             time    TYPE sy-uzeit,
             hkont   TYPE hkont,
             buzei   TYPE buzei,
             cdnuc   TYPE char2,
             xblnr   TYPE xblnr,
             budat   TYPE budat,
             prog_nr TYPE i,
             icon    TYPE icon-id,
             message TYPE bapi_msg,
           END OF ty_messages.

    TYPES: BEGIN OF ty_excel,
             a TYPE string, "Sales order number
             b TYPE string, "Item number
             c TYPE string, "Schedule line
             d TYPE string, "Starting date
             e TYPE string, "Starting date
             f TYPE string, "Material number
             g TYPE string, "Plant
             h TYPE string, "Quantity
             i TYPE string, "Unit of measure
             j TYPE string, "Net price
             k TYPE string, "Currency
             l TYPE string, "Gross price
             m TYPE string, "Discount
             n TYPE string, "Tax
             o TYPE string, "Customer name
             p TYPE string, "Customer address
             q TYPE string, "Customer phone
             r TYPE string, "Email
             s TYPE string, "Payment terms
             t TYPE string, "Delivery date
             u TYPE string, "Shipping method
             v TYPE string, "Carrier
           END OF ty_excel,
           tt_excel TYPE STANDARD TABLE OF ty_excel.


    DATA: mt_messages TYPE STANDARD TABLE OF ty_messages,
          mt_incasso  TYPE tt_incasso,
          mo_salv_msg TYPE REF TO cl_salv_table,
          mt_excel    TYPE tt_excel.

    METHODS upload_file.
    METHODS data_checks.
    METHODS registrazione_incassi.
    METHODS pareggio_partite.

ENDCLASS.                    "

DATA go_class_name TYPE REF TO lcl_class_name.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class_name IMPLEMENTATION.

  METHOD execute.

    DATA: p_check1 TYPE char1 VALUE 'X',
          p_check2 TYPE char1 VALUE 'X',
          p_check3 TYPE char1 VALUE 'X',
          p_check4 TYPE char1 VALUE 'X',
          p_blart  TYPE char1 VALUE 'X',
          p_blart2 TYPE char1 VALUE 'X',
          p_hkont  TYPE char1 VALUE 'X',
          s_cdnuc  TYPE char1 VALUE 'X',
          p_budat  TYPE char1 VALUE 'X'.


    IF p_check1 IS INITIAL AND
       p_check2 IS INITIAL AND
       p_check3 IS INITIAL AND
       p_check4 IS INITIAL.

      MESSAGE 'You need to select an action to perform' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF (   p_check1 IS NOT INITIAL OR p_check2 IS NOT INITIAL )
      AND  ( ( p_file   IS INITIAL )   OR
             ( p_blart  IS INITIAL )   OR
             ( p_hkont  IS INITIAL ) ).
      MESSAGE 'Filename, Document type and GL Account are obligatory for incoming registration' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF (   p_check2 IS NOT INITIAL OR   p_check3 IS NOT INITIAL )
      AND  ( ( p_blart2  IS INITIAL )   OR ( p_budat  IS INITIAL     ) ).
      MESSAGE 'Document type and Registration Date are obligatory for pareggio partite' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    upload_file( ).

    IF ( p_check1 IS NOT INITIAL ) OR ( p_check2 IS NOT INITIAL ).
      registrazione_incassi( ).
    ENDIF.

    IF ( p_check1 IS NOT INITIAL ) OR ( p_check2 IS NOT INITIAL ).
      pareggio_partite( ).
    ENDIF.

    IF mt_messages IS NOT INITIAL.
      display_msg( ).
    ENDIF.

  ENDMETHOD.                    "execute

  METHOD registrazione_incassi.

    DATA: ls_incasso TYPE ty_incasso,
          s_cdnuc    TYPE char1.

    LOOP AT mt_excel ASSIGNING FIELD-SYMBOL(<ls_exc>)." WHERE i <> s_cdnuc.

      MOVE <ls_exc>-b TO ls_incasso-data_incasso.
      MOVE <ls_exc>-g TO ls_incasso-importing_qty.
      MOVE <ls_exc>-h TO ls_incasso-gl_account.
      MOVE <ls_exc>-i TO ls_incasso-cdnuc.
      MOVE <ls_exc>-o TO ls_incasso-xblnr.
      MOVE <ls_exc>-p TO ls_incasso-budat.

      APPEND  ls_incasso TO mt_incasso.
      CLEAR ls_incasso.

    ENDLOOP.

    data_checks( ).



  ENDMETHOD.                    "extract_data

  METHOD data_checks.

    DATA: lv_prev_gl_account TYPE hkont,
          lv_kunnr           TYPE kunnr,
          lv_altkn           TYPE altkn.

    GET TIME FIELD DATA(lv_uzeit).

    SELECT bukrs,
           budat,
           xblnr,
           kunnr,
           belnr,
           gjahr
    FROM bsid
      INTO TABLE @DATA(lt_bsid)
      FOR ALL ENTRIES IN @mt_incasso
      WHERE bukrs = @mt_incasso-bukrs
      AND   budat = @mt_incasso-budat
      AND   xblnr = @mt_incasso-xblnr.

    SORT mt_incasso BY gl_account.

    LOOP AT mt_incasso ASSIGNING FIELD-SYMBOL(<ls_incasso>).

      IF lv_prev_gl_account IS INITIAL OR lv_prev_gl_account <> <ls_incasso>-gl_account.
        lv_prev_gl_account = <ls_incasso>-gl_account.
        DATA(lv_buzei) = 1.
      ENDIF.

      READ TABLE lt_bsid ASSIGNING FIELD-SYMBOL(<ls_bsid>) WITH KEY bukrs = <ls_incasso>-bukrs
                                                                    budat = <ls_incasso>-budat
                                                                    xblnr = <ls_incasso>-xblnr.
      IF ( sy-subrc <> 0 ) OR ( <ls_incasso>-xblnr IS INITIAL ).

        APPEND VALUE #( time    = lv_uzeit
                        buzei   = lv_buzei
                        hkont   = <ls_incasso>-gl_account
                        cdnuc   = <ls_incasso>-cdnuc
                        xblnr   = <ls_incasso>-xblnr
                        budat   = <ls_incasso>-budat
                        prog_nr = 1
                        icon    = icon_red_light
                        message = 'Opened invoice not found'(017)
                      ) TO mt_messages.
        <ls_incasso>-blocked = abap_true.
        lv_buzei = lv_buzei + 1.
        CONTINUE.
      ENDIF.

      SELECT SINGLE kunnr
        FROM bseg
        INTO lv_kunnr
        WHERE bukrs = <ls_bsid>-bukrs
        AND   belnr = <ls_bsid>-belnr
        AND   gjahr = <ls_bsid>-gjahr
        AND   koart = 'D'.

      IF sy-subrc <> 0.
        APPEND VALUE #( time    = lv_uzeit
                        buzei   = lv_buzei
                        hkont   = <ls_incasso>-gl_account
                        cdnuc   = <ls_incasso>-cdnuc
                        xblnr   = <ls_incasso>-xblnr
                        budat   = <ls_incasso>-budat
                        prog_nr = 1
                        icon    = icon_red_light
                        message = 'Customer number not found'(017)
                      ) TO mt_messages.
        <ls_incasso>-blocked = abap_true.
        lv_buzei = lv_buzei + 1.
        CONTINUE.
      ENDIF.

      SELECT SINGLE altkn
        FROM knb1
        INTO lv_altkn
        WHERE bukrs = <ls_bsid>-bukrs
        AND   kunnr = lv_kunnr.

      IF ( sy-subrc <> 0 ) OR ( lv_altkn <> <ls_incasso>-gl_account ).
        APPEND VALUE #( time    = lv_uzeit
                        buzei   = lv_buzei
                        hkont   = <ls_incasso>-gl_account
                        cdnuc   = <ls_incasso>-cdnuc
                        xblnr   = <ls_incasso>-xblnr
                        budat   = <ls_incasso>-budat
                        prog_nr = 1
                        icon    = icon_red_light
                        message = 'Customer code does not match'(017)
                      ) TO mt_messages.
        <ls_incasso>-blocked = abap_true.
        lv_buzei = lv_buzei + 1.
        CONTINUE.
      ENDIF.



    ENDLOOP.
  ENDMETHOD.

  METHOD pareggio_partite.

  ENDMETHOD.                    "extract_data


  METHOD upload_file.

    DATA: lt_data    TYPE solix_tab.

    FIELD-SYMBOLS: <lt_excel> TYPE STANDARD TABLE.

    IF p_file IS INITIAL.
      MESSAGE s055(00) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = p_file    " Name of file
        filetype                = 'BIN'    " File Type (ASCII, Binary)
      CHANGING
        data_tab                = lt_data   " Transfer table for file contents
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
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring(
                        it_solix   = lt_data
                    ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
        document_name     = p_file
        xdocument         = lv_bin_data
    ).

    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheet)
    ).

    IF lt_worksheet IS INITIAL.
      MESSAGE 'No worksheet was found. Please, check your Excel file!'(005)
         TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_table) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet[ 1 ] ).

    ASSIGN lo_worksheet_table->* TO <lt_excel>.

    DELETE <lt_excel> INDEX 1.

    IF <lt_excel> IS INITIAL.

      MESSAGE 'Excel worksheet can not be empty!'(006)
         TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    MOVE-CORRESPONDING <lt_excel> TO mt_excel.

  ENDMETHOD.

  METHOD display_msg.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv_msg
          CHANGING
            t_table      = mt_messages ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    mo_salv_msg->get_functions( )->set_all( 'X' ).

    adjust_cols_msg( ).
    mo_salv_msg->display( ).
  ENDMETHOD.

  METHOD adjust_cols_msg.

    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv_msg->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        lo_column ?= lo_cols->get_column( 'TIME' ).
        lo_column->set_key( ).

        DATA(lv_text) = CONV string( 'Number of Line Item Within Accounting Document'(009) ).
        lo_column ?= lo_cols->get_column( 'AWKEY' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'BUZEI' ).
        lo_column->set_key( ).

        lv_text = 'Progressive number'(010).
        lo_column ?= lo_cols->get_column( 'PROG_NR' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = CONV scrtext_s( 'Status'(011) ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'(012).
        lo_column ?= lo_cols->get_column( 'MESSAGE' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lo_column ?= lo_cols->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.

  METHOD value_request.

    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_filename      = '*.XLSX'
        file_filter           = '*.XLSX'
        multiselection        = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).

    IF sy-subrc = 0.
      READ TABLE lt_filetable INDEX 1 INTO p_file.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_class_name=>value_request( ).

START-OF-SELECTION.
  CREATE OBJECT go_class_name.
  go_class_name->execute( ).
