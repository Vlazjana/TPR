Ben upload excel ne menyre dinamike 
REPORT zvq_upload.

TRY.
    DATA: lv_rc TYPE i.
    DATA: it_files TYPE filetable.
    DATA: lv_action TYPE i.

* FileOpen-Dialog aufrufen
    cl_gui_frontend_services=>file_open_dialog( EXPORTING file_filter = |xlsx (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_all }|
                                                CHANGING  file_table  = it_files
                                                          rc          = lv_rc
                                                          user_action = lv_action ).

    IF lv_action = cl_gui_frontend_services=>action_ok.
* wenn Datei ausgewählt wurde
      IF lines( it_files ) > 0.
* ersten Tabelleneintrag lesen
        DATA: lv_filesize TYPE w3param-cont_len.
        DATA: lv_filetype TYPE w3param-cont_type.
        DATA: it_bin_data TYPE w3mimetabtype.

* Excel-Datei auf Appl. Server hochladen (binary)
        cl_gui_frontend_services=>gui_upload( EXPORTING filename   = |{ it_files[ 1 ]-filename }|
                                                        filetype   = 'BIN'
                                              IMPORTING filelength = lv_filesize
                                              CHANGING  data_tab   = it_bin_data ).

* solix -> xstring
        DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = it_bin_data ).

**********************************************************************
* XLSX Handling: Kapselt cl_xlsx_document
**********************************************************************
        DATA(o_excel) = cl_ehfnd_xlsx=>get_instance( ).

* XLSX Workbook (XString --> XML)
        DATA(o_doc) = o_excel->load_doc( iv_file_data = lv_bin_data ).

* XLSX Sheets des Workbooks holen
        DATA(it_sheets) = o_doc->get_sheets( ).

        IF lines( it_sheets ) > 0.
* XLSX Sheet: erste Sheet holen
          DATA(o_sheet) = o_doc->get_sheet_by_id( iv_sheet_id = 1 ).
          DATA(lv_sheet_name) = it_sheets[ 1 ]-name.

* max. Zeilenzahl der Sheet
          DATA(lv_max_rows) = o_sheet->get_last_row_number( ).
          DATA(lv_max_cols) = o_sheet->get_last_column_number_in_row( 1 ).

          IF lv_max_rows > 0 AND lv_max_cols > 0.
* Komponenten (Spalten) der Tabelle --> generische Stringtable bauen
            DATA(it_components) = VALUE cl_abap_structdescr=>component_table( ).

* Überschriften (Header) für ALV-Grid
            DATA(it_colnames) = VALUE stringtab( ).

            DO lv_max_cols TIMES.
* Spaltenbezeichner aus 1. Zeile (Header) der Excel-Tabelle holen
              DATA(lv_col_header) = o_sheet->get_cell_content( iv_row    = 1
                                                               iv_column = sy-index ).

* alle Vorkommen, die nicht [a-zA-Z0-9_] entsprechen, durch '_' ersetzen
              REPLACE ALL OCCURRENCES OF REGEX '([^\w]|[äöüÄÖÜß])+' IN lv_col_header WITH '_'.

* Tabelle mit Überschriften für ALV-Grid füllen
              APPEND lv_col_header TO it_colnames.

* Spalte vom Typ String mit generischem Namen zur Komponententabelle hinzufügen
              APPEND VALUE #( name   = |COL{ sy-index }|
                              type   = cl_abap_elemdescr=>get_string( )
                            ) TO it_components.
            ENDDO.

**********************************************************************
* generische interne Tabelle mit hilfe dynamischer Objekte erzeugen
**********************************************************************
* Strukturdeskriptor für Komponententabelle erzeugen
            DATA(o_struct_desc) = cl_abap_structdescr=>create( it_components ).

* Tabellendeskriptor erzeugen
            DATA(o_table_desc) = cl_abap_tabledescr=>create( p_line_type = o_struct_desc ).

* dynamisches Tabellenobjekt anhand des Tabellendeskriptors erstellen
            DATA: o_table TYPE REF TO data.
            CREATE DATA o_table TYPE HANDLE o_table_desc.

* Feldsymbol auf das Tabellenobjekt vom Typ STANDARD TABLE anlegen
            FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
            ASSIGN o_table->* TO <table>.

* Inhalt (ohne Header) aus XLSX in interne Tabelle schreiben
            DATA(lv_row) = 2.

            DO lv_max_rows - 1 TIMES.
              DATA(lv_col) = 1.

* neue Ausgabezeile anfügen
              APPEND INITIAL LINE TO <table>.

* neue Ausgabezeile holen und Feldsymbol zuweisen
              DATA(lv_lc) = lines( <table> ).
              ASSIGN <table>[ lv_lc ] TO FIELD-SYMBOL(<row>).

              IF <row> IS ASSIGNED.
* für alle Spalten der Tabelle
                DO lv_max_cols TIMES.
* n-te Spalte <col> der akt. Tabellenzeile <row> ermitteln
                  ASSIGN COMPONENT lv_col OF STRUCTURE <row> TO FIELD-SYMBOL(<cell>).
                  IF <cell> IS ASSIGNED.
* Inhalt der akt. Zelle in die Zelle der internen Tabelle schreiben
                    <cell> = o_sheet->get_cell_content( iv_row    = lv_row
                                                        iv_column = lv_col ).


                  ENDIF.

                  lv_col = lv_col + 1.
                ENDDO.
              ENDIF.

              lv_row = lv_row + 1.
            ENDDO.

**********************************************************************
* Anzeige der gefüllten generischen Tabelle in einem SALV-Grid
**********************************************************************
            TRY.
*             SALV-Table
                DATA: o_salv TYPE REF TO cl_salv_table.

                cl_salv_table=>factory( IMPORTING r_salv_table = o_salv
                                        CHANGING  t_table      = <table> ).

*             Grundeinstellungen
                o_salv->get_functions( )->set_all( abap_true ).
                o_salv->get_columns( )->set_optimize( abap_true ).
                o_salv->get_display_settings( )->set_list_header( CONV #( lv_sheet_name ) ).
                o_salv->get_display_settings( )->set_striped_pattern( abap_true ).
                o_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*             Spaltenüberschriften: technischer Name und Beschreibungstexte, Short Text und Medium Text leer lassen für Autosize
                LOOP AT o_salv->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<c>).
                  DATA(lv_idx) = sy-tabix.

                  DATA(o_col) = <c>-r_column.
                  o_col->set_short_text( || ).
                  o_col->set_medium_text( || ).
                  o_col->set_long_text( CONV #( it_colnames[ lv_idx ] ) ).
                ENDLOOP.

                o_salv->display( ).
              CATCH cx_root INTO DATA(e_txt).
                WRITE: / e_txt->get_text( ).
            ENDTRY.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  CATCH cx_root INTO DATA(e_text).
    MESSAGE e_text->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.

