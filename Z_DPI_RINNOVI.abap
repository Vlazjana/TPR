*&---------------------------------------------------------------------*
*& Report  Z_DPI_RINNOVI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_dpi_rinnovi.

TABLES: zdpi_consegne.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-004.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_data FOR zdpi_consegne-kodat.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-002.
PARAMETERS: p_path TYPE btch0000-text80 OBLIGATORY DEFAULT '/home//interfacce/SSLL/output/ADR',
            p_comd TYPE sxpgcolist-name DEFAULT 'ZHR_OUT_SSLLADR'.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_del_dt FOR zdpi_consegne-kodat.
SELECTION-SCREEN END OF BLOCK b03.


CLASS lcl_dpi_rinnovi DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_output,
             id_consegna  TYPE zdpi_consegne-id_consegna,
             stcd1        TYPE zdpi_consegne-stcd1,
             pernr        TYPE zdpi_consegne-pernr,
             idnrk        TYPE zdpi_consegne-idnrk,
             kodat        TYPE zdpi_consegne-kodat,
             data_rinnovo TYPE d,
             tipo_dato    TYPE char30,
             vfdat        TYPE zdpi_consegne-vfdat,
             frequenza    TYPE zmm_freq_dpi-frequenza,
             office_d     TYPE i,
             remote_d     TYPE i,
             not_used_d   TYPE i,
           END OF ty_output.

    CONSTANTS: cv_separator TYPE char1  VALUE '|',
               cv_file_name TYPE char20 VALUE 'Rinnovi_DPI',
               cv_file_ext  TYPE char4  VALUE '.csv'.

    DATA: mt_output TYPE STANDARD TABLE OF ty_output,
          mo_salv   TYPE REF TO cl_salv_table,
          mv_csv_no TYPE i.

    METHODS execute.

  PRIVATE SECTION.
    METHODS get_csv_data.
    METHODS get_deleted_data.
    METHODS display_data.
    METHODS save_data_file.
    METHODS delete_data.
    METHODS send_canopo_file IMPORTING iv_del_dataset TYPE string
                                       iv_file_nm     TYPE string.

    METHODS get_absence_codes_subty RETURNING VALUE(rt_subty) TYPE rsdsselopt_t.
ENDCLASS.

CLASS lcl_dpi_rinnovi IMPLEMENTATION.
  METHOD execute.
    get_csv_data( ).
    get_deleted_data( ).

    IF p_test IS INITIAL.
      save_data_file( ).
      delete_data( ).
    ENDIF.

    display_data( ).
  ENDMETHOD.

  METHOD send_canopo_file.

    DATA : lv_status   TYPE extcmdexex-status,
           lv_exitcode TYPE extcmdexex-exitcode,
           lv_param    TYPE sxpgcolist-parameters.

    lv_param = iv_file_nm.

    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = p_comd
        additional_parameters         = lv_param
      IMPORTING
        status                        = lv_status
        exitcode                      = lv_exitcode
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    DELETE DATASET iv_del_dataset.
  ENDMETHOD.

  METHOD get_deleted_data.
    DATA: lv_icon_del  TYPE char30.

    IF s_del_dt[] IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = 'ICON_DELETE'
        info                  = 'Dato Cancellato'
      IMPORTING
        result                = lv_icon_del
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT id_consegna,
           pernr,
           idnrk,
           kodat,
           @lv_icon_del AS tipo_dato
    FROM zdpi_consegne
      APPENDING CORRESPONDING FIELDS OF TABLE @mt_output
      WHERE erdat IN @s_del_dt.

  ENDMETHOD.
  METHOD get_csv_data.

    DATA: lv_icon_csv       TYPE char30,
          lv_frequenza      TYPE i,
          lv_idnrk          TYPE idnrk,
          lv_attin_dpi_scad TYPE ausp-atinn,
          lt_scad_obj       TYPE STANDARD TABLE OF ausp-objek.

    CONSTANTS : cv_satza TYPE teven-satza VALUE 'P10'.
    CONSTANTS : cv_stokz TYPE teven-stokz VALUE 'X'.

    SELECT stcd1,
           idnrk,
           MAX( kodat ) AS kodat
          FROM zdpi_consegne
         WHERE kodat IN @s_data
      GROUP BY stcd1,
               idnrk
          INTO TABLE @DATA(lt_consegne).
    IF sy-subrc <> 0.
      MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT id_consegna,
           stcd1,
           pernr,
           idnrk,
           kodat,
           vfdat
      FROM zdpi_consegne
      FOR ALL ENTRIES IN @lt_consegne
      WHERE idnrk = @lt_consegne-idnrk
        AND stcd1 = @lt_consegne-stcd1
        AND kodat = @lt_consegne-kodat
      INTO CORRESPONDING FIELDS OF TABLE @mt_output.
    IF sy-subrc <> 0.
      MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT codice_dpi,
           stcd1,
           frequenza
      FROM zesigenze_dpi
      INTO TABLE @DATA(lt_freq)
      FOR ALL ENTRIES IN @mt_output
      WHERE codice_dpi = @mt_output-idnrk
        AND stcd1      = @mt_output-stcd1.

    SORT lt_freq BY codice_dpi stcd1.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = 'ICON_EXPORT'
        info                  = 'Dato CSV'
      IMPORTING
        result                = lv_icon_csv
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    lt_scad_obj = VALUE #( FOR <ls_con> IN lt_consegne
                           ( <ls_con>-idnrk ) ).
    SORT lt_scad_obj.
    DELETE ADJACENT DUPLICATES FROM lt_scad_obj.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'Z_DPI_SCADENZA'
      IMPORTING
        output = lv_attin_dpi_scad.

    SELECT objek,
           atwrt
      FROM ausp
      INTO TABLE @DATA(lt_ausp_dpi_scad)
      FOR ALL ENTRIES IN @lt_scad_obj
      WHERE objek = @lt_scad_obj-table_line
        AND atinn = @lv_attin_dpi_scad.

    SORT lt_ausp_dpi_scad BY objek.
    CLEAR: lt_scad_obj,
           lt_consegne.

    DATA(lr_subty) = get_absence_codes_subty( ).

    LOOP AT mt_output ASSIGNING FIELD-SYMBOL(<fs_out>).

      <fs_out>-tipo_dato    = lv_icon_csv.

      READ TABLE lt_freq ASSIGNING FIELD-SYMBOL(<fs_freq>)
        WITH KEY codice_dpi = <fs_out>-idnrk
                 stcd1      = <fs_out>-stcd1 BINARY SEARCH.
      IF sy-subrc = 0.
        lv_frequenza = <fs_freq>-frequenza.
      ENDIF.

      IF lv_frequenza IS INITIAL.

        SELECT SINGLE frequenza
          FROM zmm_freq_dpi
          INTO @lv_frequenza
         WHERE codice_dpi = @<fs_out>-idnrk.

      ENDIF.

      READ TABLE lt_ausp_dpi_scad INTO DATA(ls_ausp_dpi_scad)
        WITH KEY objek = <fs_out>-idnrk BINARY SEARCH.
      IF ls_ausp_dpi_scad-atwrt IS INITIAL.
        ls_ausp_dpi_scad-atwrt = 'C'.
      ENDIF.

      IF ls_ausp_dpi_scad-atwrt = 'C'.
        SELECT COUNT( * )
          FROM teven
        INTO @DATA(lv_office_days)
          WHERE pernr  = @<fs_out>-pernr
            AND satza  = @cv_satza
            AND ldate >= @<fs_out>-kodat
            AND stokz <> @cv_stokz.

        SELECT SUM( abrtg )
          FROM pa2002
          INTO @DATA(lv_remote_days)
         WHERE pernr  = @<fs_out>-pernr
           AND begda >= @<fs_out>-kodat
           AND subty NOT IN @lr_subty.

        lv_remote_days = ceil( lv_remote_days ).

        DATA(lv_days_diff) = sy-datum - <fs_out>-kodat + 1.
        DATA(lv_days_not_used) = lv_days_diff - ( lv_office_days + lv_remote_days ).
      ENDIF.

      <fs_out>-data_rinnovo = <fs_out>-kodat + lv_frequenza + lv_days_not_used.

      IF <fs_out>-vfdat IS NOT INITIAL AND <fs_out>-vfdat < <fs_out>-data_rinnovo.
        <fs_out>-data_rinnovo = <fs_out>-vfdat.
      ENDIF.

      <fs_out>-frequenza  = lv_frequenza.
      <fs_out>-office_d   = lv_office_days.
      <fs_out>-remote_d   = lv_remote_days.
      <fs_out>-not_used_d = lv_days_not_used.

      CLEAR: lv_remote_days,
             lv_office_days,
             lv_days_not_used,
             lv_days_diff,
             lv_frequenza,
             ls_ausp_dpi_scad.
    ENDLOOP.

    mv_csv_no = lines( mt_output ).
  ENDMETHOD.
  METHOD display_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = mt_output ).
      CATCH cx_salv_msg.

    ENDTRY.

    DATA(lo_cols) = mo_salv->get_columns( ).
    lo_cols->set_optimize( ).

    TRY.
        DATA(lo_col) =  CAST cl_salv_column_table( lo_cols->get_column( 'TIPO_DATO' ) ).
        lo_col->set_icon( ).
        lo_col->set_short_text( 'Tipo' ).
        lo_col->set_medium_text( 'Tipo' ).
        lo_col->set_long_text( 'Tipo' ).

        lo_cols->set_column_position(
          EXPORTING
            columnname = 'TIPO_DATO'
            position   = 1
        ).

        lo_col =  CAST cl_salv_column_table( lo_cols->get_column( 'DATA_RINNOVO' ) ).
        lo_col->set_icon( ).
        lo_col->set_short_text( 'D. Rinnovo' ).
        lo_col->set_medium_text( 'Data Rinnovo' ).
        lo_col->set_long_text( 'Data Rinnovo' ).

        lo_col =  CAST cl_salv_column_table( lo_cols->get_column( 'VFDAT' ) ).
        lo_col->set_visible(
            value = if_salv_c_bool_sap=>false
        ).

        lo_col =  CAST cl_salv_column_table( lo_cols->get_column( 'FREQUENZA' ) ).
        lo_col->set_short_text( 'Frequenza' ).
        lo_col->set_medium_text( 'Frequenza' ).
        lo_col->set_long_text( 'Frequenza' ).
        lo_col->set_visible( abap_false ).

        lo_col =  CAST cl_salv_column_table( lo_cols->get_column( 'OFFICE_D' ) ).
        lo_col->set_short_text( 'Office' ).
        lo_col->set_medium_text( 'Office' ).
        lo_col->set_long_text( 'Office' ).
        lo_col->set_visible( abap_false ).

        lo_col =  CAST cl_salv_column_table( lo_cols->get_column( 'REMOTE_D' ) ).
        lo_col->set_short_text( 'Remote' ).
        lo_col->set_medium_text( 'Remote' ).
        lo_col->set_long_text( 'Remote' ).
        lo_col->set_visible( abap_false ).

        lo_col =  CAST cl_salv_column_table( lo_cols->get_column( 'NOT_USED_D' ) ).
        lo_col->set_short_text( 'Not used' ).
        lo_col->set_medium_text( 'Not used' ).
        lo_col->set_long_text( 'Not used' ).
        lo_col->set_visible( abap_false ).


      CATCH cx_salv_not_found.
    ENDTRY.

    mo_salv->get_selections( )->set_selection_mode(
        value = if_salv_c_selection_mode=>cell
    ).

    DATA(lv_header) = EXACT lvc_title( |DPI Rinnovi : Dati CSV { mv_csv_no } / Dati cancellati { lines( mt_output ) - mv_csv_no } | ).
    mo_salv->get_display_settings( )->set_list_header( value = lv_header ).

    mo_salv->get_functions( )->set_all( ).
    mo_salv->display( ).

  ENDMETHOD.

  METHOD save_data_file.

    DATA lv_transfer TYPE string.

    DATA(lv_date)  = sy-datum.
    DATA(lv_time)  = sy-uzeit.
    DATA(lv_day)   = sy-datum+6(2).
    DATA(lv_month) = sy-datum+4(2).
    DATA(lv_year)  = sy-datum(4).

    DATA(lv_file_nm) =  cv_file_name && lv_day && lv_month && lv_year && cv_file_ext.
    DATA(lv_file) = p_path && |/| && lv_file_nm.

    OPEN DATASET lv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE 'File outbound non si puo aprire' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CONCATENATE  'DPI Id Consegna' 'Componente' 'Data Rinnovo'
    INTO lv_transfer SEPARATED BY cv_separator.
    lv_transfer = lv_transfer && cl_abap_char_utilities=>cr_lf.

    LOOP AT mt_output ASSIGNING FIELD-SYMBOL(<ls_out>).

      lv_transfer = lv_transfer && <ls_out>-id_consegna && cv_separator
                    && <ls_out>-idnrk && cv_separator
                    && <ls_out>-data_rinnovo && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.

    TRANSFER lv_transfer TO lv_file.

    CLOSE DATASET lv_file.

    MESSAGE 'Dati salvati in directory' TYPE 'S'.

    send_canopo_file( EXPORTING iv_del_dataset = lv_file
                                iv_file_nm     = lv_file_nm ).
  ENDMETHOD.
  METHOD delete_data.
    IF s_del_dt[] IS INITIAL.
      RETURN.
    ENDIF.

    DELETE FROM zdpi_consegne WHERE erdat IN @s_del_dt.
  ENDMETHOD.

  METHOD get_absence_codes_subty.

    DATA: lv_setid      TYPE sethier-setid,
          lt_set_values TYPE STANDARD TABLE OF rgsb4.

    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname                = 'ZSUBTY'
      IMPORTING
        new_setid                = lv_setid
      EXCEPTIONS
        no_set_found             = 1
        no_set_picked_from_popup = 2
        wrong_class              = 3
        wrong_subclass           = 4
        table_field_not_found    = 5
        fields_dont_match        = 6
        set_is_empty             = 7
        formula_in_set           = 8
        set_is_dynamic           = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = lv_setid
      TABLES
        set_values    = lt_set_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_set_values ASSIGNING FIELD-SYMBOL(<ls_values>).

      IF <ls_values>-from EQ <ls_values>-to.

        APPEND VALUE #( sign    = 'I'
                        option  = 'EQ'
                        low     = <ls_values>-from ) TO rt_subty.
      ELSE.

        APPEND VALUE #( sign    = 'I'
                        option  = 'BT'
                        low     = <ls_values>-from
                        high    = <ls_values>-to ) TO rt_subty.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_dpi_rinnovi( )->execute( ).
