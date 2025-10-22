*&---------------------------------------------------------------------*
*& Report  ZBANK_PAYMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zbank_payment.

TABLES: zpaym_file, epsf.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_txt  RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X', "Read txt response
            r_disp RADIOBUTTON GROUP rb. " Alv display
SELECTION-SCREEN END OF BLOCK b1.

*ALV display
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_bukrs FOR zpaym_file-bukrs MODIF ID bl2,
                 s_budat FOR zpaym_file-budat MODIF ID bl2,
*                 s_xblnr FOR zpaym_file-xblnr MODIF ID bl2,
                 s_usnam FOR zpaym_file-usnam MODIF ID bl2.

PARAMETERS: p_stat  TYPE zesito MODIF ID bl2,
            p_inbiz TYPE zstatus_inbiz MODIF ID bl2.

SELECTION-SCREEN END OF BLOCK b2.

*RFC parameters
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.

PARAMETERS : p_fdir TYPE eps2filnam DEFAULT '\\DSTEST.aquafil.com\InBiz_LINKSFG_D02\Esiti' MODIF ID bl1.
SELECT-OPTIONS: s_mask FOR epsf-epsfilnam  MODIF ID bl1.

SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
*       CLASS lcl_bank_payment DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bank_payment DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS: execute,
      set_first_display.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_alv_display,
              bukrs       TYPE bukrs,
              budat       TYPE budat,
*              xblnr       TYPE xblnr1,
              usnam       TYPE usnam,
              zlsch       TYPE schzw_bseg,
              dtkey       TYPE dtkey_d,
              wrbtr       TYPE wrbtr,
              waers       TYPE waers,
              esito       TYPE zesito, " status
              stat_light  TYPE icon_d, " status light
              stato_inbiz TYPE zstatus_inbiz, " esito inbiz
            END OF ty_alv_display,
            tt_alv_display TYPE STANDARD TABLE OF ty_alv_display.

    DATA: mt_alv_display      TYPE tt_alv_display,
          mo_alv_grid         TYPE REF TO cl_gui_alv_grid,
          mo_cust_grid        TYPE REF TO cl_gui_custom_container,
          mt_gridfcat         TYPE lvc_t_fcat,
          mt_update_paym_file TYPE STANDARD TABLE OF zpaym_file.

    METHODS: extract_data,
      get_fcat_grid,
      read_txt_file.

ENDCLASS.                    "

DATA go_bank_payment TYPE REF TO lcl_bank_payment.

DATA: BEGIN OF gs_screen100,
        ok_code TYPE syucomm,
      END OF gs_screen100.
*----------------------------------------------------------------------*
*       CLASS lcl_bank_payment IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bank_payment IMPLEMENTATION.

  METHOD execute.

    CASE abap_true.

      WHEN r_txt.

        IF p_fdir IS INITIAL .
          MESSAGE 'Please fill obligatory fields!'(004) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
* read txt file from AL11
        read_txt_file( ).

      WHEN r_disp.
        extract_data( ).

    ENDCASE.

  ENDMETHOD.                    "execute

  METHOD extract_data.

    IF p_inbiz IS NOT INITIAL.
      DATA(lv_inbiz) = 'zpaym_file~stato_inbiz = @p_inbiz'.
    ENDIF.

    IF p_stat IS NOT INITIAL.
      DATA(lv_esito) = 'zpaym_msg~esito = @p_stat'.
    ENDIF.

    SELECT  zpaym_file~bukrs,
            zpaym_file~budat,
*            zpaym_file~xblnr,
            zpaym_file~usnam,
            zpaym_file~zlsch,
            zpaym_file~dtkey,
            zpaym_file~wrbtr,
            zpaym_file~waers,
            zpaym_msg~esito, " status
            zpaym_file~stato_inbiz "esito inbiz
       FROM zpaym_file
       JOIN zpaym_msg
       ON zpaym_file~stato_inbiz = zpaym_msg~stato_inbiz
       INTO CORRESPONDING FIELDS OF TABLE @mt_alv_display
       WHERE zpaym_file~bukrs IN @s_bukrs
       AND zpaym_file~budat IN @s_budat
*       AND zpaym_file~xblnr IN @s_xblnr
       AND zpaym_file~usnam IN @s_usnam
       AND (lv_esito)
       AND (lv_inbiz).

    IF mt_alv_display IS INITIAL.
      MESSAGE 'No data found!'(005) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CLEAR: lv_inbiz, lv_esito.

*    status light
    LOOP AT mt_alv_display ASSIGNING FIELD-SYMBOL(<ls_alv_display>).

      CASE <ls_alv_display>-esito.
        WHEN 'OK'.
          <ls_alv_display>-stat_light = icon_led_green.
        WHEN 'KO'.
          <ls_alv_display>-stat_light = icon_led_red.
      ENDCASE.
    ENDLOOP.

    set_first_display( ).

  ENDMETHOD.                    "extract_data

  METHOD read_txt_file.

    DATA : lv_dir            TYPE eps2filnam,
           lt_files          TYPE TABLE OF eps2fili,
           lv_line           TYPE string,
           lt_filetable      TYPE TABLE OF string,
           lv_file_processed TYPE abap_bool VALUE abap_false,
           lt_messages       TYPE esp1_message_tab_type.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = p_fdir
      TABLES
        dir_list               = lt_files
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_files>) WHERE name IN s_mask.

      DATA(lv_filename) = p_fdir && '\' && <ls_files>-name.

      " Clear filetable for each new file
      IF lv_file_processed = abap_true.
        CLEAR lt_filetable.
      ENDIF.

      OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc = 0.
        DO.
          READ DATASET lv_filename INTO lv_line.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          APPEND lv_line TO lt_filetable.
        ENDDO.

        CLOSE DATASET lv_filename.
      ELSE.

        APPEND VALUE #(
                      msgty     = 'E'
                      msgid     = 'DB'
                      msgno     = '000'
                      msgv1     = |Failed to open file { <ls_files>-name }|
              ) TO lt_messages.

      ENDIF.

      " Process only if we have data in filetable
      IF lt_filetable IS NOT INITIAL.

        IF lv_file_processed = abap_false.
          lv_file_processed = abap_true.
        ENDIF.

        READ TABLE lt_filetable ASSIGNING FIELD-SYMBOL(<ls_key_line>) INDEX 7.
        IF sy-subrc = 0.
          DATA(lv_key) = <ls_key_line>+7.
        ELSE.
          APPEND VALUE #(
                         msgty     = 'E'
                         msgid     = 'DB'
                         msgno     = '000'
                         msgv1     = |File { <ls_files>-name } missing key line|
                         ) TO lt_messages.
        ENDIF.

        READ TABLE lt_filetable ASSIGNING FIELD-SYMBOL(<ls_line>) INDEX 10.
        IF sy-subrc = 0.
          DATA(lv_status) = <ls_line>+13(1).
          DATA(lv_description) = <ls_line>+17.
        ENDIF.

        DATA(lv_formatted_key) = CONV belnr_d( lv_key ).

        SELECT SINGLE *
          FROM zpaym_file
          INTO @DATA(ls_paym_line)
          WHERE belnr = @lv_formatted_key.

        IF sy-subrc = 0.
          ls_paym_line-stato_inbiz = lv_status.
          ls_paym_line-msg_inbiz = lv_description.
          UPDATE zpaym_file FROM ls_paym_line.

          APPEND VALUE #(
                        msgty     = 'S'
                        msgid     = 'DB'
                        msgno     = '000'
                        msgv1     = |Updated record { lv_formatted_key } from file { <ls_files>-name }|
                        ) TO lt_messages.

        ELSE.

          APPEND VALUE #(
                        msgty     = 'E'
                        msgid     = 'DB'
                        msgno     = '000'
                        msgv1     = |Failed to update record { lv_formatted_key } from file { <ls_files>-name }|
                        ) TO lt_messages.

        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
      <ls_messages>-lineno =  sy-tabix.
    ENDLOOP.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_messages.

  ENDMETHOD.

  METHOD set_first_display.

    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CONT' ).
    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = mo_cust_grid ).

    get_fcat_grid( ).

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = abap_true ).

    TRY.
        mo_alv_grid->set_table_for_first_display( EXPORTING is_layout       = ls_layout
                                                  CHANGING  it_outtab       = mt_alv_display
                                                            it_fieldcatalog = mt_gridfcat ).
        CALL SCREEN 100.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root TYPE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD get_fcat_grid.

    mt_gridfcat = VALUE #(
              ( fieldname = 'BUKRS'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'BUDAT'       ref_table = 'ZPAYM_FILE' )
*              ( fieldname = 'XBLNR'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'USNAM'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'ZLSCH'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'DTKEY'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'WRBTR'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'WAERS'       ref_table = 'ZPAYM_FILE' )
*              ( fieldname = 'ESITO'       ref_table = 'ZPAYM_MSG'  coltext = 'Status' )
              ( fieldname = 'STAT_LIGHT'   coltext = 'Stato' )
              ( fieldname = 'STATO_INBIZ' ref_table = 'ZPAYM_FILE' coltext = 'Esito INBIZ' )
              ).
  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    CASE abap_true.

      WHEN r_txt.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 1.
          WHEN 'BL2'.
            screen-active = 0.
        ENDCASE.
      WHEN r_disp.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 0.
          WHEN 'BL2'.
            screen-active = 1.
        ENDCASE.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

START-OF-SELECTION.
  CREATE OBJECT go_bank_payment.
  go_bank_payment->execute( ).
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZPF_PAYMENT'.
  SET TITLEBAR 'ZTB_PAYMENT'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE gs_screen100-ok_code.
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
