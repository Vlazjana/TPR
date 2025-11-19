*&---------------------------------------------------------------------*
*& Report ZMM_DRCHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_drcheck.

*----------------------------------------------------------------------*
*       CLASS lcl_DRCHECK DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_drcheck DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS :execute,
      user_command_0100,
      status_0100.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_display_data,
             status       TYPE char1,
             bname        TYPE bapibname-bapibname,
             usrname      TYPE text80,
             zzdataimp    TYPE zmm_controlli-zzdataimp,
             werks        TYPE zmm_controlli-werks,
             sernr        TYPE zmm_controlli-sernr,
             zzodp        TYPE zmm_controlli-zzodp,
             zzmodmat     TYPE zmm_controlli-zzmodmat,
             zzcheckigi   TYPE zmm_controlli-zzcheckigi,
             zzesito      TYPE zmm_controlli-zzesito,
             zzcasaprod   TYPE zmm_controlli-zzcasaprod,
             zznpietre    TYPE zmm_controlli-zznpietre,
             zzdatacheck  TYPE zmm_controlli-zzdatacheck,
             zzartforn    TYPE zmm_controlli-zzartforn,
             pernr        TYPE zmm_controlli-pernr,
             sname        TYPE zmm_controlli-sname,
             zznddt       TYPE zmm_controlli-zznddt,
             zzdataddt    TYPE zmm_controlli-zzdataddt,
             zzaltricheck TYPE zmm_controlli-zzaltricheck,
             zznote       TYPE zmm_controlli-zznote,
           END OF ty_display_data,
           tt_display_data TYPE STANDARD TABLE OF ty_display_data.

    DATA: mt_display_data TYPE tt_display_data,
          mo_alv_grid     TYPE REF TO cl_gui_alv_grid.

    METHODS: extract_data,
      set_first_display.
ENDCLASS.

DATA: go_drcheck TYPE REF TO lcl_drcheck,

      BEGIN OF gs_screen100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen100.

*----------------------------------------------------------------------*
*       CLASS lcl_DRCHECK IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_drcheck IMPLEMENTATION.

  METHOD execute.
    extract_data( ).
  ENDMETHOD.

  METHOD extract_data.

    TYPES : BEGIN OF ty_users,
              bname    TYPE bapibname-bapibname,
              fullname TYPE text80,
            END OF ty_users.
    DATA: ls_address TYPE bapiaddr3,
          lt_return  TYPE STANDARD TABLE OF bapiret2,
          lt_users   TYPE SORTED TABLE OF ty_users WITH UNIQUE KEY bname.

    SELECT bname,
           zzdataimp,
           werks,
           sernr,
           zzodp,
           zzmodmat,
           zzcheckigi,
           zzesito,
           zzcasaprod,
           zznpietre,
           zzdatacheck,
           zzartforn,
           pernr,
           sname,
           zznddt,
           zzdataddt,
           zzaltricheck,
           zznote
      FROM zmm_controlli AS a
      WHERE a~zzesito = 'DR'
        AND a~zzcheckigi IS INITIAL

      AND NOT EXISTS ( SELECT *
                        FROM zmm_controlli AS b
                       WHERE b~werks       = a~werks
                         AND b~sernr       = a~sernr
                         AND b~zzodp       = a~zzodp
                         AND b~zzmodmat    = a~zzmodmat
                         AND b~zzdatacheck = a~zzdatacheck
                         AND b~zzcheckigi  IS NOT INITIAL )

       INTO CORRESPONDING FIELDS OF TABLE @mt_display_data.

    IF mt_display_data IS INITIAL.
      MESSAGE 'No data found'(002) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO SCREEN 0.
    ENDIF.

    LOOP AT mt_display_data ASSIGNING FIELD-SYMBOL(<ls_display_data>).

      CLEAR: ls_address, lt_return.

      <ls_display_data>-status = 3.

      READ TABLE lt_users ASSIGNING FIELD-SYMBOL(<ls_user>) WITH TABLE KEY bname = <ls_display_data>-bname.
      IF sy-subrc <> 0.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = <ls_display_data>-bname
          IMPORTING
            address  = ls_address
          TABLES
            return   = lt_return.

        INSERT VALUE #( bname    = <ls_display_data>-bname
                        fullname = ls_address-fullname ) INTO TABLE lt_users ASSIGNING <ls_user>.

      ENDIF.

      <ls_display_data>-usrname = <ls_user>-fullname.

    ENDLOOP.

    set_first_display( ).

  ENDMETHOD.

  METHOD set_first_display.

    DATA: lo_cust_grid TYPE REF TO cl_gui_custom_container,
          lt_gridfcat  TYPE lvc_t_fcat.

    lo_cust_grid = NEW cl_gui_custom_container( container_name = 'CONT' ).
    mo_alv_grid  = NEW cl_gui_alv_grid( i_parent = lo_cust_grid ).

    lt_gridfcat = VALUE #(
        ( fieldname = 'STATUS'         coltext   = 'Status'(003)   )
        ( fieldname = 'BNAME'          ref_table = 'USR02'         )
        ( fieldname = 'USRNAME'        coltext   = 'Importer’s full name'(004) )
        ( fieldname = 'ZZDATAIMP'      ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'WERKS'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'SERNR'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZODP'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZMODMAT'       ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZCHECKIGI'     ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZESITO'        ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZCASAPROD'     ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZNPIETRE'      ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZDATACHECK'    ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZARTFORN'      ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'PERNR'          ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'SNAME'          ref_table = 'PA0001'        )
        ( fieldname = 'ZZNDDT'         ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZDATADDT'      ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZALTRICHECK'   ref_table = 'ZMM_CONTROLLI' )
        ( fieldname = 'ZZNOTE'         ref_table = 'ZMM_CONTROLLI' )
        ).

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt  = abap_true
                                        excp_fname  = 'STATUS' ).

    mo_alv_grid->set_table_for_first_display(
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = mt_display_data
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

  METHOD user_command_0100.
    CASE gs_screen100-ok_code.
      WHEN 'FC_BACK'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.

  METHOD status_0100.
    SET PF-STATUS 'ZMM_DRCHECK_PS'.
    SET TITLEBAR 'ZMM_DRCHECK_TB'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT go_drcheck.
  go_drcheck->execute( ).

MODULE status_0100 OUTPUT.

  IF go_drcheck IS NOT BOUND.
    CREATE OBJECT go_drcheck.
    go_drcheck->execute( ).
  ENDIF.

  go_drcheck->status_0100( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  go_drcheck->user_command_0100( ).
ENDMODULE.
