REPORT zvq_test3alv.
INCLUDE zoop_top.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_year  TYPE gjahr OBLIGATORY DEFAULT 2022,
             p_month TYPE monam  AS LISTBOX VISIBLE LENGTH 12 OBLIGATORY MODIF ID oad DEFAULT '01',
             p_show  AS CHECKBOX,
             p_salv  RADIOBUTTON GROUP rb1 DEFAULT 'X',
             p_grid  RADIOBUTTON GROUP rb1,
             p_reuse RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_timesheet DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_employee,
             id      TYPE i,
             name    TYPE string,
             surname TYPE string,
           END OF ty_employee,
           tt_employee TYPE TABLE OF ty_employee.

    TYPES : BEGIN OF ty_timesheet,
              employeeid  TYPE i,
              project     TYPE string,
              overtime(1) TYPE c,
              date        TYPE d,
              hours(3)    TYPE p DECIMALS 2,
            END OF ty_timesheet,
            tt_timesheet TYPE TABLE OF ty_timesheet.

    TYPES : BEGIN OF ty_display,
              selected     TYPE char1,
              employeeid   TYPE i,
              fullname(50) TYPE c,
              project(50)  TYPE c,
              overtime(1)  TYPE c,
              hours(3)     TYPE p  DECIMALS 2,
              day(3)       TYPE p  DECIMALS 2,
              day_01       TYPE p  LENGTH 2 DECIMALS 2,
              day_02       TYPE p  LENGTH 2 DECIMALS 2,
              day_03       TYPE p  LENGTH 2 DECIMALS 2,
              day_04       TYPE p  LENGTH 2 DECIMALS 2,
              day_05       TYPE p  LENGTH 2 DECIMALS 2,
              day_06       TYPE p  LENGTH 2 DECIMALS 2,
              day_07       TYPE p  LENGTH 2 DECIMALS 2,
              day_08       TYPE p  LENGTH 2 DECIMALS 2,
              day_09       TYPE p  LENGTH 2 DECIMALS 2,
              day_10       TYPE p  LENGTH 2 DECIMALS 2,
              day_11       TYPE p  LENGTH 2 DECIMALS 2,
              day_12       TYPE p  LENGTH 2 DECIMALS 2,
              day_13       TYPE p  LENGTH 2 DECIMALS 2,
              day_14       TYPE p  LENGTH 2 DECIMALS 2,
              day_15       TYPE p  LENGTH 2 DECIMALS 2,
              day_16       TYPE p  LENGTH 2 DECIMALS 2,
              day_17       TYPE p  LENGTH 2 DECIMALS 2,
              day_18       TYPE p  LENGTH 2 DECIMALS 2,
              day_19       TYPE p  LENGTH 2 DECIMALS 2,
              day_20       TYPE p  LENGTH 2 DECIMALS 2,
              day_21       TYPE p  LENGTH 2 DECIMALS 2,
              day_22       TYPE p  LENGTH 2 DECIMALS 2,
              day_23       TYPE p  LENGTH 2 DECIMALS 2,
              day_24       TYPE p  LENGTH 2 DECIMALS 2,
              day_25       TYPE p  LENGTH 2 DECIMALS 2,
              day_26       TYPE p  LENGTH 2 DECIMALS 2,
              day_27       TYPE p  LENGTH 2 DECIMALS 2,
              day_28       TYPE p  LENGTH 2 DECIMALS 2,
              day_29       TYPE p  LENGTH 2 DECIMALS 2,
              day_30       TYPE p  LENGTH 2 DECIMALS 2,
              day_31       TYPE p  LENGTH 2 DECIMALS 2,
              cellcolor    TYPE slis_t_specialcol_alv,
              color        TYPE  lvc_t_scol,
            END OF ty_display,
            tt_display TYPE TABLE OF ty_display.


    DATA : mt_employee  TYPE tt_employee,
           gs_employee  TYPE tt_employee,
           gs_timesheet TYPE ty_timesheet,
           mt_timesheet TYPE tt_timesheet,
           gt_timesheet TYPE tt_timesheet,
           gv_monun     TYPE n LENGTH 2,
           lt_display   TYPE tt_display,
           ls_display   TYPE ty_display,
           ok_code      TYPE sy-ucomm,
           mv_show      TYPE abap_bool,
           lt_fcat      TYPE slis_t_fieldcat_alv.


    DATA: salv_event    TYPE REF TO cl_salv_events_table,
          lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          r_columns     TYPE REF TO cl_salv_columns,
          r_column      TYPE REF TO cl_salv_column,
          gr_columns    TYPE REF TO cl_salv_columns_table,
          gr_column     TYPE REF TO cl_salv_column_table,
          lo_functions  TYPE REF TO cl_salv_functions_list,   " Variables for ALV properties
          lo_salv       TYPE REF TO cl_salv_table,
*          lo_display    TYPE REF TO cl_salv_display_settings,
          lo_display    TYPE REF TO lcl_timesheet,
          it_rows       TYPE  salv_t_row. " Variable for layout settings


    DATA:  mt_gridfcat  TYPE lvc_t_fcat,
           r_cont       TYPE REF TO cl_gui_custom_container,
           lo_grid      TYPE REF TO cl_gui_alv_grid,
           ms_grid_fcat TYPE lvc_s_fcat.


    METHODS data_fetch.
    METHODS get_fcat_reuse.
    METHODS   set_data.
    METHODS display_reusealv .
    METHODS update_reusealv.
    METHODS execute.

    METHODS salv_table.
    METHODS update_salv_table.
    METHODS  on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function .
    METHODS color_reuse.


    METHODS user_command_reusealv IMPORTING iv_ucomm    TYPE sy-ucomm
                                  CHANGING  cs_selfield TYPE slis_selfield.
    METHODS display_grid.
    METHODS fcat_grid.
    METHODS get_layout_grid RETURNING VALUE(rs_layout_grid) TYPE lvc_s_layo .
    METHODS get_container.
    METHODS hide_show.
    METHODS user_command.


ENDCLASS.

CLASS lcl_timesheet IMPLEMENTATION.

  METHOD data_fetch.

    DATA : month     TYPE char10,
           year      TYPE i,
           ls_fcat   TYPE slis_fieldcat_alv,
           lv_record TYPE i.

    mt_employee = VALUE #(
      ( id = 1234 name = 'Filan'   surname = 'Fisteku' )
      ( id = 32   name = 'Filane' surname = 'Fisteke'  )
      ( id = 54   name = 'Mondi'  surname = 'Nafies'   )
      ( id = 632  name = 'Limi'   surname = 'Feruzes'  ) ).

    gt_timesheet = VALUE #(
      ( employeeid = 1234 project = 'Arts' date = '20220102' hours = '5.25' overtime = abap_true  )
      ( employeeid = 1234 project = 'Arts' date = '20220202' hours = 8 )
      ( employeeid = 1234 project = 'Arts' date = '20220115' hours = 8 )
      ( employeeid = 1234 project = 'Arts' date = '20220103' hours = 8 )
      ( employeeid = 1234 project = 'Arts' date = '20220101' hours = 5 )
      ( employeeid = 32   project = 'Arts' date = '20220101' hours = 5 )
      ( employeeid = 54   project = 'Arts' date = '20220101' hours = 5 )
      ( employeeid = 632  project = 'Arts' date = '20220101' hours = 5 ) ).

    month = p_month.
    year = p_year.

    DATA(lv_rec) = year && month.
    READ TABLE gt_timesheet INTO  gs_timesheet WITH KEY date(6) = lv_rec.
    IF sy-subrc NE 0.
      MESSAGE 'There are no record' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

  METHOD set_data.

    DATA:
      lv_field    TYPE slis_fieldcat_alv-fieldname,
*      lv_day(2)   TYPE n,
      lv_fulldate TYPE sy-datum.

    LOOP AT gt_timesheet INTO gs_timesheet
          GROUP BY ( employeeid = gs_timesheet-employeeid
                     project    = gs_timesheet-project
                   overtime   = gs_timesheet-overtime )
            ASSIGNING FIELD-SYMBOL(<lg_date>).

      READ TABLE mt_employee ASSIGNING FIELD-SYMBOL(<ls_employee>) WITH KEY id = <lg_date>-employeeid.


      ls_display-fullname = |{ <ls_employee>-name } { <ls_employee>-surname }|.
      ls_display-project = <lg_date>-project.
      ls_display-overtime = <lg_date>-overtime.


      LOOP AT GROUP <lg_date> ASSIGNING FIELD-SYMBOL(<ls_date>).

        DATA(lv_comp) = |DAY_{ <ls_date>-date+6(2) }|.

        ASSIGN COMPONENT lv_comp OF STRUCTURE ls_display TO FIELD-SYMBOL(<lv_day>).
        <lv_day> = <ls_date>-hours.


        IF <lg_date>-overtime IS INITIAL.
          READ TABLE lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WITH KEY fieldname = lv_comp.
          IF sy-subrc = 0.
            CASE <ls_fcat>-seltext_m(3).

              WHEN 'Sun' OR 'Sat'.
                CASE abap_true.
                  WHEN p_salv.
                    APPEND VALUE #( fname = lv_comp
                    color-col = 6 ) TO ls_display-color.
                    IF lv_comp = 'HOURS'.
                      APPEND VALUE #( fname = 'HOURS'
                      color-col = 1 ) TO ls_display-color.
                    ENDIF.
                  WHEN p_reuse.

                    APPEND VALUE #( fieldname = lv_comp
                    color-col = 6 ) TO ls_display-cellcolor.
                    IF lv_comp = 'HOURS'.
                      APPEND VALUE #( fieldname = 'HOURS'
                      color-col = 1 ) TO ls_display-cellcolor.
                    ENDIF.
                  WHEN p_grid.
                    APPEND VALUE #( fieldname = lv_comp
                   color-col = 6 ) TO ls_display-cellcolor.
                    IF lv_comp = 'HOURS'.
                      APPEND VALUE #( fieldname = 'HOURS'
                      color-col = 1 ) TO ls_display-cellcolor.
                    ENDIF.
                ENDCASE.
            ENDCASE.
          ENDIF.
        ENDIF.

        ADD <lv_day> TO ls_display-hours.

      ENDLOOP.

      ls_display-day = ls_display-hours / 8.

      APPEND ls_display TO lt_display.
      CLEAR: ls_display.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_fcat_reuse.

    DATA: ls_fcat TYPE slis_fieldcat_alv.

    lt_fcat = VALUE #(
        ( fieldname = 'SELECTED'  tabname = 'LT_DISPLAY'  no_out = 'X'  )
    ( fieldname = 'FULLNAME'   tabname = 'LT_DISPLAY'    seltext_m = 'Employee' key = abap_true )
    ( fieldname = 'PROJECT'   tabname = 'LT_DISPLAY'      seltext_m = 'Project' key = abap_true )
    ( fieldname = 'HOURS'   tabname = 'LT_DISPLAY'        seltext_m = 'Hours' key = abap_true )
    ( fieldname = 'DAY'   tabname = 'LT_DISPLAY'      seltext_m = 'Days' key = abap_true )
    ).

    DATA : par_month   TYPE  t009b-bumon,
           par_year    TYPE  t009b-bdatj,
           lv_totdays  TYPE t009b-butag,
           lv_text     TYPE dtresr-weekday,
           lv_name     TYPE slis_fieldcat_alv-seltext_m,
           lv_field    TYPE slis_fieldcat_alv-fieldname,
           lv_fulldate TYPE sy-datum.

    par_month = p_month.
    par_year = p_year.

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = par_month
        par_year  = par_year
      IMPORTING
        par_days  = lv_totdays.

    DATA : lv_day         TYPE i,
           lv_day_char(2) TYPE c.

    DO lv_totdays TIMES.
      lv_day = sy-index.

      UNPACK lv_day  TO lv_day_char.

      lv_field = 'DAY_' &&   lv_day_char.

      lv_fulldate = p_year && p_month &&  lv_day_char .

      CALL FUNCTION 'DATE_TO_DAY'
        EXPORTING
          date    = lv_fulldate
        IMPORTING
          weekday = lv_text.

      CASE abap_true.
        WHEN p_show.
          IF lv_text CS 'Sat' OR lv_text CS 'Sun'.
            ls_fcat-no_out = 'X'.
          ENDIF.
      ENDCASE.

      lv_name = lv_text(3) && '-' && lv_day_char.

      APPEND VALUE #( fieldname = lv_field
                      tabname =  'LT_DISPLAY'
                      seltext_m =  lv_name
                      no_out = ls_fcat-no_out
)  TO lt_fcat.

      CLEAR: lv_text, ls_fcat.
    ENDDO.
  ENDMETHOD.
  METHOD user_command_reusealv .

    CASE iv_ucomm.
      WHEN 'FC_SUM'.

        DATA : lv_totday  TYPE p LENGTH 3 DECIMALS 2,
               lv_tothour TYPE p LENGTH 3 DECIMALS 2.
        DATA lv_count TYPE i.
        LOOP AT lt_display ASSIGNING FIELD-SYMBOL(<fs_display>) WHERE selected = abap_true.
          lv_totday = lv_totday + <fs_display>-day.
          lv_tothour = lv_tothour + <fs_display>-hours.
          lv_count = 1  + lv_count.
        ENDLOOP.
        IF lv_count > 1.
          MESSAGE |Sum of days is : { lv_totday }' Sum of hours is : '{ lv_tothour }| TYPE 'I'.
        ELSE.
          MESSAGE 'Select at least 2 rows' TYPE 'I'.
        ENDIF.
      WHEN 'BACK'.
        LEAVE TO SCREEN 0 .
      WHEN 'FC_SHOW' .
        IF p_show = abap_false.
          p_show = abap_true.
          me->update_reusealv( ).
        ELSE.
          p_show = abap_false.
          me->update_reusealv( ).
        ENDIF.
        cs_selfield-refresh = abap_true.
    ENDCASE.
  ENDMETHOD.

  METHOD color_reuse.
*    DATA(ls_layout)  = VALUE slis_layout_alv(
*        colwidth_optimize = abap_true
*        box_fieldname = 'SELECTED'
*        box_tabname = 'LT_DISPLAY'
*        coltab_fieldname = 'CELLCOLOR'
*    ).

    MODIFY lt_fcat FROM VALUE #( no_out = p_show ) TRANSPORTING no_out WHERE seltext_m CS 'Sun' OR seltext_m CS 'Sat' .
*
    LOOP AT lt_display INTO ls_display.
      IF ls_display-overtime = abap_true.

        APPEND VALUE #( color = VALUE #( col = 6 ) ) TO ls_display-cellcolor.
        MODIFY lt_display FROM ls_display TRANSPORTING cellcolor.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD   display_reusealv.

    DATA(ls_layout)  = VALUE slis_layout_alv(
       colwidth_optimize = abap_true
       box_fieldname = 'SELECTED'
       box_tabname = 'LT_DISPLAY'
       coltab_fieldname = 'CELLCOLOR'
   ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND_REUSEALV'
        is_layout                = ls_layout
        it_fieldcat              = lt_fcat
      TABLES
        t_outtab                 = lt_display
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.

  METHOD update_reusealv.

    DATA ls_layout TYPE slis_layout_alv.


    CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
      IMPORTING
        et_fieldcat   = lt_fcat
        es_layout     = ls_layout
      EXCEPTIONS
        no_infos      = 1
        program_error = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      <ls_fcat>-col_pos = sy-tabix.
    ENDLOOP.
    MODIFY lt_fcat FROM VALUE #( no_out = p_show ) TRANSPORTING no_out WHERE seltext_m CS 'Sun' OR seltext_m CS 'Sat' .
    ls_layout-colwidth_optimize = abap_true.



    CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_SET'
      EXPORTING
        it_fieldcat = lt_fcat
        is_layout   = ls_layout.

  ENDMETHOD.

  METHOD salv_table.

    DATA seltext TYPE char10.

    TRY.
        CALL METHOD cl_salv_table=>factory
*    EXPORTING
*      list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*      r_container    = cl_gui_custom_container=>screen0
*      container_name =
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = lt_display.



      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.


    lo_salv->get_functions( )->set_all( abap_true ).
    lo_salv->get_columns( )->set_optimize( abap_true ).


    lo_selections = lo_salv->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).



    DATA(lo_cols) = lo_salv->get_columns( ).

    TRY.
        gr_column ?=  lo_cols->get_column( 'FULLNAME' ).

        gr_column->set_long_text( 'Employee' ).
        gr_column->set_medium_text( 'Employee' ).
        gr_column->set_short_text( 'Employee' ).
        gr_column->set_key(
     value = if_salv_c_bool_sap=>true
 ).

      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        gr_column ?=  lo_cols->get_column( 'HOURS' ).

        gr_column->set_long_text( 'Hours' ).
        gr_column->set_medium_text( 'Hours' ).
        gr_column->set_short_text( 'Hours' ).
        gr_column->set_key(
           value = if_salv_c_bool_sap=>true
       ).
      CATCH cx_salv_not_found.
    ENDTRY.
    TRY.
        gr_column ?=  lo_cols->get_column( 'DAY' ).

        gr_column->set_long_text( 'Day' ).
        gr_column->set_medium_text( 'Day' ).
        gr_column->set_short_text( 'Day' ).
        gr_column->set_key(
           value = if_salv_c_bool_sap=>true
       ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        gr_column ?=  lo_cols->get_column( 'PROJECT' ).

        gr_column->set_long_text( 'Project' ).
        gr_column->set_medium_text( 'Project' ).
        gr_column->set_short_text( 'Project' ).
        gr_column->set_key(
           value = if_salv_c_bool_sap=>true
       ).
      CATCH cx_salv_not_found.
    ENDTRY.

    DATA : par_month   TYPE  t009b-bumon,
           par_year    TYPE  t009b-bdatj,
           lv_totdays  TYPE t009b-butag,
           lv_text     TYPE dtresr-weekday,
           lv_name     TYPE slis_fieldcat_alv-seltext_m,
           lv_field    TYPE slis_fieldcat_alv-fieldname,
           lv_fulldate TYPE sy-datum.

    DATA lv_dita TYPE scrtext_l.

    DATA seltext_m TYPE char10.

    par_month = p_month.
    par_year = p_year.

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = par_month
        par_year  = par_year
      IMPORTING
        par_days  = lv_totdays.

    DATA : lv_day         TYPE i,
           lv_day_char(2) TYPE c.

    DO lv_totdays TIMES.
      lv_day = sy-index.

      UNPACK lv_day  TO lv_day_char.

      lv_field = 'DAY_' &&   lv_day_char.

      lv_fulldate = p_year && p_month &&  lv_day_char .

      CALL FUNCTION 'DATE_TO_DAY'
        EXPORTING
          date    = lv_fulldate
        IMPORTING
          weekday = lv_text.

      lv_name = lv_text(3) && '-' && lv_day_char.
      lv_dita = lv_name.

      TRY.
          gr_column ?=  lo_cols->get_column( lv_field ).
          gr_column->set_long_text( lv_dita ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          gr_column ?=  lo_cols->get_column( 'EMPLOYEEID' ).
          gr_column->set_visible( value  = if_salv_c_bool_sap=>false ).

        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          gr_column ?=  lo_cols->get_column( 'OVERTIME'  ) .

          gr_column->set_visible( value  = if_salv_c_bool_sap=>false ).

        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          gr_column ?=  lo_cols->get_column( 'SELECTED' ).
          gr_column->set_visible( value  = if_salv_c_bool_sap=>false ).

        CATCH cx_salv_not_found.
      ENDTRY.

    ENDDO.


    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WHERE seltext_m CS 'Sun' OR seltext_m CS 'Sat'.
      TRY.

          gr_column ?= lo_cols->get_column( <ls_fcat>-fieldname ).

          IF p_show = abap_true AND p_salv = abap_true.
            gr_column->set_visible( abap_false ).
          ELSE.
            gr_column->set_visible( ).
          ENDIF.


        CATCH cx_salv_not_found.
      ENDTRY.
    ENDLOOP.

*************
    lo_functions = lo_salv->get_functions( ).
    lo_functions->set_all( abap_true ).

    TRY.
        lo_salv->get_columns( )->set_color_column('COLOR').
      CATCH cx_salv_data_error.
    ENDTRY.

    LOOP AT lt_display ASSIGNING FIELD-SYMBOL(<ls_display>).
      IF <ls_display>-overtime = abap_true.
        APPEND VALUE #( color = VALUE #( col = 6  ) ) TO <ls_display>-color.
        CONTINUE.
      ENDIF.
    ENDLOOP.
****************

    SET PF-STATUS 'MENUU'.
    TRY.
        lo_salv->set_screen_status(
               EXPORTING
                 report = sy-repid
                 pfstatus = 'MENUU'

                 set_functions = lo_salv->c_functions_all ).

      CATCH cx_salv_object_not_found INTO DATA(lx_msg1).
        MESSAGE lx_msg1->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        lo_salv->get_columns( )->set_optimize( abap_true ).
    ENDTRY.

    DATA(lo_events) = lo_salv->get_event( ).
    SET HANDLER on_user_command FOR lo_events.
    IF p_show IS INITIAL.
    ELSE.
      CLEAR p_show.
      p_show = abap_true.
    ENDIF.

    lo_functions = lo_salv->get_functions( ).
    lo_functions->set_all( abap_true ).

    lo_salv->display( ).

  ENDMETHOD.

  METHOD on_user_command.

    DATA: lt_rows     TYPE salv_t_row,
          lt_fieldcat TYPE slis_t_fieldcat_alv.

    DATA: ls_rows TYPE i.

    DATA: message TYPE string.

    lo_selections = lo_salv->get_selections( ).
    DATA(lt_sel) = lo_selections->get_selected_rows( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    CASE e_salv_function.

      WHEN 'FC_SHOW'.

        IF p_show IS INITIAL.

          LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
            WHERE seltext_m CS 'Sat' OR seltext_m CS 'Sun'.

            TRY.
                gr_columns = lo_salv->get_columns( )  .
                gr_column ?= gr_columns->get_column( <ls_fcat>-fieldname ).
                gr_column->set_visible( value = abap_false ).

              CATCH cx_salv_not_found.

            ENDTRY.

          ENDLOOP.

          p_show = abap_true.

        ELSE.
          LOOP AT lt_fcat ASSIGNING <ls_fcat>
            WHERE seltext_m CS 'Sat' OR seltext_m CS 'Sun'.

            TRY.
                gr_columns = lo_salv->get_columns( )  .
                gr_column ?= gr_columns->get_column( <ls_fcat>-fieldname ).
                gr_column->set_visible( value = abap_true ).


              CATCH cx_salv_not_found.
            ENDTRY.
          ENDLOOP.

          p_show = abap_false.

        ENDIF.
        update_salv_table( ).
        lo_salv->get_columns( )->set_optimize( abap_true ).



      WHEN 'FC_SUM'.
        DATA : lv_totday  TYPE p LENGTH 3 DECIMALS 2,
               lv_tothour TYPE p LENGTH 3 DECIMALS 2,
               lv_count   TYPE i.

        it_rows = lo_selections->get_selected_rows( ).
        LOOP AT it_rows INTO DATA(ls_row).
          READ TABLE lt_display ASSIGNING FIELD-SYMBOL(<ls_display>) INDEX ls_row.
          lv_totday = lv_totday + <ls_display>-day.
          lv_tothour = lv_tothour + <ls_display>-hours.
          lv_count = 1  + lv_count.
        ENDLOOP.
        IF lv_count > 1.
          MESSAGE |Sum of days is : { lv_totday }' Sum of hours is : '{ lv_tothour }| TYPE 'I'.
        ELSE.
          MESSAGE 'Select at least 2 rows' TYPE 'I'.
        ENDIF.


      WHEN 'FC_BACK'.

        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.
  METHOD update_salv_table.
    DATA seltext TYPE char10.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)  .
      DATA(count) = sy-tabix.

      seltext = <ls_fcat>-seltext_m.
      TRY.
          r_column = lo_salv->get_columns( )->get_column( <ls_fcat>-fieldname ) .
        CATCH cx_salv_not_found.
      ENDTRY.

      lo_salv->get_columns( )->set_column_position(
        EXPORTING
          columnname =     <ls_fcat>-fieldname
          position   = count

      ).


    ENDLOOP.

    lo_salv->refresh(
      EXPORTING
        s_stable     = VALUE #( col = abap_true )    " ALV Control: Refresh Stability
*    refresh_mode =   abap_true  " ALV: Data Element for Constants
    ).
  ENDMETHOD.

***************************************************************GRID*************************************************
  METHOD get_container. "A SAP Container is a control that accommodates other controls

    r_cont = NEW cl_gui_custom_container( container_name = 'CUST_CONT' ).


    CREATE OBJECT lo_grid
      EXPORTING
        i_parent          = r_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.



  METHOD fcat_grid.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) .
      DATA(seltext) = <ls_fcat>-seltext_m.
      ms_grid_fcat-fieldname  = <ls_fcat>-fieldname.
      ms_grid_fcat-tabname    = 'LT_DISPLAY'.
      ms_grid_fcat-scrtext_m  = seltext.
      CASE <ls_fcat>-fieldname.

*          ms_grid_fcat-no_out = abap_true.
        WHEN  'OVERTIME' OR 'SELECTED'.
          ms_grid_fcat-key = abap_true.
          ms_grid_fcat-no_out = abap_true.
        WHEN 'EMPLOYEEID'.
          ms_grid_fcat-key = abap_true.
          ms_grid_fcat-no_out = abap_true.
        WHEN 'FULLNAME' OR 'PROJECT'.
          ms_grid_fcat-key = abap_true.
        WHEN 'HOURS' OR 'DAY' .
          ms_grid_fcat-key = abap_true.

      ENDCASE.

      APPEND ms_grid_fcat TO mt_gridfcat.
      CLEAR ms_grid_fcat.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_layout_grid.


    rs_layout_grid = VALUE #(
 cwidth_opt  = abap_true
 box_fname   = 'SELECTED'
  sel_mode    = 'D'
  ctab_fname =  'CELLCOLOR'    ).



    LOOP AT lt_display INTO ls_display.
      IF ls_display-overtime = abap_true.

        APPEND VALUE #( color = VALUE #( col = 6 ) ) TO ls_display-cellcolor.
        MODIFY lt_display FROM ls_display TRANSPORTING cellcolor.
      ENDIF.
    ENDLOOP.



  ENDMETHOD.

  METHOD display_grid.
    data_fetch( ).
    get_container( ).
    fcat_grid( ).
*    user_command( ).
*    hide_show( ).

    DATA(ls_layout_grid) = get_layout_grid( ).


  MODIFY mt_gridfcat FROM VALUE #( no_out = p_show ) TRANSPORTING no_out WHERE scrtext_m CS 'Sun' OR scrtext_m CS 'Sat' .

    lo_grid->set_table_for_first_display(
         EXPORTING
           is_layout                     = ls_layout_grid
         CHANGING
           it_outtab                     = lt_display
           it_fieldcatalog               = mt_gridfcat
         EXCEPTIONS
           invalid_parameter_combination = 1
           program_error                 = 2
           too_many_lines                = 3
           OTHERS                        = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CALL SCREEN 100.
    ENDIF.
  ENDMETHOD.

  METHOD hide_show.

    lo_grid->get_frontend_layout(
      IMPORTING
        es_layout = DATA(ls_layout)   " Layout
    ).

    IF p_show IS NOT INITIAL.
      CLEAR p_show.
    ELSE.
      p_show = abap_true.
    ENDIF.

    MODIFY mt_gridfcat FROM VALUE #( no_out = p_show ) TRANSPORTING no_out WHERE scrtext_m CS 'Sun' OR scrtext_m CS 'Sat'.

    ls_layout-cwidth_opt = abap_true.

    lo_grid->set_frontend_fieldcatalog( it_fieldcatalog = mt_gridfcat ).
    lo_grid->set_frontend_layout( is_layout = ls_layout ).

    lo_grid->refresh_table_display(
      EXPORTING
        is_stable      = VALUE #( col = abap_true )   " With Stable Rows/Columns
        i_soft_refresh = abap_true    " Without Sort, Filter, etc.
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.



  ENDMETHOD.

  METHOD user_command.
    DATA: lv_t_hours TYPE p LENGTH 3 DECIMALS 2,
          lv_t_days  TYPE p LENGTH 3 DECIMALS 2,
          lv_count   TYPE i.

    lo_grid->get_selected_rows(
     IMPORTING
       et_index_rows     =  DATA(lt_indx_rows)
   ).

    LOOP AT lt_indx_rows INTO DATA(lv_row).
      READ TABLE lt_display INTO DATA(ls_tab) INDEX lv_row.
      IF sy-subrc = 0.
        lv_t_hours = ls_tab-hours + lv_t_hours.
        lv_t_days = ls_tab-day + lv_t_days.
        lv_count = 1  + lv_count.
      ENDIF.
    ENDLOOP.

    IF lv_count > 1.
      MESSAGE |Sum of days is : { lv_t_days } 'Sum of hours is : '{ lv_t_hours }| TYPE 'I'.
    ELSE.
      MESSAGE 'Select at least 2 rows' TYPE 'I'.
    ENDIF.
  ENDMETHOD.




  METHOD execute.
    data_fetch( ).
    get_fcat_reuse( ).
    set_data( ).
    color_reuse( ).

    CASE abap_true.
      WHEN p_reuse.
        display_reusealv( ).

      WHEN p_grid.
        display_grid( ).

      WHEN p_salv.
        salv_table( ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month .

  DATA: lt_value TYPE vrm_values,
        ls_value TYPE vrm_value,
        name     TYPE vrm_id.
  name = 'P_MONTH'.
  ls_value-key = '01'.
  ls_value-text = 'Janar'.
  APPEND ls_value TO lt_value.
  ls_value-key = '02'.
  ls_value-text = 'Shkurt'.
  APPEND ls_value TO lt_value.
  ls_value-key = '03'.
  ls_value-text = 'Mars'.
  APPEND ls_value TO lt_value.
  ls_value-key = '04'.
  ls_value-text = 'Prill'.
  APPEND ls_value TO lt_value.
  ls_value-key = '05'.
  ls_value-text = 'Maj'.
  APPEND ls_value TO lt_value.
  ls_value-key = '06'.
  ls_value-text = 'Qershor'.
  APPEND ls_value TO lt_value.
  ls_value-key = '07'.
  ls_value-text = 'Korrik'.
  APPEND ls_value TO lt_value.
  ls_value-key = '08'.
  ls_value-text = 'Gusht'.
  APPEND ls_value TO lt_value.
  ls_value-key = '09'.
  ls_value-text = 'Shtator'.
  APPEND ls_value TO lt_value.
  ls_value-key = '10'.
  ls_value-text = 'Tetor'.
  APPEND ls_value TO lt_value.
  ls_value-key = '11'.
  ls_value-text = 'Nentor'.
  APPEND ls_value TO lt_value.
  ls_value-key = '12'.
  ls_value-text = 'Dhjetor'.
  APPEND ls_value TO lt_value.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name
      values          = lt_value
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

START-OF-SELECTION.

  DATA lo_object TYPE REF TO lcl_timesheet.

  CREATE OBJECT lo_object.
  lo_object->execute( ).
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_REUSEALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_reusealv USING r_ucomm LIKE sy-ucomm
                                 rs_selfield TYPE slis_selfield.

  lo_object->user_command_reusealv(
  EXPORTING
  iv_ucomm = r_ucomm
  CHANGING
  cs_selfield = rs_selfield ).
ENDFORM.
FORM pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'MENUU'.
ENDFORM.

INCLUDE zvq_test3alv_status_0100o01.

INCLUDE zvq_test3alv_status_100o01.

INCLUDE zvq_test3alv_user_command_0i01.

INCLUDE zvq_test3alv_init_alv_grido01.
