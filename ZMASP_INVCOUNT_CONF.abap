*&---------------------------------------------------------------------*
*& Report ZMASP_INVCOUNT_CONF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmasp_invcount_conf.

TABLES: zmasp_invcnt_in, iseg.

DATA:BEGIN OF gs_screen100,
       ok_code TYPE sy-ucomm,
 	END OF gs_screen100.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_iblnr FOR zmasp_invcnt_in-iblnr,
                s_zeili FOR zmasp_invcnt_in-zeili,
                s_bldat FOR zmasp_invcnt_in-bldat,
                s_werks FOR iseg-werks NO INTERVALS NO-EXTENSION,
                s_lgort FOR zmasp_invcnt_in-lgort NO INTERVALS NO-EXTENSION,
                s_matnr FOR zmasp_invcnt_in-matnr,
                s_cntdt FOR zmasp_invcnt_in-wsti_countdate,
                s_zconf FOR zmasp_invcnt_in-zconfirm.
SELECTION-SCREEN END OF BLOCK b01.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
	METHODS :execute,
      user_command_0100,
      status_0100.
  PRIVATE SECTION.

	TYPES: BEGIN OF ty_counting,
             iblnr      	TYPE zmasp_invcnt_in-iblnr,
             zeili      	TYPE zmasp_invcnt_in-zeili,
             bldat      	TYPE zmasp_invcnt_in-bldat,
             werks      	TYPE iseg-werks,
             lgort      	TYPE iseg-lgort,
             matnr      	TYPE zmasp_invcnt_in-matnr,
             menge      	TYPE zmasp_invcnt_in-menge,
             wsti_countdate TYPE zmasp_invcnt_in-wsti_countdate,
             zmenge2    	TYPE zmasp_invcnt_in-zmenge2,
             meins_menge	TYPE iseg-meins,
             labst      	TYPE mard-labst,
             meins_labst	TYPE iseg-meins,
             delta_menge	TYPE zmasp_invcnt_in-menge,
             zconfirm   	TYPE zmasp_invcnt_in-zconfirm,
             style      	TYPE lvc_t_styl,
             int_locked 	TYPE abap_bool,
             foreign_lock   TYPE char100,
             modified   	TYPE abap_bool,
       	END OF ty_counting.

	DATA : mt_counting 	TYPE STANDARD TABLE OF ty_counting,
           mo_cust_grid	TYPE REF TO cl_gui_custom_container,
           mo_alv_grid 	TYPE REF TO cl_gui_alv_grid,
           mv_foreign_lock TYPE abap_bool.

	METHODS : get_data,
      set_first_display,
      save,
      confirm,
      reconfirm,
      handle_data_changed_fn FOR EVENT data_changed_finished OF cl_gui_alv_grid
    	IMPORTING et_good_cells,
      confirm_leaving.
ENDCLASS.
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.

    get_data( ).
    set_first_display( ).

  ENDMETHOD.

  METHOD set_first_display.

	DATA(lt_fcat) = VALUE lvc_t_fcat(
	( fieldname = 'IBLNR'       	ref_table = 'ZMASP_INVCNT_IN' )
	( fieldname = 'ZEILI'       	ref_table = 'ZMASP_INVCNT_IN' )
	( fieldname = 'BLDAT'       	ref_table = 'ZMASP_INVCNT_IN' )
	( fieldname = 'WERKS'       	ref_table = 'ISEG' )
	( fieldname = 'LGORT'       	ref_table = 'ISEG' )
	( fieldname = 'MATNR'       	ref_table = 'ZMASP_INVCNT_IN' )
	( fieldname = 'MENGE'       	ref_table = 'ZMASP_INVCNT_IN'  qfieldname = 'MEINS_MENGE' scrtext_s = 'Count 1' reptext = 'Count 1' scrtext_m = 'Count 1' scrtext_l = 'Count 1' )
	( fieldname = 'WSTI_COUNTDATE'  ref_table = 'ZMASP_INVCNT_IN' )
	( fieldname = 'ZMENGE2'     	ref_table = 'ZMASP_INVCNT_IN'  edit = abap_true qfieldname = 'MEINS_MENGE' reptext = 'Count 2' scrtext_s = 'Count 2' scrtext_m = 'Count 2' scrtext_l = 'Count 2' )
	( fieldname = 'MEINS_MENGE' 	ref_table = 'ISEG'  ref_field  = 'MEINS' )
	( fieldname = 'LABST'       	qfieldname = 'MEINS_LABST' scrtext_s = 'Book Quantity' scrtext_m = 'Book Quantity' scrtext_l = 'Book Quantity' )
	( fieldname = 'MEINS_LABST' 	ref_table = 'ISEG'  ref_field  = 'MEINS' )
	( fieldname = 'DELTA_MENGE' 	ref_table = 'ZMASP_INVCNT_IN' ref_field = 'MENGE' qfieldname = 'MEINS_MENGE' reptext = 'Delta Quantity' scrtext_s = 'Delta quantity' scrtext_m = 'Delta quantity' scrtext_l = 'Delta quantity')
	( fieldname = 'ZCONFIRM'    	ref_table = 'ZMASP_INVCNT_IN' checkbox  = abap_true )
	( fieldname = 'FOREIGN_LOCK' ref_table = 'BAPIRET2'  ref_field = 'MESSAGE' tech = xsdbool( mv_foreign_lock = abap_false  ) )
	).

	DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                        stylefname = 'STYLE' ).

	DATA(lt_toolbar_exc) =  VALUE ui_functions(
                  	( cl_gui_alv_grid=>mc_fc_loc_cut        	)
                  	( cl_gui_alv_grid=>mc_fc_loc_append_row 	)
                  	( cl_gui_alv_grid=>mc_fc_loc_insert_row 	)
                  	( cl_gui_alv_grid=>mc_fc_loc_delete_row 	)
                  	( cl_gui_alv_grid=>mc_fc_loc_copy       	)
                  	( cl_gui_alv_grid=>mc_fc_loc_copy_row   	)
                  	( cl_gui_alv_grid=>mc_fc_loc_paste      	)
                  	( cl_gui_alv_grid=>mc_fc_loc_paste_new_row  )
                  	( cl_gui_alv_grid=>mc_fc_info           	)
                  	( cl_gui_alv_grid=>mc_fc_check          	)
                  	( cl_gui_alv_grid=>mc_fc_refresh        	)
                  	( cl_gui_alv_grid=>mc_fc_graph          	)
                  	( cl_gui_alv_grid=>mc_fc_print          	)
                  	( cl_gui_alv_grid=>mc_fc_view_excel     	)
                  	( cl_gui_alv_grid=>mc_fc_view_grid      	)
                  	( cl_gui_alv_grid=>mc_fc_loc_undo       	)
                  	( cl_gui_alv_grid=>mc_fc_detail         	)
                  	).

	DATA(ls_variant) = VALUE disvariant( report = sy-repid
                                         username = sy-uname ).

    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CONT').
    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = mo_cust_grid ).

	SET HANDLER handle_data_changed_fn FOR mo_alv_grid.

    mo_alv_grid->register_edit_event(
  	EXPORTING
        i_event_id =   mo_alv_grid->mc_evt_modified           	" Event ID
  	EXCEPTIONS
        error  	= 1            	" Error
    	OTHERS 	= 2
	).
	IF sy-subrc <> 0.
  	MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    	WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.

    mo_alv_grid->set_table_for_first_display(
  	EXPORTING
        is_layout                 	= ls_layout
        it_toolbar_excluding      	= lt_toolbar_exc
        is_variant                	= ls_variant
        i_save                    	= abap_true
  	CHANGING
        it_outtab                 	= mt_counting
        it_fieldcatalog           	= lt_fcat
  	EXCEPTIONS
        invalid_parameter_combination = 1            	" Wrong Parameter
        program_error             	= 2            	" Program Errors
        too_many_lines            	= 3            	" Too many Rows in Ready for Input Grid
    	OTHERS                    	= 4
	).


	IF sy-subrc <> 0.
  	MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    	WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.
	CALL SCREEN 100.

  ENDMETHOD.

  METHOD get_data.

	TYPES: BEGIN OF ty_inv_count,
             iblnr TYPE iseg-iblnr,
             zeili TYPE iseg-zeili,
             matnr TYPE iseg-matnr,
             lgort TYPE iseg-lgort,
       	END OF ty_inv_count.

	DATA : lt_inv_count TYPE STANDARD TABLE OF ty_inv_count,
           lv_message   TYPE char100.

	SELECT iblnr,
           zeili,
           bldat,
           matnr,
           lgort,
           menge,
           wsti_countdate,
           zmenge2,
           zconfirm
  	FROM zmasp_invcnt_in
  	INTO TABLE @DATA(lt_inventory)
  	WHERE iblnr      	IN @s_iblnr
    	AND zeili      	IN @s_zeili
    	AND bldat      	IN @s_bldat
    	AND lgort      	IN @s_lgort
    	AND matnr      	IN @s_matnr
    	AND wsti_countdate IN @s_cntdt
    	AND zconfirm   	IN @s_zconf.

	IF sy-subrc <> 0.
  	RETURN.
	ENDIF.

	LOOP AT lt_inventory ASSIGNING FIELD-SYMBOL(<ls_invt>).
<ls_invt>-zeili = CONV numc3( <ls_invt>-zeili ).
  	APPEND CORRESPONDING #( <ls_invt> ) TO lt_inv_count.
	ENDLOOP.

	SELECT
           iseg~iblnr,
           iseg~zeili,
           iseg~lgort,
           iseg~matnr,

           iseg~meins,
           iseg~gjahr,
           iseg~werks,
           mard~labst
  	FROM iseg
  	JOIN mard
    	ON iseg~werks = mard~werks
   	AND iseg~lgort = mard~lgort
   	AND iseg~matnr = mard~matnr
  	INTO TABLE @DATA(lt_physical_inventory)
  	FOR ALL ENTRIES IN @lt_inv_count
  	WHERE iseg~iblnr = @lt_inv_count-iblnr
    	AND iseg~zeili = @lt_inv_count-zeili
    	AND iseg~lgort = @lt_inv_count-lgort
    	AND iseg~matnr = @lt_inv_count-matnr
    	AND iseg~werks IN @s_werks .
	IF sy-subrc <> 0.
  	RETURN.
	ENDIF.

	SORT lt_inventory BY iblnr
                         zeili
                         lgort
                         matnr.

	LOOP AT lt_physical_inventory ASSIGNING FIELD-SYMBOL(<ls_pinv>).

  	READ TABLE lt_inventory ASSIGNING <ls_invt>
  	WITH KEY iblnr = <ls_pinv>-iblnr
               zeili = <ls_pinv>-zeili
               lgort = <ls_pinv>-lgort
               matnr = <ls_pinv>-matnr BINARY SEARCH.
  	IF sy-subrc <> 0.
    	CONTINUE.
  	ENDIF.

  	APPEND INITIAL LINE TO mt_counting ASSIGNING FIELD-SYMBOL(<ls_counting>).

  	CALL FUNCTION 'ENQUEUE_EZMASP_INVCNT_IN'
    	EXPORTING
          mode_zmasp_invcnt_in = 'E'
          mandt            	= sy-mandt
          iblnr            	= <ls_invt>-iblnr
          zeili            	= <ls_invt>-zeili
    	EXCEPTIONS
          foreign_lock     	= 1
          system_failure   	= 2
      	OTHERS           	= 3.

  	IF sy-subrc = 0.
<ls_counting>-int_locked = abap_true.
  	ELSE.

    	MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      	WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.

        mv_foreign_lock = abap_true.

    	CALL FUNCTION 'ICON_CREATE'
      	EXPORTING
            name              	= 'ICON_LOCKED'
            info              	= lv_message
      	IMPORTING
            result            	= <ls_counting>-foreign_lock
      	EXCEPTIONS
            icon_not_found    	= 1
            outputfield_too_short = 2
        	OTHERS            	= 3.
    	IF sy-subrc <> 0.
* Implement suitable error handling here
    	ENDIF.

  	ENDIF.

<ls_counting> = VALUE #( BASE <ls_counting>
            iblnr       	= <ls_invt>-iblnr
            zeili       	= <ls_invt>-zeili
            bldat       	= <ls_invt>-bldat
            werks       	= <ls_pinv>-werks
            lgort       	= <ls_pinv>-lgort
            matnr       	= <ls_invt>-matnr
            menge       	= <ls_invt>-menge
            wsti_countdate  = <ls_invt>-wsti_countdate
            zmenge2     	= <ls_invt>-zmenge2
            meins_menge 	= <ls_pinv>-meins
            labst       	= <ls_pinv>-labst
            meins_labst 	= <ls_pinv>-meins
            delta_menge 	= <ls_pinv>-labst - <ls_invt>-zmenge2
            zconfirm    	= <ls_invt>-zconfirm
            style       	= VALUE #( ( fieldname = 'ZMENGE2'
                                         style 	= COND #( WHEN <ls_counting>-int_locked = abap_false
                                                             THEN cl_gui_alv_grid=>mc_style_disabled
                                                         	WHEN <ls_invt>-zconfirm = abap_true
                                                             THEN cl_gui_alv_grid=>mc_style_disabled
                                                         	ELSE cl_gui_alv_grid=>mc_style_enabled  ) )
                                  	)
  	).
	ENDLOOP.

  ENDMETHOD.

  METHOD handle_data_changed_fn.

	LOOP AT et_good_cells ASSIGNING FIELD-SYMBOL(<ls_cell>) WHERE fieldname = 'ZMENGE2'.

  	READ TABLE mt_counting ASSIGNING FIELD-SYMBOL(<ls_count>) INDEX <ls_cell>-row_id.

  	IF sy-subrc = 0.
<ls_count>-modified = abap_true.
<ls_count>-delta_menge = <ls_count>-labst - <ls_count>-zmenge2.
  	ENDIF.

	ENDLOOP.

    mo_alv_grid->refresh_table_display(
  	EXCEPTIONS
        finished   	= 1
    	OTHERS     	= 2
	).
	IF sy-subrc <> 0.
  	MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    	WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.

  ENDMETHOD.

  METHOD user_command_0100.

	CASE gs_screen100-ok_code.

  	WHEN 'FC_SAVE'.
        save( ).

  	WHEN 'FC_CONF'.
        confirm( ).

  	WHEN 'FC_RECONF'.
        reconfirm( ).

  	WHEN 'FC_BACK'.
        confirm_leaving( ).
	ENDCASE.

  ENDMETHOD.

  METHOD confirm_leaving.

	DATA : lv_answer TYPE char1.

	READ TABLE mt_counting TRANSPORTING NO FIELDS WITH KEY modified = abap_true.

	IF sy-subrc = 0.

  	CALL FUNCTION 'POPUP_TO_CONFIRM'
    	EXPORTING
      	titlebar          	= 'Confirm leaving program'
          text_question     	= 'Data has been changed.Are you sure you want to leave the program?'
          text_button_1     	= 'Yes'
          text_button_2     	= 'No'
          default_button    	= '2'
          display_cancel_button = ' '
    	IMPORTING
          answer            	= lv_answer
    	EXCEPTIONS
          text_not_found    	= 1
      	OTHERS            	= 2.
  	IF sy-subrc <> 0.
* Implement suitable error handling here
  	ENDIF.

  	IF lv_answer <> '1'.
    	RETURN.
  	ENDIF.

	ENDIF.

	LOOP AT mt_counting ASSIGNING FIELD-SYMBOL(<ls_count>) WHERE int_locked = abap_true.

  	CALL FUNCTION 'DEQUEUE_EZMASP_INVCNT_IN'
    	EXPORTING
          mode_zmasp_invcnt_in = 'E'
          mandt            	= sy-mandt
          iblnr            	= <ls_count>-iblnr
          zeili            	= <ls_count>-zeili.

	ENDLOOP.
	LEAVE TO SCREEN 0.

  ENDMETHOD.

  METHOD status_0100.

	SET PF-STATUS 'PF-ZMASP'.
	SET TITLEBAR 'TB-ZMASP'.

  ENDMETHOD.

  METHOD save.

	READ TABLE mt_counting ASSIGNING FIELD-SYMBOL(<ls_counting>) WITH KEY zconfirm = abap_false.

	IF sy-subrc = 0.
  	MESSAGE 'Not possible to save unconfirmed records!' TYPE 'S' DISPLAY LIKE 'E'.
  	RETURN.
	ENDIF.

    mo_alv_grid->check_changed_data( ).

	LOOP AT mt_counting ASSIGNING <ls_counting> WHERE modified = abap_true
                                            	AND int_locked = abap_true.

  	UPDATE zmasp_invcnt_in SET zmenge2 = <ls_counting>-zmenge2
                                 zconfirm = <ls_counting>-zconfirm
                             	WHERE iblnr = <ls_counting>-iblnr
                             	AND zeili = <ls_counting>-zeili.
  	IF sy-subrc <> 0.
    	DATA(lv_error) = abap_true.
    	EXIT.

  	ENDIF.
<ls_counting>-modified = abap_false.

	ENDLOOP.

	IF lv_error IS INITIAL.

  	MESSAGE 'Data updated successfully!' TYPE 'S'.
  	COMMIT WORK.

	ELSE.
  	MESSAGE 'No data saved' TYPE 'S' DISPLAY LIKE 'E'.
  	ROLLBACK WORK.
	ENDIF.

  ENDMETHOD.

  METHOD confirm.

	DATA: lt_selected_rows TYPE lvc_t_roid.

    mo_alv_grid->get_selected_rows(
  	IMPORTING
        et_row_no 	= lt_selected_rows
	).

	IF lt_selected_rows IS INITIAL.
  	MESSAGE 'Please, select at least 1 line to continue!' TYPE 'S' DISPLAY LIKE 'E'.
  	RETURN.
	ENDIF.

	LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_rows>).

  	READ TABLE mt_counting ASSIGNING FIELD-SYMBOL(<ls_counting>) INDEX <ls_selected_rows>-row_id.

  	IF sy-subrc = 0 AND <ls_counting>-zconfirm = abap_false AND <ls_counting>-int_locked = abap_true.

    	DATA(lv_conf) = abap_true.

    	IF <ls_counting>-zmenge2 IS INITIAL.
<ls_counting>-zmenge2 = <ls_counting>-menge.
<ls_counting>-delta_menge = <ls_counting>-labst - <ls_counting>-zmenge2.
    	ENDIF.

<ls_counting>-zconfirm = abap_true.
<ls_counting>-style = VALUE #( ( fieldname = 'ZMENGE2'
                                         style 	=  cl_gui_alv_grid=>mc_style_disabled  ) ).
<ls_counting>-modified = abap_true.

  	ENDIF.

	ENDLOOP.

	IF lv_conf IS INITIAL.
  	MESSAGE 'The records have already been confirmed or are locked!' TYPE 'S' DISPLAY LIKE 'E'.
  	RETURN.
	ENDIF.

    mo_alv_grid->refresh_table_display(
  	EXCEPTIONS
        finished   	= 1
    	OTHERS     	= 2
	).
	IF sy-subrc <> 0.
  	MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    	WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.
  ENDMETHOD.

  METHOD reconfirm.

	DATA: lt_selected_rows TYPE lvc_t_roid.

    mo_alv_grid->get_selected_rows(
  	IMPORTING
        et_row_no 	= lt_selected_rows
	).

	IF lt_selected_rows IS INITIAL.
  	MESSAGE 'Please, select at least 1 line to continue!' TYPE 'S' DISPLAY LIKE 'E'.
  	RETURN.
	ENDIF.

	LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_rows>).

  	READ TABLE mt_counting ASSIGNING FIELD-SYMBOL(<ls_counting>) INDEX <ls_selected_rows>-row_id.

  	IF sy-subrc = 0 AND <ls_counting>-zconfirm = abap_true AND <ls_counting>-int_locked = abap_true.

    	DATA(lv_found) = abap_true.

<ls_counting>-zconfirm = abap_false.
<ls_counting>-style = VALUE #( ( fieldname = 'ZMENGE2'
                                         style 	=  cl_gui_alv_grid=>mc_style_enabled  ) ).
<ls_counting>-modified = abap_true.

  	ENDIF.

	ENDLOOP.

	IF lv_found IS INITIAL.
  	MESSAGE 'The records have still to be confirmed and cannot be re-opened' TYPE 'S' DISPLAY LIKE 'E'.
  	RETURN.
	ENDIF.

    mo_alv_grid->refresh_table_display(
  EXCEPTIONS
    finished   	= 1
	OTHERS     	= 2
).
	IF sy-subrc <> 0.
  	MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    	WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.

  ENDMETHOD.
ENDCLASS.

DATA: go_report TYPE REF TO lcl_report.

START-OF-SELECTION.

  go_report = NEW #(  ).

  IF go_report IS BOUND.
    go_report->execute( ).
  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  go_report->status_0100( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_report->user_command_0100( ).
ENDMODULE.
