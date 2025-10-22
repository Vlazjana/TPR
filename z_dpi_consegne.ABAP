REPORT z_dpi_consegne.

TABLES zvest_dotaz_st.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_kodat FOR zvest_dotaz_st-kodat ."OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-002.
PARAMETERS: p_path TYPE btch0000-text80 OBLIGATORY DEFAULT '/home//interfacce/SSLL/output/ADR',
            p_comd TYPE sxpgcolist-name DEFAULT 'ZHR_OUT_SSLLADR'.
SELECTION-SCREEN END OF BLOCK b02.
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-003.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b03.

CLASS lcl_dpi_consegne DEFINITION FINAL.
  PUBLIC SECTION.

TYPES: BEGIN OF ty_dpi_dir,
             idnrk   	TYPE zvest_dotaz_st-idnrk,
             qt_imp  	TYPE zvest_dotaz_st-qt_imp,
             meins   	TYPE zvest_dotaz_st-meins,
             pernr   	TYPE zvest_dotaz_st-pernr,
             stcd1   	TYPE zvest_dotaz_st-stcd1,
             kodat   	TYPE zvest_dotaz_st-kodat,
             usnam   	TYPE mkpf-usnam,
             vfdat   	TYPE mcha-vfdat,
             id_consegna TYPE zdpi_consegne-id_consegna,
       	END OF ty_dpi_dir.

	CONSTANTS: cv_separator  TYPE char1  VALUE '|',
               cv_file_name  TYPE char20 VALUE 'Consegne_DPI',
               cv_file_ext   TYPE char4  VALUE '.csv',
               cv_atinn_vest TYPE char30 VALUE 'Z_VEST_TIPOLOGIA'.

	DATA: mt_dpi_dir TYPE STANDARD TABLE OF ty_dpi_dir,
          mo_salv	TYPE REF TO cl_salv_table.

	METHODS execute.

  PRIVATE SECTION.
	TYPES : BEGIN OF ty_pernr,
           perid TYPE pa0002-perid,
           pernr TYPE pa0002-pernr,
        	END OF ty_pernr,
            tt_pernr TYPE STANDARD TABLE OF ty_pernr WITH DEFAULT KEY.

	METHODS get_data.
	METHODS display_data.
	METHODS save_data_file.
	METHODS get_new_pernr_for_initial RETURNING VALUE(rt_pernr) TYPE tt_pernr.
	METHODS send_canopo_file IMPORTING iv_del_dataset TYPE string
                                       iv_file_nm 	TYPE string.

ENDCLASS.

CLASS lcl_dpi_consegne IMPLEMENTATION.
  METHOD execute.
     get_data( ).
IF p_test IS INITIAL.
      save_data_file( ).
	ENDIF.
    display_data( ).
  ENDMETHOD.


  METHOD send_canopo_file.

	DATA : lv_status   TYPE extcmdexex-status,
           lv_exitcode TYPE extcmdexex-exitcode,
           lv_param	TYPE sxpgcolist-parameters.
    lv_param = iv_file_nm.


	CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
  	EXPORTING
        commandname               	= p_comd
        additional_parameters     	= lv_param
  	IMPORTING
        status                    	= lv_status
        exitcode                  	= lv_exitcode
  	EXCEPTIONS
        no_permission             	= 1
        command_not_found         	= 2
        parameters_too_long       	= 3
        security_risk             	= 4
        wrong_check_call_interface	= 5
        program_start_error       	= 6
        program_termination_error 	= 7
        x_error                   	= 8
        parameter_expected        	= 9
        too_many_parameters       	= 10
        illegal_command           	= 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry      	= 13
        jobcount_generation_error 	= 14
    	OTHERS                    	= 15.
	IF sy-subrc <> 0.
  	MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          	WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  	RETURN.
	ENDIF.

	DELETE DATASET iv_del_dataset.
  ENDMETHOD.

  METHOD get_data.

DATA lv_attin_vest TYPE ausp-atinn.

	CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
  	EXPORTING
    	input  = cv_atinn_vest
  	IMPORTING
    	output = lv_attin_vest.

	SELECT DISTINCT
           dt~idnrk,
           dt~rsnum,
           dt~qt_imp,
           dt~pernr,
           dt~meins,
           dt~stcd1,
           dt~kodat,
           mkpf~usnam,
           mkpf~mblnr,
           mkpf~mjahr,
     CAST( dt~idnrk && '_' && dt~taglia AS CHAR( 18 ) ) AS  matnr_taglia
FROM zvest_dotaz_st AS dt
JOIN ausp
  ON ausp~objek = dt~idnrk
 AND ausp~atinn = @lv_attin_vest
 AND ausp~atwrt = 'P'
JOIN mseg
  ON mseg~rsnum = dt~rsnum
JOIN mkpf
  ON mseg~mblnr = mkpf~mblnr
 AND mseg~mjahr = mkpf~mjahr
INTO TABLE @DATA(lt_dpi_consegne)
WHERE kodat IN @s_kodat.
	IF sy-subrc <> 0.
  	MESSAGE 'Nessun dato trovato' TYPE 'S' DISPLAY LIKE 'E'.
  	LEAVE LIST-PROCESSING.
	ENDIF.


	SELECT mseg~rsnum, mseg~matnr, mcha~vfdat
  	FROM mseg
  	JOIN mcha
    	ON mcha~charg = mseg~charg
	FOR ALL ENTRIES IN @lt_dpi_consegne
  	WHERE mseg~rsnum = @lt_dpi_consegne-rsnum
    	AND mcha~matnr = @lt_dpi_consegne-matnr_taglia
	INTO TABLE @DATA(lt_mcha).
	SORT lt_mcha BY rsnum matnr.

	LOOP AT lt_dpi_consegne ASSIGNING FIELD-SYMBOL(<ls_dpi>).
  	APPEND VALUE #(
          idnrk   	= <ls_dpi>-idnrk
          qt_imp  	= <ls_dpi>-qt_imp
          pernr   	= <ls_dpi>-pernr
          meins   	= <ls_dpi>-meins
          stcd1   	= <ls_dpi>-stcd1
          kodat   	= <ls_dpi>-kodat
          usnam   	= <ls_dpi>-usnam

id_consegna = <ls_dpi>-mblnr && <ls_dpi>-mjahr && <ls_dpi>-idnrk && <ls_dpi>-stcd1
) TO mt_dpi_dir ASSIGNING FIELD-SYMBOL(<ls_dpi_dir>).

  	READ TABLE lt_mcha ASSIGNING FIELD-SYMBOL(<ls_mcha>)
    	WITH KEY rsnum = <ls_dpi>-rsnum
                 matnr = <ls_dpi>-matnr_taglia BINARY SEARCH.

  	IF sy-subrc = 0.
<ls_dpi_dir>-vfdat = <ls_mcha>-vfdat.
  	ENDIF.

	ENDLOOP.

  ENDMETHOD.


  METHOD display_data.

	TRY.
        cl_salv_table=>factory(
      	IMPORTING
            r_salv_table = mo_salv
      	CHANGING
            t_table  	= mt_dpi_dir ).
  	CATCH cx_salv_msg.
	ENDTRY.

    mo_salv->get_columns( )->set_optimize( ).
    mo_salv->get_selections( )->set_selection_mode( value = if_salv_c_selection_mode=>cell ).
    mo_salv->get_functions( )->set_all( ).
    mo_salv->display( ).

  ENDMETHOD.


  METHOD save_data_file.

	DATA lt_dpi_consegne TYPE STANDARD TABLE OF zdpi_consegne.
	DATA lv_transfer TYPE string.
	DATA(lv_date)  = sy-datum.
	DATA(lv_time)  = sy-uzeit.
	DATA(lv_day)   = sy-datum+6(2).
	DATA(lv_month) = sy-datum+4(2).
	DATA(lv_year)  = sy-datum(4).
	DATA(lv_file_nm) =  cv_file_name && lv_day && lv_month && lv_year && cv_file_ext.
	DATA(lv_file) = p_path && |/| && lv_file_nm.

CLEAR sy-subrc.
	OPEN DATASET lv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

	IF sy-subrc <> 0.
  	MESSAGE 'File outbound non si puo aprire' TYPE 'S' DISPLAY LIKE 'E'.
  	RETURN.
	ENDIF.

	CONCATENATE  	'Componente'
   'Qtà fabb.'   	'C.I.D.'
   'Unità mis. base' 'Cd.fiscale'
   'Data prelievo'   'Nome utente'
   'Scadenza/DMC'	'DPI Id Consegna'
	INTO lv_transfer SEPARATED BY cv_separator.

    lv_transfer = lv_transfer && cl_abap_char_utilities=>cr_lf.

	DATA(lt_new_pernr) = get_new_pernr_for_initial( ).

	LOOP AT mt_dpi_dir ASSIGNING FIELD-SYMBOL(<ls_dpi_dir>).
IF <ls_dpi_dir>-pernr IS INITIAL.
    	READ TABLE lt_new_pernr ASSIGNING FIELD-SYMBOL(<fs_pernr>) WITH KEY perid = <ls_dpi_dir>-stcd1.
    	IF sy-subrc = 0.
<ls_dpi_dir>-pernr = <fs_pernr>-pernr.
    	ENDIF.
  	ENDIF.

 lv_transfer = lv_transfer &&
<ls_dpi_dir>-idnrk      && cv_separator &&
<ls_dpi_dir>-qt_imp     && cv_separator &&
<ls_dpi_dir>-pernr      && cv_separator &&
<ls_dpi_dir>-meins      && cv_separator &&
<ls_dpi_dir>-stcd1      && cv_separator &&
<ls_dpi_dir>-kodat+6(2) && '.' && <ls_dpi_dir>-kodat+4(2) && '.' && <ls_dpi_dir>-kodat(4) && cv_separator &&
<ls_dpi_dir>-usnam      && cv_separator &&
<ls_dpi_dir>-vfdat+6(2) && '.' && <ls_dpi_dir>-vfdat+4(2) && '.' && <ls_dpi_dir>-vfdat(4) && cv_separator &&
<ls_dpi_dir>-id_consegna && cl_abap_char_utilities=>cr_lf.


  	APPEND VALUE #(
        mandt    	= sy-mandt
        id_consegna  = <ls_dpi_dir>-id_consegna
        idnrk    	= <ls_dpi_dir>-idnrk
        pernr    	= <ls_dpi_dir>-pernr
        meins    	= <ls_dpi_dir>-meins
        stcd1    	= <ls_dpi_dir>-stcd1
        kodat    	= <ls_dpi_dir>-kodat
        usnam    	= <ls_dpi_dir>-usnam
        vfdat    	= <ls_dpi_dir>-vfdat
        qt_imp   	= <ls_dpi_dir>-qt_imp
        erdat    	= lv_date
        erzet    	= lv_time
  	) TO lt_dpi_consegne.

	ENDLOOP.

	TRANSFER lv_transfer TO lv_file.

	CLOSE DATASET lv_file.

	MODIFY zdpi_consegne FROM TABLE lt_dpi_consegne.
	MESSAGE 'Dati salvati in database e directory' TYPE 'S'.

    send_canopo_file( EXPORTING iv_del_dataset = lv_file


                                iv_file_nm 	= lv_file_nm ).
  ENDMETHOD.

  METHOD get_new_pernr_for_initial.

TYPES : BEGIN OF ty_dpi,
              stcd1 TYPE zesigenze_dpi-stcd1,
              kodat TYPE zvest_dotaz_st-kodat,
        	END OF ty_dpi,
            tt_dpi TYPE STANDARD TABLE OF ty_dpi WITH DEFAULT KEY.

	DATA(lt_dpi) = VALUE tt_dpi( FOR ls_dpi IN mt_dpi_dir
                                	WHERE ( pernr IS INITIAL )
                              	( stcd1 = ls_dpi-stcd1
                                    kodat = ls_dpi-kodat ) 	).

	IF lt_dpi IS INITIAL.
  	RETURN.
ENDIF.

	SELECT DISTINCT pa2~perid, pa2~pernr, pa0~aedtm
  	FROM pa0002 AS pa2
  	JOIN pa0000 AS pa0  	ON pa2~pernr = pa0~pernr
  	INTO TABLE @DATA(lt_pa)
  	FOR ALL ENTRIES IN @lt_dpi
 	WHERE pa2~perid = @lt_dpi-stcd1
   	AND pa2~begda <= @lt_dpi-kodat
   	AND pa2~endda = '99991231'
   	AND pa0~endda = '99991231'
   	AND pa0~stat2 = '3'.

	SORT lt_pa BY perid pernr aedtm .

	DELETE ADJACENT DUPLICATES FROM lt_pa COMPARING perid pernr.

	LOOP AT lt_pa ASSIGNING FIELD-SYMBOL(<fs_pa2>).

  	APPEND VALUE #( pernr = <fs_pa2>-pernr
                      perid = <fs_pa2>-perid ) TO rt_pernr.

	ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_dpi_consegne( )->execute( ).
 
