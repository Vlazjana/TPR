REPORT zesigenze_canopo_dpi.

TABLES btch0000.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.

PARAMETERS: p_infile TYPE btch0000-text80 OBLIGATORY DEFAULT '/home//interfacce/interf_SSLL/input/ADR/DPI',
            p_instor TYPE btch0000-text80 OBLIGATORY DEFAULT '/home//interfacce/interf_SSLL/input/ADR/DPI/storico',
            p_comd   TYPE sxpgcolist-name DEFAULT 'ZESIGENZE_DPI'.

SELECTION-SCREEN END OF BLOCK b01.

CLASS lcl_esig_canopo_dpi DEFINITION.

  PUBLIC SECTION.

	TYPES: tt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.
	TYPES: BEGIN OF ty_dpi,
             filename TYPE epspath.
         	INCLUDE TYPE zesigenze_dpi.
         	TYPES: t_colors TYPE lvc_t_scol,
       	END OF ty_dpi.
	DATA: mt_dpi  TYPE STANDARD TABLE OF ty_dpi,
        mo_salv TYPE REF TO cl_salv_table.

	METHODS execute.
	METHODS get_dpi_files RETURNING VALUE(rt_files) TYPE tt_string.
  PRIVATE SECTION.
	METHODS write_file_to_db
  	IMPORTING iv_file     	TYPE string
      RETURNING VALUE(rt_lines) TYPE tt_string.
	METHODS move_file_to_storic
  	IMPORTING
        iv_file  	TYPE string
        it_dir_lines TYPE tt_string.
	METHODS get_canopo_file.
	METHODS display_dpi.
ENDCLASS.

CLASS lcl_esig_canopo_dpi IMPLEMENTATION.
  METHOD get_canopo_file.
	DATA : lv_status   TYPE extcmdexex-status,
         lv_exitcode TYPE extcmdexex-exitcode.

	CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
  	EXPORTING
        commandname               	= p_comd
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
	ENDIF.
  ENDMETHOD.

  METHOD execute.
    get_canopo_file( ).
	DATA(lt_files) = get_dpi_files( ).

	IF lt_files IS INITIAL.
  	RETURN.
	ENDIF.

	LOOP AT lt_files INTO DATA(lv_file).
  	DATA(lv_curr_file)   = p_infile && '/' && lv_file.
  	DATA(lv_storic_file) = p_instor && '/' && lv_file .
  	DATA(lt_dir_lines) = write_file_to_db( iv_file = lv_curr_file ).

      move_file_to_storic(
    	EXPORTING
          iv_file  	= lv_storic_file
          it_dir_lines = lt_dir_lines
  	).

  	DELETE DATASET lv_curr_file.

  	CLEAR : lv_curr_file,
            lv_storic_file,
            lt_dir_lines.
	ENDLOOP.

    display_dpi( ).
  ENDMETHOD.



  METHOD get_dpi_files.

	DATA: lv_dir_name TYPE epsf-epsdirnam,
        lt_dir_list TYPE STANDARD TABLE OF epsfili.

	CALL FUNCTION 'PFL_CHECK_DIRECTORY'
  	EXPORTING
        directory               	= p_infile
  	EXCEPTIONS
        pfl_dir_not_exist       	= 1
        pfl_permission_denied   	= 2
        pfl_cant_build_dataset_name = 3
        pfl_file_not_exist      	= 4
        pfl_authorization_missing   = 5
    	OTHERS                  	= 6.
	IF sy-subrc <> 0.
  	MESSAGE 'Cartella di input non trovata' TYPE 'E'.
	ENDIF.

    lv_dir_name = p_infile.

	CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
  	EXPORTING
      dir_name           	= lv_dir_name
      file_mask          	= 'Esigenze_DPI*' "'Esigenze_DPI*.csv'
  	TABLES
        dir_list           	= lt_dir_list
  	EXCEPTIONS
        invalid_eps_subdir 	= 1
        sapgparam_failed   	= 2
        build_directory_failed = 3
        no_authorization   	= 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
    	OTHERS             	= 8.

	IF sy-subrc <> 0.
  	MESSAGE 'Non ci sono file con nome "Esigenze_DPI*.csv"' TYPE 'E'.
	ENDIF.

	LOOP AT lt_dir_list ASSIGNING FIELD-SYMBOL(<ls_dir>) WHERE name CP '*.csv'.
  	APPEND <ls_dir>-name TO rt_files.
	ENDLOOP.

	IF rt_files IS INITIAL.
  	MESSAGE 'Non ci sono file con nome "Esigenze_DPI*.csv"' TYPE 'E'.
	ENDIF.
  ENDMETHOD.


  METHOD write_file_to_db.

	DATA: lv_file_line TYPE string,
        lt_dd07v 	TYPE STANDARD TABLE OF dd07v,
        lt_dpi   	TYPE STANDARD TABLE OF zesigenze_dpi,
        lv_langu 	TYPE c LENGTH 2,
        lv_meins 	TYPE meins.

	CONSTANTS cv_langu TYPE syst_langu VALUE 'I'.

	CALL FUNCTION 'DD_DOMVALUES_GET'
  	EXPORTING
      domname    	= 'ZDPI_DOM_STATO_ESIGENZA'
    	text       	= 'X'
      langu      	= 'I'
  	TABLES
     dd07v_tab  	= lt_dd07v
  	EXCEPTIONS
        wrong_textflag = 1
    	OTHERS     	= 2.
	IF sy-subrc <> 0.
* Implement suitable error handling here
	ENDIF.

	SORT lt_dd07v BY ddtext.

	LOOP AT lt_dd07v ASSIGNING FIELD-SYMBOL(<ls_dd07v>).
<ls_dd07v>-ddtext = to_upper( <ls_dd07v>-ddtext ).
	ENDLOOP.

	OPEN DATASET iv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.

	DO.
  	DATA(lv_index) = sy-index.
  	READ DATASET iv_file INTO lv_file_line.
  	IF sy-subrc <> 0.
    	EXIT.
  	ENDIF.
  	APPEND lv_file_line TO rt_lines.
  	IF lv_index = 1.
    	CONTINUE.
  	ENDIF.

  	APPEND INITIAL LINE TO lt_dpi ASSIGNING FIELD-SYMBOL(<ls_dpi>).
  	APPEND INITIAL LINE TO mt_dpi ASSIGNING FIELD-SYMBOL(<ls_dpi_all>).

<ls_dpi>-datum = sy-datum.
<ls_dpi>-ora   = sy-uzeit.

  	SPLIT lv_file_line AT '|'

  	INTO <ls_dpi>-codice_dpi

    DATA(lv_qta_dot)

   	DATA(lv_frequenza)
<ls_dpi>-meins
<ls_dpi>-stcd1


  DATA(lv_stato_esig)
<ls_dpi>-cod_mansione
<ls_dpi>-descr_mansione.


  	IF <ls_dpi>-codice_dpi IS INITIAL OR <ls_dpi>-stcd1 IS INITIAL.
    	MESSAGE 'Controlla i file caricato'(001) TYPE 'E'.
  	ENDIF.

  	CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    	EXPORTING
      	input      	= <ls_dpi>-meins
      	language   	= cv_langu
    	IMPORTING
      	output     	= lv_meins
    	EXCEPTIONS
        unit_not_found = 1
      	OTHERS     	= 2.
  	IF sy-subrc = 0.
<ls_dpi>-meins = lv_meins.
  	ENDIF.

<ls_dpi>-qta_dotazione = lv_qta_dot.

  	MOVE lv_frequenza TO <ls_dpi>-frequenza.

  	READ TABLE lt_dd07v ASSIGNING <ls_dd07v>

    	WITH KEY ddtext = to_upper( lv_stato_esig ) BINARY SEARCH.

  	IF sy-subrc = 0.
<ls_dpi>-stato_esigenza = <ls_dd07v>-domvalue_l.
  	ENDIF.

<ls_dpi_all> = CORRESPONDING #( <ls_dpi> ).
<ls_dpi_all>-filename = iv_file.


  	CLEAR: lv_qta_dot,
           lv_frequenza,
          lv_stato_esig.
	ENDDO.

	CLOSE DATASET iv_file.
	DELETE DATASET iv_file.
	MODIFY zesigenze_dpi FROM TABLE lt_dpi.

  ENDMETHOD.



  METHOD move_file_to_storic.

	OPEN DATASET iv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
	IF sy-subrc <> 0.
  	MESSAGE 'File storico non si puo aprire'  TYPE 'E'.
	ENDIF.

	LOOP AT it_dir_lines ASSIGNING FIELD-SYMBOL(<ls_dir_line>).
  	TRANSFER <ls_dir_line> TO iv_file.
	ENDLOOP.

	CLOSE DATASET iv_file.

  ENDMETHOD.

  METHOD display_dpi.
	TRY.
        cl_salv_table=>factory(
      	IMPORTING
            r_salv_table = mo_salv
      	CHANGING
            t_table  	= mt_dpi ).
  	CATCH cx_salv_msg.

	ENDTRY.
    mo_salv->get_columns( )->set_optimize( ).
	TRY.
        mo_salv->get_columns( )->set_color_column( 'T_COLOR' ).
  	CATCH cx_salv_data_error.
	ENDTRY.

	IF mt_dpi IS NOT INITIAL.
  	MESSAGE 'Dati caricati con successo' TYPE 'S'.
	ENDIF.

    mo_salv->display( ).

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  NEW lcl_esig_canopo_dpi( )->execute( ).
 
