*&---------------------------------------------------------------------*
*& Report ZFI_DIRECTORY_FILE_MOVEMENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFI_DIRECTORY_FILE_MOVEMENT.

TYPES: BEGIN OF ty_files,
         filename TYPE string,
       END OF ty_files.

SELECTION-SCREEN: BEGIN OF BLOCK b01 WITH FRAME.
  PARAMETERS: p_folda TYPE string DEFAULT '.' OBLIGATORY,
              p_foldb TYPE string DEFAULT '/tmp' OBLIGATORY LOWER CASE,
              p_days  TYPE i OBLIGATORY DEFAULT 1500.
SELECTION-SCREEN: END OF BLOCK b01.

DATA: gs_files TYPE ty_files,
      gt_files TYPE STANDARD TABLE OF ty_files.

PERFORM get_data.

DATA: gv_source_path TYPE string,
      gv_dest_path   TYPE string,
      gv_error       TYPE string,
      gv_buffer      TYPE xstring,
      gv_name        TYPE string,
      gt_name        TYPE TABLE OF string,
      gv_extension   TYPE string,
      gs_name        TYPE string,
      gv_date TYPE string.

LOOP AT gt_files INTO gs_files.
  CLEAR: gt_name , gv_name ,  gv_source_path, gv_dest_path, gv_error, gv_buffer.


  SPLIT gs_files-filename AT '.' INTO TABLE gt_name .
  DESCRIBE TABLE gt_name  LINES   DATA(lv_lines).

  LOOP AT gt_name INTO gs_name.

CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO gv_date.

    IF sy-tabix = 1.
      gv_name = gs_name.
      CONTINUE.
    ENDIF.

    IF  sy-tabix = lv_lines.
      gv_extension = gs_name.
      EXIT.
    ENDIF.

    CONCATENATE  gv_name gs_name INTO gv_name SEPARATED BY '.'.

  ENDLOOP.

  CONCATENATE p_folda '/' gs_files-filename INTO gv_source_path.
  CONCATENATE p_foldb '/' gv_name '_' gv_date '_' sy-uzeit '.' gv_extension INTO gv_dest_path.

  OPEN DATASET gv_source_path FOR INPUT IN BINARY MODE MESSAGE gv_error.
  IF sy-subrc <> 0.
    MESSAGE gv_error TYPE 'E'.
    CONTINUE.
  ENDIF.

  READ DATASET gv_source_path INTO gv_buffer.
  IF sy-subrc <> 0.
    MESSAGE gv_error TYPE 'E'.
    CLOSE DATASET gv_source_path.
    CONTINUE.
  ENDIF.
  CLOSE DATASET gv_source_path.

  OPEN DATASET gv_dest_path FOR OUTPUT IN BINARY MODE MESSAGE gv_error.
  IF sy-subrc <> 0.
    MESSAGE gv_error TYPE 'E'.
    CONTINUE.
  ENDIF.

  TRANSFER gv_buffer TO gv_dest_path.
  CLOSE DATASET gv_dest_path.


ENDLOOP.


FORM get_data.

  DATA: lv_checkdate     TYPE d,
        lv_last_date     TYPE d,
        lv_last_time(10) TYPE c,
        lv_day(2)        TYPE c,
        lv_month(2)      TYPE c,
        lv_year(4)       TYPE c,
        lv_date(10)      TYPE c.

  data: lv_dir       TYPE eps2filnam,
        it_dir_files     TYPE TABLE OF eps2fili,
        ls_dir_files     TYPE eps2fili.

  lv_dir = p_folda.


  CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
    EXPORTING
      iv_dir_name            = lv_dir
    TABLES
      dir_list               = it_dir_files
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
* Implement suitable error handling here
  ENDIF.


  LOOP AT it_dir_files INTO ls_dir_files.

    SPLIT ls_dir_files-mtim AT ' ' INTO lv_date lv_last_time.

    SPLIT lv_date AT '.' INTO lv_day lv_month lv_year.
    CONCATENATE lv_year lv_month lv_day INTO lv_last_date.
    gs_files-filename = ls_dir_files-name.


    lv_checkdate = sy-datum - p_days.

    IF  lv_last_date >= lv_checkdate.
      CONTINUE.
    ENDIF.

    APPEND gs_files TO gt_files.

  ENDLOOP.
ENDFORM.
