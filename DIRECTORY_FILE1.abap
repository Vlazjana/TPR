**&---------------------------------------------------------------------*
**& Report ZFI_DIRECTORY_FILE_MOVEMENT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT zfi_directory_file_movement.


TYPES: BEGIN OF ty_file,
         filename TYPE string,
         mod_date TYPE d,
         mod_time TYPE t,
       END OF ty_file,
       tt_files TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.


SELECTION-SCREEN: BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_folda TYPE string DEFAULT '.' OBLIGATORY,
              p_foldb TYPE string DEFAULT '/tmp' OBLIGATORY LOWER CASE,
              p_days  TYPE i OBLIGATORY DEFAULT 1500.
SELECTION-SCREEN: END OF BLOCK b01.


CLASS lcl_file_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      process_files
        IMPORTING
                  iv_source_folder TYPE string
                  iv_dest_folder   TYPE string
                  iv_days_old      TYPE i
        RAISING   cx_sy_file_open_mode
                  cx_sy_file_io.

  PRIVATE SECTION.
    METHODS:
      get_old_files
        IMPORTING
                  iv_source_folder TYPE string
                  iv_days_old      TYPE i
        CHANGING
                  ct_files         TYPE tt_files
        RAISING   cx_sy_file_open_mode,

      parse_filename
        IMPORTING
          iv_filename  TYPE string
        EXPORTING
          ev_name      TYPE string
          ev_extension TYPE string,

      copy_file
        IMPORTING
                  iv_source_file TYPE string
                  iv_dest_file   TYPE string
        RAISING   cx_sy_file_open_mode
                  cx_sy_file_io,

      build_destination_path
        IMPORTING
          iv_dest_folder TYPE string
          iv_filename    TYPE string
        RETURNING
          VALUE(rv_path) TYPE string.

ENDCLASS.

DATA: lo_processor TYPE REF TO lcl_file_processor.

CLASS lcl_file_processor IMPLEMENTATION.

  METHOD process_files.

    DATA: lv_source_path TYPE string,
          lv_dest_path   TYPE string,
          lt_files       TYPE tt_files.


    get_old_files(
      EXPORTING
        iv_source_folder = iv_source_folder
        iv_days_old      = iv_days_old
      CHANGING
        ct_files         = lt_files
    ).

    " Process each file
    LOOP AT lt_files INTO DATA(ls_file).
      TRY.
          " Build full paths
          lv_source_path = |{ iv_source_folder }/{ ls_file-filename }|.
          lv_dest_path   = build_destination_path(
                             iv_dest_folder = iv_dest_folder
                             iv_filename    = ls_file-filename
                           ).


          copy_file(
            EXPORTING
              iv_source_file = lv_source_path
              iv_dest_file   = lv_dest_path
          ).

          WRITE: / 'File copied:', ls_file-filename, 'to', lv_dest_path.

        CATCH cx_sy_file_open_mode cx_sy_file_io INTO DATA(lx_error).
          MESSAGE lx_error->get_text( ) TYPE 'E' DISPLAY LIKE 'W'.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    IF lt_files IS INITIAL.
      WRITE: / 'No files found matching criteria.'.
    ENDIF.
  ENDMETHOD.

  METHOD get_old_files.
    DATA: lv_dir       TYPE eps2filnam,
          lt_dir_files TYPE TABLE OF eps2fili,
          lv_checkdate TYPE d,
          lv_last_date TYPE d,
          lv_last_time TYPE c LENGTH 10,
          lv_date      TYPE c LENGTH 10,
          lv_day       TYPE c LENGTH 2,
          lv_month     TYPE c LENGTH 2,
          lv_year      TYPE c LENGTH 4.

    lv_dir = iv_source_folder.
    lv_checkdate = sy-datum - iv_days_old.


    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = lv_dir
      TABLES
        dir_list               = lt_dir_files
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
      MESSAGE 'Error reading directory' TYPE 'E'.
      RETURN.
    ENDIF.

    " Filter files by age
    LOOP AT lt_dir_files INTO DATA(ls_dir_file).
      SPLIT ls_dir_file-mtim AT space INTO lv_date lv_last_time.
      SPLIT lv_date AT '.' INTO lv_day lv_month lv_year.
      lv_last_date = |{ lv_year }{ lv_month }{ lv_day }|.

      IF lv_last_date >= lv_checkdate.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( filename = ls_dir_file-name
                      mod_date = lv_last_date
                      mod_time = lv_last_time ) TO ct_files.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_filename.

    DATA: lt_parts TYPE TABLE OF string,
          lv_lines TYPE i,
          lv_name  TYPE string,
          ls_part  TYPE string.

    CLEAR: ev_name, ev_extension.

    SPLIT iv_filename AT '.' INTO TABLE lt_parts.
    DESCRIBE TABLE lt_parts LINES lv_lines.

    IF lv_lines = 1.
      ev_name = iv_filename.
      CLEAR ev_extension.
      RETURN.
    ENDIF.

    LOOP AT lt_parts INTO ls_part.
      IF sy-tabix = 1.
        " Handle filename starting with dot
        IF ls_part IS INITIAL.
          lv_name = ''.
        ELSE.
          lv_name = ls_part.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF sy-tabix = lv_lines.
        ev_extension = ls_part.
        EXIT.
      ENDIF.

      lv_name = |{ lv_name }.{ ls_part }|.
    ENDLOOP.

    ev_name = lv_name.
  ENDMETHOD.

  METHOD build_destination_path.
    DATA: lv_name      TYPE string,
          lv_extension TYPE string,
          lv_date      TYPE string.


    parse_filename(
      EXPORTING
        iv_filename  = iv_filename
      IMPORTING
        ev_name      = lv_name
        ev_extension = lv_extension
    ).

    " Format current date
    lv_date = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.


    IF lv_extension IS INITIAL.
      rv_path = |{ iv_dest_folder }/{ lv_name }_{ lv_date }_{ sy-uzeit }|.
    ELSE.
      rv_path = |{ iv_dest_folder }/{ lv_name }_{ lv_date }_{ sy-uzeit }.{ lv_extension }|.
    ENDIF.
  ENDMETHOD.

  METHOD copy_file.

    DATA: lv_buffer TYPE x LENGTH 1024.

    " Open source file for binary input
    OPEN DATASET iv_source_file FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      MESSAGE |Failed to open source file { iv_source_file }| TYPE 'E'.
    ENDIF.

    " Open destination file for binary output
    OPEN DATASET iv_dest_file FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      CLOSE DATASET iv_source_file.
      MESSAGE |Failed to open destination file { iv_dest_file }| TYPE 'E'.
    ENDIF.

    " Read and write in chunks
    WHILE sy-subrc = 0.
      READ DATASET iv_source_file INTO lv_buffer MAXIMUM LENGTH 1024.
      IF sy-subrc = 0.
        TRANSFER lv_buffer TO iv_dest_file.
      ENDIF.
    ENDWHILE.

    CLOSE DATASET iv_source_file.
    CLOSE DATASET iv_dest_file.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  TRY.
      CREATE OBJECT lo_processor.
      lo_processor->process_files(
        EXPORTING
          iv_source_folder = p_folda
          iv_dest_folder   = p_foldb
          iv_days_old      = p_days
      ).
    CATCH cx_sy_file_open_mode cx_sy_file_io INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.
