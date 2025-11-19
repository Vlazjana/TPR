*&---------------------------------------------------------------------*
*& Report ZMM_DASHCHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_dashcheck.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_imp  RADIOBUTTON GROUP g1,
              p_view RADIOBUTTON GROUP g1.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_result RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 3(40) TEXT-002 FOR FIELD p_result.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_DASHCHECK DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dashcheck DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_DASHCHECK IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dashcheck IMPLEMENTATION.

  METHOD execute.

    CASE abap_true.
      WHEN p_imp.
        DATA(lv_tcode) = EXACT sy-tcode( 'ZMM_IMPCHECK' ).
      WHEN p_view.
        lv_tcode = 'ZMM_CONSCHECK'.
      WHEN p_result.
        lv_tcode = 'ZMM_DRCHECK'.
    ENDCASE.

    IF lv_tcode IS INITIAL.
      MESSAGE |Transaction { lv_tcode } does not exist| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL TRANSACTION lv_tcode WITH AUTHORITY-CHECK.

  ENDMETHOD.                    "execute

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_dashcheck( )->execute( ).
