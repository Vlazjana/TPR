Containers image

REPORT zvq_image.


TYPE-POOLS: cndp.

PARAMETERS:  p_objid TYPE w3objid OBLIGATORY DEFAULT 'ZVQ_IMAGE',
             rb_upl  RADIOBUTTON GROUP r1,
             rb_web  RADIOBUTTON GROUP r1 DEFAULT 'X'.
DATA:  ok_code TYPE sy-ucomm.

CLASS lcl_image DEFINITION.

  PUBLIC SECTION.
    DATA : container TYPE REF TO cl_gui_custom_container,
           picture   TYPE REF TO cl_gui_picture,
           url       TYPE cndp_url.
    METHODS upload.
    METHODS web.
ENDCLASS.

CLASS lcl_image IMPLEMENTATION.

  METHOD upload.

    SELECT COUNT(*) FROM wwwparams
    WHERE objid = p_objid.
    IF sy-subrc <> 0.
      MESSAGE e001(00) WITH 'MIME Object not found'.
    ENDIF.


    IF container IS INITIAL.

      CREATE OBJECT container
        EXPORTING
          container_name              = 'IMG_CONTAINER'
          repid                       = 'ZVQ_IMAGE'
          dynnr                       = '0100'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE i001(00) WITH 'Error while creating container'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
    IF picture IS INITIAL.
      CREATE OBJECT picture
        EXPORTING
          parent = container
        EXCEPTIONS
          error  = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
        MESSAGE i001(00) WITH 'Error while displaying picture'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
    IF picture IS NOT INITIAL.

      CALL FUNCTION 'DP_PUBLISH_WWW_URL'
        EXPORTING
          objid    = p_objid
          lifetime = cndp_lifetime_transaction
        IMPORTING
          url      = url
        EXCEPTIONS
          OTHERS   = 1.

      IF sy-subrc = 0.
        CALL METHOD picture->load_picture_from_url_async
          EXPORTING
            url = url.

        CALL METHOD picture->set_display_mode
          EXPORTING
            display_mode = cl_gui_picture=>display_mode_fit.
      ELSE.
        MESSAGE i001(00) WITH 'Error while load picture'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

  ENDMETHOD .

  METHOD web.


  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CASE abap_true.
    WHEN rb_upl.
      DATA(go_upl) = NEW lcl_image( ).
      go_upl->upload( ).
    WHEN rb_web.
      DATA(go_web) = NEW lcl_image( ).
      go_web->web( ).
  ENDCASE.



  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'S0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'E' OR 'ECAN'.
      SET SCREEN 00.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT


—---------------------------------------------------

TYPE-POOLS: cndp.

PARAMETERS:  p_objid TYPE w3objid ,
             rb_upl  RADIOBUTTON GROUP r1,
             rb_web  RADIOBUTTON GROUP r1 DEFAULT 'X'.

DATA:  ok_code TYPE sy-ucomm.

CLASS lcl_image DEFINITION.

  PUBLIC SECTION.
    DATA : container TYPE REF TO cl_gui_custom_container,
           picture   TYPE REF TO cl_gui_picture,
           url       TYPE cndp_url.
    METHODS upload.

ENDCLASS.


CLASS lcl_image IMPLEMENTATION.

  METHOD upload.

    SELECT COUNT(*) FROM wwwparams
    WHERE objid = p_objid.
    IF sy-subrc <> 0.
      MESSAGE e001(00) WITH 'MIME Object not found'.
    ENDIF.


    IF container IS INITIAL.

      CREATE OBJECT container
        EXPORTING
          container_name              = 'IMG_CONTAINER'
          repid                       = 'ZVQ_IMAGE'
          dynnr                       = '0100'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE i001(00) WITH 'Error while creating container'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
    IF picture IS INITIAL.
      CREATE OBJECT picture
        EXPORTING
          parent = container
        EXCEPTIONS
          error  = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
        MESSAGE i001(00) WITH 'Error while displaying picture'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
    IF picture IS NOT INITIAL.

      CALL FUNCTION 'DP_PUBLISH_WWW_URL'
        EXPORTING
          objid    = p_objid
          lifetime = cndp_lifetime_transaction
        IMPORTING
          url      = url
        EXCEPTIONS
          OTHERS   = 1.

      IF sy-subrc = 0.
        CALL METHOD picture->load_picture_from_url_async
          EXPORTING
            url = url.

        CALL METHOD picture->set_display_mode
          EXPORTING
            display_mode = cl_gui_picture=>display_mode_fit.
      ELSE.
        MESSAGE i001(00) WITH 'Error while load picture'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

  ENDMETHOD .

endclass.
CLASS picture_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: main1.
  PRIVATE SECTION.
    TYPES: html     TYPE c LENGTH 255,
           html_tab TYPE STANDARD TABLE OF html WITH EMPTY KEY.
    TYPES: pict_line(1022) TYPE x,
           pict_tab        TYPE STANDARD TABLE OF pict_line
                                WITH EMPTY KEY.
    CLASS-METHODS get_pict_tab
      IMPORTING
        mime_url        TYPE csequence
      RETURNING
        VALUE(pict_tab) TYPE pict_tab.
ENDCLASS.

CLASS picture_demo IMPLEMENTATION.
  METHOD main1.
    DATA html_url TYPE c LENGTH 255.
    DATA pict_url TYPE c LENGTH 255.

    DATA(custom_container) = NEW
      cl_gui_custom_container( container_name = 'IMG_CONTAINER' ).
    DATA(html_control) = NEW
     cl_gui_html_viewer( parent = custom_container ).

    DATA(pict_tab) = get_pict_tab(
      mime_url = '/SAP/BC/BSP/SAP/PUBLIC/ZVQ_CONTAINER/dummy-user.png' ).
    html_control->load_data(
      EXPORTING
        url          = 'picture_url'
        type         = 'image'
        subtype      = '.png'
      IMPORTING
        assigned_url = pict_url
      CHANGING
        data_table   = pict_tab ).

    DATA(html_tab) = VALUE html_tab(
      ( '<html><body><basefont face="arial">' )
      ( 'Picture with CL_GUI_HTML_VIEWER<br><br>' )
      ( '<img src="' && pict_url && '">' )
      ( '</body></html>' ) ).
    html_control->load_data(
      IMPORTING
        assigned_url = html_url
      CHANGING
        data_table   = html_tab ).

    html_control->show_url(
       EXPORTING
         url = html_url ).
  ENDMETHOD.



  METHOD get_pict_tab.
    cl_mime_repository_api=>get_api( )->get(
      EXPORTING i_url = mime_url
      IMPORTING e_content = DATA(pict_wa)
      EXCEPTIONS OTHERS = 4 ).
    IF sy-subrc = 4.
      RETURN.
    ENDIF.
    pict_tab =
      VALUE #( LET l1 = xstrlen( pict_wa ) l2 = l1 - 1022 IN
               FOR j = 0 THEN j + 1022  UNTIL j >= l1
                 ( COND #( WHEN j <= l2 THEN
                                pict_wa+j(1022)
                           ELSE pict_wa+j ) ) ).
  ENDMETHOD.
ENDCLASS.






START-OF-SELECTION.
  CASE abap_true.
    WHEN rb_upl.
      DATA(go_upl) = NEW lcl_image( ).
      go_upl->upload( ).

    WHEN rb_web.
     picture_demo=>main1( ).


  ENDCASE.

 CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'S0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'E' OR 'ECAN'.
      SET SCREEN 00.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.


**********************************

INCLUDE zvq_image_status_0200o01.

INCLUDE zvq_image_user_command_0200i01.
