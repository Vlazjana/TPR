
Kishim per te dalluar se kush do ishte del_ind S/L kush eshte s duhet qe ebelni te ekzistoj dhe ne tab eket edhe ne ekpo , ndersa L duhet te ekz veten ne ekpo , dhe me pas te vendosnim shkronjat ne tab ekpo cfare i perkiste L/S


**&---------------------------------------------------------------------*
**& Report Z_DEL_SA
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT z_del_sa.

TABLES: ekko, ekpo , eket .
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_ebeln FOR ekko-ebeln.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_duplicate DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_duplicate DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.
    METHODS extract_data.
    METHODS display_logs.

    TYPES: BEGIN OF ty_alv_display,
             status   TYPE char30,
             ebeln    TYPE ekpo-ebeln,
             loekz    TYPE ekpo-loekz,
             msg_type TYPE char1,
             msg      TYPE string,
           END OF ty_alv_display.

    DATA: mt_alv_display TYPE STANDARD TABLE OF ty_alv_display,
          ms_alv_display TYPE ty_alv_display.


ENDCLASS.
DATA go_duplicate TYPE REF TO lcl_duplicate.
*----------------------------------------------------------------------*
*       CLASS lcl_duplicate IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_duplicate IMPLEMENTATION.
  METHOD execute.
    extract_data( ).
  ENDMETHOD.
  "execute              "
  METHOD extract_data.

    DATA: lt_items_sag  TYPE STANDARD TABLE OF bapimeoutitem,
          ls_items_sag  TYPE bapimeoutitem,
          lt_itemsx_sag TYPE STANDARD TABLE OF bapimeoutitemx,
          ls_itemsx_sag TYPE bapimeoutitemx,
          lt_all_msg    TYPE STANDARD TABLE OF esp1_message_wa_type,
          lt_messages   TYPE STANDARD TABLE OF bapiret2.

    SELECT ebeln , ebelp , loekz
      FROM ekpo
      WHERE ebeln IN @s_ebeln
      AND loekz IS INITIAL
      INTO TABLE @DATA(lt_ekpo).

    SELECT ebeln
      FROM eket
      INTO TABLE @DATA(lt_eket)
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE ebeln = @lt_ekpo-ebeln.

    LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<ls_ekpo>) GROUP BY ( ebeln = <ls_ekpo>-ebeln  )
                                                      ASSIGNING FIELD-SYMBOL(<lg_ekpo>).

      READ TABLE lt_eket ASSIGNING FIELD-SYMBOL(<fs_eket>) WITH KEY ebeln = <lg_ekpo>-ebeln .

      DATA(lv_del_ind) = COND #( WHEN sy-subrc = 0
                                 THEN 'S'
                                 ELSE 'L' ).


      LOOP AT GROUP <lg_ekpo> ASSIGNING FIELD-SYMBOL(<ls_item>).

        APPEND VALUE #( item_no = <ls_item>-ebelp
                        no_more_gr = COND #( WHEN lv_del_ind = 'S' THEN abap_true ELSE abap_false )
                        final_inv  = COND #( WHEN lv_del_ind = 'S' THEN abap_true ELSE abap_false )
                        delete_ind = lv_del_ind ) TO lt_items_sag.
        APPEND VALUE #( item_no = <ls_item>-ebelp
                        no_more_gr = COND #( WHEN lv_del_ind = 'S' THEN abap_true ELSE abap_false )
                        final_inv  = COND #( WHEN lv_del_ind = 'S' THEN abap_true ELSE abap_false )
                        delete_ind = abap_true ) TO lt_itemsx_sag.

      ENDLOOP.

      CALL FUNCTION 'BAPI_SAG_CHANGE'
        EXPORTING
          purchasingdocument = <lg_ekpo>-ebeln
        TABLES
          return             = lt_messages
          item               = lt_items_sag
          itemx              = lt_itemsx_sag.


      CLEAR : lt_items_sag, lt_itemsx_sag.

      LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).
        IF <ls_msg>-type CA 'EAX'.
          DATA(lv_error) = abap_true.
        ENDIF.

        CASE <ls_msg>-type.
          WHEN 'S'.
            ms_alv_display-status = icon_green_light.
          WHEN 'W'.
            ms_alv_display-status = icon_yellow_light.    ‘’ ky case zevendesohet me llogjiken poshte
          WHEN 'E'.
            ms_alv_display-status = icon_red_light.
        ENDCASE.

        APPEND VALUE #( ebeln = <lg_ekpo>-ebeln
                        loekz = lv_del_ind
                        msg_type = <Ls_msg>-type
                        msg = <Ls_msg>-message
                        STATUS = SWITCH #( <ls_msg>-type
                                           WHEN 'S' THEN icon_green_light
                                           WHEN 'W' THEN icon_yellow_light
                                           WHEN 'E' THEN icon_red_light )
                      ) TO mt_alv_display.

      ENDLOOP.


      IF lv_error = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR lv_error.
      ENDIF.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDLOOP.

    IF mt_alv_display IS INITIAL .
      MESSAGE 'Data has already been changed'(002) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    display_logs( ).

  ENDMETHOD.                    "extract_data



  METHOD display_logs.

    DATA: lo_salv     TYPE REF TO cl_salv_table,
          lo_column   TYPE REF TO cl_salv_column_table,
          lo_tooltips TYPE REF TO cl_salv_tooltips,
          lt_all_msg  TYPE STANDARD TABLE OF esp1_message_wa_type,
          lv_value    TYPE lvc_value.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = mt_alv_display ).


        lo_salv->get_columns( )->set_optimize( ).
        lo_salv->get_functions( )->set_all( ).

        lo_salv->get_sorts( )->add_sort(
          EXPORTING
            columnname = 'EBELN'
        ).

        lo_salv->get_sorts( )->add_sort(
          EXPORTING
            columnname = 'LOEKZ'
        ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'STATUS' ).
        lo_column->set_medium_text( 'Status' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'EBELN' ).
        lo_column->set_medium_text( 'Purchasing Doc.' ).
        lo_column->set_key( ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'LOEKZ' ).
        lo_column->set_medium_text( 'Deletion ind.' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'MSG' ).
        lo_column->set_medium_text( 'Message' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'MSG_TYPE' ).
        lo_column->set_medium_text( 'Message Type' ).

      CATCH CX_ROOT.

    ENDTRY.

* display the table
    lo_salv->display( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT go_duplicate.
  go_duplicate->execute( ).
