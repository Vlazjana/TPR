



*&---------------------------------------------------------------------*
*& Report  ZBCF_HIER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zbcf_hier.

DATA: BEGIN OF gs_screen0100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen0100.



DATA gv_changed TYPE char1.

DATA: BEGIN OF gs_screen0101,
        ok_code      TYPE sy-ucomm,
        rb_hier      TYPE char1,
        rb_sibling   TYPE char1,
        inp_end_val  TYPE lvc_value,
        inp_tech_val TYPE lvc_value,
      END OF gs_screen0101.
*----------------------------------------------------------------------*
*       CLASS lcl_double_hierarchy DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_double_hierarchy DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_hier_descr,
             hier_val    TYPE bapicharactkey-charactname,
             value_char  TYPE atwrt,
             description TYPE atwtb,
           END OF ty_hier_descr.

    TYPES tt_hier_val TYPE STANDARD TABLE OF ty_hier_descr WITH DEFAULT KEY.

    TYPES ty_hierarchy TYPE zbcf_hier1.

    DATA: BEGIN OF ty_hier.
            INCLUDE TYPE ty_hierarchy.
            DATA: z_descr TYPE lvc_value.
    DATA: delete_ind TYPE char1.
    DATA END OF ty_hier.

    TYPES: ty_hier_data LIKE ty_hier,
           tt_nodes     TYPE TABLE OF ty_hier_data WITH EMPTY KEY.

    TYPES: BEGIN OF ty_disp_nodes,
             tech_value TYPE char30,
             option     TYPE char30,
             end_value  TYPE char30,
             z_descr    TYPE lvc_value,
           END OF ty_disp_nodes.

    TYPES: BEGIN OF ty_folders,
             node_key   TYPE lvc_nkey,
             nodelevel  TYPE i,
             tech_value TYPE char30,
           END OF ty_folders.

    DATA : mo_split_row   TYPE REF TO cl_gui_splitter_container,
           mo_left_tree   TYPE REF TO cl_gui_alv_tree,
           mo_right_tree  TYPE REF TO cl_gui_alv_tree,
           mt_nodes       TYPE tt_nodes,
           mt_left_nodes  TYPE TABLE OF ty_disp_nodes,
           ls_node_layout  TYPE lvc_s_layn,
           mt_right_nodes TYPE TABLE OF ty_disp_nodes.


    CONSTANTS cv_first_nodekey TYPE lvc_nkey VALUE 1.

    METHODS execute.
    METHODS save_data.

  PRIVATE SECTION.
    METHODS extract_data.
    METHODS update_node_icon  IMPORTING
                                iv_nodekey TYPE lvc_nkey
                                iv_sender  TYPE REF TO cl_gui_alv_tree.
    METHODS reset_id.
    METHODS start_container.
    METHODS generate_nodes.
    METHODS display_tree.

    METHODS:
      handle_right_click
                  FOR EVENT node_context_menu_request OF cl_gui_alv_tree
        IMPORTING sender node_key menu ,

      handle_node_context_menu_sel
      FOR EVENT node_context_menu_selected
                  OF cl_gui_alv_tree
        IMPORTING node_key fcode sender,

      handle_item_context_menu_req
                  FOR EVENT item_context_menu_request OF cl_gui_alv_tree
        IMPORTING node_key  menu sender,

      handle_item_context_menu_sel
                  FOR EVENT item_context_menu_selected OF cl_gui_alv_tree
        IMPORTING node_key fcode sender.

    METHODS handle_menu
      IMPORTING
        menu     TYPE REF TO cl_ctmenu
        node_key TYPE  lvc_nkey
        sender   TYPE REF TO cl_gui_alv_tree.

    METHODS handle_menu_fcode
      IMPORTING
        fcode    TYPE sy-ucomm
        node_key TYPE lvc_nkey
        sender   TYPE REF TO cl_gui_alv_tree.

    METHODS get_hierarchy_vals
      IMPORTING iv_charactname TYPE bapicharactkey-charactname
      EXPORTING ev_descr       TYPE bapicharactdescr-description
                et_hier_val    TYPE tt_hier_val.

ENDCLASS.

DATA go_double_hierarchy TYPE REF TO lcl_double_hierarchy.

INCLUDE zbcf_hier_status_100.
INCLUDE zbcf_hier_user_command_100.
*----------------------------------------------------------------------*
*       CLASS lcl_double_hierarchy IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_double_hierarchy IMPLEMENTATION.

  METHOD execute.
    start_container( ).
    display_tree( ).
    CALL SCREEN 100.
  ENDMETHOD.                    "execute

  METHOD update_node_icon.
    DATA: ls_node_layout TYPE lvc_s_lacn,
          ls_outtab_line TYPE ty_disp_nodes.

    " Get current node data
    iv_sender->get_outtab_line(
      EXPORTING i_node_key = iv_nodekey
      IMPORTING e_outtab_line = ls_outtab_line
    ).

    " Determine icon based on whether node has children
    iv_sender->get_subtree(
      EXPORTING i_node_key = iv_nodekey
      IMPORTING et_subtree_nodes = DATA(lt_children)
    ).

    IF lt_children IS NOT INITIAL.
      " Node has children - show folder icon
      ls_node_layout-exp_image = icon_characteristics_hier.
      ls_node_layout-n_image = icon_characteristics_hier.
    ELSE.
      " Node is a leaf - show value chain icon
      ls_node_layout-exp_image = icon_businav_value_chain.
      ls_node_layout-n_image = icon_businav_value_chain.
    ENDIF.

    " Update the node's layout using CHANGE_NODE
    iv_sender->change_node(
      EXPORTING
        i_node_key     = iv_nodekey
        i_outtab_line = ls_outtab_line
        is_node_layout = ls_node_layout
    ).

    iv_sender->frontend_update( ).
  ENDMETHOD.
  METHOD  save_data.

    IF mt_nodes IS NOT INITIAL.
      reset_id( ).
    ENDIF.

    DELETE FROM zbcf_hier1.
    INSERT zbcf_hier1 FROM TABLE mt_nodes.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE 'Error while saving data' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    MESSAGE 'Data save successfully' TYPE 'S'.
    gv_changed = abap_false.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      <ls_nodes>-id = |{ <ls_nodes>-id ALPHA = IN }|.
    ENDLOOP.

  ENDMETHOD.

  METHOD extract_data.

    SELECT *
      FROM zbcf_hier1
      INTO CORRESPONDING FIELDS OF TABLE mt_nodes.
    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      <ls_nodes>-id = |{ <ls_nodes>-id ALPHA = IN }|.
    ENDLOOP.
    SORT mt_nodes BY charact id.

  ENDMETHOD.                    "extract_data

  METHOD  reset_id.

    DATA: lv_old_char TYPE char30.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      IF lv_old_char <> <ls_nodes>-charact.
        DATA(lv_new_id) = 0.
      ENDIF.
      <ls_nodes>-id = lv_new_id + 1.
      lv_new_id     = lv_new_id + 1.
      lv_old_char   = <ls_nodes>-charact.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_hierarchy_vals.
    DATA: lt_return     TYPE STANDARD TABLE OF bapiret2,
          lt_descr      TYPE STANDARD TABLE OF bapicharactdescr,
          lt_hier_descr TYPE STANDARD TABLE OF bapicharactvaluesdescr.

    CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
      EXPORTING
        charactname        = iv_charactname
        language           = 'I'
      TABLES
        charactdescr       = lt_descr
        charactvaluesdescr = lt_hier_descr
        return             = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF lt_descr IS NOT INITIAL.
      ev_descr = lt_descr[ 1 ]-description.
    ENDIF.

    LOOP AT lt_hier_descr ASSIGNING FIELD-SYMBOL(<ls_hier_descr>).
      APPEND VALUE #(
       hier_val    = iv_charactname
       value_char  = <ls_hier_descr>-value_char
       description = <ls_hier_descr>-description
       ) TO et_hier_val.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_nodes.

    DATA : lv_parent      TYPE lvc_nkey,
           lt_folders     TYPE STANDARD TABLE OF ty_folders,
           lv_node_key    TYPE lvc_nkey,
           ls_node_layout  TYPE lvc_s_layn.

    DATA(lt_all_values) = VALUE tt_hier_val( ).

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>) WHERE opti IS INITIAL.
      get_hierarchy_vals(
        EXPORTING
          iv_charactname = CONV #( <ls_nodes>-tech_value )
        IMPORTING
          ev_descr       = DATA(lv_descr)
          et_hier_val    = DATA(lt_hier)
            ).

      IF lv_descr IS NOT INITIAL .
        <ls_nodes>-z_descr = lv_descr.
      ELSE.
        <ls_nodes>-z_descr = <ls_nodes>-tech_value.
      ENDIF.

      APPEND LINES OF lt_hier TO lt_all_values.
      CLEAR: lv_descr, lt_hier.
    ENDLOOP.

    SORT lt_all_values BY hier_val value_char.
    DELETE ADJACENT DUPLICATES FROM lt_all_values COMPARING hier_val value_char.

    LOOP AT mt_nodes ASSIGNING <ls_nodes>.
      DATA(lv_next_tabix) = sy-tabix + 1.

      CLEAR lv_parent.

      DELETE lt_folders WHERE nodelevel >= <ls_nodes>-node_level.
      READ TABLE lt_folders ASSIGNING FIELD-SYMBOL(<ls_folder>) INDEX 1.
      IF sy-subrc = 0.
        lv_parent = <ls_folder>-node_key.
        DATA(lv_parent_val) = <ls_folder>-tech_value.
      ENDIF.

      CLEAR ls_node_layout.

      READ TABLE mt_nodes ASSIGNING FIELD-SYMBOL(<ls_next_node>) INDEX lv_next_tabix.
      IF sy-subrc = 0 AND <ls_next_node>-node_level <= <ls_nodes>-node_level.
        IF <ls_nodes>-opti IS NOT INITIAL.
          ls_node_layout-exp_image = icon_businav_value_chain.
          ls_node_layout-n_image   = icon_businav_value_chain.
        ENDIF.
      ENDIF.

      IF <ls_nodes>-opti IS INITIAL.
        ls_node_layout-exp_image = icon_characteristics_hier.
        ls_node_layout-n_image   = icon_characteristics_hier.
      ENDIF.

      IF <ls_nodes>-z_descr IS INITIAL.
        READ TABLE lt_all_values ASSIGNING FIELD-SYMBOL(<ls_hier>)
          WITH KEY hier_val   = lv_parent_val
                   value_char = <ls_nodes>-tech_value BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_nodes>-z_descr = <ls_hier>-description.
        ELSE.
          <ls_nodes>-z_descr = <ls_nodes>-tech_value.
        ENDIF.
      ENDIF.



      DATA(ls_data) = VALUE ty_disp_nodes(
        tech_value = <ls_nodes>-tech_value
        end_value  = <ls_nodes>-end_value
        option     = SWITCH #( <ls_nodes>-opti WHEN 'CP' THEN icon_pattern_include_green WHEN 'EQ' THEN icon_equal_green )  ).

      TRY.
          CASE <ls_nodes>-charact.
            WHEN 'ATWRT1'.
              mo_left_tree->add_node(
                EXPORTING
                  i_relat_node_key     = lv_parent
                  is_outtab_line       = ls_data
                  i_relationship       = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )
                  i_node_text          = <ls_nodes>-z_descr
                  is_node_layout       = ls_node_layout
                IMPORTING
                  e_new_node_key       = lv_node_key
                EXCEPTIONS
                  relat_node_not_found = 1
                  node_not_found       = 2
                  OTHERS               = 3 ).

            WHEN 'ATWRT2'.
              mo_right_tree->add_node(
                EXPORTING
                  i_relat_node_key     = lv_parent
                  is_outtab_line       = ls_data
                  i_relationship       = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )
                  i_node_text          = <ls_nodes>-z_descr
                  is_node_layout       = ls_node_layout
                IMPORTING
                  e_new_node_key       = lv_node_key
                EXCEPTIONS
                  relat_node_not_found = 1
                  node_not_found       = 2
                  OTHERS               = 3 ).
          ENDCASE.

          INSERT VALUE #( node_key   = lv_node_key
                          nodelevel  = <ls_nodes>-node_level
                          tech_value = <ls_nodes>-tech_value
                          ) INTO lt_folders INDEX 1.

        CATCH cx_salv_msg.
      ENDTRY.

      CLEAR ls_data.
    ENDLOOP.
  ENDMETHOD.

  METHOD  display_tree.

    DATA lt_event TYPE cntl_simple_events.
    DATA(ls_header) = VALUE treev_hhdr( heading   = 'Description' tooltip   = 'Description' width     = 40 width_pix = '' ).
    DATA(lt_fcat) = VALUE lvc_t_fcat( ( fieldname = 'OPTION'      scrtext_s = 'Option'      icon = abap_true )
                                      ( fieldname = 'TECH_VALUE'  scrtext_s = 'Tech.Value'  outputlen = 30  )
                                      ( fieldname = 'END_VALUE'   scrtext_s = 'End.Value'   outputlen = 30 )
                                       ).

    extract_data( ).

    mo_left_tree->set_table_for_first_display(
    EXPORTING
      is_hierarchy_header = ls_header
    CHANGING
      it_outtab           = mt_left_nodes
      it_fieldcatalog     = lt_fcat
      ).

    mo_right_tree->set_table_for_first_display(
    EXPORTING
      is_hierarchy_header = ls_header
    CHANGING
      it_outtab           = mt_right_nodes
      it_fieldcatalog     = lt_fcat
      ).

    mo_left_tree->get_registered_events(
      IMPORTING
        events     = lt_event    " simple_events
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lt_event = VALUE #( BASE lt_event
    ( eventid = cl_gui_column_tree=>eventid_header_context_men_req )
    ( eventid = cl_gui_column_tree=>eventid_node_context_menu_req )
    ( eventid = cl_gui_column_tree=>eventid_item_context_menu_req )
    ).


    mo_left_tree->set_registered_events(
     EXPORTING
       events                    = lt_event
     EXCEPTIONS
       cntl_error                = 1
       cntl_system_error         = 2
       illegal_event_combination = 3
       OTHERS                    = 4
       ).

    SET HANDLER: handle_right_click
                 handle_node_context_menu_sel
                 handle_item_context_menu_req
                 handle_item_context_menu_sel  FOR mo_left_tree.


    mo_right_tree->set_registered_events(
     EXPORTING
       events                    = lt_event
     EXCEPTIONS
       cntl_error                = 1
       cntl_system_error         = 2
       illegal_event_combination = 3
       OTHERS                    = 4
       ).

    SET HANDLER: handle_right_click
                 handle_node_context_menu_sel
                 handle_item_context_menu_req
                 handle_item_context_menu_sel  FOR mo_right_tree.

    generate_nodes( ).
    mo_left_tree->frontend_update( ).
    mo_right_tree->frontend_update( ).

    mo_left_tree->expand_node(
      EXPORTING
        i_node_key          = cv_first_nodekey    " Node Key
      EXCEPTIONS
        failed              = 1
        illegal_level_count = 2
        cntl_system_error   = 3
        node_not_found      = 4
        cannot_expand_leaf  = 5
        OTHERS              = 6
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_right_tree->expand_node(
      EXPORTING
        i_node_key          = cv_first_nodekey    " Node Key
      EXCEPTIONS
        failed              = 1
        illegal_level_count = 2
        cntl_system_error   = 3
        node_not_found      = 4
        cannot_expand_leaf  = 5
        OTHERS              = 6
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD start_container.

    DATA: lo_cont TYPE REF TO cl_gui_custom_container.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CUSTOM_CONT100'
        repid                       = sy-repid
        dynnr                       = '0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_split_row
      EXPORTING
        parent            = lo_cont
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    mo_split_row->set_column_width(
      EXPORTING
        id                =   1
        width             =   50
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

    mo_split_row->set_row_sash( id    = 1
                                type  = cl_gui_splitter_container=>type_sashvisible
                                value = cl_gui_splitter_container=>true ).

    CREATE OBJECT mo_left_tree
      EXPORTING
        parent                      = mo_split_row->get_container( row = 1 column = 1 )
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_toolbar                  = ' '
        no_html_header              = 'X'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.

    CREATE OBJECT mo_right_tree
      EXPORTING
        parent                      = mo_split_row->get_container( row = 1 column = 2 )
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_toolbar                  = ' '
        no_html_header              = 'X'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.
  ENDMETHOD.                    "start_container

  METHOD  handle_node_context_menu_sel.

    handle_menu_fcode(
      EXPORTING
        fcode    = fcode
        node_key = node_key
        sender   = sender
    ).

  ENDMETHOD.
  METHOD  handle_menu_fcode.

    DATA: lv_answer   TYPE char1,
          lv_node_key TYPE lvc_nkey,
          ls_line     TYPE ty_disp_nodes.

    DATA(lv_id) = |{  node_key ALPHA = IN } |.

    CASE sender.
      WHEN mo_left_tree.
        DATA(lv_char) = 'ATWRT1'.
      WHEN mo_right_tree.
        lv_char = 'ATWRT2'.
    ENDCASE.


    READ TABLE mt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>) WITH KEY id = lv_id charact = lv_char.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_row_index) = sy-tabix.


    CASE fcode.
      WHEN 'INSERT_NODE'.

        DATA(lt_node_fields) = VALUE ty_sval(
          ( tabname = 'ZBCF_HIER1' fieldname = 'TECH_VALUE' value = '' fieldtext = 'Technical Value' field_obl = 'X' )
          ( tabname = 'ZBCF_HIER1' fieldname = 'END_VALUE' value = '' fieldtext = 'End Value' )
          ( tabname = 'ZBCF_HIER1' fieldname = 'OPTI' value = 'EQ' fieldtext = 'Operation (EQ/CP)' field_obl = 'X' ) ).

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title     = 'Insert New Node'
            start_column    = 5
            start_row       = 5
          IMPORTING
            returncode      = lv_answer
          TABLES
            fields          = lt_node_fields
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.

        IF lv_answer = 'A' OR sy-subrc <> 0.
          RETURN.
        ENDIF.

        " Get values from popup
        DATA(lv_tech_val) = lt_node_fields[ 1 ]-value.
        DATA(lv_end_val)  = lt_node_fields[ 2 ]-value.
        DATA(lv_opti)     = lt_node_fields[ 3 ]-value.

        " Validate option
        IF lv_opti <> 'EQ' AND lv_opti <> 'CP'.
          MESSAGE 'Invalid operation - must be EQ or CP' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        " Check for duplicate technical values

        sender->get_children(
          EXPORTING
            i_node_key         = node_key    " Parent Node Key
          IMPORTING
            et_children        = DATA(lt_children)   " Node Key Table of Children
          EXCEPTIONS
            historic_error     = 1
            node_key_not_found = 2
            OTHERS             = 3
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<lv_child>).
          sender->get_outtab_line(
            EXPORTING
              i_node_key      = <lv_child>    " Node Key
             IMPORTING
               e_outtab_line  = ls_line    " Line of Outtab
             EXCEPTIONS
               node_not_found = 1
               OTHERS         = 2
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF ls_line-tech_value = lv_tech_val.
            DATA(lv_dupl) = abap_true.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF lv_dupl IS NOT INITIAL.
          MESSAGE |Technical value { lv_tech_val } already exists at this level!| TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        sender->get_outtab_line(
          EXPORTING
            i_node_key      = node_key    " Node Key
           IMPORTING
             e_outtab_line  = ls_line    " Line of Outtab
           EXCEPTIONS
             node_not_found = 1
             OTHERS         = 2
        ).

        " Get hierarchy description and values
        get_hierarchy_vals(
          EXPORTING
            iv_charactname = CONV #( <ls_node>-tech_value )
          IMPORTING
            et_hier_val    = DATA(lt_hier_values)
        ).

        READ TABLE lt_hier_values INTO DATA(ls_hier_val) WITH KEY value_char = lv_tech_val.

        " Create and add the new node
        DATA(ls_new_node) = VALUE ty_hier_data(
          tech_value = lv_tech_val
          end_value  = lv_end_val
          opti       = lv_opti
          z_descr    =  COND #( WHEN ls_hier_val-description IS NOT INITIAL
                        THEN ls_hier_val-description
                        ELSE lv_tech_val )
          charact    = lv_char
          node_level = <ls_node>-node_level + 1
        ).

        DATA(ls_disp_node) = VALUE ty_disp_nodes(
          tech_value = ls_new_node-tech_value
          end_value  = ls_new_node-end_value
          option     = SWITCH #( ls_new_node-opti
                               WHEN 'CP' THEN icon_pattern_include_green
                               WHEN 'EQ' THEN icon_equal_green )
        ).

        DATA(ls_layout) = VALUE lvc_s_layn(
          exp_image = icon_businav_value_chain
          n_image   = icon_businav_value_chain
        ).

        " Add node to tree
        sender->add_node(
          EXPORTING
            i_relat_node_key = node_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            is_outtab_line   = ls_disp_node
            i_node_text      = ls_new_node-z_descr
            is_node_layout   = ls_layout
          IMPORTING
            e_new_node_key   = DATA(lv_new_key)
          EXCEPTIONS
            OTHERS           = 1
        ).
        IF sy-subrc <> 0.
          MESSAGE 'Failed to add node' TYPE 'E'.
          RETURN.
        ENDIF.


        ls_new_node-id = |{ lv_new_key ALPHA = IN }|.
        INSERT ls_new_node INTO mt_nodes INDEX lv_row_index + 1.
        gv_changed = abap_true.

      WHEN 'INSERT_HIER'.

        DATA(lt_hier_fields) = VALUE ty_sval(
           ( tabname = 'ZBCF_HIER1' fieldname = 'TECH_VALUE' value = '' fieldtext = 'Technical Value' )
           ( tabname = 'ZBCF_HIER1' fieldname = 'END_VALUE' value = '' fieldtext = 'End Value' )
         ).

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title     = 'Insert New Hierarchy'
            start_column    = 5
            start_row       = 5
          IMPORTING
            returncode      = lv_answer
          TABLES
            fields          = lt_hier_fields
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.

        IF lv_answer = 'A' OR sy-subrc <> 0.
          RETURN.
        ENDIF.


        lv_tech_val = lt_hier_fields[ 1 ]-value.
        lv_end_val  = lt_hier_fields[ 2 ]-value.


        DATA(lv_current_level) = <ls_node>-node_level + 1.
        READ TABLE mt_nodes WITH KEY charact = lv_char
                                     node_level = lv_current_level
                                     tech_value = lv_tech_val
                                     TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          MESSAGE |Technical value { lv_tech_val } already exists at this level!| TYPE'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        get_hierarchy_vals(
          EXPORTING
            iv_charactname = CONV #( lv_tech_val )
          IMPORTING
            et_hier_val    = lt_hier_values
        ).

        ls_new_node = VALUE ty_hier_data(
          tech_value = lv_tech_val
          end_value  = lv_end_val
          charact    = lv_char
          node_level = lv_current_level
        ).


        ls_disp_node = VALUE ty_disp_nodes(
          tech_value = ls_new_node-tech_value
          end_value  = ls_new_node-end_value
          option     = ''
        ).

        ls_layout = VALUE lvc_s_layn(
          exp_image = icon_characteristics_hier
          n_image   = icon_characteristics_hier
        ).


        sender->add_node(
          EXPORTING
            i_relat_node_key = node_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            is_outtab_line   = ls_disp_node
            i_node_text      = ls_new_node-z_descr
            is_node_layout   = ls_layout
          IMPORTING
            e_new_node_key   = lv_new_key
          EXCEPTIONS
            OTHERS           = 1
        ).
        IF sy-subrc <> 0.
          MESSAGE 'Failed to add hierarchy node' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.


        ls_new_node-id = |{ lv_new_key ALPHA = IN }|.
        DATA(lv_insert_index) = lv_row_index + 1.
        INSERT ls_new_node INTO mt_nodes INDEX lv_insert_index.
        ADD 1 TO lv_insert_index.

        IF lt_hier_values IS NOT INITIAL.
          LOOP AT lt_hier_values ASSIGNING FIELD-SYMBOL(<ls_child>).
            DATA(ls_child_node) = VALUE ty_hier_data(
              tech_value = <ls_child>-value_char
              end_value  = lv_end_val
              z_descr    = <ls_child>-description
              opti       = 'EQ'
              charact    = lv_char
              node_level = ls_new_node-node_level + 1
            ).

            ls_disp_node = VALUE ty_disp_nodes(
              tech_value = ls_child_node-tech_value
              end_value  = ls_child_node-end_value
              option     = icon_equal_green
            ).

            ls_layout = VALUE lvc_s_layn(
              exp_image = icon_businav_value_chain
              n_image   = icon_businav_value_chain
            ).


            sender->add_node(
              EXPORTING
                i_relat_node_key = lv_new_key
                i_relationship   = cl_gui_column_tree=>relat_last_child
                is_outtab_line   = ls_disp_node
                i_node_text      = ls_child_node-z_descr
                is_node_layout   = ls_layout
              IMPORTING
                e_new_node_key   = DATA(lv_child_key)
              EXCEPTIONS
                OTHERS           = 1
            ).
            IF sy-subrc <> 0.
              MESSAGE 'Failed to add child node' TYPE 'W'.
              CONTINUE.
            ENDIF.

            ls_child_node-id = |{ lv_child_key ALPHA = IN }|.
            INSERT ls_child_node INTO mt_nodes INDEX lv_insert_index.
            ADD 1 TO lv_insert_index.
          ENDLOOP.
        ENDIF.

        gv_changed = abap_true.

      WHEN 'DELETE'.

        DATA deleted_tab TYPE lvc_t_nkey.
        DATA(lv_confirm) = abap_false.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirm Deletion'
            text_question         = 'Are you sure you want to delete this node and all its children?'
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            default_button        = '2'
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_confirm.

        IF lv_confirm <> '1'.
          RETURN.
        ENDIF.
        sender->get_subtree(
          EXPORTING
            i_node_key         =    node_key
          IMPORTING
            et_subtree_nodes   =     deleted_tab
          EXCEPTIONS
            node_key_not_found = 1
            OTHERS             = 2 ).

        sender->delete_subtree(
          EXPORTING
            i_node_key                =  node_key
            i_update_parents_expander = 'X'
            i_update_parents_folder   = 'X'
          EXCEPTIONS
            node_key_not_in_model     = 1
            OTHERS                    = 2  ).

        LOOP AT deleted_tab ASSIGNING FIELD-SYMBOL(<ls_deleted>).
          DELETE mt_nodes WHERE id =  |{ <ls_deleted> ALPHA = IN }| AND charact = lv_char.
        ENDLOOP.
        gv_changed = abap_true.


      WHEN 'MODIFY'.
        IF <ls_node>-opti IS INITIAL. " Hierarchy node
          DATA(lt_values) = VALUE ty_sval(
            ( tabname = 'ZBCF_HIER1' fieldname = 'TECH_VALUE' value = <ls_node>-tech_value fieldtext = 'Technical Value' field_obl = 'X' )
            ( tabname = 'ZBCF_HIER1' fieldname = 'END_VALUE' value = <ls_node>-end_value fieldtext = 'End Value' )
          ).
        ELSE.
          lt_values = VALUE ty_sval(
            ( tabname = 'ZBCF_HIER1' fieldname = 'TECH_VALUE' value = <ls_node>-tech_value fieldtext = 'Technical Value' field_obl = 'X' )
            ( tabname = 'ZBCF_HIER1' fieldname = 'END_VALUE' value = <ls_node>-end_value fieldtext = 'End Value' )
            ( tabname = 'ZBCF_HIER1' fieldname = 'OPTI' value = <ls_node>-opti fieldtext = 'Operation (EQ/CP)' field_obl = 'X' )
          ).
        ENDIF.

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title     = 'Modify Node'
            start_column    = '5'
            start_row       = '5'
          IMPORTING
            returncode      = lv_answer
          TABLES
            fields          = lt_values
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.

        IF lv_answer = 'A' OR sy-subrc <> 0.
          RETURN.
        ENDIF.

        IF node_key = cv_first_nodekey.
          MESSAGE 'You cannot modify the first node.' TYPE 'I'.
          RETURN.
        ENDIF.

        " Store new values
        DATA(lv_new_tech_val) = lt_values[ 1 ]-value.
        DATA(lv_new_end_val) = lt_values[ 2 ]-value.
        DATA(lv_new_opti) = COND #( WHEN <ls_node>-opti IS NOT INITIAL THEN lt_values[ 3 ]-value ELSE '' ).


        IF <ls_node>-opti IS INITIAL AND lv_new_tech_val <> <ls_node>-tech_value.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirm Change'
              text_question         = 'Changing technical value will update the description and children. Continue?'
              text_button_1         = 'Yes'
              text_button_2         = 'No'
              default_button        = '2'
              display_cancel_button = ' '
            IMPORTING
              answer                = lv_confirm.

          IF lv_confirm <> '1'.
            RETURN.
          ENDIF.


          get_hierarchy_vals(
            EXPORTING
              iv_charactname = CONV #( lv_new_tech_val )
            IMPORTING
              ev_descr       = DATA(lv_new_descr)
              et_hier_val    = DATA(lt_child_values)
          ).


          lt_children = VALUE lvc_t_nkey( ).
          sender->get_subtree(
            EXPORTING
              i_node_key       = node_key
            IMPORTING
              et_subtree_nodes = lt_children
          ).

          DELETE lt_children INDEX 1.

          LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<lv_child_key>).
            CALL METHOD sender->delete_subtree
              EXPORTING
                i_node_key                = <lv_child_key>
                i_update_parents_expander = abap_true
                i_update_parents_folder   = abap_true
              EXCEPTIONS
                node_key_not_in_model     = 1
                OTHERS                    = 2.
            IF sy-subrc = 0.
              DELETE mt_nodes WHERE id = |{ <lv_child_key> ALPHA = IN }| AND charact = lv_char.
            ENDIF.
          ENDLOOP.

          <ls_node>-tech_value = lv_new_tech_val.
          <ls_node>-end_value = lv_new_end_val.
          <ls_node>-z_descr = COND #( WHEN lv_new_descr IS NOT INITIAL
                                     THEN lv_new_descr
                                     ELSE lv_new_tech_val ).

          IF lt_child_values IS NOT INITIAL.
            DATA(lv_child_index) = lv_row_index + 1.

            LOOP AT lt_child_values ASSIGNING <ls_child>.
              ls_child_node = VALUE ty_hier_data(
                 tech_value = <ls_child>-value_char
                 end_value  = lv_new_end_val
                 z_descr    = <ls_child>-description
                 opti       = 'EQ'
                 charact    = lv_char
                 node_level = <ls_node>-node_level + 1
               ).

              DATA(ls_child_disp) = VALUE ty_disp_nodes(
                tech_value = ls_child_node-tech_value
                end_value  = ls_child_node-end_value
                option     = icon_equal_green
              ).

              DATA(ls_child_layout) = VALUE lvc_s_layn(
                exp_image = icon_businav_value_chain
                n_image   = icon_businav_value_chain
              ).


              sender->add_node(
                EXPORTING
                  i_relat_node_key = node_key
                  i_relationship   = cl_gui_column_tree=>relat_last_child
                  is_outtab_line   = ls_child_disp
                  i_node_text      = ls_child_node-z_descr
                  is_node_layout   = ls_child_layout
                IMPORTING
                  e_new_node_key   = lv_child_key
              ).

              ls_child_node-id = |{ lv_child_key ALPHA = IN }|.
              INSERT ls_child_node INTO mt_nodes INDEX lv_child_index.
              ADD 1 TO lv_child_index.
            ENDLOOP.
          ENDIF.
        ELSE.

          <ls_node>-tech_value = lv_new_tech_val.
          <ls_node>-end_value = lv_new_end_val.


          IF <ls_node>-opti IS NOT INITIAL.
            <ls_node>-opti = lv_new_opti.
          ENDIF.
        ENDIF.


        ls_disp_node = VALUE ty_disp_nodes(
          tech_value = <ls_node>-tech_value
          end_value  = <ls_node>-end_value
          option     = SWITCH #( <ls_node>-opti
                               WHEN 'CP' THEN icon_pattern_include_green
                               WHEN 'EQ' THEN icon_equal_green
                               ELSE '' )
          z_descr    = <ls_node>-z_descr
        ).


        sender->change_node(
          EXPORTING
            i_node_key     = node_key
            i_node_text    = <ls_node>-z_descr
            i_outtab_line  = ls_disp_node
            is_node_layout = VALUE #(
              exp_image = COND #( WHEN <ls_node>-opti IS INITIAL
                                 THEN icon_characteristics_hier
                                 ELSE icon_businav_value_chain )
              n_image   = COND #( WHEN <ls_node>-opti IS INITIAL
                                 THEN icon_characteristics_hier
                                 ELSE icon_businav_value_chain )
            )
          EXCEPTIONS
            node_not_found = 1
            OTHERS         = 2
        ).


        IF <ls_node>-opti IS NOT INITIAL.
          sender->change_item(
            EXPORTING
              i_node_key     = node_key
              i_fieldname    = 'OPTION'
              i_data         = ls_disp_node-option
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2
          ).
        ENDIF.

        gv_changed = abap_true.
        sender->frontend_update( ).

    ENDCASE.

    sender->frontend_update( ).

  ENDMETHOD.

  METHOD  handle_right_click.

    handle_menu(
      EXPORTING
        menu     = menu
        node_key = node_key
        sender   = sender
    ).

  ENDMETHOD.

  METHOD  handle_item_context_menu_req.

    handle_menu(
      EXPORTING
        menu     = menu
        node_key = node_key
        sender   = sender
    ).
  ENDMETHOD.
  METHOD handle_menu.

    DATA ls_line TYPE ty_disp_nodes.

    sender->get_outtab_line(
       EXPORTING i_node_key = node_key
       IMPORTING e_outtab_line = ls_line
     ).

    IF ls_line-option IS NOT INITIAL.
      menu->add_function( fcode = 'INSERT_HIER' text  = 'Insert Hierarchy').
    ELSE.
      menu->add_function( fcode = 'INSERT_NODE' text  = 'Insert Node').
    ENDIF.

    menu->add_function( fcode = 'MODIFY' text  = 'Modify' ).

    IF node_key <> cv_first_nodekey.
      menu->add_separator( ).
      menu->add_function( fcode = 'DELETE' text  = 'Delete').
    ENDIF.

  ENDMETHOD.

  METHOD  handle_item_context_menu_sel.

    handle_menu_fcode(
      EXPORTING
        fcode    = fcode
        node_key = node_key
        sender   = sender
    ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT go_double_hierarchy.
  go_double_hierarchy->execute( ).








FM

   Get key to get MRP profile & Prod.sched.
*----------------------------------------------------------------------*
FORM get_key_mrpp_ps.

  CLEAR: v_atwrt01,
         v_atwrt02.


  CALL FUNCTION 'Z_MM_EU_BCF_V0'
    EXPORTING
      matnr     = it_seldata-matnr
      cuobf     = it_seldata-cuobf
      werks     = it_seldata-werks
*     AUFNR     =
    IMPORTING
      atwrt_out = v_atwrt01.
  .



  CALL FUNCTION 'Z_MM_EU_BCF_V01'
    EXPORTING
      matnr     = it_seldata-matnr
      cuobf     = it_seldata-cuobf
      werks     = it_seldata-werks
*     AUFNR     =
    IMPORTING
      atwrt_out = v_atwrt02.

  PERFORM get_prod_sched_01.
  "Se non trovo il valore FEVOR, mostro il warning
  IF zmm_eu_bcf_001-fevor IS INITIAL AND gv_warn_once IS INITIAL.
    MESSAGE 'Responsabile di schedulazione produzione non identificato'
            TYPE 'I' DISPLAY LIKE 'W'.
    gv_warn_once = abap_true.
    RETURN.
  ENDIF.



*  CALL FUNCTION 'ZZ_MM_EU_BCF_000'
*    EXPORTING
*      matnr     = it_seldata-matnr
*      cuobf     = it_seldata-cuobf
*      werks     = it_seldata-werks
**     AUFNR     =
*    IMPORTING
*      atwrt_out = v_atwrt01.

***  CALL FUNCTION 'Z_MM_EU_BCF_001'
***    EXPORTING
***      matnr     = it_seldata-matnr
***      cuobf     = it_seldata-cuobf
***      werks     = it_seldata-werks
****     AUFNR     =
***    IMPORTING
***      atwrt_out = v_atwrt02.

ENDFORM.                    " GET_KEY_MRPP_PS

*&---------------------------------------------------------------------*
*&      Form  GET_PROD_SCHED_01
*&---------------------------------------------------------------------*
*       Get production scheduler
*----------------------------------------------------------------------*
FORM get_prod_sched_01.

  CLEAR zmm_eu_bcf_001.

  SELECT SINGLE *
    FROM zmm_eu_bcf_001
    WHERE atwrt01 = v_atwrt01 AND
          atwrt02 = v_atwrt02.

*if zmm_eu_bcf_001-fevor is INITIAL.
*   MESSAGE 'Production scheduler not identified' TYPE 'I' DISPLAY LIKE 'W'.
*  endif.

ENDFORM.

—------
 FUNCTION z_mm_eu_bcf_v0.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(MATNR) TYPE  MATNR
*"     REFERENCE(CUOBF) TYPE  CUOBM
*"     REFERENCE(WERKS) TYPE  WERKS_D
*"     REFERENCE(AUFNR) TYPE  AUFNR OPTIONAL
*"  EXPORTING
*"     VALUE(ATWRT_OUT) TYPE  ATWRT
*"----------------------------------------------------------------------

   PERFORM init.

   CALL FUNCTION 'Z_PP_ESTR_CARATT'
     EXPORTING
       instance  = cuobf
       matnr     = matnr
       werks     = werks
       aufnr     = aufnr
     TABLES
       tb_caratt = gt_caratt.

   SELECT *
     FROM zbcf_hier1
     INTO TABLE @gt_hier
     WHERE charact = 'ATWRT1'
     ORDER BY id.

   READ TABLE gt_hier INTO DATA(ls_atwrt1) INDEX 1.

   PERFORM get_hierch_atwrt_out USING cv_root_id '' CHANGING atwrt_out.

   IF atwrt_out IS INITIAL.
     atwrt_out = ls_atwrt1-end_value.
   ENDIF.

 ENDFUNCTION.   

*----------------------------------------------------------------------*
***INCLUDE LZBC_HIER1F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_HIERCH_ATWRT_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LS_HIER>_ID  text
*      <--P_ATWRT_OUT  text
*----------------------------------------------------------------------*
FORM get_hierch_atwrt_out  USING    uv_id           TYPE zbcf_hier1-id
                                    uv_caratt_val   TYPE atwrt
                           CHANGING cv_atwrt_out    TYPE atwrt.

  READ TABLE gt_hier ASSIGNING FIELD-SYMBOL(<ls_hier>) WITH KEY id = uv_id BINARY SEARCH.
  DATA(lv_tabix) = sy-tabix + 1.
  DATA(lv_child_node_level) = <ls_hier>-node_level + 1.

  LOOP AT gt_hier INTO DATA(ls_child) FROM lv_tabix.
    IF ls_child-node_level <= <ls_hier>-node_level.
      EXIT.
    ENDIF.

    IF ls_child-node_level <> lv_child_node_level.
      CONTINUE.
    ENDIF.

* if we found a characteristic and the value of the child is not it, skip it
    IF uv_caratt_val IS NOT INITIAL.
      IF lv_child_node_level <> ls_child-node_level.
        CONTINUE.
      ENDIF.

      CASE ls_child-opti.
        WHEN 'EQ'.
          IF ls_child-tech_value <> uv_caratt_val.
            CONTINUE.
          ENDIF.
        WHEN 'CP'.
          IF uv_caratt_val NP ls_child-tech_value.
            CONTINUE.
          ENDIF.

      ENDCASE.

    ENDIF.

    lv_tabix = sy-tabix + 1.

    READ TABLE gt_hier INTO DATA(ls_next_node) INDEX lv_tabix.
    IF sy-subrc = 0.
      IF ls_next_node-node_level > ls_child-node_level.
* it is a hierarchy; check values inside

        IF ls_child-opti IS INITIAL.
          READ TABLE gt_caratt INTO DATA(ls_caratt) WITH KEY atnam = ls_child-tech_value.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          PERFORM get_hierch_atwrt_out
            USING
              ls_child-id
              ls_caratt-atwrt
            CHANGING
              cv_atwrt_out.

        ELSE.
          PERFORM get_hierch_atwrt_out
            USING
              ls_child-id
              ''
            CHANGING
              cv_atwrt_out.

        ENDIF.

      ENDIF.
    ENDIF.

    IF  cv_atwrt_out IS INITIAL
      AND ls_child-end_value IS NOT INITIAL.

      cv_atwrt_out = ls_child-end_value.
    ENDIF.

    IF cv_atwrt_out IS NOT INITIAL.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM init.
  CLEAR: gt_caratt,
         gt_hier.

ENDFORM.


—-----------
  FUNCTION z_mm_eu_bcf_v01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(MATNR) TYPE  MATNR
*"     REFERENCE(CUOBF) TYPE  CUOBM
*"     REFERENCE(WERKS) TYPE  WERKS_D
*"     REFERENCE(AUFNR) TYPE  AUFNR OPTIONAL
*"  EXPORTING
*"     VALUE(ATWRT_OUT) TYPE  ATWRT
*"----------------------------------------------------------------------
    DATA lv_atwrt1 TYPE atwrt.

    PERFORM init.

    CALL FUNCTION 'Z_PP_ESTR_CARATT'
      EXPORTING
        instance  = cuobf
        matnr     = matnr
        werks     = werks
        aufnr     = aufnr
      TABLES
        tb_caratt = gt_caratt.

    CALL FUNCTION 'Z_MM_EU_BCF_V0'
      EXPORTING
        matnr     = matnr
        cuobf     = cuobf
        werks     = werks
        aufnr     = aufnr
      IMPORTING
        atwrt_out = lv_atwrt1.

    SELECT * FROM zbcf_hier1
      INTO TABLE gt_hier
      WHERE charact = 'ATWRT2'
      ORDER BY id.

    READ TABLE gt_hier INTO DATA(ls_atwrt2) INDEX 1.
    PERFORM get_hierch_atwrt_out USING cv_root_id lv_atwrt1 CHANGING atwrt_out.

    IF atwrt_out IS INITIAL.
      atwrt_out = ls_atwrt2-end_value.
    ENDIF.

  ENDFUNCTION.



*----------------------------------------------------------------------*
***INCLUDE LZBC_HIER1F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_HIERCH_ATWRT_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LS_HIER>_ID  text
*      <--P_ATWRT_OUT  text
*----------------------------------------------------------------------*
FORM get_hierch_atwrt_out  USING    uv_id           TYPE zbcf_hier1-id
                                    uv_caratt_val   TYPE atwrt
                           CHANGING cv_atwrt_out    TYPE atwrt.

  READ TABLE gt_hier ASSIGNING FIELD-SYMBOL(<ls_hier>) WITH KEY id = uv_id BINARY SEARCH.
  DATA(lv_tabix) = sy-tabix + 1.
  DATA(lv_child_node_level) = <ls_hier>-node_level + 1.

  LOOP AT gt_hier INTO DATA(ls_child) FROM lv_tabix.
    IF ls_child-node_level <= <ls_hier>-node_level.
      EXIT.
    ENDIF.

    IF ls_child-node_level <> lv_child_node_level.
      CONTINUE.
    ENDIF.

* if we found a characteristic and the value of the child is not it, skip it
    IF uv_caratt_val IS NOT INITIAL.
      IF lv_child_node_level <> ls_child-node_level.
        CONTINUE.
      ENDIF.

      CASE ls_child-opti.
        WHEN 'EQ'.
          IF ls_child-tech_value <> uv_caratt_val.
            CONTINUE.
          ENDIF.
        WHEN 'CP'.
          IF uv_caratt_val NP ls_child-tech_value.
            CONTINUE.
          ENDIF.

      ENDCASE.

    ENDIF.

    lv_tabix = sy-tabix + 1.

    READ TABLE gt_hier INTO DATA(ls_next_node) INDEX lv_tabix.
    IF sy-subrc = 0.
      IF ls_next_node-node_level > ls_child-node_level.
* it is a hierarchy; check values inside

        IF ls_child-opti IS INITIAL.
          READ TABLE gt_caratt INTO DATA(ls_caratt) WITH KEY atnam = ls_child-tech_value.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          PERFORM get_hierch_atwrt_out
            USING
              ls_child-id
              ls_caratt-atwrt
            CHANGING
              cv_atwrt_out.

        ELSE.
          PERFORM get_hierch_atwrt_out
            USING
              ls_child-id
              ''
            CHANGING
              cv_atwrt_out.

        ENDIF.

      ENDIF.
    ENDIF.

    IF  cv_atwrt_out IS INITIAL
      AND ls_child-end_value IS NOT INITIAL.

      cv_atwrt_out = ls_child-end_value.
    ENDIF.

    IF cv_atwrt_out IS NOT INITIAL.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM init.
  CLEAR: gt_caratt,
         gt_hier.

ENDFORM.
              
