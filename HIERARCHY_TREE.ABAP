*&---------------------------------------------------------------------*
*& Report ZVQ_HIERARCHY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvq_hierarchy.

TYPE-POOLS icon.

TYPES: BEGIN OF ty_scr_100,
         ok_code TYPE syucomm,
         hier(12) TYPE c,
       END OF ty_scr_100.

DATA: gs_scr_100 TYPE ty_scr_100.

INCLUDE zkp_hier_class_def.

DATA: go_hierarchy TYPE REF TO lcl_hierarchy.

INCLUDE zvqhierarchy_pbo.

INCLUDE zvq_hierarchy_pai.

START-OF-SELECTION.

  CREATE OBJECT go_hierarchy.


*----------------------------------------------------------------------*
***INCLUDE ZVQ_HIERARCHY_PBO.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
 SET PF-STATUS 'PF0100'.
 SET TITLEBAR 'TB0100'.
ENDMODULE.

*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  go_hierarchy->execute( iv_ok = gs_scr_100-ok_code iv_hier_name = gs_scr_100-hier ).

ENDMODULE.

*&---------------------------------------------------------------------*
*& Include          ZVQ_HIER_CLASS_DEF
*&---------------------------------------------------------------------*

CLASS lcl_hierarchy DEFINITION.

  PUBLIC SECTION.

    METHODS: constructor,
      execute IMPORTING iv_ok TYPE syucomm iv_hier_name TYPE c.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_alv_table,
             expand_icon  TYPE icon_d,
             text(30)     TYPE c,
             maktx(30)    TYPE c,
             node_key(12) TYPE c,
             relatkey(12) TYPE c,
             order        TYPE i,
             visible      TYPE abap_bool,
           END OF ty_alv_table,
           tt_alv_table TYPE STANDARD TABLE OF ty_alv_table.

    DATA: mo_container TYPE REF TO cl_gui_custom_container,
          mo_splitter  TYPE REF TO cl_gui_splitter_container,
          mo_tree_cnt  TYPE REF TO cl_gui_container,
          mo_alv_cnt   TYPE REF TO cl_gui_container,
          mo_tree      TYPE REF TO cl_gui_simple_tree,
          mo_alv       TYPE REF TO cl_gui_alv_grid,
          mt_nodes     TYPE STANDARD TABLE OF mtreesnode,
          mt_alv_table TYPE tt_alv_table,
          mt_alv_view  TYPE tt_alv_table,
          mv_leaf_key  TYPE i.
      CONSTANTS cv_root(12)  TYPE c VALUE 0.

    METHODS:
      setup_ui,
      get_data IMPORTING iv_hier TYPE c,
      build_tree,
      build_alv,
      build_alv_data_recursive IMPORTING is_node TYPE mtreesnode CHANGING cv_order TYPE i OPTIONAL,
      get_children_recursive IMPORTING iv_setname TYPE c iv_relatkey TYPE i OPTIONAL,
      on_alv_button_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,
      toggle_children_recursive IMPORTING iv_relatkey TYPE c iv_expand TYPE abap_bool iv_row TYPE lvc_index.

ENDCLASS.

CLASS lcl_hierarchy IMPLEMENTATION.

  METHOD execute.

    CASE iv_ok.

      WHEN 'FC_BACK'.

        LEAVE PROGRAM.

      WHEN 'FC_ENTER'.

        CLEAR: mt_nodes, mt_alv_table, mt_alv_view, mv_leaf_key.
        mo_tree->delete_all_nodes( ).

        get_data( iv_hier = iv_hier_name ).
        build_tree( ).
        build_alv( ).

    ENDCASE.

  ENDMETHOD.

  METHOD constructor.

    setup_ui( ).

    CALL SCREEN 100.

  ENDMETHOD.

  METHOD setup_ui.

    CREATE OBJECT mo_container
      EXPORTING
        container_name              = 'GS_SCR_100_CONTAINER'
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

    CREATE OBJECT mo_splitter
      EXPORTING
        link_dynnr        = '0100'
        link_repid        = sy-repid
        parent            = mo_container
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    mo_splitter->get_container( EXPORTING row = 1 column = 1
                                 RECEIVING container = mo_tree_cnt ).

    mo_splitter->get_container( EXPORTING row = 1 column = 2
                                 RECEIVING container = mo_alv_cnt ).

    mo_splitter->set_column_width( id = 1 width = 25 ).
    mo_splitter->set_column_width( id = 2 width = 75 ).

    CREATE OBJECT mo_tree
      EXPORTING
        parent                      = mo_tree_cnt
        node_selection_mode         = cl_gui_simple_tree=>node_sel_mode_single
      EXCEPTIONS
        lifetime_error              = 1
        cntl_system_error           = 2
        create_error                = 3
        failed                      = 4
        illegal_node_selection_mode = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    CREATE OBJECT mo_alv
      EXPORTING
        i_parent          = mo_alv_cnt
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

  METHOD get_data.


    SELECT SINGLE setname,
                  descript
      FROM setheadert
      INTO @DATA(ls_hier)
      WHERE setname = @iv_hier
        AND langu   = @sy-langu.

    IF sy-subrc = 0.

      mt_nodes = VALUE #( ( node_key  = cv_root
                            isfolder  = abap_true
                            expander  = abap_true
                            text      = |{ ls_hier-setname }     { ls_hier-descript }|
                            n_image   = icon_folder
                            exp_image = icon_folder ) ).

      get_children_recursive( iv_setname = ls_hier-setname ).

    ELSE.

      MESSAGE 'No hierarchies found with the specified name in the database!' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ENDMETHOD.

  METHOD get_children_recursive.

    DATA: lv_maktx(30) TYPE c.

    SELECT valfrom
      FROM setleaf
      INTO TABLE @DATA(lt_setleaf)
      WHERE setname = @iv_setname.

    IF sy-subrc = 0.

      LOOP AT lt_setleaf ASSIGNING FIELD-SYMBOL(<ls_setleaf>).

        mv_leaf_key += 1.

        SELECT SINGLE maktx
          FROM makt
          INTO lv_maktx
          WHERE matnr = <ls_setleaf>-valfrom
          AND spras = sy-langu.

        APPEND VALUE #( node_key  = mv_leaf_key
                             relatkey  = iv_relatkey
                             isfolder  = abap_false
                             expander  = abap_false
                             text      = |{ <ls_setleaf>-valfrom }     { lv_maktx }|
                             n_image   = '@0Y@'
                             exp_image = '@0Y@' )  TO mt_nodes.

      ENDLOOP.

    ENDIF.

    SELECT subsetname
      FROM setnode
      INTO TABLE @DATA(lt_setnode)
      WHERE setname = @iv_setname.

    IF sy-subrc = 0.

      LOOP AT lt_setnode ASSIGNING FIELD-SYMBOL(<ls_setnode>).

        SELECT SINGLE
               descript
          FROM setheadert
          INTO @DATA(lv_descript)
          WHERE setname = @<ls_setnode>-subsetname
            AND langu   = @sy-langu.



        mv_leaf_key += 1.

        APPEND VALUE #( node_key  = mv_leaf_key
                              relatkey  = iv_relatkey
                              isfolder  = abap_true
                              expander  = abap_true
                              text      = |{ <ls_setnode>-subsetname }     { lv_descript }|
                              n_image   = icon_folder
                              exp_image = icon_folder )  TO mt_nodes.



        DATA(lv_relatkey) = mv_leaf_key.
        get_children_recursive( iv_setname = <ls_setnode>-subsetname iv_relatkey = lv_relatkey ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD build_tree.

    mo_tree->add_nodes(
      EXPORTING
        table_structure_name           = 'MTREESNODE'
        node_table                     = mt_nodes
      EXCEPTIONS
        error_in_node_table            = 1
        failed                         = 2
        dp_error                       = 3
        table_structure_name_not_found = 4
        OTHERS                         = 5 ).
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    READ TABLE mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>) WITH KEY node_key = cv_root.

    IF sy-subrc = 0.

      mo_tree->expand_node( node_key = <ls_nodes>-node_key ).

    ENDIF.

  ENDMETHOD.

  METHOD build_alv.

    DATA: lt_fcat   TYPE lvc_t_fcat,
          ls_layout TYPE lvc_s_layo.

    READ TABLE mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>) WITH KEY relatkey = ''.

    IF sy-subrc = 0.

      build_alv_data_recursive( EXPORTING is_node = <ls_nodes> ).

    ENDIF.

    lt_fcat = VALUE #( ( fieldname = 'EXPAND_ICON' icon = abap_true hotspot = abap_true )
    ( fieldname = 'TEXT' coltext = 'Node' )
    ( fieldname = 'MAKTX' coltext = 'Description' )
    ( fieldname = 'NODE_KEY' tech = abap_true )
    ( fieldname = 'RELATKEY' tech = abap_true ) ).

    ls_layout-zebra = abap_true.

    LOOP AT mt_alv_table ASSIGNING FIELD-SYMBOL(<ls_alv>) WHERE visible = abap_true.

      APPEND <ls_alv> TO mt_alv_view.

    ENDLOOP.

    SORT mt_alv_table BY expand_icon order.

    SET HANDLER on_alv_button_click FOR mo_alv.

    mo_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = mt_alv_view
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.

  METHOD build_alv_data_recursive.

    DATA: ls_alv TYPE ty_alv_table.

    ls_alv-node_key = is_node-node_key.
    ls_alv-relatkey = is_node-relatkey.

    cv_order += 1.
    ls_alv-order = cv_order.

    SPLIT is_node-text AT space INTO DATA(lv_text) DATA(lv_maktx).

    ls_alv-text  = lv_text.
    ls_alv-maktx = lv_maktx.

    IF is_node-isfolder = abap_true AND is_node-relatkey IS INITIAL.

      ls_alv-expand_icon = icon_collapse.

    ELSEIF is_node-isfolder = abap_true.

      ls_alv-expand_icon = icon_expand.

    ENDIF.

    IF is_node-relatkey IS INITIAL OR is_node-relatkey = cv_root.

      ls_alv-visible = abap_true.

    ENDIF.

    INSERT ls_alv INTO mt_alv_table INDEX 1.
    CLEAR ls_alv.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>) WHERE relatkey = is_node-node_key.

      build_alv_data_recursive( EXPORTING is_node = <ls_nodes> CHANGING cv_order = cv_order ).

    ENDLOOP.

  ENDMETHOD.

  METHOD on_alv_button_click.

    DATA: lt_rows TYPE lvc_t_row,
          lt_view TYPE tt_alv_table,
          ls_view TYPE ty_alv_table.

    IF e_column_id-fieldname <> 'EXPAND_ICON'.

      RETURN.

    ENDIF.

    mo_alv->get_selected_rows( IMPORTING et_index_rows = lt_rows ).

    READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<ls_rows>) INDEX 1.

    IF sy-subrc = 0.

      READ TABLE mt_alv_view ASSIGNING FIELD-SYMBOL(<ls_alv_view>) INDEX <ls_rows>-index.

      IF sy-subrc = 0 AND <ls_alv_view>-expand_icon IS NOT INITIAL.

        READ TABLE mt_alv_table ASSIGNING FIELD-SYMBOL(<ls_alv_tab>) WITH KEY node_key = <ls_alv_view>-node_key.

        IF sy-subrc = 0.

          CASE <ls_alv_tab>-expand_icon.

            WHEN icon_expand.

              <ls_alv_tab>-expand_icon  = icon_collapse.
              <ls_alv_view>-expand_icon = icon_collapse.

              toggle_children_recursive( iv_relatkey = <ls_alv_view>-node_key
                                         iv_expand   = abap_true
                                         iv_row      = <ls_rows>-index ).

            WHEN icon_collapse.

              <ls_alv_tab>-expand_icon = icon_expand.
              <ls_alv_view>-expand_icon = icon_expand.

              toggle_children_recursive( iv_relatkey = <ls_alv_view>-node_key
                                         iv_expand   = abap_false
                                         iv_row      = <ls_rows>-index ).

          ENDCASE.

        ENDIF.

      ENDIF.

    ENDIF.

    mo_alv->refresh_table_display( ).

  ENDMETHOD.

  METHOD toggle_children_recursive.

    LOOP AT mt_alv_table ASSIGNING FIELD-SYMBOL(<ls_alv>) WHERE relatkey = iv_relatkey.

      CASE iv_expand.

        WHEN abap_true.

          <ls_alv>-visible = abap_true.
          INSERT <ls_alv> INTO mt_alv_view INDEX iv_row.

          IF <ls_alv>-expand_icon = icon_collapse.

            toggle_children_recursive( iv_relatkey = <ls_alv>-node_key
                                           iv_expand   = abap_true
                                           iv_row      = iv_row ).
          ENDIF.

        WHEN abap_false.

          <ls_alv>-visible = abap_false.
          DELETE mt_alv_view WHERE relatkey = iv_relatkey.

          toggle_children_recursive( iv_relatkey = <ls_alv>-node_key
                                           iv_expand   = abap_false
                                           iv_row      = iv_row ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
