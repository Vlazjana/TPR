Tree salv tree


PARAMETERS: p_sname TYPE setheader-setname DEFAULT 'ZHIER4',
            rb_tree RADIOBUTTON GROUP r1,
            rb_salv RADIOBUTTON GROUP r1 DEFAULT 'X'.

CLASS lcl_hier DEFINITION ABSTRACT.

  PUBLIC SECTION.

*    TYPES: BEGIN OF ty_set,
**             expand_icon TYPE icon-id,
*             setname    TYPE setheader-setname,
*             subsetname TYPE setnode-subsetname,
*             parent     TYPE setheader-setname,
*             desc       TYPE  setheadert-descript,
*             level      TYPE i,
**             key        TYPE salv_de_node_key,
*           END OF ty_set,
*           tt_set TYPE STANDARD TABLE OF  ty_set WITH EMPTY KEY.


    TYPES: BEGIN OF ty_select,
             desc TYPE makt-maktx,
           END OF ty_select,
           tt_select TYPE STANDARD TABLE OF  ty_select.



    DATA : gt_select TYPE STANDARD TABLE OF ty_select.
    DATA: lt_set TYPE STANDARD TABLE OF ty_select.

    DATA: mt_hier TYPE STANDARD TABLE OF snodetext,
          gs_hier TYPE snodetext.

    METHODS execute ABSTRACT.
    METHODS get_node ABSTRACT IMPORTING iv_name  TYPE setheader-setname
                                        iv_level TYPE i OPTIONAL.
ENDCLASS.





CLASS lcl_hier IMPLEMENTATION.


ENDCLASS.


CLASS lcl_tree DEFINITION INHERITING FROM lcl_hier.

  PUBLIC SECTION.
    METHODS execute REDEFINITION.


    METHODS get_node REDEFINITION.


  PRIVATE SECTION.
    METHODS display_tree.

ENDCLASS.

CLASS lcl_tree IMPLEMENTATION.

  METHOD get_node.


    SELECT setheader~setname,
         setheader~settype,
         setnode~subsetname,
         setheadert~descript

    FROM setheader

    LEFT JOIN setnode
    ON  setheader~setclass = setnode~setclass
    AND setheader~subclass = setnode~subclass
    AND setheader~setname = setnode~setname

    LEFT JOIN setheadert
    ON setheadert~setclass = setheader~setclass
   AND setheadert~setname = setheader~setname
   AND setheadert~langu = @sy-langu

    INTO TABLE @DATA(gt_hierarchy_data)

    WHERE setheader~setname = @iv_name .

    APPEND VALUE #(   name    = iv_name
                      tlevel  = iv_level
                      nlength = 10
                      color   = 7
*                     text    = 10
*                     tlength = 10
                      tcolor  = 7
                                              ) TO mt_hier.

    DATA(lv_level) = iv_level + 1.

    LOOP AT gt_hierarchy_data ASSIGNING FIELD-SYMBOL(<fs_hierarchy_data>).

      SELECT valfrom
                  FROM setleaf
                  INTO TABLE @DATA(lt_leaf)
                  WHERE setname = @<fs_hierarchy_data>-setname.

      IF lt_leaf IS NOT INITIAL .

        LOOP AT lt_leaf ASSIGNING FIELD-SYMBOL(<fs_setleaf>).

          SELECT SINGLE maktx
               FROM makt
               INTO gs_hier-text
               WHERE matnr = <fs_setleaf>-valfrom
               AND spras = sy-langu.

          APPEND VALUE #(  name    = |{ <fs_setleaf>-valfrom ALPHA = OUT }|
                              nlength    =   24
                              tlevel     =  iv_level + 1
                              text       = gs_hier-text

                              tlength    = 40   ) TO mt_hier.

        ENDLOOP.

      ENDIF.

      IF <fs_hierarchy_data>-subsetname IS NOT INITIAL OR <fs_hierarchy_data>-settype EQ 'S'.
        get_node(
        iv_name  = <fs_hierarchy_data>-subsetname
        iv_level = lv_level ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD display_tree.

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      TABLES
        nodetab            = mt_hier
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
      EXPORTING
        callback_program     = sy-repid
        check_duplicate_name = '1'
        color_of_node        = '4'
        color_of_mark        = '3'
        color_of_link        = '1'
        color_of_match       = '5'
        node_length          = 30
        text_length          = 75
        use_control          = 'L'.
  ENDMETHOD.
  METHOD execute.


    get_node( EXPORTING iv_name = p_sname
                        iv_level =  1 ).
    display_tree( ).
  ENDMETHOD.
ENDCLASS.
—--------------------------------------------------------
Salv tree
REPORT zvq_salv_tree.


PARAMETERS: p_sname TYPE setheader-setname DEFAULT 'ZHIER4'.


CLASS lcl_salvtree DEFINITION .

  PUBLIC SECTION.
    DATA: mo_tree TYPE REF TO cl_salv_tree.

    TYPES: BEGIN OF ty_select,
             desc TYPE makt-maktx,
           END OF ty_select,
           tt_select TYPE STANDARD TABLE OF  ty_select.

    DATA: mt_hier TYPE STANDARD TABLE OF snodetext,
          gs_hier TYPE snodetext.

    METHODS execute.

  PRIVATE SECTION.
    METHODS get_node IMPORTING iv_name TYPE setheader-setname
                               iv_key  TYPE salv_de_node_key OPTIONAL.
    METHODS display_salvtree.
    DATA: mo_salv_tree TYPE REF TO cl_salv_tree,
          mt_tree      TYPE tt_select,
          mo_nodes     TYPE REF TO cl_salv_nodes.


ENDCLASS.

CLASS lcl_salvtree IMPLEMENTATION.

  METHOD display_salvtree.

    TRY.
        CALL METHOD cl_salv_tree=>factory
          IMPORTING
            r_salv_tree = mo_salv_tree
          CHANGING
            t_table     = mt_tree.
      CATCH cx_salv_error.
    ENDTRY.

    mo_nodes = mo_salv_tree->get_nodes( ).

    get_node( iv_name = p_sname iv_key = '' ).


    DATA: lv_settings TYPE REF TO cl_salv_tree_settings.
    lv_settings = mo_salv_tree->get_tree_settings( ).
    lv_settings->set_hierarchy_header( 'Tree Header                .' ).
    lv_settings->set_hierarchy_tooltip( 'Tree Header-Tool' ).
    lv_settings->set_hierarchy_size( 30 ).


    DATA: lv_title TYPE salv_de_tree_text.
    lv_title = sy-title.
    lv_settings->set_header( lv_title ).

    mo_nodes->expand_all( ).

    mo_salv_tree->display( ).
  ENDMETHOD.

  METHOD get_node.
    DATA ls_set_hier TYPE ty_select.

    TRY.
        SELECT SINGLE h~settype, t~descript
          FROM setheader AS h
          LEFT JOIN setheadert AS t
            ON h~setname = t~setname
           AND t~langu = @sy-langu
           INTO ( @DATA(lv_settype), @ls_set_hier-desc )
          WHERE h~setname = @iv_name.

        DATA(lv_key) = mo_nodes->add_node(
                EXPORTING
                  related_node   = iv_key     " Key to Related Node
                  relationship   = cl_gui_column_tree=>relat_last_child    " Node Relation in Tree
                  text           = CONV #( iv_name )
                  data_row       = ls_set_hier
              )->get_key( ).


        IF lv_settype <> 'B'.
          SELECT subsetname
          FROM setnode
          INTO TABLE @DATA(lt_hierarchy_data)
          WHERE setname = @iv_name.

          LOOP AT lt_hierarchy_data ASSIGNING FIELD-SYMBOL(<ls_hierarchy_data>).

            get_node(
              iv_name  = <ls_hierarchy_data>-subsetname
              iv_key = lv_key ).

          ENDLOOP.

        ENDIF.

        SELECT l~valfrom, m~maktx
            FROM setleaf AS l
          LEFT JOIN makt AS m
            ON l~valfrom = m~matnr
           AND spras = @sy-langu
          INTO TABLE @DATA(lt_leaf)
               WHERE setname = @iv_name.

        LOOP AT lt_leaf ASSIGNING FIELD-SYMBOL(<ls_leaf>).
          ls_set_hier = VALUE #( desc = <ls_leaf>-maktx ).
          DATA(lv_matnr) = conv matnr( |{ <ls_leaf>-valfrom ALPHA = OUT }| ).

          mo_nodes->add_node(
            EXPORTING
              related_node   = lv_key    " Key to Related Node
              relationship   = cl_gui_column_tree=>relat_last_child    " Node Relation in Tree
              text           = CONV #( lv_matnr )  " ALV Control: Cell Content
              data_row       = ls_set_hier

          ).
        ENDLOOP.


      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD execute.

    display_salvtree( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(go_salv_hier) = NEW lcl_salvtree( ).
  go_salv_hier->execute( ).
