

 methods top_of_page         for event top_of_page   of cl_gui_alv_grid.


    set handler : handle_toolbar
                  top_of_page
                  on_hotspot
                  handle_user_command for mo_grid.


    mo_grid->set_table_for_first_display(
       exporting
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
         it_toolbar_excluding = lt_toolbar_excluding
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_stanziamenti ).


    mo_top->initialize_document( ).


    mo_grid->list_processing_events(
     exporting
       i_event_name = 'TOP_OF_PAGE'
       i_dyndoc_id  = mo_top ).
 


    create object mo_top
      exporting
        style = 'ALV_GRID'.












method top_of_page.


   constants: lc_color   type string value '#E5E8E8'.


   data: lv_fatt_blocked type i,
         lv_fatt_unblckd type i,
         lv_fatt_error   type i,
         lt_html_tab     type sdydo_html_table.


   case mv_stanz_status.
     when ms_status-fatto.
       data(lv_status) = exact char9('Fatto').
       data(lv_color)  = exact char6('green').
       data(lv_title)  = exact string('Elaborazione con successo').
     when ms_status-errore.
       lv_status = 'Errore'.
       lv_color = 'red'.
       lv_title = |Errore durante l'elaborazione|.
     when ms_status-attesa.
       lv_status = 'In Attesa'.
       lv_color = 'orange'.
       lv_title = |In attesa di elaborazione|.
   endcase.


   loop at mt_stanziamenti assigning field-symbol(<ls_stanziamenti>).
     if <ls_stanziamenti>-id = ca_red
     or <ls_stanziamenti>-id = ca_yellow.
       lv_fatt_blocked = lv_fatt_blocked + 1.
     elseif <ls_stanziamenti>-id = ca_green.
       lv_fatt_unblckd = lv_fatt_unblckd + 1.
     endif.
   endloop.


   data(lv_text) = exact sdydo_text_element( |Fatture estratte : { lines( mt_stanziamenti ) }| ).


   lt_html_tab = value #(
   ( |<style>| )
   ( | .header2 { '{' }| )
   ( |   font-size: 10pt;| )
   ( |   font-family:Sans-Serif;| )
   ( |   user-select:none;| )
   ( |   font-weight:bold;| )
   ( |{ '}' }| )
   ( |</style>| )
   ( |<body style="background-color: { lc_color }; border: 1px solid gray; margin: 0; padding: 10px ">| )
   ( |  <div style="display: flex; justify-content: space-between;">| )
   ( |    <font style = "font-size: 12pt; font-weight:bold; color="black"> { lv_text } </font>| )
   ( |    <span>| )
   ( |      <font class="header2" color="black"; title ="{ lv_title }"> Stanziamento status: </font>| )
   ( |      <font class="header2" color="{ lv_color }" title ="{ lv_title }"> { lv_status } </font>| )
   ( |    </span>| )
   ( |  </div>| )
   ( |  <hr style=" margin-left: 0;">| )
   ( |  <font class="header2" color="black"> N. fatture stanziabile : </font>| )
   ( |  <font class="header2" color="green"> { lv_fatt_unblckd } </font> &nbsp &nbsp &nbsp| )
   ( |  <font class="header2" color="black"> N. fatture con errore : </font>| )
   ( |  <font class="header2" color="red"> { lv_fatt_blocked } </font>| )
   ( |</body>| ) ).


   mo_top->add_static_html( table_with_html  = lt_html_tab ).
   mo_top->display_document(
    exporting
      reuse_control      = 'X'
      parent             = mo_top_cnt
    exceptions
      html_display_error = 1
      others             = 2 ).


 endmethod.

