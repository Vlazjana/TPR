    TYPES:
      BEGIN OF ty_excel,
        icon_descr TYPE text15.
        INCLUDE STRUCTURE z2fi_riscat_riep_alv.
        TYPES color      TYPE lvc_t_scol.
    TYPES: END OF ty_excel.

    DATA: lt_sheet_data TYPE zcl_fdt_xl_spreadsheet_v2=>tt_sheet_data,
          lt_excel      TYPE STANDARD TABLE OF ty_excel,
          lt_field_desc TYPE zcl_fdt_xl_spreadsheet_v2=>tt_desc.

    LOOP AT tb_out ASSIGNING FIELD-SYMBOL(<ls_out>).
      APPEND CORRESPONDING #( <ls_out> ) TO lt_excel ASSIGNING FIELD-SYMBOL(<ls_excel>).
      CASE <ls_out>-stato_oda.
        WHEN icon_green_light.
          <ls_excel>-icon_descr = 'Completo'.
          APPEND VALUE #( fname = 'ICON_DESCR' color-col = 5 ) TO <ls_excel>-color.
        WHEN icon_yellow_light.
          <ls_excel>-icon_descr = 'Incompleto'.
          APPEND VALUE #( fname = 'ICON_DESCR' color-col = 3 ) TO <ls_excel>-color.
      ENDCASE.
    ENDLOOP.

    lt_field_desc = VALUE #( FOR <ls_fcat> IN tb_fc
                             ( fieldname  = COND #( WHEN <ls_fcat>-fieldname = 'STATO_ODA' THEN 'ICON_DESCR' ELSE <ls_fcat>-fieldname )
                               descr      = <ls_fcat>-seltext ) ).

    lt_sheet_data = VALUE #(
  ( sheet_name  = 'Sheet1'
    tab_data    = REF #( lt_excel )
    field_desc  = lt_field_desc
    color_field = 'COLOR'
  ) ).

    DATA(lv_bin_data) = zcl_fdt_xl_spreadsheet_v2=>create_document(
                        name              = lv_filename
                        it_sheet_data     = lt_sheet_data ).

    DATA(lt_raw_data) = cl_bcs_convert=>xstring_to_solix( EXPORTING iv_xstring = lv_bin_data ).

    cl_gui_frontend_services=>gui_download( EXPORTING
                                              filename     = lv_fullpath
                                              filetype     = 'BIN'
                                              bin_filesize = xstrlen( lv_bin_data )
                                            CHANGING
                                              data_tab     = lt_raw_data ).

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document               = lv_fullpath
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10.













copied from standard with z**************************************
CLASS zcl_fdt_xl_spreadsheet_v2 DEFINITION
  PUBLIC
  INHERITING FROM cl_fdt_xl_document
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_fdt_doc_spreadsheet .

    TYPES:
      BEGIN OF ty_desc,
        fieldname TYPE fieldname,
        descr     TYPE string,
      END OF ty_desc .
    TYPES:
      tt_desc TYPE STANDARD TABLE OF ty_desc WITH DEFAULT KEY .

    TYPES:
      BEGIN OF ty_map_descr,
        fieldname TYPE fieldname,
        type      TYPE REF TO cl_abap_datadescr,
        descr     TYPE string,
      END OF ty_map_descr .
    TYPES:
      tt_map_descr TYPE STANDARD TABLE OF ty_map_descr WITH DEFAULT KEY .


    TYPES:
      BEGIN OF ty_sheet_data,
        sheet_name  TYPE string,
        tab_data    TYPE REF TO data,
        field_desc  TYPE tt_desc,
        color_field TYPE fieldname,
      END OF ty_sheet_data .
    TYPES:
      tt_sheet_data TYPE STANDARD TABLE OF ty_sheet_data .

    METHODS constructor
      IMPORTING
        VALUE(document_name) TYPE string
        VALUE(xdocument)     TYPE xstring
        !mime_type           TYPE string OPTIONAL
      RAISING
        cx_fdt_excel_core .
    CLASS-METHODS get_langu_text
      IMPORTING
        !iv_langu      TYPE char1
      RETURNING
        VALUE(rv_text) TYPE string .
    CLASS-METHODS create_document
      IMPORTING
        !name            TYPE string DEFAULT if_fdt_doc_spreadsheet=>default_filename
        !it_sheet_data   TYPE tt_sheet_data
      RETURNING
        VALUE(xdocument) TYPE xstring
      RAISING
        cx_fdt_excel_core .
  PROTECTED SECTION.
*"* protected components of class ZCL_FDT_XL_SPREADSHEET_V2
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF ooxml_worksheet.
    TYPES name        TYPE string.
    TYPES id          TYPE string.
    TYPES location    TYPE string.
    TYPES END OF ooxml_worksheet .
    TYPES:
      ooxml_worksheets TYPE STANDARD TABLE OF ooxml_worksheet .
    TYPES:
      BEGIN OF struc_named_cell.
    TYPES name        TYPE string.
    TYPES sheet_name  TYPE string.
    TYPES cell        TYPE string.
    TYPES format      TYPE string.
    TYPES END OF struc_named_cell .
    TYPES:
      tab_named_cells TYPE STANDARD TABLE OF struc_named_cell .
    TYPES:
      BEGIN OF struc_named_range.
    TYPES name        TYPE string.
    TYPES sheet_name  TYPE string.
    TYPES start_cell  TYPE string.
    TYPES end_cell    TYPE string.
    TYPES END OF struc_named_range .
    TYPES:
      tab_named_ranges TYPE STANDARD TABLE OF struc_named_range .
    TYPES:
      BEGIN OF t_struc_numfmtid.
    TYPES id          TYPE i.
    TYPES formatcode  TYPE string.
    TYPES END OF t_struc_numfmtid .
    TYPES:
      t_numfmtids TYPE STANDARD TABLE OF t_struc_numfmtid .
private section.

  types:
    BEGIN OF ty_shared_string_buffer,
        str_val TYPE string,
        pos     TYPE string,
      END OF ty_shared_string_buffer .
  types:
    th_shared_string_buffer TYPE HASHED TABLE OF ty_shared_string_buffer WITH UNIQUE KEY str_val .

  constants DEFAULT_SHEET_NAME type STRING value 'Sheet1' ##NO_TEXT.
  constants GC_RELS_EXTN type STRING value '.rels' ##NO_TEXT.
  constants GC_RELS_PATH type STRING value '_rels/' ##NO_TEXT.
  data DATEFORMAT1904 type ABAP_BOOL .
  data NAMED_CELLS type TAB_NAMED_CELLS .
  data NAMED_RANGES type TAB_NAMED_RANGES .
  data NUMFMTIDS type T_NUMFMTIDS .
  data:
    mt_shared_strings TYPE STANDARD TABLE OF string .
  data WORKBOOK_FILENAME type STRING .
  data WORKBOOK_FOLDER type STRING .
  data WORKBOOK_RELATIONS type T_POD_RELATIONS .
  data WORKSHEETS type OOXML_WORKSHEETS .
  data MTH_SHARED_STRINGS type TH_SHARED_STRING_BUFFER .

  methods CREATE_WORKSHEET_ROW_NTL
    importing
      !IT_DATA type STANDARD TABLE
      !IS_ROW type ANY
      !IV_ROW_INDEX type SY-TABIX
      !IT_COLUMN type IF_FDT_DOC_SPREADSHEET=>T_COLUMN optional
      !IT_COLUMN_DESCRIPTION type TT_MAP_DESCR
      !IO_SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IO_SHEET_DATA_NODE type ref to IF_IXML_NODE
      !IV_IS_ALV_DOCUMENT type ABAP_BOOL default ABAP_FALSE
    changing
      !CV_ROW_NUMBER type INT4
      !CV_GROUP_NUMBER_MAX type INT4 .
  methods CREATE_WORKSHEET_ROW
    importing
      !IS_ROW type ANY
      !IV_ROW_NUMBER type INT4
      !IV_COLOR_FNAME type FIELDNAME
      !IT_COLUMN_DESCRIPTION type TT_MAP_DESCR
      !IO_SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IO_SHEET_DATA_NODE type ref to IF_IXML_NODE .
  methods CREATE_CELL_ELEMENTS_HEADER
    importing
      !IO_ROW_ELEMENT type ref to IF_IXML_ELEMENT
      !IT_COLUMN_DESCRIPTION type TT_MAP_DESCR
      !IO_SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  methods CREATE_CELL_ELEMENTS
    importing
      !IS_ROW type ANY
      !IO_ROW_ELEMENT type ref to IF_IXML_ELEMENT
      !IV_ROW_NUMBER type STRING
      !IV_COLOR_FNAME type FIELDNAME
      !IT_COLUMN_DESCRIPTION type TT_MAP_DESCR
      !IO_SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  class-methods CREATE_BLANK_DOCUMENT_STRUCT
    importing
      !META_TITLE type STRING optional
      !META_SUBJECT type STRING optional
      !META_DESCRIPTION type STRING optional
      !META_KEYWORDS type STRING optional
      !IT_SHEET_DATA type TT_SHEET_DATA
    returning
      value(XBLANK_DOCUMENT) type XSTRING .
  methods CLEAR_WORKSHEET_SHEETDATA
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
    returning
      value(SHEETDATA_NODE) type ref to IF_IXML_NODE .
  methods CONVERT_CELL_VALUE_BY_NUMFMT
    importing
      !CELL_VALUE type STRING
      !NUMBER_FORMAT type STRING
    returning
      value(FORMATTED_VALUE) type STRING .
  methods CONVERT_COLUMN_TO_I
    importing
      !COLUMN type STRING
    returning
      value(COL_NUMBER) type I .
  methods CONVERT_DEC_TIME_TO_HHMMSS
    importing
      !DEC_TIME_STRING type STRING
    returning
      value(TIME) type T .
  methods CONVERT_I_TO_COLUMN
    importing
      !INT_VAL type I
    returning
      value(COLUMN) type STRING .
  methods CONVERT_LONG_TO_DATE
    importing
      !DATE_STRING type STRING
    returning
      value(DATE) type D .
  methods CONVERT_SER_VAL_TO_DATE_TIME
    importing
      !SERIAL_VALUE_STRING type STRING
    exporting
      !DATE type D
      !TIME type T .
  methods EXTRACT_COL_FROM_A1_STYLE
    importing
      !INPUT type STRING
    returning
      value(COLUMN) type STRING .
  methods GET_CELLXFS
    exporting
      !CELLXFS type STANDARD TABLE .
  methods GET_CELL_STYLE_ID
    importing
      !XML_DOC type ref to IF_IXML_DOCUMENT
      !CELL_COORDINATE type STRING
    returning
      value(STYLE_ID) type I .
  methods GET_CELL_VALUE
    importing
      !SHEET_NAME type STRING
      !CELL_COORDINATE type STRING
    returning
      value(CELL_VALUE) type STRING .
  methods GET_CELL_VALUE_BY_XMLDOC_SHRDS
    importing
      !CELL_COORDINATE type STRING
      !SHARED_STRINGS type STANDARD TABLE
      !XML_DOC type ref to IF_IXML_DOCUMENT
    returning
      value(CELL_VALUE) type STRING .
  methods GET_COL_AND_ROW_FROM_A1_STYLE
    importing
      !COORDINATE type STRING
    exporting
      !COLUMN_NUMBER type I
      !ROW_NUMBER type I .
  methods GET_COL_FROM_A1_STYLE
    importing
      !COORDINATE type STRING
    returning
      value(COLUMN_NUMBER) type I .
  methods GET_ITAB_FOR_ALV_BY_XMLDOC
    importing
      !RANGE_START type STRING
      !RANGE_END type STRING
      !XML_DOC type ref to IF_IXML_DOCUMENT
    returning
      value(ITAB) type ref to DATA .
  methods GET_ITAB_FOR_ALV_UPDATE
    returning
      value(ITAB) type ref to DATA .
  methods GET_ITAB_FROM_SHEET
    importing
      !WORKSHEET_NAME type STRING
      !IV_CALLER type I optional
      !IV_GET_LANGUAGE type ABAP_BOOL optional
    returning
      value(ITAB) type ref to DATA .
  methods GET_MAPPING
    returning
      value(MAPPING) type ref to CL_FDT_DOC_MAPPING .
  methods GET_NAMED_CELL
    importing
      !CELL_NAME type STRING
    returning
      value(CELL_VALUE) type STRING .
  methods GET_NAMED_CELLS
    returning
      value(NAMED_CELLS) like NAMED_CELLS .
  methods GET_NAMED_RANGES
    returning
      value(NAMED_RANGES) like NAMED_RANGES .
  methods GET_NAMED_RANGE_ITAB
    importing
      !RANGE type STRING
    returning
      value(ITAB) type ref to DATA .
  methods GET_OR_CREATE_INDEX_SHARED_STR
    importing
      !STRING type STRING
    returning
      value(INDEX) type I .
  methods GET_RANGE_ITAB_FROM_XMLDOC
    importing
      !RANGE_START type STRING
      !RANGE_END type STRING
      !XML_DOC type ref to IF_IXML_DOCUMENT
    returning
      value(ITAB) type ref to DATA .
  methods GET_SHARED_STRINGS
    exporting
      value(STRINGS) type STANDARD TABLE .
  methods GET_WORKSHEET_BY_ID
    importing
      !WORKSHEET_ID type STRING
    returning
      value(XSHEET) type XSTRING .
  methods GET_WORKSHEET_BY_NAME
    importing
      !WORKSHEET_NAME type STRING
    returning
      value(XSHEET) type XSTRING .
  methods GET_WORKSHEET_NAMES
    exporting
      !WORKSHEET_NAMES type STANDARD TABLE .
  methods INIT_SHARED_STRINGS_FILE .
  methods LOAD_DEFINED_NAMES .
  methods LOAD_NUMFMTIDS .
  methods LOAD_SHARED_STRINGS .
  methods LOAD_WORKBOOK .
  methods LOAD_WORKSHEETS .
  methods OVERWRITE_WORKSHEET_BY_ID
    importing
      !WORKSHEET_ID type STRING
      !XSHEET type XSTRING .
  methods OVERWRITE_WORKSHEET_BY_NAME
    importing
      !WORKSHEET_NAME type STRING
      !XSHEET type XSTRING .
  methods SET_CELL_VALUE_BY_XMLDOC
    importing
      !VALUE type ref to DATA
      !CELL_COORDINATE type STRING
    changing
      !XML_DOC type ref to IF_IXML_DOCUMENT .
  methods SET_HELP_DT_SHEETDATA
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  methods SET_HELP_SIM_SHEETDATA
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  methods SET_MAPPING
    importing
      !MAPPING type ref to CL_FDT_DOC_MAPPING .
  methods SET_NAMED_CELL_VALUE
    importing
      !CELL_NAME type STRING
      !CELL_VALUE type STRING .
  methods SET_NAMED_RANGE_VALUES
    importing
      !RANGE_NAME type STRING
      !ITAB type ref to DATA .
  methods SET_SHARED_STRINGS
    importing
      !SHARED_STRINGS type STANDARD TABLE optional .
  methods SET_WORKSHEET_COLS
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !COLUMN_DESCRIPTION_TAB type TT_MAP_DESCR .
  methods SET_WORKSHEET_DIMENSION
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !DIMENSION type STRING .
  methods SET_WORKSHEET_SHEETDATA
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IS_SHEET_DATA type TY_SHEET_DATA .
  methods STORE_WORKBOOK_RELATIONS .
  methods SET_WORKSHEET_FORMATPR
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !OUTLINELEVEL type STRING .
  methods SET_WORKSHEET_WRAPTEXT
    importing
      !SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  methods INSERT_EMPTY_WORKSHEET_ROW
    importing
      !IO_SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IO_SHEET_DATA_NODE type ref to IF_IXML_NODE
    changing
      !CV_ROW type INT4 .
  methods CREATE_WORKSHEET_ROW_BASE
    importing
      !IO_SHEET_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IV_ROW type INT4
    returning
      value(RO_ROW_ELEMENT) type ref to IF_IXML_ELEMENT .
  methods SET_SHARED_STRING_BUFFER
    importing
      !IV_SHARED_STRING type STRING
    returning
      value(RV_CELL_CONTENT_INT) type STRING .
ENDCLASS.



CLASS ZCL_FDT_XL_SPREADSHEET_V2 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CLEAR_WORKSHEET_SHEETDATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* | [<-()] SHEETDATA_NODE                 TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD clear_worksheet_sheetdata.

    DATA:
      worksheet_root_node    TYPE REF TO if_ixml_node,
      worksheet_root_element TYPE REF TO if_ixml_element,
      sheetdata_element      TYPE REF TO if_ixml_element,
      sheetdata_collection   TYPE REF TO if_ixml_node_collection,
      sheetdata_iterator     TYPE REF TO if_ixml_node_iterator,
      row_node               TYPE REF TO if_ixml_node.


* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

* Get the root node and element ( <worksheet> )
    worksheet_root_node ?= sheet_xml_document->get_first_child( ).
    worksheet_root_element ?= worksheet_root_node->query_interface( ixml_iid_element ).

* Checking wether cols node exist
    sheetdata_element = worksheet_root_element->find_from_name_ns( name = 'sheetData' uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
    IF sheetdata_element IS INITIAL.
      " <sheetData> is obligatory... leaving
      EXIT.
    ENDIF.

    " Check for <sheetData> childs
    sheetdata_collection = sheetdata_element->get_elements_by_tag_name_ns( name = 'row' uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
    IF sheetdata_collection IS NOT INITIAL.
      sheetdata_iterator = sheetdata_collection->create_iterator( ).
      row_node = sheetdata_iterator->get_next( ).
      WHILE row_node IS NOT INITIAL.
        row_node->remove_node( ).
        row_node = sheetdata_iterator->get_next( ).
      ENDWHILE.
    ENDIF.
    sheetdata_node ?= sheetdata_element.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCUMENT_NAME                  TYPE        STRING
* | [--->] XDOCUMENT                      TYPE        XSTRING
* | [--->] MIME_TYPE                      TYPE        STRING(optional)
* | [!CX!] CX_FDT_EXCEL_CORE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        iv_document_name = document_name
        iv_xdocument     = xdocument
        iv_mime_type     = mime_type.

    me->load_workbook( ).       " Loading workbook relations, workbook filename, workbook folder
    me->load_worksheets( ).     " Loading worksheets
    me->load_numfmtids( ).      " Loading standard and custom number formats
    me->load_defined_names( ).  " Loading the defined cell or range names in workbook
  ENDMETHOD.                                            "#EC CI_VALPAR.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CONVERT_CELL_VALUE_BY_NUMFMT
* +-------------------------------------------------------------------------------------------------+
* | [--->] CELL_VALUE                     TYPE        STRING
* | [--->] NUMBER_FORMAT                  TYPE        STRING
* | [<-()] FORMATTED_VALUE                TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_cell_value_by_numfmt.
    DATA:
          clean_format TYPE string.

    formatted_value = ''.
    clean_format = number_format.
    REPLACE REGEX '"[^"]*"' IN clean_format WITH ''.
    REPLACE REGEX '\[Red\]' IN clean_format WITH ''.


    IF cl_abap_matcher=>matches( pattern = '.*(y+|m+|d+|h+|s+).*' text = clean_format ) = abap_true.
      " Contains a date or time... checking both
      DATA:
        date TYPE d,
        time TYPE t.
      convert_ser_val_to_date_time(
        EXPORTING
          serial_value_string = cell_value
        IMPORTING
          date                = date
          time                = time ).
      IF date NE '00000000'.
        " Make it fancy and ISO conform
        DATA:
          year(4)  TYPE c,
          month(2) TYPE c,
          day(2)   TYPE c.
        year  = date(4).
        month = date+4(2).
        day   = date+6(2).

        formatted_value = formatted_value && year && '-' && month && '-' && day.
      ENDIF.
      IF time NE '000000'.
        " Make it fancy
        DATA:
          hour(2) TYPE c,
          min(2)  TYPE c,
          sec(2)  TYPE c.
        hour = time(2).
        min  = time+2(2).
        sec  = time+4(2).
        formatted_value = formatted_value && ` ` && hour && ':' && min && ':' && sec .
      ENDIF.
    ENDIF.

    IF formatted_value = ''.
      formatted_value = cell_value.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CONVERT_COLUMN_TO_I
* +-------------------------------------------------------------------------------------------------+
* | [--->] COLUMN                         TYPE        STRING
* | [<-()] COL_NUMBER                     TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_column_to_i.
    DATA:
      input       TYPE string,
      cc          TYPE c VALUE '',
      strl        TYPE i,
      cnt         TYPE i VALUE 0,
      ascii_val   TYPE i,
      ascii_val_a TYPE i.

    input = column.
    ascii_val_a = cl_abap_conv_out_ce=>uccp( 'A' ).
    TRANSLATE input TO UPPER CASE.
    strl = strlen( input ) - 1.

    WHILE strl >= 0.
      cc = input+cnt(1).
      ascii_val = cl_abap_conv_out_ce=>uccp( cc ).
      IF cc >= 'A' AND cc <= 'Z'.
        col_number = col_number + ( ascii_val - ascii_val_a + 1 ) * 26 ** strl .
        cnt = cnt + 1.
        strl = strl - 1 .
      ELSE.
        col_number = 0.
        EXIT.
      ENDIF.
    ENDWHILE.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CONVERT_DEC_TIME_TO_HHMMSS
* +-------------------------------------------------------------------------------------------------+
* | [--->] DEC_TIME_STRING                TYPE        STRING
* | [<-()] TIME                           TYPE        T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_dec_time_to_hhmmss.
    DATA:
      dec_time   TYPE decfloat16,
      hour       TYPE i,
      hour_str   TYPE string,
      minute     TYPE i,
      minute_str TYPE string,
      second     TYPE decfloat16.

    TRY.
        dec_time = dec_time_string.
      CATCH cx_root.
        " Cannot convert string to dec float... leaving undone
        time = dec_time_string.
        EXIT.
    ENDTRY.

    dec_time = frac( dec_time ). " Make sure that only the fraction is considered

    " Thanks to Excel, we have to round at this point to be compliant
    dec_time = round( val = dec_time dec = 15 ).

    dec_time = dec_time * 24.
    hour = floor( dec_time ).
    dec_time = ( dec_time - hour ) * 60.
    minute = floor( dec_time ).
    second = round( val = ( ( dec_time - minute ) * 60 ) dec = 3 ).
    IF second >= 60.
      second = 0.
      minute = minute + 1.
    ENDIF.

    IF hour < 10.
      hour_str = '0' && hour.
    ELSE.
      hour_str = hour.
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = hour_str ).

    IF minute < 10.
      minute_str = '0' && minute.
    ELSE.
      minute_str = minute.
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = minute_str ).

    time = hour_str && minute_str &&  second .
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CONVERT_I_TO_COLUMN
* +-------------------------------------------------------------------------------------------------+
* | [--->] INT_VAL                        TYPE        I
* | [<-()] COLUMN                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_i_to_column.
    DATA:
      cc         TYPE sychar02 VALUE '',
      frag       TYPE f,
      comp       TYPE i VALUE 1,
      c_i_val    TYPE i,
      c_x_val(2) TYPE x,
      input      TYPE i.

    IF int_val IS INITIAL OR int_val < 1.
      EXIT.
    ENDIF.

    IF int_val = 1.
      column = 'A'.
      EXIT.
    ENDIF.

    column = ''.
    frag = int_val.

    WHILE frag > 1 .
      input = floor( frag ).
      c_i_val = input MOD 26.
      IF c_i_val = 0.
        c_i_val = 26.
        input = input - 25.
      ENDIF.
      c_x_val = c_i_val + cl_abap_conv_out_ce=>uccp( 'A' ) - 1.
      cc = cl_abap_conv_in_ce=>uccp( c_x_val ).
      column = cc && column.
      frag = input / 26 .
    ENDWHILE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CONVERT_LONG_TO_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATE_STRING                    TYPE        STRING
* | [<-()] DATE                           TYPE        D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_long_to_date.
    DATA num_days TYPE i.

    num_days = floor( date_string ).

    IF me->dateformat1904 = abap_false.
      " 1900 based
      date = '18991231'.
      IF date_string > 59.
        " Microsoft thinks the year 1900 is a leap year... it is not!
        date = date + num_days - 1.
      ELSE.
        " From 1899-12-31 to 1900-02-28 Microsoft guesses the correct date
        date = date + num_days.
      ENDIF.
      " 1904 based
    ELSE.
      date = '19040101'.
      date = date + num_days.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CONVERT_SER_VAL_TO_DATE_TIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERIAL_VALUE_STRING            TYPE        STRING
* | [<---] DATE                           TYPE        D
* | [<---] TIME                           TYPE        T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_ser_val_to_date_time.
    DATA:
      date_str  TYPE string,
      time_str  TYPE string,
      date_time TYPE decfloat34.

    TRY.
        date_time = serial_value_string.
      CATCH cx_root.
        "Not able to interpret as dec value
        EXIT.
    ENDTRY.
    date_str = floor( date_time ).
    IF date_str NE '0'.
      date = convert_long_to_date( date_str ).
    ENDIF.

    time_str = frac( date_time ).
    IF time_str NE '0'.
      time = convert_dec_time_to_hhmmss( time_str ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_FDT_XL_SPREADSHEET_V2=>CREATE_BLANK_DOCUMENT_STRUCT
* +-------------------------------------------------------------------------------------------------+
* | [--->] META_TITLE                     TYPE        STRING(optional)
* | [--->] META_SUBJECT                   TYPE        STRING(optional)
* | [--->] META_DESCRIPTION               TYPE        STRING(optional)
* | [--->] META_KEYWORDS                  TYPE        STRING(optional)
* | [--->] IT_SHEET_DATA                  TYPE        TT_SHEET_DATA
* | [<-()] XBLANK_DOCUMENT                TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_blank_document_struct.


    DATA: lo_blank_doc TYPE REF TO cl_abap_zip,
          lo_converter TYPE REF TO cl_abap_conv_out_ce.

    DATA: xml           TYPE string,
          timestamp     TYPE timestamp,
          utc           TYPE string,
          utc_str       TYPE string,
          lv_sheet_name TYPE string.

* Create the empty zip package
    CREATE OBJECT lo_blank_doc.

* Create the obligatory [Content_Types].xml
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">`. "#EC NOTEXT
    xml = xml && `<Override PartName="/xl/theme/theme1.xml" ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>`. "#EC NOTEXT
    xml = xml && `<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>`. "#EC NOTEXT
    xml = xml && `<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>`. "#EC NOTEXT
    xml = xml && `<Default Extension="xml" ContentType="application/xml"/>`. "#EC NOTEXT
    xml = xml && `<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>`. "#EC NOTEXT
    xml = xml && `<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>`. "#EC NOTEXT

    LOOP AT it_sheet_data ASSIGNING FIELD-SYMBOL(<ls_sheet_data>).

      DATA(lv_sheet_num) = sy-tabix.

      xml = xml && '<Override PartName="/xl/worksheets/sheet'
      && lv_sheet_num
      && '.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'. "#EC NOTEXT

    ENDLOOP.

    xml = xml && `<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>`. "#EC NOTEXT
    xml = xml && `<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>`. "#EC NOTEXT
    xml = xml && `</Types>`.                                "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = '[Content_Types].xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

* Create the obligatory _rels/.rels
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`. "#EC NOTEXT
    xml = xml && `<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>`. "#EC NOTEXT
    xml = xml && `<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>`. "#EC NOTEXT
    xml = xml && `<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>`. "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = '_rels/.rels' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

* Create the obligatory docProps/app.xml
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" `. "#EC NOTEXT
    xml = xml && `xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">`. "#EC NOTEXT
    xml = xml && `<Application>SAP NetWeaver Application Server</Application>`. "#EC NOTEXT
    xml = xml && `<DocSecurity>0</DocSecurity>`.            "#EC NOTEXT
    xml = xml && `<TitlesOfParts>`.                         "#EC NOTEXT
    xml = xml && `<vt:vector size="2" baseType="lpstr"><vt:lpstr>` && lv_sheet_name && `</vt:lpstr>`. "#EC NOTEXT
    xml = xml && `<vt:lpstr>HELP</vt:lpstr></vt:vector>`.   "#EC NOTEXT
    xml = xml && `</TitlesOfParts>`.                        "#EC NOTEXT
    xml = xml && `<Company>SAP</Company>`.                  "#EC NOTEXT
    xml = xml && `<AppVersion>7.1000</AppVersion>`.         "#EC NOTEXT
    xml = xml && `</Properties>`.                           "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = 'docProps/app.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

* Create the obligatory docProps/core.xml
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" `. "#EC NOTEXT
    xml = xml && `xmlns:dc="http://purl.org/dc/elements/1.1/" `. "#EC NOTEXT
    xml = xml && `xmlns:dcterms="http://purl.org/dc/terms/" `. "#EC NOTEXT
    xml = xml && `xmlns:dcmitype="http://purl.org/dc/dcmitype/" `. "#EC NOTEXT
    xml = xml && `xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">`. "#EC NOTEXT
    xml = xml && `<dc:title>` && meta_title && `</dc:title>`. "#EC NOTEXT
    xml = xml && `<dc:subject>` && meta_subject && `</dc:subject>`. "#EC NOTEXT
    xml = xml && `<dc:creator>` && sy-uname && `</dc:creator>`. "#EC NOTEXT
    xml = xml && `<cp:keywords>` && meta_keywords && `</cp:keywords>`. "#EC NOTEXT
    xml = xml && `<dc:description>` && meta_description && `</dc:description>`. "#EC NOTEXT
    GET TIME STAMP FIELD timestamp. " Get current UTC
    utc = timestamp.
    CONCATENATE utc+0(4) '-' utc+4(2) '-' utc+6(2) 'T' utc+8(2) ':' utc+10(2) ':' utc+12(2) 'Z'  INTO utc_str. "#EC NOTEXT
    xml = xml && `<dcterms:created xsi:type="dcterms:W3CDTF">` && utc_str && `</dcterms:created>`. "#EC NOTEXT
    xml = xml && `<dcterms:modified xsi:type="dcterms:W3CDTF">` && utc_str && `</dcterms:modified>`. "#EC NOTEXT
    xml = xml && `</cp:coreProperties>`.                    "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = 'docProps/core.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

* Create the obligatory xl/_rels/workbook.xml.rels
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`. "#EC NOTEXT

    xml = xml && `<Relationship Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>`. "#EC NOTEXT

    LOOP AT it_sheet_data ASSIGNING <ls_sheet_data>.
      lv_sheet_num = sy-tabix.

      xml = xml && `<Relationship Id="rId` && lv_sheet_num &&
            `" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" ` &&
            `Target="worksheets/sheet` && lv_sheet_num && `.xml"/>`. "#EC NOTEXT
    ENDLOOP.

    xml = xml && `<Relationship Id="rId6" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>`. "#EC NOTEXT

    xml = xml && `</Relationships>`.                        "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = 'xl/_rels/workbook.xml.rels' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

    LOOP AT it_sheet_data ASSIGNING <ls_sheet_data>.

      lv_sheet_num = sy-tabix.

      xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
      xml = xml && `<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">`. "#EC NOTEXT
      xml = xml && `<dimension ref="A1"/>`.                 "#EC NOTEXT
      xml = xml && `<sheetViews><sheetView workbookViewId="0"/></sheetViews>`. "#EC NOTEXT
      xml = xml && `<sheetFormatPr defaultRowHeight="15"/>`. "#EC NOTEXT
      xml = xml && `<sheetData/>`.                          "#EC NOTEXT
      xml = xml && `<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>`. "#EC NOTEXT
      xml = xml && `</worksheet>`.                          "#EC NOTEXT
      lo_converter = cl_abap_conv_out_ce=>create( ).
      lo_converter->write( data = xml ).
      lo_blank_doc->add( name = 'xl/worksheets/sheet' && lv_sheet_num && '.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
      FREE lo_converter.

    ENDLOOP.

* Create the obligatory xl/sharedStrings.xml
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"></sst>`. "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = 'xl/sharedStrings.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

* Create the obligatory xl/styles.xml
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">`. "#EC NOTEXT
    xml = xml && `<fonts count="2">`.                       "#EC NOTEXT
    xml = xml && `<font><sz val="11"/><color theme="1"/><name val="Calibri"/><family val="2"/><scheme val="minor"/></font>`. "#EC NOTEXT
    xml = xml && `<font><b/><sz val="11"/><color theme="1"/><name val="Calibri"/><family val="2"/><scheme val="minor"/></font>`. "#EC NOTEXT
    xml = xml && `</fonts>`.                                "#EC NOTEXT
    xml = xml && `<fills>`.                                 "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="none"/></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="gray125" /></fill>`. "#EC NOTEXT
    "xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="0" tint="-0.14999847407452621" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFDBD5BF" /></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFE2EBF1" /></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFF3E9BE" /></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFEDF4F8" /></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="9" tint="0.59999389629810485" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFFC6868" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="4" tint="0.59999389629810485" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="6" tint="0.59999389629810485" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFFFFF00" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFFF3300" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FF92D050" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
    xml = xml && `</fills>`.                                "#EC NOTEXT
    xml = xml && '<borders count="2"><border><left/><right/><top/><bottom/><diagonal/></border>'. "#EC NOTEXT
    xml = xml && '<border><left style="thin"><color rgb="FFFFFFFF" /></left><right style="thin"><color rgb="FFFFFFFF" /></right>'. "#EC NOTEXT
    xml = xml && '<top style="thin"><color rgb="FFFFFFFF" /></top><bottom style="thin"><color rgb="FFFFFFFF" /></bottom><diagonal /></border>'. "#EC NOTEXT
    xml = xml && '</borders>'.                              "#EC NOTEXT
    xml = xml && `<cellStyleXfs>`.                          "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="0" fillId="0" borderId="0"/>`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="2" borderId="1" applyFont="0"/>`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="3" borderId="1" applyFont="0"/>`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="4" borderId="1" applyFont="0"/>`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="5" borderId="1" applyFont="0"/>`. "#EC NOTEXT
    xml = xml && `</cellStyleXfs>`.                         "#EC NOTEXT
*  IF is_alv_document = abap_true.
    xml = xml && `<cellXfs>`.                               "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="0" fillId="0" borderId="0" xfId="0"/>`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="2" borderId="0" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="3" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="4" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="5" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="6" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="7" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="8" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="9" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="10" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="11" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `<xf numFmtId="49" fontId="1" fillId="12" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
    xml = xml && `</cellXfs>`.                              "#EC NOTEXT
*  ELSE.
*  xml = xml && `<cellXfs count="2"><xf numFmtId="49" fontId="0" fillId="0" borderId="0" xfId="0"/><xf numFmtId="49" fontId="1" fillId="2" borderId="0" xfId="0" applyFont="1"applyFill="1" /></cellXfs>`. "#EC NOTEXT
*  ENDIF.
    xml = xml && `<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>`. "#EC NOTEXT
    xml = xml && `<dxfs count="0"/><tableStyles count="0" defaultTableStyle="TableStyleMedium9" defaultPivotStyle="PivotStyleLight16"/>`. "#EC NOTEXT
    "xml = xml && '<colors><mruColors><color rgb="FFC6FBC6" /></mruColors></colors>'.
    xml = xml && `</styleSheet>`.                           "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = 'xl/styles.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

* Create the obligatory xl/workbook.xml
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">`. "#EC NOTEXT
    xml = xml && `<fileVersion appName="xl" lastEdited="1" lowestEdited="1" rupBuild="4505"/>`. "#EC NOTEXT
    xml = xml && `<workbookPr filterPrivacy="1" defaultThemeVersion="124226"/>`. "#EC NOTEXT
    xml = xml && `<bookViews><workbookView xWindow="240" yWindow="105" windowWidth="14805" windowHeight="8010"/></bookViews>`. "#EC NOTEXT
    xml = xml && `<sheets>`.                                "#EC NOTEXT

    LOOP AT it_sheet_data ASSIGNING <ls_sheet_data>.
      lv_sheet_num = sy-tabix.

      xml = xml && `<sheet name="` && <ls_sheet_data>-sheet_name && `" sheetId="` && lv_sheet_num && `" r:id="rId` && lv_sheet_num && `"/>`. "#EC NOTEXT
    ENDLOOP.

    xml = xml && `</sheets>`.                               "#EC NOTEXT
    xml = xml && `<calcPr calcId="124519"/>`.               "#EC NOTEXT
    xml = xml && `</workbook>`.                             "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = 'xl/workbook.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

    xblank_document = lo_blank_doc->save( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CREATE_CELL_ELEMENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ROW                         TYPE        ANY
* | [--->] IO_ROW_ELEMENT                 TYPE REF TO IF_IXML_ELEMENT
* | [--->] IV_ROW_NUMBER                  TYPE        STRING
* | [--->] IV_COLOR_FNAME                 TYPE        FIELDNAME
* | [--->] IT_COLUMN_DESCRIPTION          TYPE        TT_MAP_DESCR
* | [--->] IO_SHEET_XML_DOCUMENT          TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_cell_elements.

    DATA: lv_index               TYPE i,
          ls_column_description  LIKE LINE OF it_column_description,
          ls_column              TYPE if_fdt_doc_spreadsheet=>s_column,
          lo_cell_element        TYPE REF TO if_ixml_element,    " <c> = cell
          lo_cell_value_element  TYPE REF TO if_ixml_element,
          lv_column_type         TYPE REF TO cl_abap_typedescr,
          lv_cell_content_int    TYPE string,
          lv_cell_content_string TYPE string,
          lv_text                TYPE REF TO if_ixml_text,
          lt_scol                TYPE lvc_t_scol.

    FIELD-SYMBOLS: <la_status>   TYPE any,
                   <la_type>     TYPE any,
                   <la_color>    TYPE string,
                   <la_msg_type> TYPE any,
                   <la_cell>     TYPE any,
                   <lt_scol>     TYPE lvc_t_scol.

    IF iv_color_fname IS NOT INITIAL.
      TRY.
          ASSIGN COMPONENT iv_color_fname OF STRUCTURE is_row TO <lt_scol>.
          lt_scol = <lt_scol>.
          SORT lt_scol BY fname.
        CATCH cx_root.
      ENDTRY.
    ENDIF.

    LOOP AT it_column_description INTO ls_column_description.
      lv_index = sy-tabix.
      ASSIGN COMPONENT ls_column_description-fieldname OF STRUCTURE is_row TO <la_cell>. "#EC CI_BOX_OK
      lo_cell_element = io_sheet_xml_document->create_element_ns( 'c' ).
      lo_cell_element->set_attribute_ns( name = 'r' value = convert_i_to_column( lv_index ) && iv_row_number ).

      IF lt_scol IS NOT INITIAL.
        READ TABLE lt_scol ASSIGNING FIELD-SYMBOL(<ls_scol>) WITH KEY fname = ls_column_description-fieldname BINARY SEARCH.
        IF sy-subrc = 0.

          CASE <ls_scol>-color-col.
            WHEN 3.
              lo_cell_element->set_attribute_ns( name = 's' value = '10'  ).  " Warning
            WHEN 5.
              lo_cell_element->set_attribute_ns( name = 's' value = '12'  ).  " Green  "#EC NOTEXT
            WHEN 6.
              lo_cell_element->set_attribute_ns( name = 's' value = '7'  ).  " Red  "#EC NOTEXT
          ENDCASE.
        ENDIF.

      ENDIF.

      lo_cell_value_element = io_sheet_xml_document->create_element_ns( 'v' ).

      lv_column_type ?= ls_column_description-type.

      CLEAR: lv_cell_content_string,
             lv_cell_content_int.

      CASE lv_column_type->type_kind.
        WHEN
          cl_abap_typedescr=>typekind_char         OR
          cl_abap_typedescr=>typekind_clike        OR
          cl_abap_typedescr=>typekind_csequence    OR
          cl_abap_typedescr=>typekind_num          OR
          cl_abap_typedescr=>typekind_numeric      OR
          cl_abap_typedescr=>typekind_simple       OR
          cl_abap_typedescr=>typekind_string       OR
          cl_abap_typedescr=>typekind_w            OR
          cl_abap_typedescr=>typekind_date         OR
          cl_abap_typedescr=>typekind_time         .

          " String value
          CASE lv_column_type->type_kind.
            WHEN cl_abap_typedescr=>typekind_data OR
                 cl_abap_typedescr=>typekind_date.

              DATA(lv_date) = CONV d( <la_cell> ).
              lv_cell_content_string = |{ lv_date DATE = ENVIRONMENT }|.

            WHEN cl_abap_typedescr=>typekind_time.

              DATA(lv_time) = CONV t( <la_cell> ).
              lv_cell_content_string = |{ lv_time TIME = ENVIRONMENT }|.

            WHEN OTHERS.
              lv_cell_content_string = <la_cell>.
          ENDCASE.

          lo_cell_element->set_attribute_ns( name = 't' value = 's' ).
          lv_cell_content_int = set_shared_string_buffer( lv_cell_content_string ).

        WHEN
         cl_abap_typedescr=>typekind_decfloat     OR
         cl_abap_typedescr=>typekind_decfloat16   OR
         cl_abap_typedescr=>typekind_decfloat34   OR
         cl_abap_typedescr=>typekind_float        OR
         cl_abap_typedescr=>typekind_int          OR
         cl_abap_typedescr=>typekind_int1         OR
         cl_abap_typedescr=>typekind_int2         OR
*          cl_abap_typedescr=>TYPEKIND_INT8         OR
         cl_abap_typedescr=>typekind_packed       .

          lv_cell_content_int = <la_cell>.

        WHEN OTHERS.

*        WHEN
*          cl_abap_typedescr=>typekind_class        OR
*          cl_abap_typedescr=>typekind_dref         OR
*          cl_abap_typedescr=>typekind_hex          OR
*          cl_abap_typedescr=>typekind_intf         OR
*          cl_abap_typedescr=>typekind_iref         OR
*          cl_abap_typedescr=>typekind_oref         OR
*          cl_abap_typedescr=>typekind_struct1      OR
*          cl_abap_typedescr=>typekind_struct2      OR
*          cl_abap_typedescr=>typekind_table        OR
*          cl_abap_typedescr=>typekind_xsequence    OR
*          cl_abap_typedescr=>typekind_data         OR
*          cl_abap_typedescr=>typekind_xstring.

      ENDCASE.

      cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_cell_content_int ).
      lv_text = io_sheet_xml_document->create_text( lv_cell_content_int ).

      lo_cell_value_element->append_child( lv_text ).
      lo_cell_element->append_child( lo_cell_value_element ).
      io_row_element->append_child( lo_cell_element ).
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CREATE_CELL_ELEMENTS_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ROW_ELEMENT                 TYPE REF TO IF_IXML_ELEMENT
* | [--->] IT_COLUMN_DESCRIPTION          TYPE        TT_MAP_DESCR
* | [--->] IO_SHEET_XML_DOCUMENT          TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_cell_elements_header.

    DATA: lv_index               TYPE i,
          ls_column_description  LIKE LINE OF it_column_description,
          ls_column              TYPE if_fdt_doc_spreadsheet=>s_column,
          lo_cell_element        TYPE REF TO if_ixml_element,    " <c> = cell
          lo_cell_value_element  TYPE REF TO if_ixml_element,
          lv_ls_column_type      TYPE REF TO cl_abap_typedescr,
          lv_cell_content_int    TYPE string,
          lv_cell_content_string TYPE string,
          lv_text                TYPE REF TO if_ixml_text.

    FIELD-SYMBOLS: <itab_status>   TYPE any,
                   <itab_type>     TYPE any,
                   <itab_color>    TYPE string,
                   <itab_msg_type> TYPE any,
                   <itab_cell>     TYPE any.


    LOOP AT it_column_description INTO ls_column_description.
      lv_index = sy-tabix.
      lo_cell_element = io_sheet_xml_document->create_element_ns( 'c' ).
      lo_cell_element->set_attribute_ns( name = 'r' value = convert_i_to_column( sy-tabix ) && '1' ).

      lo_cell_element->set_attribute_ns( name = 's' value = '3' ). " Bold

      lo_cell_element->set_attribute_ns( name = 't' value = 's' ). " String value
      lo_cell_value_element = io_sheet_xml_document->create_element_ns( 'v' ).
      lv_cell_content_int = set_shared_string_buffer( ls_column_description-descr ).
      lv_text = io_sheet_xml_document->create_text( lv_cell_content_int ).
      lo_cell_value_element->append_child( lv_text ).
      lo_cell_element->append_child( lo_cell_value_element ).
      io_row_element->append_child( lo_cell_element ).
    ENDLOOP.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FDT_XL_SPREADSHEET_V2=>CREATE_DOCUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] NAME                           TYPE        STRING (default =IF_FDT_DOC_SPREADSHEET=>DEFAULT_FILENAME)
* | [--->] IT_SHEET_DATA                  TYPE        TT_SHEET_DATA
* | [<-()] XDOCUMENT                      TYPE        XSTRING
* | [!CX!] CX_FDT_EXCEL_CORE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_document.
    DATA:
      xblank_document  TYPE xstring,
      xsheet           TYPE xstring,
      obj_ssheet_ooxml TYPE REF TO zcl_fdt_xl_spreadsheet_v2,
      ws_tab           TYPE STANDARD TABLE OF string,
      ws               TYPE string,
      xml_doc          TYPE REF TO if_ixml_document,
      iv_call_type     TYPE i,
      lt_sheet_data    TYPE tt_sheet_data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

    xblank_document = zcl_fdt_xl_spreadsheet_v2=>create_blank_document_struct(
                        it_sheet_data = it_sheet_data ).

    CREATE OBJECT obj_ssheet_ooxml
      EXPORTING
        document_name = name
        xdocument     = xblank_document.
    obj_ssheet_ooxml->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = ws_tab ).


    LOOP AT it_sheet_data ASSIGNING FIELD-SYMBOL(<ls_sheet_data>).

      xsheet = obj_ssheet_ooxml->get_worksheet_by_name( <ls_sheet_data>-sheet_name ).

      xml_doc = obj_ssheet_ooxml->parse_xml( xsheet ).
      IF xml_doc IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND <ls_sheet_data> TO lt_sheet_data.

      obj_ssheet_ooxml->set_worksheet_sheetdata(
        EXPORTING
          sheet_xml_document = xml_doc
          is_sheet_data      = <ls_sheet_data>
      ).

      CLEAR lt_sheet_data.

      obj_ssheet_ooxml->overwrite_worksheet_by_name(
        worksheet_name = <ls_sheet_data>-sheet_name
        xsheet        = obj_ssheet_ooxml->render_xml( xml_doc )
      ).

      CLEAR xml_doc.
    ENDLOOP.

* Set the shared strings finally
    obj_ssheet_ooxml->set_shared_strings( ).

* returning the document as XSTRING
    xdocument = obj_ssheet_ooxml->if_fdt_document~get_document( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CREATE_WORKSHEET_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ROW                         TYPE        ANY
* | [--->] IV_ROW_NUMBER                  TYPE        INT4
* | [--->] IV_COLOR_FNAME                 TYPE        FIELDNAME
* | [--->] IT_COLUMN_DESCRIPTION          TYPE        TT_MAP_DESCR
* | [--->] IO_SHEET_XML_DOCUMENT          TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IO_SHEET_DATA_NODE             TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_worksheet_row.

    DATA: lo_row_element         TYPE REF TO if_ixml_element,
          lv_row_number_string   TYPE string,
          lv_group_number_string TYPE string.
    FIELD-SYMBOLS: <la_status> TYPE any.


* Create new row
    lo_row_element = create_worksheet_row_base( io_sheet_xml_document = io_sheet_xml_document
                                                iv_row = iv_row_number ).
    lv_row_number_string = iv_row_number.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_row_number_string ). "remove blanks
    create_cell_elements( is_row                = is_row
                          io_row_element        = lo_row_element
                          iv_row_number         = lv_row_number_string
                          IV_COLOR_FNAME        = IV_COLOR_FNAME
                          it_column_description = it_column_description
                          io_sheet_xml_document = io_sheet_xml_document  ).



* Append row
    io_sheet_data_node->append_child( lo_row_element ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CREATE_WORKSHEET_ROW_BASE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SHEET_XML_DOCUMENT          TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IV_ROW                         TYPE        INT4
* | [<-()] RO_ROW_ELEMENT                 TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_worksheet_row_base.

    DATA: lv_row TYPE string.

    lv_row = iv_row.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_row ).
    ro_row_element = io_sheet_xml_document->create_element_ns( 'row' ).
    ro_row_element->set_attribute_ns( name = 'r' value = lv_row ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->CREATE_WORKSHEET_ROW_NTL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA                        TYPE        STANDARD TABLE
* | [--->] IS_ROW                         TYPE        ANY
* | [--->] IV_ROW_INDEX                   TYPE        SY-TABIX
* | [--->] IT_COLUMN                      TYPE        IF_FDT_DOC_SPREADSHEET=>T_COLUMN(optional)
* | [--->] IT_COLUMN_DESCRIPTION          TYPE        TT_MAP_DESCR
* | [--->] IO_SHEET_XML_DOCUMENT          TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IO_SHEET_DATA_NODE             TYPE REF TO IF_IXML_NODE
* | [--->] IV_IS_ALV_DOCUMENT             TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<-->] CV_ROW_NUMBER                  TYPE        INT4
* | [<-->] CV_GROUP_NUMBER_MAX            TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_worksheet_row_ntl.
*
*    DATA: lo_row_element         TYPE REF TO if_ixml_element,
*          lr_row                 TYPE REF TO data,
*          lv_row_number_string   TYPE string,
*          lv_group_number        TYPE int4,
*          lv_group_number_string TYPE string,
*          lr_int4                TYPE REF TO int4.
*    FIELD-SYMBOLS: <la_status> TYPE any,
*                   <la_row>    TYPE any,
*                   <la_level>  TYPE int4,
*                   <la_level1> TYPE int4.
*    CONSTANTS: lc_group_number_max TYPE int4 VALUE '7'.
*
*
*    CREATE DATA lr_row LIKE LINE OF it_data.
*    ASSIGN lr_row->* TO <la_row>.
*
*    ASSIGN COMPONENT 5 OF STRUCTURE is_row TO <la_level>.   " Get level
*
** First line in itab = First root row: Just create a regular row without grouping
*    IF iv_row_index = 1.
*      lo_row_element = create_worksheet_row_base( io_sheet_xml_document = io_sheet_xml_document
*                                                  iv_row = cv_row_number ).
*      lv_row_number_string = cv_row_number.
*      create_cell_elements( is_row                = is_row
*                            io_row_element        = lo_row_element
*                            iv_row_number         = lv_row_number_string
*                            it_column             = it_column
*                            it_column_description = it_column_description
*                            io_sheet_xml_document = io_sheet_xml_document
*                            iv_call_type          = if_fdt_doc_spreadsheet=>gc_call_new_lean_trace
*                            iv_is_alv_document    = iv_is_alv_document ).
*      io_sheet_data_node->append_child( lo_row_element ).
*      cv_row_number = cv_row_number + 1.
*      RETURN. " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< RETURN
*    ENDIF.
*
** Row is a root row (but not the first one): Insert an empty line beforehand to
** close the group of the previous root row. Root row itself gets no group.
*    IF <la_level> = cl_fdt_wd_lean_trace_helper=>gc_level_root.
*      insert_empty_worksheet_row( EXPORTING io_sheet_xml_document = io_sheet_xml_document
*                                            io_sheet_data_node = io_sheet_data_node
*                                   CHANGING
*                                            cv_row = cv_row_number ).
*
*      lo_row_element = create_worksheet_row_base( io_sheet_xml_document = io_sheet_xml_document
*                                                  iv_row = cv_row_number ).
*      lv_row_number_string = cv_row_number.
*      create_cell_elements( is_row                = is_row
*                            io_row_element        = lo_row_element
*                            iv_row_number         = lv_row_number_string
*                            it_column             = it_column
*                            it_column_description = it_column_description
*                            io_sheet_xml_document = io_sheet_xml_document
*                            iv_call_type          = if_fdt_doc_spreadsheet=>gc_call_new_lean_trace
*                            iv_is_alv_document    = iv_is_alv_document ).
*      io_sheet_data_node->append_child( lo_row_element ).
*      cv_row_number = cv_row_number + 1.
*      RETURN.  " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< RETURN
*    ENDIF.
*
** Handling of all non-root rows:
** Read previous line of itab (should not be a problem, since the first line
** was already handled above). Get the previous level
*    READ TABLE it_data INTO <la_row> INDEX ( iv_row_index - 1 ).
*    ASSERT sy-subrc = 0.
*    ASSIGN COMPONENT 5 OF STRUCTURE <la_row> TO <la_level1>.
*
*    " Create row
*    lo_row_element = create_worksheet_row_base( io_sheet_xml_document = io_sheet_xml_document
*                                                iv_row = cv_row_number ).
*    lv_row_number_string = cv_row_number.
*    create_cell_elements( is_row                = is_row
*                          io_row_element        = lo_row_element
*                          iv_row_number         = lv_row_number_string
*                          it_column             = it_column
*                          it_column_description = it_column_description
*                          io_sheet_xml_document = io_sheet_xml_document
*                          iv_call_type          = if_fdt_doc_spreadsheet=>gc_call_new_lean_trace
*                          iv_is_alv_document    = iv_is_alv_document ).
*
*    " Set group: Default: Overtake level as group.
*    " However, since Excel can deal only with eight group levels, we have to apply a limit here
*    " Group levels 0, 1-7
*    lv_group_number = <la_level>.
*    IF lv_group_number > lc_group_number_max.
*      lv_group_number = lc_group_number_max.
*    ENDIF.
*    IF lv_group_number > cv_group_number_max.
*      cv_group_number_max = lv_group_number.
*    ENDIF.
*    lv_group_number_string = lv_group_number.
*    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_group_number_string ).
*    lo_row_element->set_attribute_ns( name = 'outlineLevel' value = lv_group_number_string ).
*
*    " Append element
*    io_sheet_data_node->append_child( lo_row_element ).
*    cv_row_number = cv_row_number + 1.
*
** Last line in IT_DATA: Append an empty row to close the open group
*    IF iv_row_index = lines( it_data ).
*      insert_empty_worksheet_row( EXPORTING io_sheet_xml_document = io_sheet_xml_document
*                                            io_sheet_data_node = io_sheet_data_node
*                                   CHANGING
*                                            cv_row = cv_row_number ).
*    ENDIF.
*
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->EXTRACT_COL_FROM_A1_STYLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT                          TYPE        STRING
* | [<-()] COLUMN                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD extract_col_from_a1_style.

    DATA:
      coord_str TYPE string,
      cc        TYPE c,
      pos       TYPE i,
      strl      TYPE i.

    coord_str = input.
    TRANSLATE coord_str TO UPPER CASE.
    strl = strlen( coord_str ).

    DO strl TIMES.
      pos = sy-index - 1.
      cc = coord_str+pos(1).
      IF cc >= 'A' AND cc <= 'Z'.
        column = column && cc.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_CELLXFS
* +-------------------------------------------------------------------------------------------------+
* | [<---] CELLXFS                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cellxfs.

    " Get number format of defined cells
    DATA:
      x_styles       TYPE xstring,
      style_relation TYPE t_pod_relation.

    READ TABLE me->workbook_relations WITH KEY type = if_fdt_xl_types=>ooxml_relationship_styles INTO style_relation.

    TRY.
        x_styles = me->if_fdt_doc_pkg~get_file_as_xstring( workbook_folder && style_relation-target ).
        CALL TRANSFORMATION fdt_xl_get_cellxfs
          SOURCE XML x_styles
          RESULT numfmids = cellxfs.
      CATCH cx_fdt_excel_core.
        "Style XML file not found.
        EXIT.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_CELL_STYLE_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] XML_DOC                        TYPE REF TO IF_IXML_DOCUMENT
* | [--->] CELL_COORDINATE                TYPE        STRING
* | [<-()] STYLE_ID                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cell_style_id.
    DATA:
      cell          TYPE REF TO if_ixml_node,
      cell_iterator TYPE REF TO if_ixml_node_iterator.
    TYPE-POOLS ixml.

    style_id = 0.

* Process XML
    TRY.
        cell_iterator = xml_doc->get_elements_by_tag_name_ns( name = 'c'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main )->create_iterator( ).
        cell = cell_iterator->get_next( ).
      CATCH cx_root.
        " No <c> elements thus no value for the cell
        EXIT.
    ENDTRY.


* Searching cell TODO: dynamic XSLT?
    WHILE cell IS NOT INITIAL.
      IF get_attr_from_node( iv_name = 'r' io_node = cell ) = cell_coordinate.
        " Get the value
        TRY.
            style_id = get_attr_from_node( iv_name = 's' io_node  = cell  ).
          CATCH cx_root.
            " No s attribute, setting general format
            style_id = 0.
        ENDTRY.
        EXIT.
      ENDIF.
      cell = cell_iterator->get_next( ).
    ENDWHILE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_CELL_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_NAME                     TYPE        STRING
* | [--->] CELL_COORDINATE                TYPE        STRING
* | [<-()] CELL_VALUE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cell_value.
* max tab size: XFD1048576
    DATA:
      x_sheet   TYPE xstring,
      xml_doc   TYPE REF TO if_ixml_document,
      s_strings TYPE STANDARD TABLE OF string.

    TYPE-POOLS ixml.

    x_sheet = me->get_worksheet_by_name( sheet_name ).
    IF x_sheet IS INITIAL.
      " No sheet with this name
      EXIT.
    ENDIF.

* Get shared strings
    get_shared_strings( IMPORTING strings = s_strings ).

* Process XML
    xml_doc = parse_xml( x_sheet ).

    cell_value = get_cell_value_by_xmldoc_shrds(
        cell_coordinate = cell_coordinate
        shared_strings  = s_strings
        xml_doc         = xml_doc ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_CELL_VALUE_BY_XMLDOC_SHRDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] CELL_COORDINATE                TYPE        STRING
* | [--->] SHARED_STRINGS                 TYPE        STANDARD TABLE
* | [--->] XML_DOC                        TYPE REF TO IF_IXML_DOCUMENT
* | [<-()] CELL_VALUE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cell_value_by_xmldoc_shrds.

    DATA:
      cell           TYPE REF TO if_ixml_node,
      cell_iterator  TYPE REF TO if_ixml_node_iterator,
      value_element  TYPE REF TO if_ixml_element,
      value          TYPE string,
      cell_string_id TYPE string.
    TYPE-POOLS ixml.

* Process XML
    TRY.
        cell_iterator = xml_doc->get_elements_by_tag_name_ns( name = 'c'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main )->create_iterator( ).
        cell = cell_iterator->get_next( ).
      CATCH cx_root.
        " Either no <c> elements so no value for the cell
        cell_value = ''.
        EXIT.
    ENDTRY.

* Searching cell TODO: dynamic XSLT?
    WHILE cell IS NOT INITIAL.
      IF get_attr_from_node( iv_name = 'r' io_node = cell ) = cell_coordinate.
        " Get the value
        TRY.
* First child returns <f> formula if present, otherwise <v>
*          value_element ?= cell->get_first_child( )->query_interface( ixml_iid_element ).
            value_element ?= cell->query_interface( ixml_iid_element  ).
            value_element = value_element->find_from_name_ns( name = 'v'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
            value = value_element->get_value( ).
          CATCH cx_root.
            " No <v> element present so no value for this cell
            value = ''.
        ENDTRY.
        " Check type
        IF get_attr_from_node( iv_name = 't' io_node = cell ) = 's'.
          " String value... get respective sharedString
          cell_string_id = value + 1.
          READ TABLE shared_strings INDEX cell_string_id INTO cell_value.
        ELSE.
          cell_value = value.
        ENDIF.
        EXIT.
      ENDIF.
      cell = cell_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_COL_AND_ROW_FROM_A1_STYLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] COORDINATE                     TYPE        STRING
* | [<---] COLUMN_NUMBER                  TYPE        I
* | [<---] ROW_NUMBER                     TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_col_and_row_from_a1_style.

    DATA:
      coord_str  TYPE string,
      cc         TYPE c,
      pos        TYPE i,
      strl       TYPE i,
      column_str TYPE string VALUE ''.

    coord_str = coordinate.
    CLEAR column_number.
    CLEAR row_number.

    TRANSLATE coord_str TO UPPER CASE.
    strl = strlen( coord_str ).

    DO strl TIMES.
      pos = sy-index - 1.
      cc = coord_str+pos(1).
      IF cc >= 'A' AND cc <= 'Z'.
        column_str = column_str && cc.
      ELSE.
        row_number = row_number && cc.
      ENDIF.
    ENDDO.

    column_number = convert_column_to_i( column_str ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_COL_FROM_A1_STYLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] COORDINATE                     TYPE        STRING
* | [<-()] COLUMN_NUMBER                  TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_col_from_a1_style.

    DATA:
      coord_str  TYPE string,
      cc         TYPE c,
      pos        TYPE i,
      strl       TYPE i,
      column_str TYPE string VALUE ''.

    coord_str = coordinate.
    TRANSLATE coord_str TO UPPER CASE.
    strl = strlen( coord_str ).

    DO strl TIMES.
      pos = sy-index - 1.
      cc = coord_str+pos(1).
      IF cc >= 'A' AND cc <= 'Z'.
        column_str = column_str && cc.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    column_number = convert_column_to_i( column_str ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_ITAB_FOR_ALV_BY_XMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] RANGE_START                    TYPE        STRING
* | [--->] RANGE_END                      TYPE        STRING
* | [--->] XML_DOC                        TYPE REF TO IF_IXML_DOCUMENT
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_itab_for_alv_by_xmldoc.
    DATA:
      shared_strings TYPE STANDARD TABLE OF string,
      cellxfs        TYPE STANDARD TABLE OF string,
      numfmtid       LIKE LINE OF me->numfmtids.

* Process XML
    DATA:
      row             TYPE REF TO if_ixml_node,
      rows            TYPE REF TO if_ixml_node_collection,
      row_iterator    TYPE REF TO if_ixml_node_iterator,
      row_element     TYPE REF TO if_ixml_element,
      col             TYPE REF TO if_ixml_node,
      cols            TYPE REF TO if_ixml_node_collection,
      col_element     TYPE REF TO if_ixml_element,
      col_iterator    TYPE REF TO if_ixml_node_iterator,
      current_column  TYPE string,
      current_col_num TYPE i,
      style_id        TYPE string,
      cell_value      TYPE string,
      cell_string_id  TYPE string,
      cell_element    TYPE REF TO if_ixml_element,
      loopcount       TYPE i.

    FIELD-SYMBOLS: <dyn_table> TYPE STANDARD TABLE,
                   <dyn_wa>    TYPE any.
    DATA:
      dy_table TYPE REF TO data,
      dy_line  TYPE REF TO data,
      xfc      TYPE lvc_s_fcat,
      ifc      TYPE lvc_t_fcat.


    TYPE-POOLS:
                ixml,
                abap.

    IF xml_doc IS INITIAL.
      EXIT.
    ENDIF.

* Get shared strings
    get_shared_strings( IMPORTING strings = shared_strings ).

* Get styles table for number formats
    get_cellxfs( IMPORTING cellxfs = cellxfs ).

    FIELD-SYMBOLS:
      <dyn_row_count> TYPE any,
      <dyn_column>    TYPE any.


* Get collection of rows
    rows = xml_doc->get_elements_by_tag_name_ns( name  = 'row'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
    IF rows->get_length( ) = 0.
      " No columns to process, leaving
      EXIT.
    ENDIF.
    row_iterator = rows->create_iterator( ).

* Create linetype for interal table
* First row contains column heads
    row = row_iterator->get_next( ).
    row_element ?= row->query_interface( ixml_iid_element ).

    cols = row_element->get_elements_by_tag_name_ns( name = 'c'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
    col_iterator = cols->create_iterator( ).
    col = col_iterator->get_next( ).
    WHILE col IS NOT INITIAL.
      col_element ?= col->query_interface( ixml_iid_element ).
      cell_element = col_element->find_from_name_ns( name = 'v'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
      IF cell_element IS NOT INITIAL.
        cell_value = cell_element->get_value( ).
        " Check cell type
        IF get_attr_from_node( iv_name = 't' io_node = col ) = 's'.
          cell_string_id = cell_value + 1.
          READ TABLE shared_strings INDEX cell_string_id INTO cell_value.
        ELSE.
          " not a string value
        ENDIF.
        " Check format of cell value because of date/time representation
*        read table cellxfs into style_id index ( get_attr_from_node( name = 's' node = col ) + 1 ).
*        read table me->numfmtids into numfmtid with key id = style_id.
        xfc-fieldname = cell_value.
        xfc-datatype =  'g'. " TODO depending on number format?
        xfc-inttype = 'g'.
        xfc-intlen =  0.
        APPEND xfc TO ifc.
        CLEAR xfc.
      ENDIF." Cell element is not initial
      col = col_iterator->get_next( ).
    ENDWHILE." Cols

* Create internal table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = ifc " Field Catalog
      IMPORTING
        ep_table                  = dy_table " Pointer to Dynamic Data Table
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ASSIGN dy_table->* TO <dyn_table>.
    CREATE DATA dy_line LIKE LINE OF <dyn_table>.
    ASSIGN dy_line->* TO <dyn_wa>.


* Fill content
    row = row_iterator->get_next( ).
    WHILE row IS NOT INITIAL .
      CLEAR dy_line.
      UNASSIGN <dyn_wa>.
      CREATE DATA dy_line LIKE LINE OF <dyn_table>.
      ASSIGN dy_line->* TO <dyn_wa>.

      row_element ?= row->query_interface( ixml_iid_element ).

      cols = row_element->get_elements_by_tag_name_ns( name = 'c'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
      IF cols->get_length( ) = 0.
        " This row contains no column/values, continue
        row = row_iterator->get_next( ).
        CONTINUE.
      ELSE.
        col_iterator = cols->create_iterator( ).
        col = col_iterator->get_next( ).
      ENDIF.

      WHILE col IS NOT INITIAL.
        col_element ?= col->query_interface( ixml_iid_element ).
        current_col_num = convert_column_to_i( extract_col_from_a1_style( get_attr_from_node( iv_name = 'r' io_node = col ) ) ).
        READ TABLE ifc INDEX current_col_num INTO xfc.
        current_column = xfc-fieldname.

        cell_element = col_element->find_from_name_ns( name = 'v'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
        IF cell_element IS NOT INITIAL.
          cell_value = cell_element->get_value( ).
          " Check cell type
          IF get_attr_from_node( iv_name = 't' io_node = col ) = 's'.
            cell_string_id = cell_value + 1.
            READ TABLE shared_strings INDEX cell_string_id INTO cell_value.
          ELSE.
            " not a string value
          ENDIF.
          " Check format of cell value because of date/time representation
          READ TABLE cellxfs INTO style_id INDEX ( get_attr_from_node( iv_name = 's' io_node = col ) + 1 ).
          READ TABLE me->numfmtids INTO numfmtid WITH KEY id = style_id.
          " Hook method for implementing other format representations:
          cell_value = convert_cell_value_by_numfmt( cell_value = cell_value number_format = numfmtid-formatcode ).
          ASSIGN COMPONENT current_column OF STRUCTURE <dyn_wa> TO <dyn_column>.
          " write cell_value.
          <dyn_column> = cell_value.
        ENDIF." Cell element is not initial

        col = col_iterator->get_next( ).
      ENDWHILE." Cols
      INSERT <dyn_wa> INTO <dyn_table> INDEX sy-index.
      row = row_iterator->get_next( ).
    ENDWHILE.

    itab = dy_table.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_ITAB_FOR_ALV_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_itab_for_alv_update.
* max tab size: XFD1048576
    DATA:
      x_sheet   TYPE xstring,
      worksheet LIKE LINE OF zcl_fdt_xl_spreadsheet_v2=>worksheets,
      dim       TYPE string,
      dim_start TYPE string,
      dim_end   TYPE string,
      xml_doc   TYPE REF TO if_ixml_document.

    TYPE-POOLS ixml.

* Get sheet
    READ TABLE me->worksheets INDEX 1 INTO worksheet.
    IF worksheet IS INITIAL.
      " No sheet, no itab
      EXIT.
    ENDIF.

* Get dimensions of the sheet
    x_sheet = me->get_worksheet_by_name( worksheet-name ).
    CALL TRANSFORMATION fdt_xl_get_sheet_dimension
          SOURCE XML x_sheet
          RESULT dimension = dim.
    IF dim IS INITIAL.
      EXIT.
    ENDIF.
    SPLIT dim AT ':' INTO dim_start dim_end.

    IF dim_end IS INITIAL.
      " Only one cell is populated
      dim_end = dim_start.
    ENDIF.

* Parse document
    xml_doc = parse_xml( x_sheet ).
    IF xml_doc IS INITIAL.
      EXIT.
    ENDIF.

    itab = get_itab_for_alv_by_xmldoc(
        range_start = dim_start
        range_end   = dim_end
        xml_doc     = xml_doc
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_ITAB_FROM_SHEET
* +-------------------------------------------------------------------------------------------------+
* | [--->] WORKSHEET_NAME                 TYPE        STRING
* | [--->] IV_CALLER                      TYPE        I(optional)
* | [--->] IV_GET_LANGUAGE                TYPE        ABAP_BOOL(optional)
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_itab_from_sheet.
* max tab size: XFD1048576
    DATA:
      x_sheet   TYPE xstring,
      w_sheet   TYPE ooxml_worksheet,
      dim       TYPE string,
      dim_start TYPE string,
      dim_end   TYPE string,
      xml_doc   TYPE REF TO if_ixml_document.

    TYPE-POOLS ixml.

* Get dimensions of the sheet
    x_sheet = me->get_worksheet_by_name( worksheet_name ).
    CALL TRANSFORMATION fdt_xl_get_sheet_dimension
          SOURCE XML x_sheet
          RESULT dimension = dim.
    IF dim IS INITIAL.
      EXIT.
    ENDIF.
    SPLIT dim AT ':' INTO dim_start dim_end.
    IF dim_end IS INITIAL.
      " Only one cell is populated
      dim_end = dim_start.
    ENDIF.

    "language retrieve
    IF iv_get_language EQ abap_true.
      IF iv_caller EQ if_fdt_doc_spreadsheet=>gc_call_dec_table.
        dim_start = 'A48'.                                  "#EC NOTEXT
        dim_end   = 'B48'.                                  "#EC NOTEXT
      ELSEIF iv_caller EQ if_fdt_doc_spreadsheet=>gc_call_simulation.
        dim_start = 'A17'.                                  "#EC NOTEXT
        dim_end   = 'B17'.                                  "#EC NOTEXT
      ENDIF.
    ENDIF.
* Parse document
    xml_doc = parse_xml( x_sheet ).
    IF xml_doc IS INITIAL.
      EXIT.
    ENDIF.

    itab = get_range_itab_from_xmldoc(
        range_start = dim_start
        range_end   = dim_end
        xml_doc     = xml_doc
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FDT_XL_SPREADSHEET_V2=>GET_LANGU_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LANGU                       TYPE        CHAR1
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_langu_text.

    DATA: lv_text TYPE sptxt.

    SELECT SINGLE sptxt FROM t002t INTO lv_text WHERE spras = sy-langu AND sprsl = iv_langu.

    IF lv_text IS NOT INITIAL.
      rv_text = lv_text.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_MAPPING
* +-------------------------------------------------------------------------------------------------+
* | [<-()] MAPPING                        TYPE REF TO CL_FDT_DOC_MAPPING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mapping.
    DATA mapping_rel TYPE t_pod_relation.

    IF me->obj_doc_zip IS INITIAL.
      " No document loaded... no chance to set mapping
      EXIT.
    ENDIF.

    " Check whether mapping is defined
    READ TABLE me->package_relations INTO mapping_rel WITH KEY type = if_fdt_xl_types=>mapping_namespace.

    IF mapping_rel IS INITIAL.
      " No mapping defined... leaving
      EXIT.
    ENDIF.

    " Load mapping (overwrite if existing)
    CREATE OBJECT mapping.
    TRY.
        mapping->from_xml_xstring( me->if_fdt_doc_pkg~get_file_as_xstring( mapping_rel-target ) ).
      CATCH cx_fdt_excel_core.
        " Mapping file does not exist
        CLEAR mapping.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_NAMED_CELL
* +-------------------------------------------------------------------------------------------------+
* | [--->] CELL_NAME                      TYPE        STRING
* | [<-()] CELL_VALUE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_named_cell.
    IF me->named_cells IS INITIAL.
      " No named cells... leaving
      EXIT.
    ENDIF.

    DATA named_cell TYPE struc_named_cell.

    READ TABLE me->named_cells INTO named_cell WITH KEY name =  cell_name.
    IF named_cell IS INITIAL.
      " No named cell called cell_name... leaving
      EXIT.
    ENDIF.

    IF named_cell-sheet_name IS INITIAL OR named_cell-sheet_name = ''.
      " Must be a hidden definedName
      cell_value = named_cell-cell.
    ELSE.
      cell_value = get_cell_value( sheet_name = named_cell-sheet_name cell_coordinate = named_cell-cell ).
      cell_value = convert_cell_value_by_numfmt(
          cell_value      = cell_value
          number_format   = named_cell-format ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_NAMED_CELLS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] NAMED_CELLS                    LIKE        NAMED_CELLS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_named_cells.
    named_cells = me->named_cells.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_NAMED_RANGES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] NAMED_RANGES                   LIKE        NAMED_RANGES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_named_ranges.
    named_ranges = me->named_ranges.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_NAMED_RANGE_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] RANGE                          TYPE        STRING
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_named_range_itab.
    DATA:
      named_range    TYPE struc_named_range,
      x_sheet        TYPE xstring,
      xml_doc        TYPE REF TO if_ixml_document,
      shared_strings TYPE STANDARD TABLE OF string.

    TYPE-POOLS ixml.

    IF me->named_ranges IS INITIAL.
      " no named ranges or not loaded
      EXIT.
    ENDIF.
* Search the range entry
    READ TABLE me->named_ranges INTO named_range WITH KEY name = range.

    IF named_range IS INITIAL.
      " Range does not exist... leaving
      EXIT.
    ENDIF.

* Get shared strings
    get_shared_strings( IMPORTING strings = shared_strings ).

* Get appropriate sheet
    x_sheet = get_worksheet_by_name( named_range-sheet_name ).
    IF x_sheet IS INITIAL.
      " Sheet does not exist, leaving
      EXIT.
    ENDIF.

* Parse XML document
    xml_doc = parse_xml( x_sheet ).
    IF xml_doc IS INITIAL.
      " Error parsing XML document
      EXIT.
    ENDIF.

* Create itab from given range coordinates, shared string table and XML doc
    itab = get_range_itab_from_xmldoc(
              range_start    = named_range-start_cell    " A1 style start coordinate
              range_end      = named_range-end_cell      " A1 style end coordinate
              xml_doc        = xml_doc                   " IF_IXML_DOCUMENT
              ).
  ENDMETHOD.                    "GET_NAMED_RANGE_ITAB


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_OR_CREATE_INDEX_SHARED_STR
* +-------------------------------------------------------------------------------------------------+
* | [--->] STRING                         TYPE        STRING
* | [<-()] INDEX                          TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_or_create_index_shared_str.
    DATA:
      rel_s_str      TYPE t_pod_relation,
      x_file         TYPE xstring,
      comp_string    TYPE string,
      shared_strings TYPE STANDARD TABLE OF string.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

    index = -1.

    IF workbook_relations IS INITIAL.
      EXIT.
    ENDIF.

    READ TABLE me->workbook_relations WITH KEY type = if_fdt_xl_types=>ooxml_relationship_shrdstr INTO rel_s_str.
    IF rel_s_str IS INITIAL.
      " Shared Strings not present! Try to create sharedStrings.xml file
      me->init_shared_strings_file( ).
      READ TABLE me->workbook_relations WITH KEY type = if_fdt_xl_types=>ooxml_relationship_shrdstr INTO rel_s_str.
      IF rel_s_str IS INITIAL.
        " No chance... giving up
        EXIT.
      ENDIF.
    ENDIF.

    TRY.
        x_file = me->if_fdt_doc_pkg~get_file_as_xstring( workbook_folder && rel_s_str-target ).
        CALL TRANSFORMATION fdt_xl_get_shared_strings
              SOURCE XML x_file
              RESULT shared_strings = shared_strings.
      CATCH cx_fdt_excel_core.
        " Error msg
        EXIT.
    ENDTRY.

    LOOP AT shared_strings INTO comp_string.
      IF comp_string EQ string.
        index = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF index > 0.
      " Found string, setting index correction and return
      index = index - 1.
      EXIT.
    ENDIF.

* Add shared string to sharedStrings XML table.
    DATA:
      shared_xml_document TYPE REF TO if_ixml_document,
      sst_node            TYPE REF TO if_ixml_node,
      sst_element         TYPE REF TO if_ixml_element,
      si_element          TYPE REF TO if_ixml_element,
      si_node_collection  TYPE REF TO if_ixml_node_collection,
      t_element           TYPE REF TO if_ixml_element,
      t_text              TYPE REF TO if_ixml_text.

    shared_xml_document = parse_xml( x_file ).

    IF shared_xml_document IS INITIAL.
      EXIT.
    ENDIF.

    sst_node = shared_xml_document->get_first_child( ).
    sst_element ?= sst_node->query_interface( ixml_iid_element ).
    si_element = shared_xml_document->create_element_ns( 'si' ).
    t_element = shared_xml_document->create_element_ns( 't' ).
    t_text = shared_xml_document->create_text( string ).
    t_element->append_child( t_text ).
    si_element->append_child( t_element ).
    sst_node->append_child( si_element ).
    si_node_collection = sst_element->get_elements_by_tag_name_ns( name = 'si' uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
    index = si_node_collection->get_length( ) - 1.

* Render and store shared strings XML
    me->if_fdt_doc_pkg~set_xstring_as_file( abs_filename = workbook_folder && rel_s_str-target x_file = render_xml( shared_xml_document ) ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_RANGE_ITAB_FROM_XMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] RANGE_START                    TYPE        STRING
* | [--->] RANGE_END                      TYPE        STRING
* | [--->] XML_DOC                        TYPE REF TO IF_IXML_DOCUMENT
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_range_itab_from_xmldoc.
    DATA:
      row_start      TYPE i,
      row_end        TYPE i,
      row_count      TYPE i,
      col_start      TYPE i,
      col_end        TYPE i,
      col_count      TYPE i,
      shared_strings TYPE STANDARD TABLE OF string,
      cellxfs        TYPE STANDARD TABLE OF string,
      numfmtid       LIKE LINE OF me->numfmtids.

    TYPE-POOLS: ixml.

    IF xml_doc IS INITIAL.
      EXIT.
    ENDIF.

* Get shared strings
    get_shared_strings( IMPORTING strings = shared_strings ).

* Get styles table for number formats
    get_cellxfs( IMPORTING cellxfs = cellxfs ).

* Get dimensions of the range
    get_col_and_row_from_a1_style(
      EXPORTING
        coordinate    = range_start
      IMPORTING
        column_number = col_start
        row_number    = row_start ).
    get_col_and_row_from_a1_style(
      EXPORTING
        coordinate    = range_end
      IMPORTING
        column_number = col_end
        row_number    = row_end  ).

    row_count = row_end - row_start + 1.
    col_count = col_end - col_start + 1.

* Create dynamic table
    TYPE-POOLS : abap.
    FIELD-SYMBOLS: <dyn_table> TYPE STANDARD TABLE,
                   <dyn_wa>    TYPE any.
    DATA:
      dy_table TYPE REF TO data,
      dy_line  TYPE REF TO data,
      xfc      TYPE lvc_s_fcat,
      ifc      TYPE lvc_t_fcat.

    DO col_count TIMES.
      CLEAR xfc.
      xfc-fieldname = convert_i_to_column( col_start + sy-index  - 1 ).
      xfc-datatype =  'g'.
      xfc-inttype = 'g'.
      xfc-intlen =  0.
      APPEND xfc TO ifc.
    ENDDO.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = ifc " Field Catalog
      IMPORTING
        ep_table                  = dy_table " Pointer to Dynamic Data Table
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ASSIGN dy_table->* TO <dyn_table>.
* Create dynamic work area and assign to FS
    CREATE DATA dy_line LIKE LINE OF <dyn_table>.
    ASSIGN dy_line->* TO <dyn_wa>.

    FIELD-SYMBOLS:
      <dyn_row_count> TYPE any,
      <dyn_column>    TYPE any.

* Process XML
    DATA:
      row            TYPE REF TO if_ixml_node,
      rows           TYPE REF TO if_ixml_node_collection,
      row_iterator   TYPE REF TO if_ixml_node_iterator,
      row_element    TYPE REF TO if_ixml_element,
      col            TYPE REF TO if_ixml_node,
      cols           TYPE REF TO if_ixml_node_collection,
      col_element    TYPE REF TO if_ixml_element,
      col_iterator   TYPE REF TO if_ixml_node_iterator,
      current_column TYPE string,
      current_row    TYPE i,
      style_id       TYPE string,
      cell_value     TYPE string,
      cell_string_id TYPE string,
      cell_element   TYPE REF TO if_ixml_element,
      loopcount      TYPE i.


* Get a collection of rows
    rows = xml_doc->get_elements_by_tag_name_ns( name = 'row'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
    IF rows->get_length( ) = 0.
      " No columns to process, leaving
      EXIT.
    ENDIF.
    row_iterator = rows->create_iterator( ).
    row = row_iterator->get_next( ).


* Find start row
    TRY.
        WHILE row IS NOT INITIAL AND get_attr_from_node( iv_name  = 'r'  io_node  = row ) NE row_start.
          row = row_iterator->get_next( ).
        ENDWHILE.
      CATCH cx_root.
        " Start row not found in sheet... leaving
        EXIT.
    ENDTRY.

    WHILE row IS NOT INITIAL AND get_attr_from_node( iv_name = 'r' io_node = row ) LE row_end .
      loopcount = loopcount + 1.
      CLEAR dy_line.
      UNASSIGN <dyn_wa>.
      CREATE DATA dy_line LIKE LINE OF <dyn_table>.
      ASSIGN dy_line->* TO <dyn_wa>.

      row_element ?= row->query_interface( ixml_iid_element ).
      current_row = get_attr_from_node( iv_name = 'r' io_node = row ) - row_start + 1 .
      " Keep empty rows in spreadsheet
      WHILE loopcount < current_row.
        INSERT <dyn_wa> INTO <dyn_table> INDEX loopcount.
        loopcount = loopcount + 1.
      ENDWHILE.

      cols = row_element->get_elements_by_tag_name_ns( name = 'c' uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
      IF cols->get_length( ) = 0.
        " This row contains no column/values, insert empty line and continue
        INSERT <dyn_wa> INTO <dyn_table> INDEX current_row.
        row = row_iterator->get_next( ).
        CONTINUE.
      ELSE.
        col_iterator = cols->create_iterator( ).
        col = col_iterator->get_next( ).
      ENDIF.
      "Find start col
      TRY.
          DO.
            IF convert_column_to_i( extract_col_from_a1_style( get_attr_from_node( iv_name = 'r' io_node = col ) ) ) >= col_start.
              EXIT.
            ENDIF.
            col = col_iterator->get_next( ).
          ENDDO.
        CATCH cx_root.
          EXIT.
      ENDTRY.

      WHILE col IS NOT INITIAL AND convert_column_to_i( extract_col_from_a1_style( get_attr_from_node( iv_name = 'r' io_node = col ) ) ) LE col_end.
        col_element ?= col->query_interface( ixml_iid_element ).
        current_column = extract_col_from_a1_style( get_attr_from_node( iv_name = 'r' io_node = col ) ).
        cell_element = col_element->find_from_name_ns( name = 'v'  uri = if_fdt_xl_types=>ns_ooxml_ssheet_main ).
        IF cell_element IS NOT INITIAL.
          cell_value = cell_element->get_value( ).
          " Check cell type
          IF get_attr_from_node( iv_name = 't' io_node = col ) = 's'.
            cell_string_id = cell_value + 1.
            READ TABLE shared_strings INDEX cell_string_id INTO cell_value.
          ELSE.
            " not a string value
          ENDIF.
          " Check format of cell value because of date/time representation
          READ TABLE cellxfs INTO style_id INDEX ( get_attr_from_node( iv_name = 's' io_node = col ) + 1 ).
          READ TABLE me->numfmtids INTO numfmtid WITH KEY id = style_id.
          " Hook method for implementing other format representations:
          cell_value = convert_cell_value_by_numfmt( cell_value = cell_value number_format = numfmtid-formatcode ).
          ASSIGN COMPONENT current_column OF STRUCTURE <dyn_wa> TO <dyn_column>.
          " write cell_value.
          <dyn_column> = cell_value.
*      else.
*        " No value, insert empty value anyway
*        assign component current_column of structure <dyn_wa> to <dyn_column>.
*        " write empty cell_value.
*        <dyn_column> = ''.
        ENDIF." Cell element is not initial

        col = col_iterator->get_next( ).
      ENDWHILE." Cols
      INSERT <dyn_wa> INTO <dyn_table> INDEX current_row.
      row = row_iterator->get_next( ).
    ENDWHILE.

    itab = dy_table.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_SHARED_STRINGS
* +-------------------------------------------------------------------------------------------------+
* | [<---] STRINGS                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_shared_strings.
* This method loads the shared strings when required for the first time.
    IF me->mt_shared_strings IS INITIAL.
      me->load_shared_strings( ).
    ENDIF.
    strings = me->mt_shared_strings.
  ENDMETHOD.                                             "#EC CI_VALPAR


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_WORKSHEET_BY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] WORKSHEET_ID                   TYPE        STRING
* | [<-()] XSHEET                         TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_worksheet_by_id.
    DATA w_sheet TYPE ooxml_worksheet.

    IF me->worksheets IS INITIAL.
      MESSAGE e003(fdt_excel_core).
      "message 'Worksheets not loaded.' type 'I' display like 'E'.
      EXIT.
    ENDIF.

* Get the appropriate sheet from the table
    READ TABLE me->worksheets INTO w_sheet WITH KEY id = worksheet_id.
    IF w_sheet IS INITIAL.
      "data msg type string.
      "msg = `Sheet (` && worksheet_id && `) not found`.
      "message msg type 'I' display like 'E'.
      MESSAGE e004(fdt_excel_core) WITH worksheet_id.
      EXIT.
    ENDIF.

* Get the sheet
    TRY.
        xsheet = me->if_fdt_doc_pkg~get_file_as_xstring( me->workbook_folder && w_sheet-location ).
      CATCH cx_fdt_excel_core.
        MESSAGE e005(fdt_excel_core).
        "message 'Unable to load worksheet.' type 'I' display like 'E'.
        EXIT.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_WORKSHEET_BY_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] WORKSHEET_NAME                 TYPE        STRING
* | [<-()] XSHEET                         TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_worksheet_by_name.
    DATA w_sheet TYPE ooxml_worksheet.

    IF me->worksheets IS INITIAL.
      MESSAGE e003(fdt_excel_core).
      "message 'Worksheets not loaded.' type 'I' display like 'E'.
      EXIT.
    ENDIF.

* Get the appropriate sheet from the table
    READ TABLE me->worksheets INTO w_sheet WITH KEY name = worksheet_name.
    IF w_sheet IS INITIAL.
      "data msg type string.
      "msg = `Sheet (` && worksheet_name && `) not found`.
      "message msg type 'I' display like 'E'.
      MESSAGE e004(fdt_excel_core) WITH worksheet_name.
      EXIT.
    ENDIF.

* Get the sheet
    TRY.
        xsheet = me->if_fdt_doc_pkg~get_file_as_xstring( me->workbook_folder && w_sheet-location ).
      CATCH cx_fdt_excel_core.
        MESSAGE e005(fdt_excel_core).
        "message 'Unable to load worksheet.' type 'I' display like 'E'.
        EXIT.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->GET_WORKSHEET_NAMES
* +-------------------------------------------------------------------------------------------------+
* | [<---] WORKSHEET_NAMES                TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_worksheet_names.
    DATA wa_ws LIKE LINE OF worksheets.

    LOOP AT me->worksheets INTO wa_ws.
      APPEND wa_ws-name TO worksheet_names.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FDT_XL_SPREADSHEET_V2=>IF_FDT_DOC_SPREADSHEET~CREATE_DOCUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] NAME                           TYPE        STRING (default =DEFAULT_FILENAME)
* | [--->] COLUMNS                        TYPE        IF_FDT_DOC_SPREADSHEET=>T_COLUMN(optional)
* | [--->] ITAB                           TYPE REF TO DATA
* | [--->] COLUMNS_AUX                    TYPE        IF_FDT_DOC_SPREADSHEET=>T_COLUMN(optional)
* | [--->] ITAB_AUX_COL_DESC              TYPE REF TO DATA(optional)
* | [--->] IV_CALL_TYPE                   TYPE        I
* | [--->] IT_DT_MAP_COL                  TYPE        IF_FDT_DOC_SPREADSHEET=>T_COLUMN(optional)
* | [--->] IT_DT_MAP_COL_DESC             TYPE REF TO DATA(optional)
* | [--->] IV_SHEET_NAME                  TYPE        STRING(optional)
* | [<-()] XDOCUMENT                      TYPE        XSTRING
* | [!CX!] CX_FDT_EXCEL_CORE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~create_document.
*    xdocument = zcl_fdt_xl_spreadsheet=>create_document(
*                                                columns            = columns
*                                                columns_aux        = columns_aux
*                                                itab_aux_col_desc  = itab_aux_col_desc
*                                                it_dt_map_col      = it_dt_map_col
*                                                it_dt_map_col_desc = it_dt_map_col_desc
*                                                itab               = itab
*                                                name               = name
*                                                iv_call_type       = iv_call_type
*                                                is_alv_document    = abap_true
*                                                iv_sheet_name      = iv_sheet_name ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_CUSTOM_METADATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] NAME                           TYPE        STRING
* | [<-()] VALUE                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_custom_metadata.
    value = me->get_custom_metadata( name ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_ITAB_FOR_ALV_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_itab_for_alv_update.
    itab = me->get_itab_for_alv_update( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_ITAB_FROM_WORKSHEET
* +-------------------------------------------------------------------------------------------------+
* | [--->] WORKSHEET_NAME                 TYPE        STRING
* | [--->] IV_CALLER                      TYPE        I(optional)
* | [--->] IV_GET_LANGUAGE                TYPE        ABAP_BOOL(optional)
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_itab_from_worksheet.
    itab = me->get_itab_from_sheet( EXPORTING worksheet_name  = worksheet_name
                                              iv_caller       = iv_caller
                                              iv_get_language = iv_get_language ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_MAPPING
* +-------------------------------------------------------------------------------------------------+
* | [<-()] MAPPING                        TYPE REF TO CL_FDT_DOC_MAPPING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_mapping.
    mapping = me->get_mapping( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_NAMED_CELLS
* +-------------------------------------------------------------------------------------------------+
* | [<---] NAMED_CELLS                    TYPE        TAB_NAMED_CELLS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_named_cells.
    named_cells = me->get_named_cells( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_NAMED_CELL_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] CELL_NAME                      TYPE        STRING
* | [<-()] CELL_VALUE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_named_cell_value.
    cell_value = me->get_named_cell( cell_name ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_NAMED_RANGES
* +-------------------------------------------------------------------------------------------------+
* | [<---] NAMED_RANGES                   TYPE        TAB_NAMED_RANGES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_named_ranges.
    named_ranges = me->get_named_ranges( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_NAMED_RANGE_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] RANGE                          TYPE        STRING
* | [<-()] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_named_range_itab.
    itab = me->get_named_range_itab( range ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~GET_WORKSHEET_NAMES
* +-------------------------------------------------------------------------------------------------+
* | [<---] WORKSHEET_NAMES                TYPE        T_WORKSHEET_NAMES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~get_worksheet_names.
    me->get_worksheet_names( IMPORTING worksheet_names = worksheet_names ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~SET_CUSTOM_METADATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] NAME                           TYPE        STRING
* | [--->] VALUE                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~set_custom_metadata.
    me->set_custom_metadata( iv_name = name iv_value = value ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~SET_MAPPING
* +-------------------------------------------------------------------------------------------------+
* | [--->] MAPPING                        TYPE REF TO CL_FDT_DOC_MAPPING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~set_mapping.
    me->set_mapping( mapping ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~SET_NAMED_CELL_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] CELL_NAME                      TYPE        STRING
* | [--->] CELL_VALUE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~set_named_cell_value.
    me->set_named_cell_value( cell_name = cell_name  cell_value = cell_value  ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FDT_XL_SPREADSHEET_V2->IF_FDT_DOC_SPREADSHEET~SET_NAMED_RANGE_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] RANGE_NAME                     TYPE        STRING
* | [--->] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_fdt_doc_spreadsheet~set_named_range_values.
    me->set_named_range_values( range_name = range_name  itab = itab ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->INIT_SHARED_STRINGS_FILE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_shared_strings_file.
    DATA:
      converter   TYPE REF TO cl_abap_conv_out_ce,
      xml         TYPE string,
      ct_override TYPE t_pod_ct_override,
      rel_s_str   TYPE t_pod_relation.

    CONSTANTS: ss_content_type TYPE string VALUE 'application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml', "#EC NOTEXT
               ss_xml          TYPE string VALUE 'sharedStrings.xml', "#EC NOTEXT
               back_slash      TYPE string VALUE '/'.       "#EC NOTEXT
    READ TABLE me->workbook_relations WITH KEY type = if_fdt_xl_types=>ooxml_relationship_shrdstr INTO rel_s_str.
    IF rel_s_str IS NOT INITIAL.
      " Delete entry before adding new one.
      DELETE TABLE me->workbook_relations FROM rel_s_str.
      CLEAR rel_s_str.
    ENDIF.

* Create the XML structure and add it to the document
    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    CONCATENATE xml `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"></sst>` INTO xml. "#EC NOTEXT
    "xml = xml && `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"></sst>`.
    converter = cl_abap_conv_out_ce=>create( ).
    converter->write( data = xml ).
    me->if_fdt_doc_pkg~set_xstring_as_file( abs_filename = workbook_folder && ss_xml  x_file = converter->get_buffer( )  ). "#EC NOTEXT
* Add a new package relation to the document
    rel_s_str-id = 'rIdSharedStrings'.                      "#EC NOTEXT
    rel_s_str-target = 'sharedStrings.xml'.                 "#EC NOTEXT
    rel_s_str-type = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings'. "#EC NOTEXT
    APPEND rel_s_str TO me->workbook_relations.
    me->store_workbook_relations( ).

    READ TABLE me->content_types_overrides WITH KEY contenttype = ss_content_type INTO ct_override.
    IF ct_override IS NOT INITIAL.
      CONCATENATE back_slash workbook_folder ss_xml INTO ct_override-partname.
      "ct_override-partname = '/' && workbook_folder && 'sharedStrings.xml'.
      MODIFY TABLE me->content_types_overrides FROM ct_override.
    ELSE.
      CONCATENATE back_slash workbook_folder ss_xml INTO ct_override-partname.
      "ct_override-partname = '/' && workbook_folder && 'sharedStrings.xml'.
      ct_override-contenttype = ss_content_type.
      APPEND ct_override TO me->content_types_overrides.
    ENDIF.
    me->store_content_types( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->INSERT_EMPTY_WORKSHEET_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SHEET_XML_DOCUMENT          TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IO_SHEET_DATA_NODE             TYPE REF TO IF_IXML_NODE
* | [<-->] CV_ROW                         TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD insert_empty_worksheet_row.

    DATA: lo_row_element TYPE REF TO if_ixml_element.

    lo_row_element = create_worksheet_row_base( io_sheet_xml_document = io_sheet_xml_document
                                                iv_row = cv_row ).
    io_sheet_data_node->append_child( lo_row_element ).
    cv_row = cv_row + 1.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->LOAD_DEFINED_NAMES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_defined_names.

    TYPES BEGIN OF struc_defined_names.
    TYPES name TYPE string.
    TYPES reference TYPE string.
    TYPES END OF struc_defined_names.

    TYPES tab_defined_names TYPE STANDARD TABLE OF struc_defined_names.

    DATA:
      defined_names TYPE tab_defined_names,
      xworkbook     TYPE xstring.


    TRY.
        xworkbook = me->if_fdt_doc_pkg~get_file_as_xstring( me->workbook_folder && me->workbook_filename ).
      CATCH cx_fdt_excel_core.
        EXIT.
    ENDTRY.

    CALL TRANSFORMATION fdt_xl_get_defined_names
        SOURCE XML xworkbook
        RESULT defined_names = defined_names.

    IF defined_names IS INITIAL.
      " No named references in this woorkbook, nothing else to do so leaving...
      EXIT.
    ENDIF.

    DATA:
      defined_name     TYPE struc_defined_names,
      named_cell       TYPE struc_named_cell,
      named_range      TYPE struc_named_range,
      ws_name          TYPE string,                "Name of sheet
      ws_match_results TYPE match_result_tab,
      ws_match         TYPE match_result,
      ws_off_1         TYPE i,
      ws_off_2         TYPE i,
      ws_split         TYPE STANDARD TABLE OF string,
      reference        TYPE string,
      start_cell       TYPE string,
      end_cell         TYPE string.

    CLEAR: me->named_cells, me->named_ranges.

    LOOP AT defined_names INTO defined_name.
      FIND '#REF!' IN defined_name-reference.
      IF sy-subrc = 0.
        " Reference is not value... continue with next defined name
        CONTINUE.
      ENDIF.
      FIND '[' IN defined_name-reference.
      IF sy-subrc = 0.
        " Reference is external... continue with next defined name
        CONTINUE.
      ENDIF.
      CLEAR: named_cell, named_range.

      " Get the name of the worksheet
      FIND ALL OCCURRENCES OF `'` IN defined_name-reference RESULTS ws_match_results.
      IF sy-subrc = 0.
        " Sheetname contains ! and therefore it is escaped by '
        READ TABLE ws_match_results INTO ws_match INDEX 1.
        ws_off_1 = ws_match-offset + 1.
        READ TABLE ws_match_results INTO ws_match INDEX 2.
        ws_off_2 = ws_match-offset - 1.
        ws_name = substring( val = defined_name-reference off = ws_off_1 len = ws_off_2 ).
        reference = substring( val = defined_name-reference off = ws_off_2 + 3 len = strlen( defined_name-reference ) - ( ws_off_2 + 3 ) ).
      ELSE.
        " Sheetname contains no ! so split at !
        SPLIT defined_name-reference AT '!' INTO ws_name reference.
      ENDIF.

      IF reference IS INITIAL OR reference = ''.
        " definedNamed must be a hidden one, set only value and continue
        named_cell-name = defined_name-name.
        named_cell-cell = ws_name.
        APPEND named_cell TO me->named_cells.
        CONTINUE.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '$' IN reference WITH ''.

      FIND ':' IN reference.
      IF sy-subrc = 0.
        " Defined name is a range
        named_range-name = defined_name-name.
        named_range-sheet_name = ws_name.
        SPLIT reference AT ':' INTO start_cell end_cell.
        named_range-start_cell = start_cell.
        named_range-end_cell = end_cell.
        APPEND named_range TO me->named_ranges.
      ELSE.
        " Defined name is a cell
        named_cell-name = defined_name-name.
        named_cell-sheet_name = ws_name.
        named_cell-cell = reference.
        APPEND named_cell TO me->named_cells.
      ENDIF.
    ENDLOOP.

    CLEAR: named_cell, named_range.

    " Get number format of defined cells
    DATA:
          cellxfs TYPE STANDARD TABLE OF string.

    IF me->numfmtids IS INITIAL.
      " Number formats table not loaded
      me->load_numfmtids( ).
    ENDIF.

    get_cellxfs( IMPORTING cellxfs = cellxfs ).

    " ToDo sort by sheet
    FIELD-SYMBOLS:
                   <named_cell> LIKE LINE OF me->named_cells.
    DATA:
      num_id          TYPE string,
      numfmtid        TYPE t_struc_numfmtid,
      x_current_sheet TYPE xstring,
      xml_doc         TYPE REF TO if_ixml_document,
      style_id        TYPE i.

    LOOP AT me->named_cells ASSIGNING <named_cell> ." into named_cell.
      IF <named_cell>-sheet_name IS NOT INITIAL AND <named_cell>-sheet_name <> ''.
        x_current_sheet = me->get_worksheet_by_name( <named_cell>-sheet_name ).
        xml_doc = parse_xml( x_current_sheet ).
        style_id = get_cell_style_id( xml_doc = xml_doc cell_coordinate = <named_cell>-cell ).
        READ TABLE cellxfs INTO num_id INDEX ( style_id + 1 ).
        READ TABLE me->numfmtids INTO numfmtid WITH KEY id = num_id.
        <named_cell>-format = numfmtid-formatcode.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->LOAD_NUMFMTIDS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_numfmtids.
    DATA format TYPE t_struc_numfmtid.

    CLEAR me->numfmtids.
    " Filling all OOXML predifended, language independend number formats !Not a straight numbering!
    " According to 'Office Open XML Part 4 - Markup Language Reference, 3.8.30'
    format-id = 0.
    format-formatcode = `General`.                          "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 1.
    format-formatcode = `0`.                                "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 2.
    format-formatcode = `0.00`.                             "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 3.
    format-formatcode = `#,##0`.                            "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 4.
    format-formatcode = `#,##0.00`.                         "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 9.
    format-formatcode = `0%`.                               "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 10.
    format-formatcode = `0.00%`.                            "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 11.
    format-formatcode = `0.00E+00`.                         "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 12.
    format-formatcode = `# ?/?`.                            "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 13.
    format-formatcode = `# ??/??`.                          "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 14.
    format-formatcode = `mm-dd-yy`.                         "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 15.
    format-formatcode = `d-mmm-yy`.                         "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 16.
    format-formatcode = `d-mmm`.                            "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 17.
    format-formatcode = `mmm-yy`.                           "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 18.
    format-formatcode = `h:mm AM/PM`.                       "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 19.
    format-formatcode = `h:mm:ss AM/PM`.                    "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 20.
    format-formatcode = `h:mm`.                             "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 21.
    format-formatcode = `h:mm:ss`.                          "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 22.
    format-formatcode = `m/d/yy h:mm`.                      "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 37.
    format-formatcode = `#,##0;(#,##0)`.                    "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 38.
    format-formatcode = `#,##0;[Red](#,##0)`.               "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 39.
    format-formatcode = `#,##0.00;(#,##0.00)`.              "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 40.
    format-formatcode = `#,##0.00;[Red](#,##0.00)`.         "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 45.
    format-formatcode = `mm:ss`.                            "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 46.
    format-formatcode = `[h]:mm:ss`.                        "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 47.
    format-formatcode = `mmss.0`.                           "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 48.
    format-formatcode = `##0.0E+0`.                         "#EC NOTEXT
    APPEND format TO me->numfmtids.
    format-id = 49.
    format-formatcode = `@`.                                "#EC NOTEXT
    APPEND format TO me->numfmtids.

    DATA:
      style_relation TYPE t_pod_relation,
      x_style        TYPE xstring,
      custom_numfmts LIKE me->numfmtids.
    READ TABLE me->workbook_relations WITH KEY type = if_fdt_xl_types=>ooxml_relationship_styles INTO style_relation.
    IF style_relation IS INITIAL.
      "Nothing else to do, leaving.
      EXIT.
    ENDIF.

    TRY.
        x_style = me->if_fdt_doc_pkg~get_file_as_xstring( workbook_folder && style_relation-target ).
      CATCH cx_fdt_excel_core.
        "Style XML file not found.
        EXIT.
    ENDTRY.

    CALL TRANSFORMATION fdt_xl_get_numfmtids
      SOURCE XML x_style
      RESULT numfmts = custom_numfmts.

    APPEND LINES OF custom_numfmts TO me->numfmtids.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->LOAD_SHARED_STRINGS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_shared_strings.
*  This method uses the transformation FDT_XL_GET_SHARED_STRINGS to get the
*  the shared strings as table of strings
    DATA: rel_s_str   TYPE t_pod_relation,
          lv_abs_path TYPE string,
          lx_file     TYPE xstring.

    CHECK workbook_relations IS NOT INITIAL.
    READ TABLE me->workbook_relations
            WITH KEY type = if_fdt_xl_types=>ooxml_relationship_shrdstr INTO rel_s_str.

    TRY.
        CONCATENATE workbook_folder rel_s_str-target INTO lv_abs_path.
        lx_file = me->if_fdt_doc_pkg~get_file_as_xstring( lv_abs_path ).
        CALL TRANSFORMATION fdt_xl_get_shared_strings
              SOURCE XML lx_file
              RESULT shared_strings = me->mt_shared_strings.
      CATCH cx_fdt_excel_core.
        " Error msg
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->LOAD_WORKBOOK
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_workbook.
    DATA:
      wb_rel      TYPE t_pod_relation,
      wb_rel_file TYPE xstring,
      parts       TYPE TABLE OF string,
      part        TYPE string,
      sp_cnt      TYPE i.


    IF me->package_relations IS INITIAL.
      EXIT.
    ENDIF.

    " Set the default value for the date format
    me->dateformat1904 = abap_false.

    READ TABLE me->package_relations WITH KEY type = if_fdt_xl_types=>ooxml_relationship_office_doc INTO  wb_rel.

    IF wb_rel IS INITIAL.
      EXIT.
    ENDIF.

    SPLIT wb_rel-target AT '/' INTO TABLE parts.

    DESCRIBE TABLE parts LINES sp_cnt.
    READ TABLE parts INDEX sp_cnt INTO me->workbook_filename.

    WHILE sy-index < sp_cnt.
      READ TABLE parts INDEX sy-index INTO part.
      me->workbook_folder = me->workbook_folder && part && '/'.
    ENDWHILE.

    IF me->workbook_folder IS INITIAL.
      EXIT.
    ENDIF.

    TRY.
        wb_rel_file = me->if_fdt_doc_pkg~get_file_as_xstring( workbook_folder && gc_rels_path && workbook_filename && gc_rels_extn ).
        CALL TRANSFORMATION fdt_xl_get_relations
            SOURCE XML wb_rel_file
            RESULT relations = me->workbook_relations
            .
      CATCH cx_fdt_excel_core.
        " 'File  ' && workbook_folder && '/_rels/' && workbook_filename && '.rels not found in package'
        MESSAGE e007(fdt_excel_core) WITH workbook_folder workbook_filename.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->LOAD_WORKSHEETS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_worksheets.

    DATA:
      x_workbook TYPE xstring,
      w_sheet    TYPE ooxml_worksheet,
      w_sheet_n  TYPE ooxml_worksheet,
      w_sheets   TYPE ooxml_worksheets,
      w_relation TYPE t_pod_relation,
      is_df1904  TYPE string.


    IF workbook_filename IS INITIAL OR workbook_folder IS INITIAL OR workbook_relations IS INITIAL.
      MESSAGE e005(fdt_excel_core).
      "message 'Worksheet cannot be loaded' type 'I' display like 'E'.
      EXIT.
    ENDIF.


    TRY.
        x_workbook = me->if_fdt_doc_pkg~get_file_as_xstring( me->workbook_folder && me->workbook_filename ).
        CALL TRANSFORMATION fdt_xl_get_worksheets
            SOURCE XML x_workbook
            RESULT worksheets = me->worksheets.

        CALL TRANSFORMATION fdt_xl_get_date_format
          SOURCE XML x_workbook
          RESULT dateformat_1904 = is_df1904.
        IF is_df1904 = '1'.
          me->dateformat1904 = abap_true.
        ELSE.
          me->dateformat1904 = abap_false.
        ENDIF.

      CATCH cx_fdt_excel_core cx_root.
        "data msg_string type string.
        "msg_string = 'File  ' && workbook_folder && workbook_filename && ' not found in package'.
        "message msg_string type 'I' display like 'E'.
        MESSAGE e009(fdt_excel_core) WITH workbook_filename workbook_folder.
    ENDTRY.


    IF me->worksheets IS INITIAL.
      EXIT.
    ENDIF.

    LOOP AT me->worksheets INTO w_sheet.
      READ TABLE me->workbook_relations INTO w_relation WITH KEY id = w_sheet-location.
      w_sheet-location = w_relation-target.
      APPEND w_sheet TO w_sheets.
    ENDLOOP.

    me->worksheets = w_sheets.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->OVERWRITE_WORKSHEET_BY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] WORKSHEET_ID                   TYPE        STRING
* | [--->] XSHEET                         TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD overwrite_worksheet_by_id.
    DATA w_sheet TYPE ooxml_worksheet.

    IF me->worksheets IS INITIAL.
      MESSAGE e003(fdt_excel_core).
      "message 'Worksheets not loaded.' type 'I' display like 'E'.
      EXIT.
    ENDIF.

* Get the appropriate sheet from the table
    READ TABLE me->worksheets INTO w_sheet WITH KEY id = worksheet_id.
    IF w_sheet IS INITIAL.
      "data msg type string.
      "msg = `Sheet (` && worksheet_id && `) not found`.
      "message msg type 'I' display like 'E'.
      MESSAGE e004(fdt_excel_core) WITH worksheet_id.
      EXIT.
    ENDIF.

* Overwrite the sheet
    me->if_fdt_doc_pkg~set_xstring_as_file( abs_filename = me->workbook_folder && w_sheet-location x_file = xsheet ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->OVERWRITE_WORKSHEET_BY_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] WORKSHEET_NAME                 TYPE        STRING
* | [--->] XSHEET                         TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD overwrite_worksheet_by_name.
    DATA w_sheet TYPE ooxml_worksheet.

    IF me->worksheets IS INITIAL.
      MESSAGE e003(fdt_excel_core).
      "message 'Worksheets not loaded.' type 'I' display like 'E'.
      EXIT.
    ENDIF.

* Get the appropriate sheet from the table
    READ TABLE me->worksheets INTO w_sheet WITH KEY name = worksheet_name.
    IF w_sheet IS INITIAL.
      "data msg type string.
      "msg = `Sheet (` && worksheet_name && `) not found`.
      "message msg type 'I' display like 'E'.
      MESSAGE e004(fdt_excel_core) WITH worksheet_name.
      EXIT.
    ENDIF.

* Overwrite the sheet
    me->if_fdt_doc_pkg~set_xstring_as_file( abs_filename = me->workbook_folder && w_sheet-location x_file = xsheet ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_CELL_VALUE_BY_XMLDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE REF TO DATA
* | [--->] CELL_COORDINATE                TYPE        STRING
* | [<-->] XML_DOC                        TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_cell_value_by_xmldoc.
    DATA:
      data_type      TYPE REF TO cl_abap_typedescr,
      rows           TYPE REF TO if_ixml_node_collection,
      row_iterator   TYPE REF TO if_ixml_node_iterator,
      row            TYPE REF TO if_ixml_node,
      row_element    TYPE REF TO if_ixml_element,
      cells          TYPE REF TO if_ixml_node_collection,
      cell_list      TYPE REF TO if_ixml_node_list,
      cell_iterator  TYPE REF TO if_ixml_node_iterator,
      cell           TYPE REF TO if_ixml_node,
      cell_element   TYPE REF TO if_ixml_element,
      value_element  TYPE REF TO if_ixml_element,
      value_str      TYPE string,
      sheetdata_el   TYPE REF TO if_ixml_element,
      filter         TYPE REF TO if_ixml_node_filter,
      row_number     TYPE i,
      row_number_str TYPE string,
      insert_pos     TYPE i.
    TYPE-POOLS:
                 ixml.
    FIELD-SYMBOLS:
                   <value> TYPE any.

    IF xml_doc IS INITIAL OR cell_coordinate IS INITIAL OR value IS INITIAL.
      " Nothing to do, great... leaving
      EXIT.
    ENDIF.

    " Find out which data type to set
    ASSIGN value->* TO <value>.
    data_type = cl_abap_typedescr=>describe_by_data_ref( value ).

    " Find cell to update
    cells = xml_doc->get_elements_by_tag_name_ns( name = 'c' uri   = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main' ).
    filter = xml_doc->create_filter_attribute( name = 'r'  value = cell_coordinate ).
    cell_iterator = cells->create_iterator_filtered( filter ).
    cell = cell_iterator->get_next( ).
    IF cell IS INITIAL.
      " Cell not existing, create new one...
      cell_element = xml_doc->create_element_ns( name = 'c' ).
      cell_element->set_attribute( name = 'r' value = cell_coordinate ).

      " ...and try to find at least the proper row
      get_col_and_row_from_a1_style( EXPORTING coordinate = cell_coordinate  IMPORTING row_number = row_number  ).
      row_number_str = row_number.
      cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = row_number_str ).
      rows = xml_doc->get_elements_by_tag_name_ns( name = 'row'  uri = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main' ).
      filter = xml_doc->create_filter_attribute( name = 'r'  value = row_number_str ).
      row_iterator = rows->create_iterator_filtered( filter ).
      row = row_iterator->get_next( ).
      IF row IS INITIAL.
        " No proper row! Create new row and cell ...
        row_element = xml_doc->create_element_ns( name = 'row' ).
        row_element->set_attribute( name = 'r' value = row_number_str ).
        row_element->append_child( cell_element ).

        " ... and find position to insert
        rows = xml_doc->get_elements_by_tag_name_ns( name = 'row'  uri = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main' ).
        row_iterator = rows->create_iterator( ).
        row = row_iterator->get_next( ).
        WHILE row IS NOT INITIAL AND get_attr_from_node( iv_name = 'r' io_node = row ) < row_number.
          row = row_iterator->get_next( ).
          insert_pos = sy-index.
        ENDWHILE.

        IF row IS INITIAL.
          " No row present at all! Append it to sheetData element
          sheetdata_el = xml_doc->find_from_name_ns( name = 'sheetData'  uri = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main' ).
          IF sheetdata_el IS INITIAL.
            " Not a correct sheet XML. Giving up...
            EXIT.
          ENDIF.
          sheetdata_el->append_child( row_element ).
        ELSE.
          " Found rows. Now insert at proper position
          DATA:
            row_insert_pos TYPE REF TO if_ixml_node,
            row_parent     TYPE REF TO if_ixml_node.
          row_insert_pos  = rows->get_item( insert_pos ).
          row_parent      = row_insert_pos->get_parent( ).
          row_parent->insert_child( new_child = row_element  ref_child = row_insert_pos ).
        ENDIF.
      ELSE. " Row is not initial... find position to insert cell_element
        cell_list = row->get_children( ).
        filter = xml_doc->create_filter_name( name = 'c' ).
        cell_iterator = cell_list->create_iterator_filtered( filter ).
        cell = cell_iterator->get_next( ).
        WHILE cell IS NOT INITIAL AND get_col_from_a1_style( get_attr_from_node( iv_name = 'r' io_node = cell ) ) < get_col_from_a1_style( cell_coordinate ).
          cell = cell_iterator->get_next( ).
          insert_pos = sy-index.
        ENDWHILE.

        IF cell IS INITIAL.
          " No cell at all
          row->append_child( cell_element ).
        ELSE.
          " Cell found, insert at proper position
          DATA:
            cell_insert_pos TYPE REF TO if_ixml_node,
            cell_parent     TYPE REF TO if_ixml_node.
          cell_insert_pos = cell_list->get_item( insert_pos ).
          cell_parent     = cell_insert_pos->get_parent( ).
          cell_parent->insert_child( new_child = cell_element  ref_child = cell_insert_pos ).
        ENDIF.
      ENDIF.

    ELSE. " Cell is not initial
      cell_element ?= cell->query_interface( ixml_iid_element ).
    ENDIF.

    " Reference to cell_element should be present here
    IF cell_element IS INITIAL.
      EXIT.
    ENDIF.

    value_element = cell_element->find_from_name_ns( name = 'v' uri = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main' ).
    IF value_element IS INITIAL.
      " No value element present yet, create a new one
      value_element = xml_doc->create_element_ns( name = 'v' ).
      cell_element->append_child( value_element ).
    ENDIF.

    " Convert value to string
    value_str = <value>.
    " Set attribute 't' depending on data type
    IF data_type->type_kind = 'I' OR data_type->type_kind = 'N' OR data_type->type_kind = 'F' OR data_type->type_kind = 'P'.
      IF data_type->type_kind = 'P' AND <value> < 0.
        <value> = abs( <value> ).
        value_str = '-' && <value>.
      ENDIF.
      " Number
*    cell_element->set_attribute( name = 's' value = '1' ).
    ELSE.
      " String
      cell_element->set_attribute( name = 't' value = 's' ).
      value_str = get_or_create_index_shared_str( value_str ).
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = value_str ).
    value_element->set_value( value_str ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_HELP_DT_SHEETDATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_help_dt_sheetdata.
*    DATA:
*      sheetdata_node TYPE REF TO  if_ixml_node,
*      dimension      TYPE         string,
*      t_col          TYPE         tt_map_descr,
*      s_col          LIKE LINE OF t_col,
*      row_element    TYPE REF TO  if_ixml_element,    " <row>
*      c_element      TYPE REF TO  if_ixml_element,    " <c> = cell
*      v_element      TYPE REF TO  if_ixml_element,    " <v> = cell value
*      v_text         TYPE REF TO  if_ixml_text,       " text value of <v>.
*      col_typedescr  TYPE REF TO  cl_abap_datadescr,  " Type description for column
*      lv_long        TYPE         c LENGTH 60,
*      lv_short       TYPE         c LENGTH 25,
*      content        TYPE         string,                    " Actual cell content as string
*      row_count      TYPE         string,                    " Row count as string
*      lv_string      TYPE string.
** row no - &1
** column no - &2
** cell value - &3
** style - &4
*    DEFINE _set_help_string .
*      row_element = sheet_xml_document->create_element_ns( 'row' ).
*      row_element->set_attribute_ns( name = 'r' value = &1 ).
*      row_element->set_attribute_ns( name = 'spans' value = '1:3' ).
*      row_element->set_attribute_ns( name = 's' value = &4 ).
*      "row_element->set_attribute_ns( name = 'customFormat' value = '1' ).
*
*      c_element = sheet_xml_document->create_element_ns( 'c' ).
*      c_element->set_attribute_ns( name = 'r' value = convert_i_to_column( &2 ) && &1 ).
*      c_element->set_attribute_ns( name = 's' value = &4 ).
*      c_element->set_attribute_ns( name = 't' value = 's' ). " String value
*      v_element = sheet_xml_document->create_element_ns( 'v' ).
*      lv_string = &3.
*      content = set_shared_string_buffer( lv_string ).
*      v_text = sheet_xml_document->create_text( content ).
*      v_element->append_child( v_text ).
*      c_element->append_child( v_element ).
*      row_element->append_child( c_element ).
*      sheetdata_node->append_child( row_element ).
*    END-OF-DEFINITION.
*
**   Enabling iXML support
*    TYPE-POOLS: ixml.
*    CLASS cl_ixml DEFINITION LOAD.
*
*    "Set columns
*    col_typedescr ?= cl_abap_typedescr=>describe_by_data( lv_long  ).
*    s_col-name = 'First'.                                   "#EC NOTEXT
*    s_col-type = col_typedescr.
*    APPEND s_col TO t_col.
*    col_typedescr ?= cl_abap_typedescr=>describe_by_data( lv_short  ).
*    s_col-name = 'Second'.                                  "#EC NOTEXT
*    s_col-type = col_typedescr.
*    APPEND s_col TO t_col.
*    s_col-name = 'Third'.                                   "#EC NOTEXT
*    s_col-type = col_typedescr.
*    APPEND s_col TO t_col.
*
** Prepare XML
*    TRY.
*        "dimension = 'A1:' && me->convert_i_to_column( col_count ) && rows.
*        dimension = 'A1:C39'.
*        me->set_worksheet_dimension( sheet_xml_document = sheet_xml_document dimension = dimension ).
*        me->set_worksheet_cols( sheet_xml_document = sheet_xml_document column_description_tab = t_col ).
*        sheetdata_node = me->clear_worksheet_sheetdata( sheet_xml_document ).
*      CATCH cx_root.
*        EXIT.
*    ENDTRY.
*
*    IF sheetdata_node IS INITIAL.
*      EXIT.
*    ENDIF.
*    "Setting the help data into a table
*    _set_help_string  '1' '2' 'Guidelines for Entering Decision Table Data'(012) '9'. "#EC NOTEXT
*    "_set_help_string  '2' '1' '' '0'.                         "#EC NOTEXT
*    _set_help_string  '3' '1' 'Condition columns take range expression as their cell values'(013) '1'. "#EC NOTEXT
*    "_set_help_string  '4' '1' '' '0'.                         "#EC NOTEXT
*    _set_help_string  '4' '1' 'Range Option'(014) '1'.      "#EC NOTEXT
*    _set_help_string  '4' '2' 'Pattern'(015) '1'.           "#EC NOTEXT
*    _set_help_string  '4' '3' 'Example'(016) '1'.           "#EC NOTEXT
*
*    _set_help_string '5' '1' 'contains any'(017) '0'.       "#EC NOTEXT
*    lv_string = 'contains any'(017) && ` &1`.               "#EC NOTEXT
*    _set_help_string '5' '2' lv_string '0'.                 "#EC NOTEXT
*    lv_string = 'contains any'(017) && ` abc`.              "#EC NOTEXT
*    _set_help_string '5' '3' lv_string '0'.                 "#EC NOTEXT
*
*    _set_help_string '6' '1' 'contains only'(023) '0'.      "#EC NOTEXT
*    lv_string = 'contains only'(023) && ` &1`.              "#EC NOTEXT
*    _set_help_string '6' '2' lv_string '0'.                 "#EC NOTEXT
*    lv_string = 'contains only'(023) && ` abc`.             "#EC NOTEXT
*    _set_help_string '6' '3' lv_string '0'.                 "#EC NOTEXT
*
*    _set_help_string '7' '1' 'contains string'(024) '0'.    "#EC NOTEXT
*    lv_string = 'contains string'(024) && ` &1`.            "#EC NOTEXT
*    _set_help_string '7' '2' lv_string '0'.                 "#EC NOTEXT
*    lv_string = 'contains string'(024) && ` abc`.           "#EC NOTEXT
*    _set_help_string '7' '3' lv_string '0'.                 "#EC NOTEXT
*
*    _set_help_string '8' '1' 'currency equals'(026) '0'.    "#EC NOTEXT
*    _set_help_string '8' '2' 'currency equals'(026) '0'.    "#EC NOTEXT
*    lv_string = 'currency equals'(026) && ` EUR`.           "#EC NOTEXT
*    _set_help_string '8' '3' lv_string '0'.                 "#EC NOTEXT
*
*    _set_help_string '9' '1' 'does not contain any'(029) '0'. "#EC NOTEXT
*    lv_string = 'contains not any'(030) && ` &1`.           "#EC NOTEXT
*    _set_help_string '9' '2' lv_string '0'.                 "#EC NOTEXT
*    lv_string = 'contains not any'(030) && ` abc`.          "#EC NOTEXT
*    _set_help_string '9' '3' lv_string '0'.                 "#EC NOTEXT
*
*    _set_help_string '10' '1' 'does not contain only'(032) '0'. "#EC NOTEXT
*    lv_string = 'contains not only'(033) && ` &1`.          "#EC NOTEXT
*    _set_help_string '10' '2' lv_string '0'.                "#EC NOTEXT
*    lv_string = 'contains not only'(033) && ` abc`.         "#EC NOTEXT
*    _set_help_string '10' '3' lv_string '0'.                "#EC NOTEXT
*
*    _set_help_string '11' '1' 'does not contain string'(035) '0'. "#EC NOTEXT
*    lv_string = 'contains no string'(036) && ` &1`.         "#EC NOTEXT
*    _set_help_string '11' '2' lv_string '0'.                "#EC NOTEXT
*    lv_string = 'contains no string'(036) && ` abc`.        "#EC NOTEXT
*    _set_help_string '11' '3' lv_string '0'.                "#EC NOTEXT
*
*    _set_help_string '12' '1' 'does not match pattern'(038) '0'. "#EC NOTEXT
*    lv_string = '[<>&' && 'pattern'(039) && `]`.            "#EC NOTEXT
*    _set_help_string '12' '2' lv_string '0'.                "#EC NOTEXT
*    lv_string = '[<>' && 'pattern'(039) && `]`.             "#EC NOTEXT
*    _set_help_string '12' '3' lv_string '0'.                "#EC NOTEXT
*
*    lv_string = 'ends with'(041) && ` text`.                "#EC NOTEXT
*    _set_help_string '13' '1' 'ends with text'(041) '0'.    "#EC NOTEXT
*    lv_string = 'ends with'(041) && ` &1`.                  "#EC NOTEXT
*    _set_help_string '13' '2' lv_string '0'.                "#EC NOTEXT
*    lv_string = 'ends with'(041) && ` abc`.                 "#EC NOTEXT
*    _set_help_string '13' '3' lv_string '0'.                "#EC NOTEXT
*
*    _set_help_string '14' '1' 'is between'(042) '0'.        "#EC NOTEXT
*    _set_help_string '14' '2' '[&1..&2]' '0'.               "#EC NOTEXT
*    _set_help_string '14' '3' '[0..15]'  '0'.               "#EC NOTEXT
*
*    _set_help_string '15' '1' 'is equal to'(043) '0'.       "#EC NOTEXT
*    _set_help_string '15' '2' '=&1' '0'.                    "#EC NOTEXT
*    _set_help_string '15' '3' '=abc' '0'.                   "#EC NOTEXT
*
*    _set_help_string '16' '1' 'is greater than'(044) '0'.   "#EC NOTEXT
*    _set_help_string '16' '2' '> &1' '0'.                   "#EC NOTEXT
*    _set_help_string '16' '3' '> 100' '0'.                  "#EC NOTEXT
*
*    _set_help_string '17' '1' 'is greater than or equal to'(045) '0'. "#EC NOTEXT
*    _set_help_string '17' '2' '>= &1'  '0'.                 "#EC NOTEXT
*    _set_help_string '17' '3' '>=200' '0'.                  "#EC NOTEXT
*
*    _set_help_string '18' '1' 'is initial'(046) '0'.        "#EC NOTEXT
*    _set_help_string '18' '2' 'is initial'(046) '0'.        "#EC NOTEXT
*    _set_help_string '18' '3' 'is initial'(046) '0'.        "#EC NOTEXT
*
*    _set_help_string '19' '1' 'is less than'(047) '0'.      "#EC NOTEXT
*    _set_help_string '19' '2' '< &1' '0'.                   "#EC NOTEXT
*    _set_help_string '19' '3' '<100' '0'.                   "#EC NOTEXT
*
*    _set_help_string '20' '1' 'is less than or equal to'(048) '0'. "#EC NOTEXT
*    _set_help_string '20' '2' '<= &1' '0'.                  "#EC NOTEXT
*    _set_help_string '20' '3' '<=200' '0'.                  "#EC NOTEXT
*
*    _set_help_string '21' '1' 'is not between'(049) '0'.    "#EC NOTEXT
*    _set_help_string '21' '2' '<&1;>&2' '0'.                "#EC NOTEXT
*    _set_help_string '21' '3' '<12;>50' '0'.                "#EC NOTEXT
*
*    _set_help_string '22' '1' 'is not equal to'(050) '0'.   "#EC NOTEXT
*    _set_help_string '22' '2' '<>&1' '0'.                   "#EC NOTEXT
*    _set_help_string '22' '3' '<>abc' '0'.                  "#EC NOTEXT
*
*    _set_help_string '23' '1' 'is not initial'(051) '0'.    "#EC NOTEXT
*    _set_help_string '23' '2' 'is not initial'(051) '0'.    "#EC NOTEXT
*    _set_help_string '23' '3' 'is not initial'(051) '0'.    "#EC NOTEXT
**
**  _set_help_string '25' '1' 'is not supplied' '0'.          "#EC NOTEXT
**  _set_help_string '25' '2' 'is not supplied' '0'.          "#EC NOTEXT
**  _set_help_string '25' '3' 'is not supplied' '0'.          "#EC NOTEXT
*
*    _set_help_string '24' '1' 'is not valid'(052) '0'.      "#EC NOTEXT
*    _set_help_string '24' '2' 'is not valid'(052) '0'.      "#EC NOTEXT
*    _set_help_string '24' '3' 'is not valid'(052) '0'.      "#EC NOTEXT
**
**  _set_help_string '27' '1' 'is supplied' '0'.              "#EC NOTEXT
**  _set_help_string '27' '2' 'is supplied' '0'.              "#EC NOTEXT
**  _set_help_string '27' '3' 'is supplied' '0'.              "#EC NOTEXT
*
*    _set_help_string '25' '1' 'is valid'(053) '0'.          "#EC NOTEXT
*    _set_help_string '25' '2' 'is valid'(053) '0'.          "#EC NOTEXT
*    _set_help_string '25' '3' 'is valid'(053) '0'.          "#EC NOTEXT
*
*    _set_help_string '26' '1' 'matches pattern'(054) '0'.   "#EC NOTEXT
*    lv_string = '[&' && 'pattern'(039) && `]`.              "#EC NOTEXT
*    _set_help_string '26' '2' lv_string '0'.                "#EC NOTEXT
*    lv_string = '[' && 'pattern'(039) && `]`.               "#EC NOTEXT
*    _set_help_string '26' '3' lv_string '0'.                "#EC NOTEXT
*
*    lv_string = 'starts with'(055) && ` text`.              "#EC NOTEXT
*    _set_help_string '27' '1' lv_string '0'.                "#EC NOTEXT
*    lv_string = 'starts with'(055) && ` &1`.                "#EC NOTEXT
*    _set_help_string '27' '2' lv_string '0'.                "#EC NOTEXT
*    lv_string = 'starts with'(055) && ` abc`.               "#EC NOTEXT
*    _set_help_string '27' '3' lv_string '0'.                "#EC NOTEXT
*
*    _set_help_string '28' '1' 'unit equals'(056) '0'.       "#EC NOTEXT
*    _set_help_string '28' '2' 'unit equals'(056) '0'.       "#EC NOTEXT
*    lv_string = 'unit equals'(056) && ` KG`.                "#EC NOTEXT
*    _set_help_string '28' '3' lv_string '0'.                "#EC NOTEXT
*
*    _set_help_string '30' '1' 'Result columns take constant expression as their cell values'(057) '1'. "#EC NOTEXT
*
*    _set_help_string '32' '1' 'Range Option'(058) '1'.      "#EC NOTEXT
*    _set_help_string '32' '2' 'Pattern'(059) '1'.           "#EC NOTEXT
*    _set_help_string '32' '3' 'Example'(060) '1'.           "#EC NOTEXT
*
*    _set_help_string '33' '1' 'or'(061) '0'.                "#EC NOTEXT
*    _set_help_string '33' '2' '&1 ; &2' '0'.                "#EC NOTEXT
*    _set_help_string '33' '3' 'EUR ; USD' '0'.              "#EC NOTEXT
*
*    _set_help_string '34' '1' '(and) unless'(062)  '0'.     "#EC NOTEXT
*    lv_string = 'exclude'(063) && ` &1`.                    "#EC NOTEXT
*    _set_help_string '34' '2' 'exclude &1'  '0'.            "#EC NOTEXT
*    lv_string = 'exclude'(063) && ` JPY`.                   "#EC NOTEXT
*    _set_help_string '34' '3' 'exclude JPY' '0'.            "#EC NOTEXT
*
*
*    _set_help_string '36' '1' 'Result Column Type'(064) '1'. "#EC NOTEXT
*    _set_help_string '36' '2' 'Pattern'(059) '1'.           "#EC NOTEXT
*    _set_help_string '36' '3' 'Example'(060) '1'.           "#EC NOTEXT
*
*    _set_help_string '37' '1' 'Text'(065) '0'.              "#EC NOTEXT
*    _set_help_string '37' '2' '&1'  '0'.                    "#EC NOTEXT
*    _set_help_string '37' '3' 'abc' '0'.                    "#EC NOTEXT
*
*    _set_help_string '38' '1' 'Number'(066) '0'.            "#EC NOTEXT
*    _set_help_string '38' '2' 'NUMBER'(067) '0'.            "#EC NOTEXT
*    _set_help_string '38' '3' '123 , 200.50' '0'.           "#EC NOTEXT
*
*    _set_help_string '39' '1' 'Boolean'(068) '0'.           "#EC NOTEXT
*    _set_help_string '39' '2' 'TRUE/FALSE (change the cell format to Text before entering values)'(069) '0'. "#EC NOTEXT
*    _set_help_string '39' '3' 'TRUE,FALSE'(070) '0'.        "#EC NOTEXT
*
*    _set_help_string '40' '1' 'Amount'(071) '0'.            "#EC NOTEXT
*    _set_help_string '40' '2' 'NUMBER CURRENCY'(072) '0'.   "#EC NOTEXT
*    _set_help_string '40' '3' '100 USD' '0'.                "#EC NOTEXT
*
*    _set_help_string '41' '1' 'Quantity'(073) '0'.          "#EC NOTEXT
*    _set_help_string '41' '2' 'NUMBER UNIT'(074) '0'.       "#EC NOTEXT
*    _set_help_string '41' '3' '100 KG' '0'.                 "#EC NOTEXT
*
*    _set_help_string '42' '1' 'Timepoint - Date Format'(075) '0'. "#EC NOTEXT
*    _set_help_string '42' '2' 'YYYY-MM-DD' '0'.             "#EC NOTEXT
*    _set_help_string '42' '3' '2011-01-06' '0'.             "#EC NOTEXT
*
*    _set_help_string '43' '1' 'Timepoint - Date Time Format'(076) '0'. "#EC NOTEXT
*    _set_help_string '43' '2' 'YYYY-MM-DDThh:mm:ss' '0'.    "#EC NOTEXT
*    _set_help_string '43' '3' '2011-01-06T10:10:10' '0'.    "#EC NOTEXT
*
*    _set_help_string '44' '1' 'Timepoint - Timestamp Format'(077) '0'. "#EC NOTEXT
*    _set_help_string '44' '2' 'YYYY-MM-DDThh:mm:ssZ' '0'.   "#EC NOTEXT
*    _set_help_string '44' '3' '2011-01-06T10:10:10Z' '0'.   "#EC NOTEXT
*
*    _set_help_string '45' '1' 'Timepoint - Time Format'(078) '0'. "#EC NOTEXT
*    _set_help_string '45' '2' 'Thh:mm:ss' '0'.              "#EC NOTEXT
*    _set_help_string '45' '3' 'T10:10:10' '0'.              "#EC NOTEXT
*
*    _set_help_string '46' '1' 'Timepoint - Date Time with offset Format' '0'. "#EC NOTEXT
*    _set_help_string '46' '2' 'YYYY-MM-DDThh:mm:ss[+|-]hh:mm' '0'. "#EC NOTEXT
*    _set_help_string '46' '3' '2011-01-06T10:10:10+10:00' '0'. "#EC NOTEXT
*
*    _set_help_string '48' '1' 'Language'(082) '0'.          "#EC NOTEXT
*    _set_help_string '48' '2' sy-langu '0'.                 "#EC NOTEXT

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_HELP_SIM_SHEETDATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_help_sim_sheetdata.
*    DATA:
*      sheetdata_node TYPE REF TO  if_ixml_node,
*      dimension      TYPE         string,
*      t_col          TYPE         abap_component_tab,
*      s_col          LIKE LINE OF t_col,
*      row_element    TYPE REF TO  if_ixml_element,    " <row>
*      c_element      TYPE REF TO  if_ixml_element,    " <c> = cell
*      v_element      TYPE REF TO  if_ixml_element,    " <v> = cell value
*      v_text         TYPE REF TO  if_ixml_text,       " text value of <v>.
*      col_typedescr  TYPE REF TO  cl_abap_datadescr,  " Type description for column
*      lv_long        TYPE         c LENGTH 60,
*      lv_short       TYPE         c LENGTH 25,
*      content        TYPE         string,                    " Actual cell content as string
*      row_count      TYPE         string,                    " Row count as string
*      lv_string      TYPE         string.
** row no - &1
** column no - &2
** cell value - &3
** style - &4
*    DEFINE _set_help_string .
*      row_element = sheet_xml_document->create_element_ns( 'row' ).
*      row_element->set_attribute_ns( name = 'r' value = &1 ).
*      row_element->set_attribute_ns( name = 'spans' value = '1:3' ).
*      row_element->set_attribute_ns( name = 's' value = &4 ).
*      "row_element->set_attribute_ns( name = 'customFormat' value = '1' ).
*
*      c_element = sheet_xml_document->create_element_ns( 'c' ).
*      c_element->set_attribute_ns( name = 'r' value = convert_i_to_column( &2 ) && &1 ).
*      c_element->set_attribute_ns( name = 's' value = &4 ).
*      c_element->set_attribute_ns( name = 't' value = 's' ). " String value
*      v_element = sheet_xml_document->create_element_ns( 'v' ).
*      lv_string = &3.
*      content = set_shared_string_buffer( lv_string ).
*      v_text = sheet_xml_document->create_text( content ).
*      v_element->append_child( v_text ).
*      c_element->append_child( v_element ).
*      row_element->append_child( c_element ).
*      sheetdata_node->append_child( row_element ).
*    END-OF-DEFINITION.
*
**   Enabling iXML support
*    TYPE-POOLS: ixml.
*    CLASS cl_ixml DEFINITION LOAD.
*
*    "Set columns
*    col_typedescr ?= cl_abap_typedescr=>describe_by_data( lv_long  ).
*    s_col-name = 'First'.                                   "#EC NOTEXT
*    s_col-type = col_typedescr.
*    APPEND s_col TO t_col.
*    col_typedescr ?= cl_abap_typedescr=>describe_by_data( lv_short  ).
*    s_col-name = 'Second'.                                  "#EC NOTEXT
*    s_col-type = col_typedescr.
*    APPEND s_col TO t_col.
*    s_col-name = 'Third'.                                   "#EC NOTEXT
*    s_col-type = col_typedescr.
*    APPEND s_col TO t_col.
*
** Prepare XML
*    TRY.
*        "dimension = 'A1:' && me->convert_i_to_column( col_count ) && rows.
*        dimension = 'A1:C39'.
*        me->set_worksheet_dimension( sheet_xml_document = sheet_xml_document dimension = dimension ).
*        me->set_worksheet_cols( sheet_xml_document = sheet_xml_document column_description_tab = t_col ).
*        sheetdata_node = me->clear_worksheet_sheetdata( sheet_xml_document ).
*      CATCH cx_root.
*        EXIT.
*    ENDTRY.
*
*    IF sheetdata_node IS INITIAL.
*      EXIT.
*    ENDIF.
*    "Setting the help data into a table
*    _set_help_string  '1' '2' 'Guildelines to fill in the function simulation data'(012) '1'. "#EC NOTEXT
*    "_set_help_string  '2' '1' '' '0'.                         "#EC NOTEXT
*    _set_help_string  '3' '1' 'The first row contains all the the data object names from the context of the function.Fill in the data from the next row.'(079) '0'. "#EC NOTEXT
*    "_set_help_string  '4' '1' '' '0'.                         "#EC NOTEXT
*    _set_help_string  '5' '1' 'Data Type'(080) '1'.         "#EC NOTEXT
*    _set_help_string  '5' '2' 'Pattern'(059) '1'.           "#EC NOTEXT
*    _set_help_string  '5' '3' 'Example'(060) '1'.           "#EC NOTEXT
*    _set_help_string '6' '1' 'Text'(065) '0'.               "#EC NOTEXT
*    _set_help_string '6' '2' '&1' '0'.                      "#EC NOTEXT
*    _set_help_string '6' '3' 'abc' '0'.                     "#EC NOTEXT
*    _set_help_string '7' '1' 'Number'(066) '0'.             "#EC NOTEXT
*    _set_help_string '7' '2' 'NUMBER'(067) '0'.             "#EC NOTEXT
*    _set_help_string '7' '3' '123 , 200.50' '0'.            "#EC NOTEXT
*    _set_help_string '8' '1' 'Boolean'(068) '0'.            "#EC NOTEXT
*    _set_help_string '8' '2' 'TRUE/FALSE (Also Change the Format of the cell to TEXT before entering values)'(069) '0'. "#EC NOTEXT
*    _set_help_string '8' '3' 'TRUE , FALSE'(070) '0'.       "#EC NOTEXT
*    _set_help_string '9' '1' 'Amount'(071) '0'.             "#EC NOTEXT
*    _set_help_string '9' '2' 'NUMBER CURRENCY'(072) '0'.    "#EC NOTEXT
*    _set_help_string '9' '3' '100 USD' '0'.                 "#EC NOTEXT
*    _set_help_string '10' '1' 'Quantity'(073) '0'.          "#EC NOTEXT
*    _set_help_string '10' '2' 'NUMBER UNIT'(074) '0'.       "#EC NOTEXT
*    _set_help_string '10' '3' '100 UNIT' '0'.               "#EC NOTEXT
*    _set_help_string '11' '1' 'Timepoint - Date Format'(075) '0'. "#EC NOTEXT
*    _set_help_string '11' '2' 'YYYY-MM-DD' '0'.             "#EC NOTEXT
*    _set_help_string '11' '3' '2011-01-06' '0'.             "#EC NOTEXT
*    _set_help_string '12' '1' 'Timepoint - Date Time Format'(076) '0'. "#EC NOTEXT
*    _set_help_string '12' '2' 'YYYY-MM-DDThh:mm:ss' '0'.    "#EC NOTEXT
*    _set_help_string '12' '3' '2011-01-06T10:10:10' '0'.    "#EC NOTEXT
*    _set_help_string '13' '1' 'Timepoint - Timestamp Format'(077) '0'. "#EC NOTEXT
*    _set_help_string '13' '2' 'YYYY-MM-DDThh:mm:ssZ' '0'.   "#EC NOTEXT
*    _set_help_string '13' '3' '2011-01-06T10:10:10Z' '0'.   "#EC NOTEXT
*    _set_help_string '14' '1' 'Timepoint - Time Format'(078) '0'. "#EC NOTEXT
*    _set_help_string '14' '2' 'Thh:mm:ss' '0'.              "#EC NOTEXT
*    _set_help_string '14' '3' 'T10:10:10' '0'.              "#EC NOTEXT
*    _set_help_string '15' '1' 'Timepoint - Date Time with offset Format'(081) '0'. "#EC NOTEXT
*    _set_help_string '15' '2' 'YYYY-MM-DDThh:mm:ss[+|-]hh:mm' '0'. "#EC NOTEXT
*    _set_help_string '15' '3' '2011-01-06T10:10:10+10:00' '0'. "#EC NOTEXT
*
*    "Set the language
*    _set_help_string '17' '1' 'Language'(082) '0'.          "#EC NOTEXT
*    _set_help_string '17' '2' sy-langu '0'.                 "#EC NOTEXT
*
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_MAPPING
* +-------------------------------------------------------------------------------------------------+
* | [--->] MAPPING                        TYPE REF TO CL_FDT_DOC_MAPPING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_mapping.
    DATA mapping_rel TYPE t_pod_relation.

    IF me->obj_doc_zip IS INITIAL.
      " No document loaded... no chance to set mapping
      EXIT.
    ENDIF.

    " Check whether mapping is already defined
    READ TABLE me->package_relations INTO mapping_rel WITH KEY type = if_fdt_xl_types=>mapping_namespace.

    IF mapping_rel IS INITIAL.
      mapping_rel-id = 'sapMapping'.
      mapping_rel-type = if_fdt_xl_types=>mapping_namespace.
      mapping_rel-target = 'docProps/sapMapping.xml'.
      APPEND mapping_rel TO me->package_relations.
      me->store_package_relations( ).
    ENDIF.

    " Set mapping (overwrite if existing)
    me->if_fdt_doc_pkg~set_xstring_as_file( abs_filename = mapping_rel-target  x_file = mapping->to_xml_xstring( ) ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_NAMED_CELL_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] CELL_NAME                      TYPE        STRING
* | [--->] CELL_VALUE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_named_cell_value.

    DATA:
      xsheet     TYPE xstring,
      named_cell LIKE LINE OF me->named_cells,
      xml_doc    TYPE REF TO if_ixml_document.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.


    IF me->named_cells IS INITIAL.
      me->load_defined_names( ).
      IF  me->named_cells IS INITIAL.
        " Giving up
        EXIT.
      ENDIF.
    ENDIF.

    READ TABLE me->named_cells INTO named_cell WITH KEY name = cell_name.
    IF named_cell IS INITIAL.
      " Named cell does not exist
      EXIT.
    ENDIF.

    xsheet = me->get_worksheet_by_name( named_cell-sheet_name ).

* Parse XML sheet
    xml_doc = me->parse_xml( xsheet ).
    IF xml_doc IS INITIAL.
      EXIT.
    ENDIF.

******  TEST FOR FORAMTS
    IF named_cell-format = 'General' OR named_cell-format = 'yyyy\-mm\-dd'.
      DATA value_ref TYPE REF TO data.
      GET REFERENCE OF cell_value INTO value_ref.
    ELSE.
      DATA cell_value_num TYPE i.
      cell_value_num = cell_value.
      GET REFERENCE OF cell_value_num INTO value_ref.
    ENDIF.
******  TEST FOR FORAMTS



    me->set_cell_value_by_xmldoc(
      EXPORTING
        value           = value_ref
        cell_coordinate = named_cell-cell
      CHANGING
        xml_doc = xml_doc
    ).

    me->overwrite_worksheet_by_name( worksheet_name = named_cell-sheet_name xsheet = me->render_xml( xml_doc ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_NAMED_RANGE_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] RANGE_NAME                     TYPE        STRING
* | [--->] ITAB                           TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_named_range_values.
    DATA:
      xsheet          TYPE xstring,
      named_range     LIKE LINE OF me->named_ranges,
      xml_doc         TYPE REF TO if_ixml_document,
      itab_row        TYPE REF TO data,
      itab_columns    TYPE i,
      itab_rows       TYPE i,
      range_columns   TYPE i,
      range_rows      TYPE i,
      range_start_row TYPE i,
      range_end_row   TYPE i,
      range_start_col TYPE i,
      range_end_col   TYPE i,
      columns_descr   TYPE REF TO cl_abap_structdescr,
      columns_tab     TYPE abap_component_tab,
      current_col     TYPE i.


    FIELD-SYMBOLS:
      <itab>      TYPE STANDARD TABLE,
      <itab_row>  TYPE any,
      <itab_cell> TYPE any.
* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

    IF itab IS INITIAL.
      " No data, nothing to do...
      EXIT.
    ENDIF.

    IF me->named_ranges IS INITIAL.
      me->load_defined_names( ).
      IF  me->named_ranges IS INITIAL.
        " Named ranges could not be read... giving up
        EXIT.
      ENDIF.
    ENDIF.

    READ TABLE me->named_ranges INTO named_range WITH KEY name = range_name.
    IF named_range IS INITIAL.
      " Named range does not exist in document
      EXIT.
    ENDIF.

    get_col_and_row_from_a1_style(
      EXPORTING coordinate = named_range-start_cell
      IMPORTING column_number = range_start_col
        row_number    = range_start_row  ).

    get_col_and_row_from_a1_style(
      EXPORTING coordinate = named_range-end_cell
      IMPORTING column_number = range_end_col
        row_number    = range_end_row  ).

    range_columns = ( range_end_col - range_start_col ) + 1.
    range_rows = ( range_end_row - range_start_row ) + 1.

* Parse XML sheet
    xsheet = me->get_worksheet_by_name( named_range-sheet_name ).
    xml_doc = me->parse_xml( xsheet ).
    IF xml_doc IS INITIAL. EXIT. ENDIF.

    " Calculate whether the itab fit in the named range

* Prepare field symbols and getting table info
    ASSIGN itab->* TO <itab>.
    CREATE DATA itab_row LIKE LINE OF <itab>.
    ASSIGN itab_row->* TO <itab_row>.
    DESCRIBE TABLE <itab> LINES itab_rows.
    TRY.
        columns_descr ?= cl_abap_typedescr=>describe_by_data( <itab_row> ).
        columns_tab = columns_descr->get_components( ).  "#EC CI_BOX_OK
        DESCRIBE TABLE columns_tab LINES itab_columns.
      CATCH cx_root.
        EXIT.
    ENDTRY.

    IF itab_columns = range_columns AND itab_rows = range_rows.
      " Fits, let's start
      LOOP AT <itab> INTO <itab_row>.
        current_col = range_start_col.
        DO itab_columns TIMES.
          ASSIGN COMPONENT sy-index OF STRUCTURE <itab_row> TO <itab_cell>. "#EC CI_BOX_OK

          DATA itab_cell TYPE REF TO data.

          GET REFERENCE OF <itab_cell> INTO itab_cell.

          me->set_cell_value_by_xmldoc(
            EXPORTING
              value           = itab_cell
              cell_coordinate = convert_i_to_column( current_col ) && range_start_row
            CHANGING
              xml_doc         = xml_doc ).
          current_col = current_col + 1.
        ENDDO.
        range_start_row = range_start_row + 1.
      ENDLOOP.
    ENDIF.

    me->overwrite_worksheet_by_name( worksheet_name = named_range-sheet_name xsheet = me->render_xml( xml_doc ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_SHARED_STRINGS
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHARED_STRINGS                 TYPE        STANDARD TABLE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_shared_strings.
    DATA:
      rel_s_str   TYPE t_pod_relation,
      x_file      TYPE xstring,
      xml_doc     TYPE REF TO if_ixml_document,
      sst_element TYPE REF TO if_ixml_element,
      si_element  TYPE REF TO if_ixml_element,
      t_element   TYPE REF TO if_ixml_element,
      t_text      TYPE REF TO if_ixml_text,
      str         TYPE string,
      count       TYPE string.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

    IF shared_strings IS SUPPLIED. "Else write what is already there in mt_shared_strings
*   Storing shared_strings table in object
      CLEAR me->mt_shared_strings.
      me->mt_shared_strings = shared_strings. "Anbu"Ideally there shouldn't be any usecase for this after performance optimization changes
    ENDIF.

* Check whether workbook relations are loaded
    IF workbook_relations IS INITIAL.
      EXIT.
    ENDIF.

* Find shared strings location
    READ TABLE me->workbook_relations WITH KEY type = if_fdt_xl_types=>ooxml_relationship_shrdstr INTO rel_s_str.
    IF rel_s_str-target IS INITIAL.
      EXIT.
    ENDIF.

* Get number of lines
    DESCRIBE TABLE mt_shared_strings LINES count.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = count ).

* Create shared strings XML document
    xml_doc = cl_ixml=>create( )->create_document( ).
    sst_element = xml_doc->create_element_ns( 'sst' ).
    sst_element->set_attribute( name = 'xmlns' value = if_fdt_xl_types=>ns_ooxml_ssheet_main  ).
    sst_element->set_attribute( name = 'uniqueCount' value = count  ).
    xml_doc->append_child( sst_element ).

* Store strings
    LOOP AT mt_shared_strings INTO str.
      si_element = xml_doc->create_element_ns( 'si' ).
      t_element =  xml_doc->create_element_ns( 't' ).
      t_text = xml_doc->create_text( str ).
      t_element->append_child( t_text ).
      si_element->append_child( t_element ).
      sst_element->append_child( si_element ).
    ENDLOOP.

* Render and store shared strings XML in document package
    me->if_fdt_doc_pkg~set_xstring_as_file( abs_filename = workbook_folder && rel_s_str-target x_file = render_xml( xml_doc ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_SHARED_STRING_BUFFER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SHARED_STRING               TYPE        STRING
* | [<-()] RV_CELL_CONTENT_INT            TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_shared_string_buffer.
* This method stores the shared strings only in mt_shared_strings (non sorted) so that it can be later updated to excel at one shot.
* This is to improve performance. mth_shared_strings is a hashed table just to optimize the find operation instead of searching sequential in mt_shared_strings
    DATA:          ls_shared_string_buffer      TYPE  ty_shared_string_buffer.
    FIELD-SYMBOLS: <ls_shared_string_buffer>    TYPE  ty_shared_string_buffer.

    IF me->mt_shared_strings IS INITIAL.
      me->load_shared_strings( ).
    ENDIF.

    READ TABLE mth_shared_strings WITH TABLE KEY str_val = iv_shared_string ASSIGNING <ls_shared_string_buffer>.
    IF sy-subrc = 0.
      rv_cell_content_int = <ls_shared_string_buffer>-pos.
    ELSE.
      ls_shared_string_buffer-str_val = iv_shared_string.
      APPEND iv_shared_string TO mt_shared_strings.
      ls_shared_string_buffer-pos = sy-tabix - 1.
      CONDENSE ls_shared_string_buffer-pos NO-GAPS.
      INSERT ls_shared_string_buffer INTO TABLE mth_shared_strings.
      rv_cell_content_int = ls_shared_string_buffer-pos.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_WORKSHEET_COLS
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* | [--->] COLUMN_DESCRIPTION_TAB         TYPE        TT_MAP_DESCR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_worksheet_cols.
    DATA:
      worksheet_root_node    TYPE REF TO if_ixml_node,
      worksheet_root_element TYPE REF TO if_ixml_element,
      cols_node              TYPE REF TO if_ixml_node,
      cols_element           TYPE REF TO if_ixml_element,
      col_node               TYPE REF TO if_ixml_node,
      col_element            TYPE REF TO if_ixml_element,
      col_collection         TYPE REF TO if_ixml_node_collection,
      col_iterator           TYPE REF TO if_ixml_node_iterator,
      sheetdata_node         TYPE REF TO if_ixml_node,
      column                 LIKE LINE OF column_description_tab,
      count                  TYPE string,
      length                 TYPE string,
      col_type_descr         TYPE REF TO cl_abap_typedescr.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

* Get the root node and element ( <worksheet> )
    worksheet_root_node = sheet_xml_document->get_first_child( ).
    worksheet_root_element ?= worksheet_root_node->query_interface( ixml_iid_element ).

* Checking wether cols node exist
    cols_node = worksheet_root_element->find_from_name( 'cols' ).
    IF cols_node IS INITIAL.
      sheetdata_node = worksheet_root_element->find_from_name( 'sheetData' ).
      IF sheetdata_node IS INITIAL.
        " At least <sheetData> tag must be present... giving up
        EXIT.
      ENDIF.
      " Creating the cols entry
      cols_node ?= sheet_xml_document->create_element_ns( 'cols' ).
      worksheet_root_node->insert_child( new_child = cols_node ref_child = sheetdata_node ).
    ELSE.
      " Removing childs from <cols>
      cols_element ?= cols_node->query_interface( ixml_iid_element ).
      col_collection = cols_element->get_elements_by_tag_name( 'col' ).
      col_iterator = col_collection->create_iterator( ).
      col_node = col_iterator->get_next( ).
      WHILE col_node IS NOT INITIAL.
        col_node->remove_node( ).
        col_node = col_iterator->get_next( ).
      ENDWHILE.
    ENDIF.

    FREE: col_node, col_element.

    " Creating new <col> tags from ABAP component tab
    LOOP AT column_description_tab INTO column.
      count = sy-tabix.
      cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = count ).
      length =  strlen( column-descr ). "'30'. " default length
      col_element = sheet_xml_document->create_element_ns( 'col' ).
      col_element->set_attribute_ns( name = 'min' value = count ).
      col_element->set_attribute_ns( name = 'max' value = count ).
      TRY.
          col_type_descr ?= column-type.
        length = col_type_descr->length * '1.3' .

          cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = length ).
          col_element->set_attribute_ns( name = 'width' value = length ).
        CATCH cx_root.
          col_element->set_attribute_ns( name = 'width' value = length ).
      ENDTRY.

      col_element->set_attribute_ns( name = 'bestFit' value = '1' ).
      cols_node->append_child( col_element ).
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_WORKSHEET_DIMENSION
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* | [--->] DIMENSION                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_worksheet_dimension.

    DATA:
      worksheet_node    TYPE REF TO if_ixml_node,
      worksheet_element TYPE REF TO if_ixml_element,
      dimension_node    TYPE REF TO if_ixml_node,
      dimension_element TYPE REF TO if_ixml_element,
      dimension_attr    TYPE REF TO if_ixml_attribute.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

* Get the root node and element ( <worksheet> )
    worksheet_node = sheet_xml_document->get_first_child( ).
    worksheet_element ?= worksheet_node->query_interface( ixml_iid_element ).

    dimension_element = worksheet_element->find_from_name( 'dimension' ).

    IF dimension_element IS NOT INITIAL.
      dimension_element->set_attribute_ns( name = 'ref' value = dimension ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_WORKSHEET_FORMATPR
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* | [--->] OUTLINELEVEL                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_worksheet_formatpr.
    DATA:
      worksheet_node    TYPE REF TO if_ixml_node,
      worksheet_element TYPE REF TO if_ixml_element,
      format_element    TYPE REF TO if_ixml_element.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

* Get the root node and element ( <worksheet> )
    worksheet_node = sheet_xml_document->get_first_child( ).
    worksheet_element ?= worksheet_node->query_interface( ixml_iid_element ).

    format_element = worksheet_element->find_from_name( 'sheetFormatPr' ).

    IF format_element IS NOT INITIAL.
      format_element->set_attribute_ns( name = 'outlineLevelRow' value = outlinelevel ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_WORKSHEET_SHEETDATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IS_SHEET_DATA                  TYPE        TY_SHEET_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_worksheet_sheetdata.

    DATA: itab_row            TYPE REF TO data,
          columns_descr       TYPE REF TO cl_abap_structdescr,
          t_col               TYPE tt_map_descr,
          s_col               LIKE LINE OF t_col,
          column              TYPE if_fdt_doc_spreadsheet=>s_column,
          col_count           TYPE i,
          rows                TYPE i,
          dimension           TYPE string,
          lv_group            TYPE int4,
          lv_max_group        TYPE int4,
          lv_max_group_string TYPE string,
          sheetdata_node      TYPE REF TO if_ixml_node,
          row_element         TYPE REF TO if_ixml_element,  " <row>
          row_count           TYPE int4.

    FIELD-SYMBOLS: <itab>      TYPE STANDARD TABLE,
                   <itab_row>  TYPE any,
                   <itab_row1> TYPE any.

* Enabling iXML support
    TYPE-POOLS: ixml.
    CLASS cl_ixml DEFINITION LOAD.

    ASSIGN is_sheet_data-tab_data->* TO <itab>.
    IF <itab> IS NOT ASSIGNED OR <itab> IS INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA itab_row LIKE LINE OF <itab>.
    ASSIGN itab_row->* TO <itab_row>.
*    CREATE DATA itab_row1 LIKE LINE OF <itab>.
*    ASSIGN itab_row1->* TO <itab_row1>.
    DESCRIBE TABLE <itab> LINES rows.
    rows = rows + 1. " One row more in the spreadsheet for the column headings

    TRY.

        columns_descr = CAST cl_abap_structdescr(
            cl_abap_typedescr=>describe_by_data( <itab_row> ) ).
        DATA(t_components) = columns_descr->get_components( ).

        LOOP AT t_components ASSIGNING FIELD-SYMBOL(<ls_comp>).
          DATA(lv_tabix) = sy-tabix.

          IF <ls_comp>-as_include IS NOT INITIAL.
            DATA(lo_str) = CAST cl_abap_structdescr( <ls_comp>-type ).
            INSERT LINES OF lo_str->get_components( ) INTO t_components INDEX lv_tabix + 1.

            FREE lo_str.
            DELETE t_components INDEX lv_tabix.
            CONTINUE.
          ENDIF.
        ENDLOOP.

      CATCH cx_root.
        RETURN.
    ENDTRY.

    CLEAR t_col.

    IF is_sheet_data-field_desc IS NOT INITIAL.

      LOOP AT is_sheet_data-field_desc ASSIGNING FIELD-SYMBOL(<ls_desc>).
        APPEND INITIAL LINE TO t_col ASSIGNING FIELD-SYMBOL(<ls_col_map>).

        <ls_col_map>-fieldname = <ls_desc>-fieldname.

        IF <ls_desc>-descr IS NOT INITIAL.
          <ls_col_map>-descr   = <ls_desc>-descr.
        ELSE.
          <ls_col_map>-descr = <ls_col_map>-fieldname.
        ENDIF.

        READ TABLE t_components ASSIGNING <ls_comp>
          WITH KEY name = <ls_col_map>-fieldname.
        IF sy-subrc = 0.
          <ls_col_map>-type = <ls_comp>-type.
        ENDIF.

      ENDLOOP.

*    ENDIF.
    ELSE."If columns are not yet defined


      t_col = VALUE #( FOR <ls_compt> IN t_components
                        ( fieldname = <ls_compt>-name
                          descr     = <ls_compt>-name
                          type      = <ls_compt>-type ) ).



    ENDIF.
    DESCRIBE TABLE t_col LINES col_count.


* Prepare XML
    TRY.
        dimension = 'A1:' && me->convert_i_to_column( col_count ) && rows.
        me->set_worksheet_dimension( sheet_xml_document = sheet_xml_document dimension = dimension ).
        me->set_worksheet_cols( sheet_xml_document     = sheet_xml_document
                                column_description_tab = t_col ).

        sheetdata_node = me->clear_worksheet_sheetdata( sheet_xml_document ).
      CATCH cx_root.
        EXIT.
    ENDTRY.

    IF sheetdata_node IS INITIAL.
      EXIT.
    ENDIF.

* Set column headings
    row_element = sheet_xml_document->create_element_ns( 'row' ).
    row_element->set_attribute_ns( name = 'r' value = '1' ).
    row_element->set_attribute_ns( name = 'spans' value = '1:' && col_count ).
    row_element->set_attribute_ns( name = 's' value = '1' ). " Bold
    row_element->set_attribute_ns( name = 'customFormat' value = '1' ).

    create_cell_elements_header( io_row_element        = row_element
                                 it_column_description = t_col
                                 io_sheet_xml_document = sheet_xml_document ).
    sheetdata_node->append_child( row_element ).

* Create spreadsheet
    LOOP AT <itab> INTO <itab_row>.
      row_count = sy-tabix + 1.

      create_worksheet_row( EXPORTING is_row                = <itab_row>
                                      iv_row_number         = row_count
                                      iv_color_fname        = is_sheet_data-color_field
                                      it_column_description = t_col
                                      io_sheet_xml_document = sheet_xml_document
                                      io_sheet_data_node    = sheetdata_node  ).

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->SET_WORKSHEET_WRAPTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] SHEET_XML_DOCUMENT             TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_worksheet_wraptext.
    DATA:
      worksheet_node    TYPE REF TO if_ixml_node,
      worksheet_element TYPE REF TO if_ixml_element,
      format_element    TYPE REF TO if_ixml_element,
      lo_sheet          TYPE REF TO if_ixml_element.


* Enabling iXML support
    TYPE-POOLS: ixml.
CLASS cl_ixml DEFINITION LOAD.

* Get the root node and element ( <worksheet> )
  worksheet_node = sheet_xml_document->get_first_child( ).
  DATA: lv_name TYPE string.
  lv_name = worksheet_node->get_name( ).
  worksheet_element ?= worksheet_node->query_interface( ixml_iid_element ).
  format_element = worksheet_element->find_from_name( 'sheetView' ).
  lo_sheet = sheet_xml_document->create_element_ns( 'selection' ).
  lo_sheet->set_attribute_ns( name = 'activeCell' value = 'E1' ).
  lo_sheet->set_attribute_ns( name = 'sqref' value = 'E1:E1048576' ).
  format_element->append_child( lo_sheet ).

  "<pageSetup paperSize="9" orientation="portrait" r:id="rId1" />
  lo_sheet = sheet_xml_document->create_element_ns( 'pageSetup' ).
  lo_sheet->set_attribute_ns( name = 'paperSize' value = '9' ).
  lo_sheet->set_attribute_ns( name = 'orientation' value = 'portrait' ).
  lo_sheet->set_attribute_ns( name = 'r:id' value = 'rId1' ).
  worksheet_node->append_child( lo_sheet ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FDT_XL_SPREADSHEET_V2->STORE_WORKBOOK_RELATIONS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD store_workbook_relations.
  DATA: lv_file TYPE xstring.

  CHECK me->obj_doc_zip IS NOT INITIAL.

  IF me->workbook_folder IS INITIAL OR me->workbook_filename IS INITIAL.
    me->load_workbook( ).
    IF me->workbook_folder IS INITIAL OR me->workbook_filename IS INITIAL.
      "Leaving as they are still initial
      EXIT.
    ENDIF.
  ENDIF.

  CALL TRANSFORMATION fdt_xl_set_relations
      SOURCE relations = me->workbook_relations
      RESULT XML lv_file.

  me->if_fdt_doc_pkg~set_xstring_as_file( abs_filename = workbook_folder && gc_rels_path && workbook_filename && gc_rels_extn  x_file =  lv_file ).
ENDMETHOD.
ENDCLASS.
