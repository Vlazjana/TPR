Custom FI Development Examples - Real Project Scenarios
1. Bulk FI Document Posting Tool with CSV Upload
Business Requirement
The finance team needed to post 5000+ journal entries monthly from external systems.
Manual posting through FB01 was taking 3-4 days and causing data entry errors. Required a solution to upload CSV files, validate data, and post documents in bulk.
Technical Implementation
*&---------------------------------------------------------------------*
*& Report ZFI_BULK_DOCUMENT_POST
*&---------------------------------------------------------------------*
REPORT zfi_bulk_document_post.

" Data declarations
TYPES: BEGIN OF ty_upload,
         bukrs TYPE bukrs,           " Company Code
         blart TYPE blart,           " Document Type
         bldat TYPE bldat,           " Document Date
         budat TYPE budat,           " Posting Date
         waers TYPE waers,           " Currency
         xblnr TYPE xblnr1,          " Reference
         bktxt TYPE bktxt,           " Header Text
         hkont TYPE hkont,           " GL Account
         shkzg TYPE shkzg,           " Debit/Credit
         dmbtr TYPE dmbtr,           " Amount
         zuonr TYPE dzuonr,          " Assignment
         sgtxt TYPE sgtxt,           " Item Text
         kostl TYPE kostl,           " Cost Center
         aufnr TYPE aufnr,           " Order Number
       END OF ty_upload,
       
       BEGIN OF ty_result,
         bukrs TYPE bukrs,
         doc_no TYPE belnr_d,
         status TYPE char1,
         message TYPE string,
         row_no TYPE i,
       END OF ty_result.

DATA: it_upload TYPE TABLE OF ty_upload,
      it_result TYPE TABLE OF ty_result,
      it_alv TYPE TABLE OF ty_result,
      gv_file TYPE string,
      gv_mode TYPE char1 VALUE 'S'.  " S=Simulation, P=Post

" Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH TITLE TEXT-001.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY,
            p_bukrs TYPE bukrs OBLIGATORY,
            p_mode TYPE char1 DEFAULT 'S'.
SELECTION-SCREEN END OF BLOCK b1.

" Radio buttons for mode
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-003.
PARAMETERS: rb_sim RADIOBUTTON GROUP grp1 DEFAULT 'X',
            rb_post RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

" File upload help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_get_file_path.

START-OF-SELECTION.
  PERFORM f_upload_file.
  PERFORM f_validate_data.
  PERFORM f_process_documents.
  PERFORM f_display_results.

*&---------------------------------------------------------------------*
*& Form f_upload_file
*&---------------------------------------------------------------------*
FORM f_upload_file.
  DATA: lv_filename TYPE string.
  
  lv_filename = p_file.
  
  " Upload CSV file
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = lv_filename
      filetype = 'ASC'
      has_field_separator = 'X'
    TABLES
      data_tab = it_upload
    EXCEPTIONS
      file_open_error = 1
      file_read_error = 2
      no_batch = 3
      gui_refuse_filetransfer = 4
      invalid_type = 5
      no_authority = 6
      unknown_error = 7
      bad_data_format = 8
      header_not_allowed = 9
      separator_not_allowed = 10
      header_too_long = 11
      unknown_dp_error = 12
      access_denied = 13
      dp_out_of_memory = 14
      disk_full = 15
      dp_timeout = 16
      OTHERS = 17.
      
  IF sy-subrc <> 0.
    MESSAGE 'Error uploading file' TYPE 'E'.
  ENDIF.
  
  MESSAGE |{ lines( it_upload ) } records uploaded successfully| TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_data
*&---------------------------------------------------------------------*
FORM f_validate_data.
  DATA: wa_upload TYPE ty_upload,
        wa_result TYPE ty_result,
        lv_row TYPE i.
        
  LOOP AT it_upload INTO wa_upload.
    lv_row = sy-tabix.
    CLEAR wa_result.
    wa_result-row_no = lv_row.
    wa_result-bukrs = wa_upload-bukrs.
    
    " Basic validations
    IF wa_upload-bukrs IS INITIAL.
      wa_result-status = 'E'.
      wa_result-message = 'Company code is mandatory'.
      APPEND wa_result TO it_result.
      CONTINUE.
    ENDIF.
    
    IF wa_upload-hkont IS INITIAL.
      wa_result-status = 'E'.
      wa_result-message = 'GL Account is mandatory'.
      APPEND wa_result TO it_result.
      CONTINUE.
    ENDIF.
    
    IF wa_upload-dmbtr IS INITIAL.
      wa_result-status = 'E'.
      wa_result-message = 'Amount cannot be zero'.
      APPEND wa_result TO it_result.
      CONTINUE.
    ENDIF.
    
    " Check company code exists
    SELECT SINGLE bukrs FROM t001
      INTO @DATA(lv_bukrs)
      WHERE bukrs = @wa_upload-bukrs.
    IF sy-subrc <> 0.
      wa_result-status = 'E'.
      wa_result-message = 'Invalid company code'.
      APPEND wa_result TO it_result.
      CONTINUE.
    ENDIF.
    
    " Check GL account exists
    SELECT SINGLE saknr FROM ska1
      INTO @DATA(lv_saknr)
      WHERE saknr = @wa_upload-hkont
        AND ktopl = 'CAIN'.  " Chart of accounts
    IF sy-subrc <> 0.
      wa_result-status = 'E'.
      wa_result-message = 'Invalid GL Account'.
      APPEND wa_result TO it_result.
      CONTINUE.
    ENDIF.
    
    wa_result-status = 'S'.
    wa_result-message = 'Validation successful'.
    APPEND wa_result TO it_result.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_process_documents
*&---------------------------------------------------------------------*
FORM f_process_documents.
  DATA: it_doc_header TYPE TABLE OF bapiache09,
        it_account_gl TYPE TABLE OF bapiacgl09,
        it_currency_amount TYPE TABLE OF bapiaccr09,
        it_return TYPE TABLE OF bapiret2,
        wa_doc_header TYPE bapiache09,
        wa_account_gl TYPE bapiacgl09,
        wa_currency TYPE bapiaccr09,
        wa_return TYPE bapiret2,
        wa_upload TYPE ty_upload,
        wa_result TYPE ty_result,
        lv_doc_no TYPE belnr_d,
        lv_item_no TYPE posnr_acc,
        lv_current_ref TYPE xblnr1.

  " Set mode based on selection
  IF rb_sim = 'X'.
    gv_mode = 'S'.
  ELSE.
    gv_mode = 'P'.
  ENDIF.

  SORT it_upload BY xblnr.
  
  LOOP AT it_upload INTO wa_upload.
    " New document when reference changes
    IF wa_upload-xblnr <> lv_current_ref.
      
      " Post previous document if exists
      IF lv_current_ref IS NOT INITIAL.
        PERFORM f_post_single_document 
          USING it_doc_header it_account_gl it_currency_amount
          CHANGING lv_doc_no it_return.
        
        " Update results
        PERFORM f_update_results 
          USING lv_current_ref lv_doc_no it_return.
      ENDIF.
      
      " Initialize new document
      CLEAR: it_doc_header, it_account_gl, it_currency_amount, it_return.
      lv_current_ref = wa_upload-xblnr.
      lv_item_no = 0.
      
      " Document header
      wa_doc_header-bus_act = 'RFBU'.
      wa_doc_header-username = sy-uname.
      wa_doc_header-comp_code = wa_upload-bukrs.
      wa_doc_header-doc_date = wa_upload-bldat.
      wa_doc_header-pstng_date = wa_upload-budat.
      wa_doc_header-doc_type = wa_upload-blart.
      wa_doc_header-ref_doc_no = wa_upload-xblnr.
      wa_doc_header-header_txt = wa_upload-bktxt.
      APPEND wa_doc_header TO it_doc_header.
    ENDIF.
    
    " Add line item
    lv_item_no = lv_item_no + 1.
    
    wa_account_gl-itemno_acc = lv_item_no.
    wa_account_gl-gl_account = wa_upload-hkont.
    wa_account_gl-item_text = wa_upload-sgtxt.
    wa_account_gl-alloc_nmbr = wa_upload-zuonr.
    wa_account_gl-costcenter = wa_upload-kostl.
    wa_account_gl-orderid = wa_upload-aufnr.
    APPEND wa_account_gl TO it_account_gl.
    
    " Currency amount
    wa_currency-itemno_acc = lv_item_no.
    wa_currency-currency = wa_upload-waers.
    IF wa_upload-shkzg = 'H'.
      wa_currency-amt_doccur = wa_upload-dmbtr * -1.
    ELSE.
      wa_currency-amt_doccur = wa_upload-dmbtr.
    ENDIF.
    APPEND wa_currency TO it_currency_amount.
  ENDLOOP.
  
  " Post last document
  IF lv_current_ref IS NOT INITIAL.
    PERFORM f_post_single_document 
      USING it_doc_header it_account_gl it_currency_amount
      CHANGING lv_doc_no it_return.
    
    PERFORM f_update_results 
      USING lv_current_ref lv_doc_no it_return.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_single_document
*&---------------------------------------------------------------------*
FORM f_post_single_document 
  USING it_header TYPE ANY TABLE
        it_gl TYPE ANY TABLE
        it_curr TYPE ANY TABLE
  CHANGING cv_doc_no TYPE belnr_d
           it_ret TYPE ANY TABLE.
           
  DATA: wa_header TYPE bapiache09.
  
  READ TABLE it_header INTO wa_header INDEX 1.
  
  IF gv_mode = 'S'.
    " Simulation mode
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = wa_header
      TABLES
        accountgl = it_gl
        currencyamount = it_curr
        return = it_ret.
  ELSE.
    " Posting mode
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = wa_header
      IMPORTING
        obj_key = cv_doc_no
      TABLES
        accountgl = it_gl
        currencyamount = it_curr
        return = it_ret.
        
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_update_results
*&---------------------------------------------------------------------*
FORM f_update_results 
  USING iv_ref TYPE xblnr1
        iv_doc_no TYPE belnr_d
        it_ret TYPE ANY TABLE.
        
  DATA: wa_result TYPE ty_result,
        wa_return TYPE bapiret2.
        
  wa_result-bukrs = p_bukrs.
  wa_result-doc_no = iv_doc_no.
  
  READ TABLE it_ret INTO wa_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    wa_result-status = 'E'.
    wa_result-message = wa_return-message.
  ELSE.
    wa_result-status = 'S'.
    IF gv_mode = 'S'.
      wa_result-message = 'Simulation successful'.
    ELSE.
      wa_result-message = |Document { iv_doc_no } posted successfully|.
    ENDIF.
  ENDIF.
  
  APPEND wa_result TO it_alv.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_results
*&---------------------------------------------------------------------*
FORM f_display_results.
  DATA: lo_alv TYPE REF TO cl_salv_table,
        lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column TYPE REF TO cl_salv_column_table.
        
  TRY.
    CALL METHOD cl_salv_table=>factory
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table = it_alv.
        
    " Set column properties
    lo_columns = lo_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).
    
    " Display ALV
    lo_alv->display( ).
    
  CATCH cx_salv_msg.
    MESSAGE 'Error displaying results' TYPE 'E'.
  ENDTRY.
ENDFORM.

Key Features Implemented:
CSV Upload: GUI_UPLOAD with field separator
Data Validation: Company code, GL account, amount checks
Simulation Mode: BAPI_ACC_DOCUMENT_CHECK for testing
Bulk Posting: BAPI_ACC_DOCUMENT_POST with commit
Error Handling: Comprehensive error messages and status tracking
ALV Display: Results showing success/failure per document

2. Custom Fields Enhancement in FI Documents
Business Requirement
Finance team needed additional fields to track project codes, business unit, and approval status at line item level in FI documents. These fields should be visible in FB02/FB03 and FBL3N report.
Technical Implementation
Step 1: Append Structure to BSEG
" Create append structure ZBSEG_APPEND in SE11
" Fields added:
" ZPROJECT  - Project Code (CHAR 10)
" ZBUNIT    - Business Unit (CHAR 4) 
" ZAPPROVAL - Approval Status (CHAR 1)
" ZREQBY    - Requested By (CHAR 12)

Step 2: Screen Enhancement for FB01/FB02
*&---------------------------------------------------------------------*
*& Include for FB01/FB02 Screen Enhancement
*&---------------------------------------------------------------------*

" Screen enhancement using Screen Exit
" Enhancement: SAPLF050_CUST

" In PBO of screen 0100
MODULE pbo_custom_fields OUTPUT.
  " Make custom fields visible based on document type
  DATA: lv_blart TYPE blart.
  
  " Get document type from memory
  IMPORT blart TO lv_blart FROM MEMORY ID 'BLART'.
  
  " Show custom fields only for certain document types
  CASE lv_blart.
    WHEN 'SA' OR 'KR' OR 'RE'.  " Standard, vendor, invoice
      LOOP AT SCREEN.
        IF screen-name CS 'ZPROJECT' OR
           screen-name CS 'ZBUNIT' OR
           screen-name CS 'ZAPPROVAL'.
          screen-active = 1.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
      " Hide custom fields for other document types
      LOOP AT SCREEN.
        IF screen-name CS 'ZPROJECT' OR
           screen-name CS 'ZBUNIT' OR
           screen-name CS 'ZAPPROVAL'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDMODULE.

" In PAI of screen 0100
MODULE pai_custom_fields INPUT.
  " Validate custom field entries
  IF bseg-zproject IS NOT INITIAL.
    " Check if project exists
    SELECT SINGLE pspid FROM proj
      INTO @DATA(lv_project)
      WHERE pspid = @bseg-zproject.
    IF sy-subrc <> 0.
      MESSAGE 'Invalid project code' TYPE 'E'.
    ENDIF.
  ENDIF.
  
  " Validate business unit
  IF bseg-zbunit IS NOT INITIAL.
    SELECT SINGLE butxt FROM tgsbt
      INTO @DATA(lv_bunit)
      WHERE gsber = @bseg-zbunit.
    IF sy-subrc <> 0.
      MESSAGE 'Invalid business unit' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.

Step 3: Configuration in SPRO
" Configuration steps:
" 1. SPRO → Financial Accounting → General Ledger → Master Data
"    → Line Item Layouts → Define Line Item Layouts
" 2. Create new layout ZCUSTOM_LAYOUT
" 3. Add fields: ZPROJECT, ZBUNIT, ZAPPROVAL, ZREQBY
" 4. Assign layout to company code in configuration

" Table: T021S - Field Selection Groups
" Insert entries for custom fields to make them available

Step 4: Enhance FBL3N Report
*&---------------------------------------------------------------------*
*& Enhancement for FBL3N - Include ZFBL3N_ENHANCEMENT
*&---------------------------------------------------------------------*

" Add custom fields to selection screen
SELECTION-SCREEN BEGIN OF BLOCK cust WITH TITLE TEXT-c01.
SELECT-OPTIONS: s_zproj FOR bseg-zproject,
                s_zbunit FOR bseg-zbunit,
                s_zapprv FOR bseg-zapproval.
SELECTION-SCREEN END OF BLOCK cust.

" Modify the internal table structure
TYPES: BEGIN OF ty_bseg_enh,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         hkont TYPE hkont,
         dmbtr TYPE dmbtr,
         " Standard fields
         zproject TYPE char10,    " Custom fields
         zbunit TYPE char4,
         zapproval TYPE char1,
         zreqby TYPE char12,
       END OF ty_bseg_enh.

DATA: it_bseg_enh TYPE TABLE OF ty_bseg_enh.

" Enhanced data selection
SELECT bseg~bukrs,
       bseg~belnr,
       bseg~gjahr,
       bseg~buzei,
       bseg~hkont,
       bseg~dmbtr,
       bseg~zproject,
       bseg~zbunit,
       bseg~zapproval,
       bseg~zreqby
  FROM bseg
  INTO CORRESPONDING FIELDS OF TABLE it_bseg_enh
  WHERE bukrs IN s_bukrs
    AND hkont IN s_hkont
    AND gjahr IN s_gjahr
    AND zproject IN s_zproj
    AND zbunit IN s_zbunit
    AND zapproval IN s_zapprv.

" ALV fieldcatalog enhancement
FORM build_fieldcatalog_custom.
  DATA: wa_fieldcat TYPE slis_fieldcat_alv.
  
  " Project field
  wa_fieldcat-fieldname = 'ZPROJECT'.
  wa_fieldcat-seltext_l = 'Project Code'.
  wa_fieldcat-seltext_m = 'Project'.
  wa_fieldcat-seltext_s = 'Proj'.
  wa_fieldcat-col_pos = 15.
  wa_fieldcat-outputlen = 10.
  APPEND wa_fieldcat TO gt_fieldcat.
  
  " Business Unit field
  wa_fieldcat-fieldname = 'ZBUNIT'.
  wa_fieldcat-seltext_l = 'Business Unit'.
  wa_fieldcat-seltext_m = 'Bus Unit'.
  wa_fieldcat-seltext_s = 'BU'.
  wa_fieldcat-col_pos = 16.
  wa_fieldc


