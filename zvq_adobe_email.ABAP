*&---------------------------------------------------------------------*
*& Report ZVQ_ADOBE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvq_adobe_email.

PARAMETERS : P_vbeln TYPE vbeln_va.
DATA : ls_out        TYPE sfpoutputparams,
       lv_result     TYPE sfpjobresult,
       ls_formoutput TYPE fpformoutput,
       lt_bin_tab    TYPE solix_tab,
       lo_bcs        TYPE REF TO cl_bcs,
       id_external   TYPE REF TO cl_cam_address_bcs,
       lo_sapuser    TYPE REF TO cl_sapuser_bcs,
       lt_text       TYPE soli_tab,
       ls_text       TYPE soli,
       lo_bcs_document  TYPE REF TO cl_document_bcs,
       lv_name       TYPE funcname.

ls_out-nodialog = 'X'.
ls_out-preview = 'X'.
ls_out-dest = 'LP01'.
ls_out-getpdf = 'X'.

CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams = ls_out
  EXCEPTIONS
    cancel          = 1
    usage_error     = 2
    system_error    = 3
    internal_error  = 4
    OTHERS          = 5.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
  EXPORTING
    i_name     = 'ZVQ_ADOBE'
  IMPORTING
    e_funcname = lv_name
*   E_INTERFACE_TYPE           =
*   EV_FUNCNAME_INBOUND        =
  .

*CALL FUNCTION lv_name
CALL FUNCTION '/1BCDWB/SM00000853'
  EXPORTING
*   /1BCDWB/DOCPARAMS  =
    p_vbeln            = p_vbeln
  IMPORTING
    /1bcdwb/formoutput = ls_formoutput
  EXCEPTIONS
    usage_error        = 1
    system_error       = 2
    internal_error     = 3
    OTHERS             = 4.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
  EXPORTING
    buffer     = ls_formoutput-pdf
*   APPEND_TO_TABLE       = ' '
* IMPORTING
*   OUTPUT_LENGTH         =
  TABLES
    binary_tab = lt_bin_tab.

TRY.
    CALL METHOD cl_bcs=>create_persistent
      RECEIVING
        result = lo_bcs.
  CATCH cx_send_req_bcs.
ENDTRY.

TRY.
    CALL METHOD cl_sapuser_bcs=>create
      EXPORTING
        i_user = 'Thepr05'
      RECEIVING
        result = lo_sapuser.
  CATCH cx_address_bcs.
ENDTRY.

TRY.
    CALL METHOD lo_bcs->add_recipient
      EXPORTING
        i_recipient = lo_sapuser
*       i_express   =
*       i_copy      =
*       i_blind_copy =
*       i_no_forward =
      .
  CATCH cx_send_req_bcs.
ENDTRY.

TRY.
    CALL METHOD cl_cam_address_bcs=>create_internet_address
      EXPORTING
        i_address_string = 'Vlazjanaqama@gmail.com'
*       i_address_name   =
*       i_incl_sapuser   =
      RECEIVING
        result           = id_external.
  CATCH cx_address_bcs.
ENDTRY.
lt_text = VALUE soli_tab(
  ( line = TEXT-001 )
  ( line = TEXT-002 )
  ( line = TEXT-003 )
  ( line = TEXT-004 )
).

TRY.
    CALL METHOD cl_document_bcs=>create_document
      EXPORTING
        i_type    = 'RAW'
        i_subject = TEXT-000
*       i_length  =
*       i_language      = SPACE
*       i_importance    =
*       i_sensitivity   =
        i_text    = lt_text
*       i_hex     =
*       i_header  =
*       i_sender  =
*       iv_vsi_profile  =
*       iv_vsi_scan_off =
      RECEIVING
        result    = lo_bcs_document.
  CATCH cx_document_bcs.
ENDTRY.

DATA lv_subject TYPE  SOOD-OBJDES.
CONCATENATE TEXT-005 P_vbeln INTO lv_subject.

TRY.
    CALL METHOD lo_bcs_document->add_attachment
      EXPORTING
        i_attachment_type    = 'PDF'
        i_attachment_subject = lv_subject
*       i_attachment_size    =
*       i_attachment_language = SPACE
*       i_att_content_text   =
       i_att_content_hex    = lt_bin_tab
*       i_attachment_header  =
*       iv_vsi_profile       =
*       iv_vsi_scan_off      =
      .
  CATCH cx_document_bcs.
ENDTRY.

TRY.
CALL METHOD lo_bcs->set_document
  EXPORTING
    i_document = lo_bcs_document
    .
  CATCH cx_send_req_bcs.
ENDTRY.

TRY.
CALL METHOD lo_bcs->set_send_immediately
  EXPORTING
    i_send_immediately = 'X'
    .
  CATCH cx_send_req_bcs.
ENDTRY.

DATA LV_FINALRESULT TYPE OS_BOOLEAN.

TRY.
CALL METHOD lo_bcs->send
*  EXPORTING
*    i_with_error_screen = SPACE
  RECEIVING
    result              = LV_FINALRESULT
    .
  CATCH cx_send_req_bcs.
ENDTRY.

IF LV_FINALRESULT = 'X'.
  COMMIT WORK.
  ENDIF.

*data lv_result type
CALL FUNCTION 'FP_JOB_CLOSE'
  IMPORTING
    e_result       = lv_result
  EXCEPTIONS
    usage_error    = 1
    system_error   = 2
    internal_error = 3
    OTHERS         = 4.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
