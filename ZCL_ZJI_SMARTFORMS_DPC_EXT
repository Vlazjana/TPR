ZCL_ZJI_SMARTFORMS_DPC_EXT

METHOD get_stream.

Data: lv_fname type rs381_fnam,
         Lv_binvalue type xtring,
         Lv_po type ebeln.
Data: wa_control_parameters type ssfctrlop,
          Wa_output_options type ssfcompop,
          Wa_stream type ty_s_media_resource,
          Wa_otf_from_fm type ssfcrescl.
Data: it_lines type table of tline,
          It_otf type table of itcoo.


CALL FUNCTION ‘SSF_FUNCTION_MODULE_NAME’
 EXPORTING 
FORMNAME  = ‘NAME OF SMARTFORMS”
IMPORTING
FM_NAME =  lv_fname 


“Call fm with  smartforms fm

Lv_po = it_key_tab[1]-value 


Wa_output_options-tddest = ‘LP01’
Wa_output_options-xdfcmode = abap_true.
Wa_output_options-xsfcmode = abap_true.
Wa_output_options-tdnewid = abap_true.
Wa_output_options-tdimmed = abap_true.
Wa_output_options-no_dialog = abap_true.
Wa_output_options-preview = space.
Wa_output_options-getotf = abap_true.


Call function lv_fname
Exporting
Control_parameters = wa_control_parameters
Output_options        = wa_output_options
im_po                       =  lv_po
IMPORTING 
JOB_OUTPUT_INFO = wa_otf_from_fm

It_otf = wa_otf_from_fm-otfdata[].


CALL FUNCTION ‘CONVERT_OTF’
EXPORTING 
FORMAT   = ‘PDF’
IMPORTING 
BIN_FILE     = IT_OTF
LINES = IT_LINES.

wa_stream -value = lv_binvalue.
Wa_stream-mime_type = ‘application/pdf’.

CALL METHOD me->copy_data_to_ref
Exporting
Is_data = wa_stream
CHANGNING
Cr_data = er_stream


Endmethod.


HTTP GET
Request URI : /sap/opu/odata/sap/ZJI_SMARTFORMS_SRV/Pur_OrderSet(‘450001234’ ) /$VALUE




