RFC





FORM create_cbi USING    p_valut
                         p_wrbtr
                         p_waers "LC AD14294 D02K909540
                         p_bukrs
                         p_hbkid
                         p_lifnr
                         p_agkoa
                         p_hkont
                         p_zlsch
                         p_xblnr
                         p_belnr
                         p_gjahr
                         p_bankl
                         p_causa
                         i_belnr
                         p_koinh.

  CASE p_zlsch.                                 "CR-001
    WHEN 'B'.     "wire transfer .txt           "CR-001
*mv begin 21012016 --------
*  CALL FUNCTION 'Z_FI_CREATE_CBI_2'          "Z_FI_CREATE_CBI   20090327
*      CALL FUNCTION 'Z_FI_CREATE_CBI_2'
*        EXPORTING
**         I_IT    = 'X'
*          i_valut = p_valut
*          i_wrbtr = p_wrbtr
*          i_bukrs = p_bukrs
*          i_hbkid = p_hbkid
*          i_lifnr = p_lifnr
*          i_agkoa = p_agkoa
*          i_hkont = p_hkont
*          i_zlsch = p_zlsch
*          i_xblnr = p_xblnr
*          i_belnr = p_belnr
*          i_gjahr = p_gjahr
*          i_bankl = p_bankl
*          i_causa = p_causa
*          p_belnr = i_belnr
*          i_koinh = p_koinh
*          i_waers = p_waers.
*      .


      CALL FUNCTION 'Z_FI_CREATE_XML_B'
        EXPORTING
*         I_IT          = 'X'
          i_valut       = p_valut
          i_wrbtr       = p_wrbtr
          i_bukrs       = p_bukrs
          i_hbkid       = p_hbkid
          i_lifnr       = p_lifnr
          i_agkoa       = p_agkoa
          i_hkont       = p_hkont
          i_zlsch       = p_zlsch
          i_xblnr       = p_xblnr
          i_belnr       = p_belnr
          i_gjahr       = p_gjahr
          i_bankl       = p_bankl
          i_causa       = p_causa
          p_belnr       = i_belnr
          i_koinh       = p_koinh
          i_waers       = p_waers.
                .

    WHEN '$'.
      CALL FUNCTION 'Z_FI_CREATE_XML'
*mv end -------------------
        EXPORTING
*         I_IT    = 'X'
          i_valut = p_valut
          i_wrbtr = p_wrbtr
          i_bukrs = p_bukrs
          i_hbkid = p_hbkid
          i_lifnr = p_lifnr
          i_agkoa = p_agkoa
          i_hkont = p_hkont
          i_zlsch = p_zlsch
          i_xblnr = p_xblnr
          i_belnr = p_belnr
          i_gjahr = p_gjahr
          i_bankl = p_bankl
          i_causa = p_causa
          p_belnr = i_belnr
          i_koinh = p_koinh
          i_waers = p_waers. "LC AD14294 D02K909540

  ENDCASE.

ENDFORM.              
FUNCTION z_fi_create_xml_b .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_IT) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(I_VALUT) TYPE  VALUT
*"     REFERENCE(I_WRBTR) TYPE  WRBTR
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_HBKID) TYPE  HBKID
*"     REFERENCE(I_LIFNR) TYPE  LIFNR
*"     REFERENCE(I_AGKOA) TYPE  KOART
*"     REFERENCE(I_HKONT) TYPE  HKONT
*"     REFERENCE(I_ZLSCH) TYPE  SCHZW_BSEG
*"     REFERENCE(I_XBLNR) TYPE  XBLNR
*"     REFERENCE(I_BELNR) LIKE  BKPF-BELNR
*"     REFERENCE(I_GJAHR) LIKE  BKPF-GJAHR
*"     REFERENCE(I_BANKL) LIKE  BNKA-BANKL
*"     REFERENCE(I_CAUSA) TYPE  CHAR5
*"     REFERENCE(P_BELNR) LIKE  BKPF-BELNR
*"     REFERENCE(I_KOINH) TYPE  KOINH_FI
*"     REFERENCE(I_WAERS) TYPE  WAERS
*"----------------------------------------------------------------------
*      CR-001   08.03.2016
*               Ticket 8000024271 - Set reference data into <Ustrd>
*"----------------------------------------------------------------------
*      CR-002   31.10.2016  - Set field data valuta in xml header
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------
*&  Author:       Claudiu Bodan         (Crystal System)
*&  Requested by: Anna Iacobelli
*&  Date:         12.06.2019
*&  Description:  Ticket 7217A  Add Paym_text in XML - tcodes ZFI011/ZFI014                            *
*&  Modif ID   CR-003
*"----------------------------------------------------------------------
  DATA: lo_ixml TYPE REF TO if_ixml.
  DATA: lo_document TYPE REF TO if_ixml_document.
  DATA: lr_parser TYPE REF TO if_ixml_parser.
  DATA: lo_payment_req  TYPE REF TO if_ixml_element.
  DATA: lo_grphdr TYPE REF TO if_ixml_element.
  DATA: lo_pmtinf TYPE REF TO if_ixml_element.
  DATA: lo_initgpty TYPE REF TO if_ixml_element.
  DATA: lo_id TYPE REF TO if_ixml_element.
  DATA: lo_othr TYPE REF TO if_ixml_element.
  DATA: lo_orgid TYPE REF TO if_ixml_element.
  DATA: lo_pmttpinf TYPE REF TO if_ixml_element.
  DATA: lo_svclvl TYPE REF TO if_ixml_element.
  DATA: lo_dbtr TYPE REF TO if_ixml_element.
  DATA: lo_pstladr TYPE REF TO if_ixml_element.
  DATA: lo_dbtracct TYPE REF TO if_ixml_element.
  DATA: lo_dbtrid TYPE REF TO if_ixml_element.
  DATA: lo_pmtid TYPE REF TO if_ixml_element.
  DATA: lo_dbtragt TYPE REF TO if_ixml_element.
  DATA: lo_fininstid TYPE REF TO if_ixml_element.
  DATA: lo_clrsysmmbid TYPE REF TO if_ixml_element.
  DATA: lo_cdttrftxinf TYPE REF TO if_ixml_element.
  DATA: lo_cdtid TYPE REF TO if_ixml_element.
  DATA: lo_cdtpmtinf TYPE REF TO if_ixml_element.
  DATA: lo_ctgypurp TYPE REF TO if_ixml_element.
  DATA: lo_amt TYPE REF TO if_ixml_element.
  DATA : lo_instdamt TYPE REF TO if_ixml_element.
  DATA: lo_cdtr TYPE REF TO if_ixml_element.
  DATA: lo_cdtr_pstladr TYPE REF TO if_ixml_element.
  DATA: lo_cdtracct TYPE REF TO if_ixml_element.
  DATA: lo_cdtracctid TYPE REF TO if_ixml_element.
  DATA: lo_rmtinf TYPE REF TO if_ixml_element.
  DATA: lo_initqptynm TYPE REF TO if_ixml_element.
  DATA: lo_initqptyid TYPE REF TO if_ixml_element.
  DATA: lo_initqptyorgid TYPE REF TO if_ixml_element.
  DATA: lo_initqptyothr TYPE REF TO if_ixml_element.
  DATA: lo_cdtragt TYPE REF TO if_ixml_element.
  DATA: lo_fininstnidcdt_cdtragt TYPE REF TO if_ixml_element.

  DATA: l_dtadithr LIKE dtadithr,
        l_dtadit10 LIKE dtadit10,
        l_dtadit17 LIKE g_dtadit17,
        l_dtadit20 LIKE dtadit20,
        l_dtadit30 LIKE zfi_dtadit30,
        l_dtadit40 LIKE dtadit40,
        l_dtadit50 LIKE dtadit50,
        l_dtadit70 LIKE dtadit70,
        l_dtaditef LIKE dtaditef.

  DATA lv_iban_t TYPE tiban-iban.
  DATA lv_bankn TYPE lfbk-bankn.
  DATA lv_banks TYPE lfbk-banks.
  DATA lv_insert_type TYPE char4.

  DATA ok_ita.

  DATA l_wrbtr LIKE i_wrbtr.

  DATA lv_paym_txt_str TYPE string.

  CLEAR: dta_zaehler,
         dta_reccnt,
         cnt_formulare,
         cnt_hinweise,
         sum_abschluss,
         sum_regut,
         g_t_dta_record.

*****MK ADD TC13487A
  DATA: gs_tiban TYPE tiban.
  DATA: gv_empfb TYPE bseg-empfb.
*****MK ADD TC13487A


  REFRESH g_t_dta_record.

* Bonifico italia / estero
  PERFORM set_ita USING    i_agkoa
                           i_lifnr
                           i_belnr
                           i_gjahr
                           i_bukrs
                  CHANGING ok_ita
                           g_waers.

* Importo documento
  PERFORM read_wrbtr USING    i_bukrs
                              i_gjahr
                              i_belnr
                              i_wrbtr
                              i_waers  "LC AD14294 D02K909544
                     CHANGING l_wrbtr.

*******DS@DB01
*initial info
*  PERFORM fill_dta_record_xml USING '<?xml version="1.0" encoding="UTF-8"?>'.
***  PERFORM fill_dta_record_xml USING
***        '<CBIPaymentRequest xmlns="urn:CBI:xsd:CBIPaymentRequest.00.04.00">'.
*******DS@DB01


  DATA: lv_datetime     TYPE string,
        lv_wrbtrx       TYPE string,
        lv_dtkid        TYPE dtkid,
        lv_budat        TYPE budat,
        lv_budatx       TYPE string,
        lv_ort01        TYPE string,
        lv_land1c       TYPE string,
        lv_land1        TYPE string,
        lv_iban         TYPE iban,
        lv_value        TYPE string,
        lv_banknr       TYPE bankl,
        lv_bank_account TYPE bankk,
        lv_bank_country TYPE banks,
        lv_swift        TYPE swift,
        lv_uzeitx       TYPE char8,
        lv_next         TYPE string,
        lv_hdr1         TYPE string, ""char41.
        lv_hdr2         TYPE string,
        lv_hdr3         TYPE string,
        lv_hdr4         TYPE string,
        lv_hdr5         TYPE string,
        lv_hdr6         TYPE string,
        lv_hdr7         TYPE string,
        lv_hdr8         TYPE string.

* header section

  PERFORM get_next_nr USING i_bukrs
                      CHANGING lv_next.



***DATA: lo_grphdr TYPE REF TO if_ixml_element.
***DATA: lo_pmtinf TYPE REF TO if_ixml_element.
***DATA: lo_initgpty TYPE REF TO if_ixml_element.
***DATA: lo_id TYPE REF TO if_ixml_element.
***DATA: lo_othr TYPE REF TO if_ixml_element.
***DATA: lo_orgid TYPE REF TO if_ixml_element.
***DATA: lo_pmttpinf TYPE REF TO if_ixml_element.
***DATA: lo_svclvl TYPE REF TO if_ixml_element.
***DATA: lo_dbtr TYPE REF TO if_ixml_element.
***DATA: lo_pstladr TYPE REF TO if_ixml_element.
***DATA: lo_dbtracct TYPE REF TO if_ixml_element.
***DATA: lo_pmtid TYPE REF TO if_ixml_element.
***DATA: lo_dbtragt TYPE REF TO if_ixml_element.
***DATA: lo_fininstid TYPE REF TO if_ixml_element.
***DATA: lo_clrsysmmbid TYPE REF TO if_ixml_element.




*lo_initqpty =  lo_document->create_simple_element(
*                                      name    = 'InitgPty'
*                                      parent  = lo_grphdr ).

*  PERFORM fill_dta_record_xml USING '   <GrpHdr>'.
*  concatenate '       <MsgId>' lv_next '</MsgId>' into lv_hdr1.
*  PERFORM fill_dta_record_xml USING lv_hdr1.

*  clear lv_hdr1.
*  CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) 'T'
*              sy-uzeit INTO lv_datetime.
*  CONCATENATE '       <CreDtTm>' lv_datetime '</CreDtTm>'
*  INTO lv_hdr1.
*  PERFORM fill_dta_record_xml USING lv_hdr1.
*
*  MOVE '       <NbOfTxs>1</NbOfTxs>' TO lv_hdr2.
*  PERFORM fill_dta_record_xml USING lv_hdr2.

  lv_wrbtrx = l_wrbtr.
*  CONCATENATE '       <CtrlSum>' lv_wrbtrx '</CtrlSum>'
*  INTO lv_hdr3.
*  PERFORM fill_dta_record_xml USING lv_hdr3.

* company data
*  PERFORM fill_dtadit20 USING    i_bukrs
*                        CHANGING g_dtadit20.
*  PERFORM fill_dta_record_xml USING '       <InitgPty>'.
*  CONCATENATE '           <Nm>' g_dtadit20-tag03 '</Nm>'
*  INTO lv_hdr4.
*  PERFORM fill_dta_record_xml USING lv_hdr4.

* cuc nr
*  PERFORM fill_dta_record_xml USING '           <Id>'.
*  PERFORM fill_dta_record_xml USING '              <OrgId>'.
*  PERFORM fill_dta_record_xml USING '                 <Othr>'.
  CLEAR lv_dtkid.
  SELECT SINGLE dtkid INTO lv_dtkid
     FROM t012d WHERE bukrs = i_bukrs
                 AND hbkid = i_hbkid.
*  CONCATENATE '                    <Id>' lv_dtkid '</Id>'
*  INTO lv_hdr5.
  PERFORM fill_dta_record_xml USING lv_hdr5.
*
*  PERFORM fill_dta_record_xml USING '                    <Issr>CBI</Issr>'.
*  PERFORM fill_dta_record_xml USING '                 </Othr>'.
*  PERFORM fill_dta_record_xml USING '              </OrgId>'.
*  PERFORM fill_dta_record_xml USING '           </Id>'.
*  PERFORM fill_dta_record_xml USING '       </InitgPty>'.
*  PERFORM fill_dta_record_xml USING '   </GrpHdr>'.
*
*
** item section
*  PERFORM fill_dta_record_xml USING '   <Pmtinf>'.

*  concatenate '       <PmtInfId>' lv_next '</PmtInfId>' into lv_hdr1.
*  PERFORM fill_dta_record_xml USING lv_hdr1.
* PERFORM fill_dta_record_xml USING '       <PmtInfId>9000000001</PmtInfId>'.

*  PERFORM fill_dta_record_xml USING '       <PmtMtd>TRF</PmtMtd>'.
*  PERFORM fill_dta_record_xml USING '       <PmtTpInf>'.
*  PERFORM fill_dta_record_xml USING '          <SvcLvl>'.
*  PERFORM fill_dta_record_xml USING '             <Cd>SEPA</Cd>'.
*  PERFORM fill_dta_record_xml USING '          </SvcLvl>'.
*  PERFORM fill_dta_record_xml USING '       </PmtTpInf>'.

* posting date or payment date
  SELECT SINGLE budat INTO lv_budat
     FROM bkpf
             WHERE belnr = i_belnr
               AND bukrs = i_bukrs
               AND gjahr = i_gjahr.
  CONCATENATE lv_budat(4) '-' lv_budat+4(2) '-' lv_budat+6(2)
              INTO lv_budatx.
*  CONCATENATE '       <ReqdExctnDt>' lv_budatx '</ReqdExctnDt>'
*  INTO lv_hdr6.
  PERFORM fill_dta_record_xml USING lv_hdr6.

* company that makes the payment
*  PERFORM fill_dta_record_xml USING '       <Dbtr>'.
*  PERFORM fill_dta_record_xml USING lv_hdr4.
*  PERFORM fill_dta_record_xml USING '          <PstlAdr>'.
  PERFORM fill_dtadit20_xml USING i_bukrs
                         CHANGING g_dtadit20
                                  lv_ort01
                                  lv_land1c.
*  CONCATENATE '             <Ctry>' lv_land1c '</Ctry>'
*  INTO lv_hdr7.
  PERFORM fill_dta_record_xml USING lv_hdr7.
*  CONCATENATE '             <AdrLine>' lv_ort01 '</AdrLine>'
*  INTO lv_hdr7.
  PERFORM fill_dta_record_xml USING lv_hdr7.

*  PERFORM fill_dta_record_xml USING '          </PstlAdr>'.
*  PERFORM fill_dta_record_xml USING '       </Dbtr>'.





* Bonifico estero
  IF ok_ita = space.
    PERFORM fill_dtadithr_new USING    i_valut
                                       i_bukrs
                                       i_hbkid
                                       i_hkont
* PG - nome supporto
                                       i_belnr
                                       i_zlsch
                                       i_waers
* PG
                              CHANGING es_dtadithr.

**    PERFORM fill_dta_record USING es_dtadithr.

    PERFORM gestione_estero USING i_valut          "Valuta
                                  i_lifnr          "Fornitore/Cliente
                                  i_agkoa          "Tipo conto
                                  l_wrbtr
                                  i_bukrs          "Società
                                  i_hbkid          "Banca interna
                                  i_hkont          "CC banca interna
                                  i_belnr
                                  i_xblnr                   "RN:163643
                                  i_gjahr
                                  i_bankl
                                  i_causa
                                  g_waers
                                  p_belnr
                                  i_koinh
                                  i_zlsch.                  "20090327

    PERFORM fill_ef_estero USING l_wrbtr.          "i_wrbtr.

  ELSE.
*   Bonifico Italia
    PERFORM fill_dtadithr USING    i_valut          "Data Valuta
                                   i_bukrs
                                   'PC'
                                   i_hbkid          "Banca interna
                                   i_hkont
                                   i_zlsch
                                   i_waers
* PG - nome supporto
                                   i_belnr
* PG
                          CHANGING g_dtadithr.

**    PERFORM fill_dta_record USING g_dtadithr.

*   Ordine di trasf.
    PERFORM fill_dtadit10 USING    i_valut          "Data Valuta
                                   l_wrbtr
                                   i_lifnr          "Fornitore/Cliente
                                   i_agkoa          "Tipo conto
                                   i_bukrs          "Società
                                   i_hbkid          "Banca interna
                                   i_hkont          "CC banca interna
                                   i_bankl          "BANCA BENEFICIARIO
                                   i_koinh                  "20090327
                                   i_zlsch
                                   i_waers
                          CHANGING l_dtadit10.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit10.

*   Iban Beneficiario
    PERFORM fill_dtadit17 USING    i_hbkid
                                   i_hkont
                                   i_lifnr
                                   i_agkoa
                                   i_bukrs
                                   i_bankl
                                   i_koinh                  "20090327
                                   l_dtadit10
                          CHANGING l_dtadit17.


*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit17.

*   Committente
**    PERFORM fill_dtadit20 USING    i_bukrs
**                          CHANGING g_dtadit20.

*   Inserimento dati
**    PERFORM fill_dta_record USING g_dtadit20.

*   Dati destinat.
    PERFORM fill_dtadit30 USING    i_lifnr
                                   i_agkoa
                          CHANGING l_dtadit30.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit30.


*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit40.

*   Record 50/60 (rif. p. dest)
    PERFORM fill_dtadit50 USING    i_xblnr
                          CHANGING l_dtadit50.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit50.

*   Record 70 (rif. per banca)
    PERFORM fill_dtadit70 CHANGING l_dtadit70.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit70.
*   Record EF (rec. finale)
    PERFORM fill_dtaditef USING    g_dtadithr
                                   l_wrbtr
                          CHANGING l_dtaditef.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtaditef.

  ENDIF.

*   Dati destin.
  PERFORM fill_dtadit40_xml USING    i_lifnr
                                 i_agkoa
                                 i_bankl          "BANCA BENEFICIARIO
                                 i_koinh                    "20090327
                                 i_belnr   ""MK ADD TC13487A
                                 i_bukrs   ""MK ADD TC13487A
                                 i_gjahr   ""MK ADD TC13487A
                        CHANGING l_dtadit40
                                 lv_land1
                                 gs_tiban  ""MK ADD TC13487A
                                 gv_empfb. ""MK ADD TC13487A

* iban new determination
  CLEAR lv_iban.
  PERFORM fill_dtadit17_xml USING    i_hbkid
                                 i_hkont
                                 i_lifnr
                                 i_agkoa
                                 i_bukrs
                                 i_bankl
                                 i_koinh                    "20090327
                                 i_zlsch
                                 i_waers
                                 l_dtadit10
                        CHANGING lv_iban.


  PERFORM convert_iban USING lv_iban
                      CHANGING lv_bank_account
                               lv_bank_country
                               lv_banknr.
  PERFORM get_swift USING lv_bank_account
                            lv_bank_country
                      CHANGING lv_swift.




*  PERFORM fill_dta_record_xml USING '       <DbtrAct>'.
*  PERFORM fill_dta_record_xml USING '          <Id>'.
*  CONCATENATE '              <IBAN>' lv_iban '</IBAN>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '          </Id>'.
*  PERFORM fill_dta_record_xml USING '       </DbtrAct>'.
*
*  PERFORM fill_dta_record_xml USING '       <DbtrAgt>'.
*  PERFORM fill_dta_record_xml USING '          <FinInstnId>'.
*  PERFORM fill_dta_record_xml USING '              <ClrSysMmbId>'.
*  CLEAR lv_hdr8.

*  PERFORM convert_iban USING lv_iban
*                       CHANGING lv_bank_account
*                                lv_bank_country
*                                lv_banknr.

*  CONCATENATE '                 <MmbId>' lv_iban '</MmbId>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '              </ClrSysMmbId>'.
*  PERFORM fill_dta_record_xml USING '          </FinInstnId>'.
*  PERFORM fill_dta_record_xml USING '       </DbtrAgt>'.
*
*  PERFORM fill_dta_record_xml USING '       <ChrgBr>SLEV</ChrgBr>'.
*
*  PERFORM fill_dta_record_xml USING '       <CdtTrfTxInf>'.
*
*  PERFORM fill_dta_record_xml USING '           <PmtId>'.
*  CLEAR lv_hdr8.
*  CONCATENATE '             <InstrId>' i_bukrs i_belnr i_gjahr '</InstrId>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*
*  CLEAR lv_hdr8.
*  CONCATENATE '             <EndToEndId>' i_belnr '</EndToEndId>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '           </PmtId>'.

*  PERFORM fill_dta_record_xml USING '           <PmtTpInf>'.
*  PERFORM fill_dta_record_xml USING '              <CtgyPurp>'.
*  PERFORM fill_dta_record_xml USING '                 <Cd>SUPP</Cd>'.
*  PERFORM fill_dta_record_xml USING '              </CtgyPurp>'.
*  PERFORM fill_dta_record_xml USING '           </PmtTpInf>'.
*
*  PERFORM fill_dta_record_xml USING '           <Amt>'.
*  CONCATENATE '              <InstdAmt Ccy="EUR">' lv_wrbtrx '</InstdAmt Ccy>'
*  INTO lv_hdr3.
*  PERFORM fill_dta_record_xml USING lv_hdr3.
*  PERFORM fill_dta_record_xml USING '           </Amt>'.
*
*  PERFORM fill_dta_record_xml USING '           <Cdtr>'.
*  CONCATENATE '              <Nm>' lv_iban '</Nm>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*
*  PERFORM fill_dta_record_xml USING '              <PstlAdr>'.
*  CLEAR lv_hdr8.
*  CONCATENATE '                  <Ctry>' lv_land1 '</Ctry>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  CLEAR lv_hdr8.
*  CONCATENATE '                  <AdrLine>' l_dtadit40-tag03 '</AdrLine>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  CLEAR lv_hdr8.
*  CONCATENATE '                  <AdrLine>' l_dtadit40-tag04 l_dtadit40-tag05
*  '</AdrLine>' INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '              </PstlAdr>'.
*  PERFORM fill_dta_record_xml USING '           </Cdtr>'.
*
*  PERFORM fill_dta_record_xml USING '           <CdtrAcct>'.
*  PERFORM fill_dta_record_xml USING '              <Id>'.

*  CONCATENATE '                 <IBAN>' i_koinh
*              '</IBAN>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.


*
*  CONCATENATE '                 <SWIFT>' lv_swift
*              '</SWIFT>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.


*  PERFORM fill_dta_record_xml USING '              </Id>'.
*  PERFORM fill_dta_record_xml USING '           </CdtrAcct>'.

*  PERFORM fill_dta_record_xml USING '           <RmtInf>'.
*  CLEAR lv_hdr8.

  WRITE sy-uzeit TO lv_uzeitx.
*  CONCATENATE '               <Ustrd>INV' lv_uzeitx '</Ustrd>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '           </RmtInf>'.
******
****** end items / end file
*  PERFORM fill_dta_record_xml USING '       </CdtTrfTxInf>'.
*  PERFORM fill_dta_record_xml USING '   </Pmtinf>'.
*  PERFORM fill_dta_record_xml USING '</CBIPaymentRequest>'.


* Create iXML object
  lo_ixml = cl_ixml=>create( ).

*DATA:
**      lo_document TYPE REF TO if_ixml_document,
*      lo_stream_factory TYPE REF TO if_ixml_stream_factory,
**      lo_encoding TYPE REF TO if_ixml_encoding,
*      s_encoding_type TYPE string.
** Create stream factory
*lo_stream_factory = lo_ixml->create_stream_factory( ).
*
** Set encoding type
*s_encoding_type = 'utf-8'.
*DATA(lo_encoding)
* = lo_ixml->create_encoding( character_set = s_encoding_type
*                                        byte_order = 0 ).


  lo_document = lo_ixml->create_document( ).

  lo_payment_req   = lo_document->create_simple_element(
                                      name    = 'CBICrossBorderPaymentRequestLogMsg'
                                      parent  = lo_document ).
  lo_payment_req->set_attribute( name  = 'xmlns'
                                 value = 'urn:CBI:xsd:CBICrossBorderPaymentRequestLogMsg.00.01.01' ).

  lo_payment_req->set_attribute( name  = 'xmlns:xsi'
                                 value = 'http://www.w3.org/2001/XMLSchema-instance' ).

* Create Header
  lo_grphdr = lo_document->create_simple_element( name    = 'GrpHdr'
                                      parent  = lo_payment_req ).

  lo_document->create_simple_element( name    = 'MsgId'
                                      parent  = lo_grphdr
                                      value   = lv_next ).

  CLEAR lv_datetime.
  CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) 'T'
            sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) INTO lv_datetime.
  lo_document->create_simple_element( name    = 'CreDtTm'
                                      parent  = lo_grphdr
                                      value   = lv_datetime ).

*** ---> Begin of CR-002
*  CLEAR lv_datetime.
*  CONCATENATE i_valut(4) '-' i_valut+4(2) '-' i_valut+6(2)
*              INTO lv_datetime.
*
*  lo_document->create_simple_element( name    = 'ValueDate'
*                                      parent  = lo_grphdr
*                                      value   = lv_datetime ).
***  <--- End of CR-002


  lo_document->create_simple_element( name    = 'NbOfTxs'
                                       parent  = lo_grphdr
                                       value   = '1' ).

  CLEAR lv_wrbtrx.
  lv_wrbtrx = l_wrbtr.
  CONDENSE lv_wrbtrx NO-GAPS.

  lo_document->create_simple_element( name    = 'CtrlSum'
                                      parent  = lo_grphdr
                                      value   = lv_wrbtrx ).


  lo_initgpty = lo_document->create_simple_element(
                                      name    = 'InitgPty'
                                      parent  = lo_grphdr ).

  CLEAR lv_value.
  lv_value = g_dtadit20-tag03.
  lo_initqptynm = lo_document->create_simple_element(
                                      name    = 'Nm'
                                      parent  = lo_initgpty
                                      value = lv_value ).


  lo_initqptyid = lo_document->create_simple_element(
                                      name    = 'Id'
                                      parent  = lo_initgpty ).


  lo_initqptyorgid = lo_document->create_simple_element(
                                      name    = 'OrgId'
                                      parent  = lo_initqptyid ).

  lo_initqptyothr = lo_document->create_simple_element(
                                    name    = 'Othr'
                                    parent  = lo_initqptyorgid ).
  CLEAR lv_value.
  lv_value = lv_dtkid.

  lo_document->create_simple_element( name    = 'Id'
                                     parent  = lo_initqptyothr
                                     value   = lv_value ).

  lo_document->create_simple_element( name    = 'Issr'
                                    parent  = lo_initqptyothr
                                    value   = 'CBI' ).

* Create Item
  lo_pmtinf = lo_document->create_simple_element(
                                        name    = 'PmtInf'
                                        parent  = lo_payment_req ).

  lo_document->create_simple_element( name    = 'PmtInfId'
                                      parent  = lo_pmtinf
                                      value   = lv_next ).


  lo_document->create_simple_element( name    = 'PmtMtd'
                                      parent  = lo_pmtinf
                                      value   = 'TRF' ).

  DATA lv_zero TYPE c VALUE 0.
  lv_value = lv_zero.
  lo_document->create_simple_element( name    = 'BtchBookg'
                                      parent  = lo_pmtinf
                                      value   = lv_value ).

** ---> Begin of CR-002
  CLEAR lv_datetime.
  CONCATENATE i_valut(4) '-' i_valut+4(2) '-' i_valut+6(2)
              INTO lv_datetime.

  lo_document->create_simple_element( name    = 'ReqdExctnDt'
                                      parent  = lo_pmtinf
                                      value = lv_datetime ).


**  <--- End of CR-002
*  lo_document->create_simple_element( name    = 'ReqdExctnDt'
*                                    parent  = lo_pmtinf
*                                    value   = lv_budatx ).

  lo_dbtr = lo_document->create_simple_element( name    = 'Dbtr'
                                     parent  = lo_pmtinf ).

  CLEAR lv_value.
  lv_value = g_dtadit20-tag03.
  lo_document->create_simple_element( name    = 'Nm'
                                    parent  = lo_dbtr
                                    value   = lv_value  ).

  lo_pstladr =  lo_document->create_simple_element( name    = 'PstlAdr'
                                        parent  = lo_dbtr ).

  lo_document->create_simple_element( name    = 'Ctry'
                                    parent  = lo_pstladr
                                    value   = lv_land1c ).

  DATA(lo_iddbtr) =  lo_document->create_simple_element( name    = 'Id'
                                         parent  = lo_dbtr ).


  DATA(lo_orgiddbtr) =   lo_document->create_simple_element( name    = 'OrgId'
                                    parent  = lo_iddbtr
                                     ).

  DATA(lo_othrdbtr) =   lo_document->create_simple_element( name    = 'Othr'
                                    parent  = lo_orgiddbtr
                                    ).


  CLEAR lv_value.
  lv_value = lv_dtkid.
  lo_document->create_simple_element( name    = 'Id'
                                     parent  = lo_othrdbtr
                                     value   = lv_value ).


  lo_document->create_simple_element( name    = 'Issr'
                                  parent  = lo_othrdbtr
                                  value   = 'CBI' ).


  SELECT SINGLE land1
    FROM t001
    INTO @DATA(lv_ctryofres)
    WHERE bukrs = @i_bukrs.

  DATA(lo_ctryofres) =  lo_document->create_simple_element( name = 'CtryOfRes'
                                        parent  = lo_dbtr ") .
                                        value = CONV string( lv_ctryofres ) ).

  lo_dbtracct = lo_document->create_simple_element( name    = 'DbtrAcct'
                                      parent  = lo_pmtinf ).


  lo_dbtrid = lo_document->create_simple_element( name    = 'Id'
                                        parent  =   lo_dbtracct ).

  CLEAR lv_value.
  lv_value = lv_iban.
  lo_document->create_simple_element( name    = 'IBAN'
                                    parent  = lo_dbtrid
                                    value   = lv_value ).


  lo_dbtragt =  lo_document->create_simple_element( name    = 'DbtrAgt'
                                    parent  = lo_pmtinf ).

  lo_fininstid =  lo_document->create_simple_element( name    = 'FinInstnId'
                                    parent  = lo_dbtragt ).



  CLEAR lv_swift.

  DATA gs_bnka TYPE ty_bnka.

  PERFORM get_bnka USING    i_bankl
                   CHANGING gs_bnka
                            lv_banks.


  PERFORM get_swift2 USING i_bukrs
                           i_hbkid
                  CHANGING lv_swift.

  lo_document->create_simple_element( name    = 'BIC'
                                     parent  = lo_fininstid
                                     value   = CONV #( lv_swift ) ).


  lo_clrsysmmbid = lo_document->create_simple_element( name    = 'ClrSysMmbId'
                                      parent  = lo_fininstid  ).

  CLEAR lv_value.
  lv_value = lv_banknr(5).
  lo_document->create_simple_element( name    = 'MmbId'
                                      parent  = lo_clrsysmmbid
                                      value   = lv_value ).


  lo_document->create_simple_element( name    = 'ChrgBr'
                                      parent  = lo_pmtinf
                                      value   = 'SHAR' ).

  lo_cdttrftxinf =  lo_document->create_simple_element( name    = 'CdtTrfTxInf'
                                        parent  = lo_pmtinf ).

  lo_cdtid = lo_document->create_simple_element( name    = 'PmtId'
                                        parent  = lo_cdttrftxinf ).

  CLEAR lv_value.
  CONCATENATE i_bukrs i_belnr i_gjahr INTO lv_value.

  lo_document->create_simple_element( name    = 'InstrId'
                                    parent  = lo_cdtid
                                    value   = lv_value ).

  CLEAR lv_value.
  lv_value = i_belnr.

  lo_document->create_simple_element( name    = 'EndToEndId'
                                      parent  = lo_cdtid
                                      value   =  lv_value ).

  lo_cdtpmtinf  = lo_document->create_simple_element( name    = 'PmtTpInf'
                                        parent  = lo_cdttrftxinf ).


  lo_ctgypurp = lo_document->create_simple_element( name    = 'CtgyPurp'
                                        parent  = lo_cdtpmtinf ).

  lo_document->create_simple_element( name    = 'Cd'
                                      parent  = lo_ctgypurp
                                      value   =  'CASH' ).



  lo_amt =  lo_document->create_simple_element( name    = 'Amt'
                                      parent  = lo_cdttrftxinf ).


  CONDENSE lv_wrbtrx NO-GAPS.
  lo_instdamt = lo_document->create_simple_element( name    = 'InstdAmt'
                                     parent  = lo_amt
                                     value   =  lv_wrbtrx ).

  SELECT SINGLE waers INTO @DATA(lv_waers)
    FROM bkpf
    WHERE bukrs = @i_bukrs
    AND waers = @i_waers.


  lo_instdamt->set_attribute( name  = 'Ccy'
                              value = CONV #( lv_waers )  ).

  lo_cdtragt = lo_document->create_simple_element( name    = 'CdtrAgt'
                                        parent  = lo_cdttrftxinf ).

  lo_fininstnidcdt_cdtragt = lo_document->create_simple_element( name    = 'FinInstnId'
                                        parent  = lo_cdtragt ).

  lo_document->create_simple_element( name    = 'BIC'
                                   parent  = lo_fininstnidcdt_cdtragt
                                   value   = CONV #( gs_bnka-bic ) ).


  DATA(lo_clrsysmmbid1) =  lo_document->create_simple_element( name    = 'ClrSysMmbId'
                                   parent  = lo_fininstnidcdt_cdtragt
                                    ).

  lo_document->create_simple_element( name = 'MmbId'
                                   parent = lo_clrsysmmbid1
                                   value =  CONV #( gs_bnka-mmbid ) ).




  lo_document->create_simple_element( name    = 'Nm'
                                  parent  = lo_fininstnidcdt_cdtragt
                                        value   = CONV #( gs_bnka-nm ) ).


  DATA(pstladr_clrsys) =  lo_document->create_simple_element( name    = 'PstlAdr'
                                   parent  = lo_fininstnidcdt_cdtragt
                                   ).

********* VQ

  lo_document->create_simple_element( name    = 'Ctry'
                                 parent  = pstladr_clrsys
                                 value = CONV #( gs_bnka-ctry ) ).



  lo_cdtr = lo_document->create_simple_element( name    = 'Cdtr'
                                      parent  = lo_cdttrftxinf ).

  CLEAR lv_value.
*  lv_value = l_dtadit30-tag03.
  lv_value = lv_lifnr_name1.
  lo_document->create_simple_element( name    = 'Nm'
                                      parent  = lo_cdtr
                                      value   =  lv_value ).

  lo_cdtr_pstladr =   lo_document->create_simple_element( name    = 'PstlAdr'
                                      parent  = lo_cdtr ).


  CLEAR lv_value .
  CONCATENATE l_dtadit40-tag06 l_dtadit40-tag05 INTO lv_value.
  CONDENSE lv_value NO-GAPS.
  lo_document->create_simple_element( name    = 'TwnNm'
                                  parent  =  lo_cdtr_pstladr
                                  value   = lv_value ).


  lo_document->create_simple_element( name    = 'Ctry'
                                    parent  =  lo_cdtr_pstladr
                                    value   = lv_land1 ).



  lo_cdtracct =  lo_document->create_simple_element( name    = 'CdtrAcct'
                                     parent  = lo_cdttrftxinf ).

  lo_cdtracctid  = lo_document->create_simple_element( name    = 'Id'
                                     parent  = lo_cdtracct ).


  PERFORM check_insert_id_or_iban USING i_lifnr
                                        i_bankl
                                        i_koinh "ddm 35389
                               CHANGING lv_iban_t
                                        lv_bankn
                                        lv_insert_type.

  CASE lv_insert_type.

    WHEN 'IBAN'.

      lo_document->create_simple_element( name    = 'IBAN'
                                      parent  =  lo_cdtracctid
                                      value   = CONV #( lv_iban_t ) ).

    WHEN 'Id'.

      DATA(lo_other_cdtracc_id)  = lo_document->create_simple_element( name    = 'Othr'
                                                                      parent  = lo_cdtracctid ).

      lo_document->create_simple_element( name    = 'Id'
                                      parent  =  lo_other_cdtracc_id
                                      value   = CONV #( i_koinh ) ).
  ENDCASE.






  PERFORM fill_dtadit40_xml USING    i_lifnr
                                   i_agkoa
                                   i_bankl          "BANCA BENEFICIARIO
                                   i_koinh                  "20090327
                                   i_belnr   ""MK ADD TC13487A
                                   i_bukrs   ""MK ADD TC13487A
                                   i_gjahr   ""MK ADD TC13487A
                          CHANGING l_dtadit40
                                   lv_land1
                                   gs_tiban  ""MK ADD TC13487A
                                   gv_empfb. ""MK ADD TC13487A

  CLEAR lv_value .
**********MK ADD TC13487A
  IF i_agkoa EQ 'K' AND i_zlsch = '$' AND gv_empfb IS NOT INITIAL.
    IF gs_tiban IS NOT INITIAL.
      lv_value = gs_tiban-iban.
    ENDIF.
  ELSE.
**********MK ADD TC13487A
    lv_value = i_koinh.
  ENDIF.


  lo_rmtinf = lo_document->create_simple_element( name    = 'RmtInf'
                                    parent  = lo_cdttrftxinf ).

  PERFORM get_paym_txt USING i_bukrs          "CR-003
                             i_belnr
                             i_gjahr
                    CHANGING lv_paym_txt_str.


  CLEAR lv_value .
* CONCATENATE '/' 'INV' '/' sy-datum+4(2) '.' sy-datum+2(2) "CR-001
* '.' * sy-datum(4)  INTO lv_value.                         "CR-001

  MOVE i_xblnr TO lv_value.                                 "CR-001
  IF lv_paym_txt_str IS NOT INITIAL.                        "CR-003
    lv_value = lv_paym_txt_str.                         "CR-003
  ENDIF.                                                    "CR-003

  lo_document->create_simple_element( name    = 'Ustrd'
                                  parent  = lo_rmtinf
                                  value   = lv_value ).


*  PERFORM save_xml TABLES g_t_dta_record.

* Scarico dati
*****  PERFORM download TABLES g_t_dta_record.


**********************************************************************
*** Create Output Stream and Render **********************************
**********************************************************************
*
*  DATA:
*    lo_streamfactory TYPE REF TO if_ixml_stream_factory,
*    lo_ostream       TYPE REF TO if_ixml_ostream,
*    lo_renderer      TYPE REF TO if_ixml_renderer,
*    lv_rc            TYPE i,
*    lv_xml_size      TYPE i,
*    lo_encoding      TYPE REF TO if_ixml_encoding.
*
*  DATA lv_string TYPE xstring.
*  DATA : lt_file LIKE STANDARD TABLE OF lv_string.
*
*
*  DATA: lv_filename    TYPE string,
*        lv_filenameapp TYPE string,
*        lv_append      TYPE c,
*        lv_useraction  TYPE i,
*        lv_file        TYPE string.
*
*  CLEAR lv_file .
*  CONCATENATE sy-uname sy-uzeit INTO lv_file SEPARATED BY '_'.
*
**  CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
**    IMPORTING
**      fullpath    = lv_filename
**      user_action = lv_useraction.
*
**    IF lv_useraction = '2'.
**      lv_append = 'X'.
**    ELSE.
**      CLEAR lv_append.
**    ENDIF.
*
*  CHECK lo_ixml IS NOT INITIAL. "DS@DB01
*  CLEAR lv_filenameapp.
*  CONCATENATE '.' lv_filename INTO lv_filenameapp .
*
** Create Stream Factory
*  lo_streamfactory = lo_ixml->create_stream_factory( ).
*
*  CHECK lo_streamfactory IS NOT INITIAL. "DS@DB01
*
**Create Output Stream
*  lo_ostream  = lo_streamfactory->create_ostream_uri( system_id = lv_file ).
*
**Craete renderer
*  lo_renderer = lo_ixml->create_renderer( ostream  = lo_ostream
*                                          document = lo_document ).
*
*  CHECK lo_renderer IS NOT INITIAL. "DS@DB01
**Set Pretty Print
*
*  CHECK lo_ostream IS NOT INITIAL. "DS@DB01
*  lo_ostream->set_pretty_print( 'X' ).
*
** Render
*  lv_rc = lo_renderer->render( ).
*
**lo_ostream  = lo_streamfactory->create_istream_uri( system_id = lv_filenameapp ). "DS@DB01
*
*  CHECK lo_ostream IS NOT INITIAL.
**    lo_encoding = lo_ostream->get_encoding( ).        "DS@DB01
*
*
**    CLEAR lv_string.
**    lv_string = lo_encoding->get_character_set( ).
**
**    cl_salv_data_services=>download_xml_to_file(
**    filename = lv_filename
**    xcontent = lv_string ).
*
*
**Get XML file size
*
*
*data: s_encoding_type type string.
*  lo_streamfactory = lo_ixml->create_stream_factory( ).
*  s_encoding_type = 'utf-8'.
*  lo_encoding = lo_ixml->create_encoding( character_set = s_encoding_type
*                                    byte_order = 0 ).
*
*  lo_ostream =
*    lo_streamfactory->create_ostream_xstring( b_xml ).
*
* call method ostream->set_encoding
*        exporting  encoding = encoding.
*
*  call method document->render
*       exporting ostream = ostream
*                 recursive = 'X'.
*
*  lv_xml_size = lo_ostream->get_num_written_raw( ).
*
*
*  CLOSE DATASET lv_file.
*
*  COMMIT WORK AND WAIT.
*
*  CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
*    IMPORTING
*      fullpath    = lv_filename
*      user_action = lv_useraction.
*  IF lv_useraction NE 9. "Abort
*    IF lv_useraction = '2'.
*      lv_append = 'X'.
*    ELSE.
*      CLEAR lv_append.
*    ENDIF.
*
*    DATA : lv_targ TYPE sapb-sappfad.
*    DATA : lv_file_set TYPE sapb-sappfad.
*
*    CLEAR lv_targ .
*    lv_targ = lv_filename.
*
*    CLEAR lv_file_set.
*    lv_file_set = lv_file.
*
*
*    CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
*      EXPORTING
*        path             = lv_file_set
*        targetpath       = lv_targ
*      EXCEPTIONS
*        error_file       = 1
*        no_authorization = 2
*        OTHERS           = 3.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*
*
*    DELETE DATASET lv_file.
*
*  ENDIF.
*
*
*
  DATA: lo_streamfactory TYPE REF TO if_ixml_stream_factory,
        lo_ostream       TYPE REF TO if_ixml_ostream,
        lo_renderer      TYPE REF TO if_ixml_renderer,
        lv_rc            TYPE i,
        lv_xml_size      TYPE i,
        lo_encoding      TYPE REF TO if_ixml_encoding.

  DATA lv_string TYPE xstring.
  DATA: lt_file LIKE STANDARD TABLE OF lv_string.

  DATA: lv_filename    TYPE string,
        lv_filenameapp TYPE string,
        lv_append      TYPE c,
        lv_useraction  TYPE i,
        lv_file        TYPE string.

  CLEAR lv_file.
  CONCATENATE sy-uname sy-uzeit INTO lv_file SEPARATED BY '_'.

  CHECK lo_ixml IS NOT INITIAL.
  CLEAR lv_filenameapp.
  CONCATENATE '.' lv_filename INTO lv_filenameapp.

* Create Stream Factory
  lo_streamfactory = lo_ixml->create_stream_factory( ).

  CHECK lo_streamfactory IS NOT INITIAL.

* Create Output Stream
  lo_ostream = lo_streamfactory->create_ostream_uri( system_id = lv_file ).

* Set encoding to UTF-8
  lo_encoding = lo_ixml->create_encoding( character_set = 'UTF-8'
                                          byte_order = 0 ).
  lo_ostream->set_encoding( lo_encoding ).

* Create renderer
  lo_renderer = lo_ixml->create_renderer( ostream  = lo_ostream
                                          document = lo_document ).

  CHECK lo_renderer IS NOT INITIAL.

* Set Pretty Print
  CHECK lo_ostream IS NOT INITIAL.
  lo_ostream->set_pretty_print( 'X' ).

* Render
  lv_rc = lo_renderer->render( ).

* Get XML file size
  lv_xml_size = lo_ostream->get_num_written_raw( ).

  CLOSE DATASET lv_file.

  COMMIT WORK AND WAIT.

  CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
    IMPORTING
      fullpath    = lv_filename
      user_action = lv_useraction.

  IF lv_useraction NE 9. "Abort
    IF lv_useraction = '2'.
      lv_append = 'X'.
    ELSE.
      CLEAR lv_append.
    ENDIF.

    DATA: lv_targ     TYPE sapb-sappfad,
          lv_file_set TYPE sapb-sappfad.

    CLEAR lv_targ.
    lv_targ = lv_filename.

    CLEAR lv_file_set.
    lv_file_set = lv_file.

    CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
      EXPORTING
        path             = lv_file_set
        targetpath       = lv_targ
      EXCEPTIONS
        error_file       = 1
        no_authorization = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    DELETE DATASET lv_file.
  ENDIF.










ENDFUNCTION.



FM 2
FUNCTION z_fi_create_xml.
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  IMPORTING
*"     REFERENCE(I_IT) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(I_VALUT) TYPE  VALUT
*"     REFERENCE(I_WRBTR) TYPE  WRBTR
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_HBKID) TYPE  HBKID
*"     REFERENCE(I_LIFNR) TYPE  LIFNR
*"     REFERENCE(I_AGKOA) TYPE  KOART
*"     REFERENCE(I_HKONT) TYPE  HKONT
*"     REFERENCE(I_ZLSCH) TYPE  SCHZW_BSEG
*"     REFERENCE(I_XBLNR) TYPE  XBLNR
*"     REFERENCE(I_BELNR) LIKE  BKPF-BELNR
*"     REFERENCE(I_GJAHR) LIKE  BKPF-GJAHR
*"     REFERENCE(I_BANKL) LIKE  BNKA-BANKL
*"     REFERENCE(I_CAUSA) TYPE  CHAR5
*"     REFERENCE(P_BELNR) LIKE  BKPF-BELNR
*"     REFERENCE(I_KOINH) TYPE  KOINH_FI
*"     REFERENCE(I_WAERS) TYPE  WAERS
*"----------------------------------------------------------------------
*      CR-001   08.03.2016
*               Ticket 8000024271 - Set reference data into <Ustrd>
*"----------------------------------------------------------------------
*      CR-002   31.10.2016  - Set field data valuta in xml header
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------
*&  Author:       Claudiu Bodan         (Crystal System)
*&  Requested by: Anna Iacobelli
*&  Date:         12.06.2019
*&  Description:  Ticket 7217A  Add Paym_text in XML - tcodes ZFI011/ZFI014                            *
*&  Modif ID   CR-003
*"----------------------------------------------------------------------
  DATA: lo_ixml TYPE REF TO if_ixml.
  DATA: lo_document TYPE REF TO if_ixml_document.
  DATA: lr_parser TYPE REF TO if_ixml_parser.
  DATA: lo_payment_req  TYPE REF TO if_ixml_element.
  DATA: lo_grphdr TYPE REF TO if_ixml_element.
  DATA: lo_pmtinf TYPE REF TO if_ixml_element.
  DATA: lo_initgpty TYPE REF TO if_ixml_element.
  DATA: lo_id TYPE REF TO if_ixml_element.
  DATA: lo_othr TYPE REF TO if_ixml_element.
  DATA: lo_orgid TYPE REF TO if_ixml_element.
  DATA: lo_pmttpinf TYPE REF TO if_ixml_element.
  DATA: lo_svclvl TYPE REF TO if_ixml_element.
  DATA: lo_dbtr TYPE REF TO if_ixml_element.
  DATA: lo_pstladr TYPE REF TO if_ixml_element.
  DATA: lo_dbtracct TYPE REF TO if_ixml_element.
  DATA: lo_dbtrid TYPE REF TO if_ixml_element.
  DATA: lo_pmtid TYPE REF TO if_ixml_element.
  DATA: lo_dbtragt TYPE REF TO if_ixml_element.
  DATA: lo_fininstid TYPE REF TO if_ixml_element.
  DATA: lo_clrsysmmbid TYPE REF TO if_ixml_element.
  DATA: lo_cdttrftxinf TYPE REF TO if_ixml_element.
  DATA: lo_cdtid TYPE REF TO if_ixml_element.
  DATA: lo_cdtpmtinf TYPE REF TO if_ixml_element.
  DATA: lo_ctgypurp TYPE REF TO if_ixml_element.
  DATA: lo_amt TYPE REF TO if_ixml_element.
  DATA : lo_instdamt TYPE REF TO if_ixml_element.
  DATA: lo_cdtr TYPE REF TO if_ixml_element.
  DATA: lo_cdtr_pstladr TYPE REF TO if_ixml_element.
  DATA: lo_cdtracct TYPE REF TO if_ixml_element.
  DATA: lo_cdtracctid TYPE REF TO if_ixml_element.
  DATA: lo_rmtinf TYPE REF TO if_ixml_element.
  DATA: lo_initqptynm TYPE REF TO if_ixml_element.
  DATA: lo_initqptyid TYPE REF TO if_ixml_element.
  DATA: lo_initqptyorgid TYPE REF TO if_ixml_element.
  DATA: lo_initqptyothr TYPE REF TO if_ixml_element.
  DATA: lo_cdtragt TYPE REF TO if_ixml_element.
  DATA: lo_fininstnidcdt_cdtragt TYPE REF TO if_ixml_element.

  DATA: l_dtadithr LIKE dtadithr,
        l_dtadit10 LIKE dtadit10,
        l_dtadit17 LIKE g_dtadit17,
        l_dtadit20 LIKE dtadit20,
        l_dtadit30 LIKE zfi_dtadit30,
        l_dtadit40 LIKE dtadit40,
        l_dtadit50 LIKE dtadit50,
        l_dtadit70 LIKE dtadit70,
        l_dtaditef LIKE dtaditef.

  DATA ok_ita.

  DATA l_wrbtr LIKE i_wrbtr.

  DATA lv_paym_txt_str TYPE string.

  CLEAR: dta_zaehler,
         dta_reccnt,
         cnt_formulare,
         cnt_hinweise,
         sum_abschluss,
         sum_regut,
         g_t_dta_record.

*****MK ADD TC13487A
  DATA: gs_tiban TYPE tiban.
  DATA: gv_empfb TYPE bseg-empfb.
*****MK ADD TC13487A


  REFRESH g_t_dta_record.

* Bonifico italia / estero
  PERFORM set_ita USING    i_agkoa
                           i_lifnr
                           i_belnr
                           i_gjahr
                           i_bukrs
                  CHANGING ok_ita
                           g_waers.

* Importo documento
  PERFORM read_wrbtr USING    i_bukrs
                              i_gjahr
                              i_belnr
                              i_wrbtr
                              i_waers  "LC AD14294 D02K909544
                     CHANGING l_wrbtr.

*******DS@DB01
*initial info
***  PERFORM fill_dta_record_xml USING '<?xml version="1.0" encoding="UTF-8"?>'.
***  PERFORM fill_dta_record_xml USING
***        '<CBIPaymentRequest xmlns="urn:CBI:xsd:CBIPaymentRequest.00.04.00">'.
*******DS@DB01


  DATA: lv_datetime     TYPE string,
        lv_wrbtrx       TYPE string,
        lv_dtkid        TYPE dtkid,
        lv_budat        TYPE budat,
        lv_budatx       TYPE string,
        lv_ort01        TYPE string,
        lv_land1c       TYPE string,
        lv_land1        TYPE string,
        lv_iban         TYPE iban,
        lv_value        TYPE string,
        lv_banknr       TYPE bankl,
        lv_bank_account TYPE bankk,
        lv_bank_country TYPE banks,
        lv_swift        TYPE swift,
        lv_uzeitx       TYPE char8,
        lv_next         TYPE string,
        lv_hdr1         TYPE string, ""char41.
        lv_hdr2         TYPE string,
        lv_hdr3         TYPE string,
        lv_hdr4         TYPE string,
        lv_hdr5         TYPE string,
        lv_hdr6         TYPE string,
        lv_hdr7         TYPE string,
        lv_hdr8         TYPE string.

* header section

  PERFORM get_next_nr USING i_bukrs
                      CHANGING lv_next.



***DATA: lo_grphdr TYPE REF TO if_ixml_element.
***DATA: lo_pmtinf TYPE REF TO if_ixml_element.
***DATA: lo_initgpty TYPE REF TO if_ixml_element.
***DATA: lo_id TYPE REF TO if_ixml_element.
***DATA: lo_othr TYPE REF TO if_ixml_element.
***DATA: lo_orgid TYPE REF TO if_ixml_element.
***DATA: lo_pmttpinf TYPE REF TO if_ixml_element.
***DATA: lo_svclvl TYPE REF TO if_ixml_element.
***DATA: lo_dbtr TYPE REF TO if_ixml_element.
***DATA: lo_pstladr TYPE REF TO if_ixml_element.
***DATA: lo_dbtracct TYPE REF TO if_ixml_element.
***DATA: lo_pmtid TYPE REF TO if_ixml_element.
***DATA: lo_dbtragt TYPE REF TO if_ixml_element.
***DATA: lo_fininstid TYPE REF TO if_ixml_element.
***DATA: lo_clrsysmmbid TYPE REF TO if_ixml_element.




*lo_initqpty =  lo_document->create_simple_element(
*                                      name    = 'InitgPty'
*                                      parent  = lo_grphdr ).

*  PERFORM fill_dta_record_xml USING '   <GrpHdr>'.
*  concatenate '       <MsgId>' lv_next '</MsgId>' into lv_hdr1.
*  PERFORM fill_dta_record_xml USING lv_hdr1.

*  clear lv_hdr1.
*  CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) 'T'
*              sy-uzeit INTO lv_datetime.
*  CONCATENATE '       <CreDtTm>' lv_datetime '</CreDtTm>'
*  INTO lv_hdr1.
*  PERFORM fill_dta_record_xml USING lv_hdr1.
*
*  MOVE '       <NbOfTxs>1</NbOfTxs>' TO lv_hdr2.
*  PERFORM fill_dta_record_xml USING lv_hdr2.

  lv_wrbtrx = l_wrbtr.
*  CONCATENATE '       <CtrlSum>' lv_wrbtrx '</CtrlSum>'
*  INTO lv_hdr3.
*  PERFORM fill_dta_record_xml USING lv_hdr3.

* company data
*  PERFORM fill_dtadit20 USING    i_bukrs
*                        CHANGING g_dtadit20.
*  PERFORM fill_dta_record_xml USING '       <InitgPty>'.
*  CONCATENATE '           <Nm>' g_dtadit20-tag03 '</Nm>'
*  INTO lv_hdr4.
*  PERFORM fill_dta_record_xml USING lv_hdr4.

* cuc nr
*  PERFORM fill_dta_record_xml USING '           <Id>'.
*  PERFORM fill_dta_record_xml USING '              <OrgId>'.
*  PERFORM fill_dta_record_xml USING '                 <Othr>'.
  CLEAR lv_dtkid.
  SELECT SINGLE dtkid INTO lv_dtkid
     FROM t012d WHERE bukrs = i_bukrs
                 AND hbkid = i_hbkid.
*  CONCATENATE '                    <Id>' lv_dtkid '</Id>'
*  INTO lv_hdr5.
  PERFORM fill_dta_record_xml USING lv_hdr5.
*
*  PERFORM fill_dta_record_xml USING '                    <Issr>CBI</Issr>'.
*  PERFORM fill_dta_record_xml USING '                 </Othr>'.
*  PERFORM fill_dta_record_xml USING '              </OrgId>'.
*  PERFORM fill_dta_record_xml USING '           </Id>'.
*  PERFORM fill_dta_record_xml USING '       </InitgPty>'.
*  PERFORM fill_dta_record_xml USING '   </GrpHdr>'.
*
*
** item section
*  PERFORM fill_dta_record_xml USING '   <Pmtinf>'.

*  concatenate '       <PmtInfId>' lv_next '</PmtInfId>' into lv_hdr1.
*  PERFORM fill_dta_record_xml USING lv_hdr1.
* PERFORM fill_dta_record_xml USING '       <PmtInfId>9000000001</PmtInfId>'.

*  PERFORM fill_dta_record_xml USING '       <PmtMtd>TRF</PmtMtd>'.
*  PERFORM fill_dta_record_xml USING '       <PmtTpInf>'.
*  PERFORM fill_dta_record_xml USING '          <SvcLvl>'.
*  PERFORM fill_dta_record_xml USING '             <Cd>SEPA</Cd>'.
*  PERFORM fill_dta_record_xml USING '          </SvcLvl>'.
*  PERFORM fill_dta_record_xml USING '       </PmtTpInf>'.

* posting date or payment date
  SELECT SINGLE budat INTO lv_budat
     FROM bkpf
             WHERE belnr = i_belnr
               AND bukrs = i_bukrs
               AND gjahr = i_gjahr.
  CONCATENATE lv_budat(4) '-' lv_budat+4(2) '-' lv_budat+6(2)
              INTO lv_budatx.
*  CONCATENATE '       <ReqdExctnDt>' lv_budatx '</ReqdExctnDt>'
*  INTO lv_hdr6.
  PERFORM fill_dta_record_xml USING lv_hdr6.

* company that makes the payment
*  PERFORM fill_dta_record_xml USING '       <Dbtr>'.
*  PERFORM fill_dta_record_xml USING lv_hdr4.
*  PERFORM fill_dta_record_xml USING '          <PstlAdr>'.
  PERFORM fill_dtadit20_xml USING i_bukrs
                         CHANGING g_dtadit20
                                  lv_ort01
                                  lv_land1c.
*  CONCATENATE '             <Ctry>' lv_land1c '</Ctry>'
*  INTO lv_hdr7.
  PERFORM fill_dta_record_xml USING lv_hdr7.
*  CONCATENATE '             <AdrLine>' lv_ort01 '</AdrLine>'
*  INTO lv_hdr7.
  PERFORM fill_dta_record_xml USING lv_hdr7.

*  PERFORM fill_dta_record_xml USING '          </PstlAdr>'.
*  PERFORM fill_dta_record_xml USING '       </Dbtr>'.





* Bonifico estero
  IF ok_ita = space.
    PERFORM fill_dtadithr_new USING    i_valut
                                       i_bukrs
                                       i_hbkid
                                       i_hkont
* PG - nome supporto
                                       i_belnr
                                       i_zlsch
                                       i_waers
* PG
                              CHANGING es_dtadithr.

**    PERFORM fill_dta_record USING es_dtadithr.

    PERFORM gestione_estero USING i_valut          "Valuta
                                  i_lifnr          "Fornitore/Cliente
                                  i_agkoa          "Tipo conto
                                  l_wrbtr
                                  i_bukrs          "Società
                                  i_hbkid          "Banca interna
                                  i_hkont          "CC banca interna
                                  i_belnr
                                  i_xblnr                   "RN:163643
                                  i_gjahr
                                  i_bankl
                                  i_causa
                                  g_waers
                                  p_belnr
                                  i_koinh
                                  i_zlsch.                  "20090327

    PERFORM fill_ef_estero USING l_wrbtr.          "i_wrbtr.

  ELSE.
*   Bonifico Italia
    PERFORM fill_dtadithr USING    i_valut          "Data Valuta
                                   i_bukrs
                                   'PC'
                                   i_hbkid          "Banca interna
                                   i_hkont
                                   i_zlsch
                                   i_waers
* PG - nome supporto
                                   i_belnr
* PG
                          CHANGING g_dtadithr.

**    PERFORM fill_dta_record USING g_dtadithr.

*   Ordine di trasf.
    PERFORM fill_dtadit10 USING    i_valut          "Data Valuta
                                   l_wrbtr
                                   i_lifnr          "Fornitore/Cliente
                                   i_agkoa          "Tipo conto
                                   i_bukrs          "Società
                                   i_hbkid          "Banca interna
                                   i_hkont          "CC banca interna
                                   i_bankl          "BANCA BENEFICIARIO
                                   i_koinh                  "20090327
                                   i_zlsch
                                   i_waers
                          CHANGING l_dtadit10.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit10.

*   Iban Beneficiario
    PERFORM fill_dtadit17 USING    i_hbkid
                                   i_hkont
                                   i_lifnr
                                   i_agkoa
                                   i_bukrs
                                   i_bankl
                                   i_koinh                  "20090327
                                   l_dtadit10
                          CHANGING l_dtadit17.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit17.

*   Committente
**    PERFORM fill_dtadit20 USING    i_bukrs
**                          CHANGING g_dtadit20.

*   Inserimento dati
**    PERFORM fill_dta_record USING g_dtadit20.

*   Dati destinat.
    PERFORM fill_dtadit30 USING    i_lifnr
                                   i_agkoa
                          CHANGING l_dtadit30.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit30.


*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit40.

*   Record 50/60 (rif. p. dest)
    PERFORM fill_dtadit50 USING    i_xblnr
                          CHANGING l_dtadit50.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit50.

*   Record 70 (rif. per banca)
    PERFORM fill_dtadit70 CHANGING l_dtadit70.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtadit70.
*   Record EF (rec. finale)
    PERFORM fill_dtaditef USING    g_dtadithr
                                   l_wrbtr
                          CHANGING l_dtaditef.

*   Inserimento dati
**    PERFORM fill_dta_record USING l_dtaditef.

  ENDIF.

*   Dati destin.
  PERFORM fill_dtadit40_xml USING    i_lifnr
                                 i_agkoa
                                 i_bankl          "BANCA BENEFICIARIO
                                 i_koinh                    "20090327
                                 i_belnr   ""MK ADD TC13487A
                                 i_bukrs   ""MK ADD TC13487A
                                 i_gjahr   ""MK ADD TC13487A
                        CHANGING l_dtadit40
                                 lv_land1
                                 gs_tiban  ""MK ADD TC13487A
                                 gv_empfb. ""MK ADD TC13487A

* iban new determination
  CLEAR lv_iban.
  PERFORM fill_dtadit17_xml USING    i_hbkid
                                 i_hkont
                                 i_lifnr
                                 i_agkoa
                                 i_bukrs
                                 i_bankl
                                 i_koinh                    "20090327
                                 i_zlsch
                                 i_waers
                                 l_dtadit10
                        CHANGING lv_iban.

*  PERFORM fill_dta_record_xml USING '       <DbtrAct>'.
*  PERFORM fill_dta_record_xml USING '          <Id>'.
*  CONCATENATE '              <IBAN>' lv_iban '</IBAN>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '          </Id>'.
*  PERFORM fill_dta_record_xml USING '       </DbtrAct>'.
*
*  PERFORM fill_dta_record_xml USING '       <DbtrAgt>'.
*  PERFORM fill_dta_record_xml USING '          <FinInstnId>'.
*  PERFORM fill_dta_record_xml USING '              <ClrSysMmbId>'.
*  CLEAR lv_hdr8.

*  PERFORM convert_iban USING lv_iban
*                       CHANGING lv_bank_account
*                                lv_bank_country
*                                lv_banknr.

*  CONCATENATE '                 <MmbId>' lv_iban '</MmbId>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '              </ClrSysMmbId>'.
*  PERFORM fill_dta_record_xml USING '          </FinInstnId>'.
*  PERFORM fill_dta_record_xml USING '       </DbtrAgt>'.
*
*  PERFORM fill_dta_record_xml USING '       <ChrgBr>SLEV</ChrgBr>'.
*
*  PERFORM fill_dta_record_xml USING '       <CdtTrfTxInf>'.
*
*  PERFORM fill_dta_record_xml USING '           <PmtId>'.
*  CLEAR lv_hdr8.
*  CONCATENATE '             <InstrId>' i_bukrs i_belnr i_gjahr '</InstrId>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*
*  CLEAR lv_hdr8.
*  CONCATENATE '             <EndToEndId>' i_belnr '</EndToEndId>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '           </PmtId>'.

*  PERFORM fill_dta_record_xml USING '           <PmtTpInf>'.
*  PERFORM fill_dta_record_xml USING '              <CtgyPurp>'.
*  PERFORM fill_dta_record_xml USING '                 <Cd>SUPP</Cd>'.
*  PERFORM fill_dta_record_xml USING '              </CtgyPurp>'.
*  PERFORM fill_dta_record_xml USING '           </PmtTpInf>'.
*
*  PERFORM fill_dta_record_xml USING '           <Amt>'.
*  CONCATENATE '              <InstdAmt Ccy="EUR">' lv_wrbtrx '</InstdAmt Ccy>'
*  INTO lv_hdr3.
*  PERFORM fill_dta_record_xml USING lv_hdr3.
*  PERFORM fill_dta_record_xml USING '           </Amt>'.
*
*  PERFORM fill_dta_record_xml USING '           <Cdtr>'.
*  CONCATENATE '              <Nm>' lv_iban '</Nm>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*
*  PERFORM fill_dta_record_xml USING '              <PstlAdr>'.
*  CLEAR lv_hdr8.
*  CONCATENATE '                  <Ctry>' lv_land1 '</Ctry>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  CLEAR lv_hdr8.
*  CONCATENATE '                  <AdrLine>' l_dtadit40-tag03 '</AdrLine>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  CLEAR lv_hdr8.
*  CONCATENATE '                  <AdrLine>' l_dtadit40-tag04 l_dtadit40-tag05
*  '</AdrLine>' INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '              </PstlAdr>'.
*  PERFORM fill_dta_record_xml USING '           </Cdtr>'.
*
*  PERFORM fill_dta_record_xml USING '           <CdtrAcct>'.
*  PERFORM fill_dta_record_xml USING '              <Id>'.

*  CONCATENATE '                 <IBAN>' i_koinh
*              '</IBAN>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.


*
*  CONCATENATE '                 <SWIFT>' lv_swift
*              '</SWIFT>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.


*  PERFORM fill_dta_record_xml USING '              </Id>'.
*  PERFORM fill_dta_record_xml USING '           </CdtrAcct>'.

*  PERFORM fill_dta_record_xml USING '           <RmtInf>'.
*  CLEAR lv_hdr8.

  WRITE sy-uzeit TO lv_uzeitx.
*  CONCATENATE '               <Ustrd>INV' lv_uzeitx '</Ustrd>'
*  INTO lv_hdr8.
*  PERFORM fill_dta_record_xml USING lv_hdr8.
*  PERFORM fill_dta_record_xml USING '           </RmtInf>'.
******
****** end items / end file
*  PERFORM fill_dta_record_xml USING '       </CdtTrfTxInf>'.
*  PERFORM fill_dta_record_xml USING '   </Pmtinf>'.
*  PERFORM fill_dta_record_xml USING '</CBIPaymentRequest>'.


* Create iXML object
  lo_ixml = cl_ixml=>create( ).

  lo_document = lo_ixml->create_document( ).


  lo_payment_req   = lo_document->create_simple_element(
                                      name    = 'CBIPaymentRequest'
                                      parent  = lo_document ).

  lo_payment_req->set_attribute( name  = 'xmlns'
                                 value = 'urn:CBI:xsd:CBIPaymentRequest.00.04.01' ).

* Create Header
  lo_grphdr = lo_document->create_simple_element( name    = 'GrpHdr'
                                      parent  = lo_payment_req ).

  lo_document->create_simple_element( name    = 'MsgId'
                                      parent  = lo_grphdr
                                      value   = lv_next ).

  CLEAR lv_datetime.
  CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) 'T'
            sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) INTO lv_datetime.
  lo_document->create_simple_element( name    = 'CreDtTm'
                                      parent  = lo_grphdr
                                      value   = lv_datetime ).

*** ---> Begin of CR-002
*  CLEAR lv_datetime.
*  CONCATENATE i_valut(4) '-' i_valut+4(2) '-' i_valut+6(2)
*              INTO lv_datetime.
*
*  lo_document->create_simple_element( name    = 'ValueDate'
*                                      parent  = lo_grphdr
*                                      value   = lv_datetime ).
***  <--- End of CR-002
  lo_document->create_simple_element( name    = 'NbOfTxs'
                                      parent  = lo_grphdr
                                      value   = '1' ).

  CLEAR lv_wrbtrx.
  lv_wrbtrx = l_wrbtr.

  lo_document->create_simple_element( name    = 'CtrlSum'
                                      parent  = lo_grphdr
                                      value   = lv_wrbtrx ).


  lo_initgpty = lo_document->create_simple_element(
                                      name    = 'InitgPty'
                                      parent  = lo_grphdr ).

  CLEAR lv_value.
  lv_value = g_dtadit20-tag03.
  lo_initqptynm = lo_document->create_simple_element(
                                      name    = 'Nm'
                                      parent  = lo_initgpty
                                      value = lv_value ).


  lo_initqptyid = lo_document->create_simple_element(
                                      name    = 'Id'
                                      parent  = lo_initgpty ).


  lo_initqptyorgid = lo_document->create_simple_element(
                                      name    = 'OrgId'
                                      parent  = lo_initqptyid ).

  lo_initqptyothr = lo_document->create_simple_element(
                                    name    = 'Othr'
                                    parent  = lo_initqptyorgid ).
  CLEAR lv_value.
  lv_value = lv_dtkid.

  lo_document->create_simple_element( name    = 'Id'
                                     parent  = lo_initqptyothr
                                     value   = lv_value ).

  lo_document->create_simple_element( name    = 'Issr'
                                    parent  = lo_initqptyothr
                                    value   = 'CBI' ).

* Create Item
  lo_pmtinf = lo_document->create_simple_element(
                                        name    = 'PmtInf'
                                        parent  = lo_payment_req ).

  lo_document->create_simple_element( name    = 'PmtInfId'
                                      parent  = lo_pmtinf
                                      value   = lv_next ).


  lo_document->create_simple_element( name    = 'PmtMtd'
                                      parent  = lo_pmtinf
                                      value   = 'TRF' ).

  lo_pmttpinf = lo_document->create_simple_element(
                                        name    = 'PmtTpInf'
                                        parent  = lo_pmtinf ).

  lo_svclvl = lo_document->create_simple_element(
                                          name    = 'SvcLvl'
                                          parent  = lo_pmttpinf ).

  lo_document->create_simple_element( name    = 'Cd'
                                      parent  = lo_svclvl
                                      value   = 'SEPA' ).

** ---> Begin of CR-002
  CLEAR lv_datetime.
  CONCATENATE i_valut(4) '-' i_valut+4(2) '-' i_valut+6(2)
              INTO lv_datetime.



    DATA(LO_REQDT) = lo_document->create_simple_element( NAME = 'ReqdExctnDt'
                                                    parent  = lo_pmtinf ).

  lo_document->create_simple_element( name    = 'Dt'
                                    parent  = LO_REQDT
                                    value   = lv_datetime  ).

**  <--- End of CR-002
*  lo_document->create_simple_element( name    = 'ReqdExctnDt'
*                                    parent  = lo_pmtinf
*                                    value   = lv_budatx ).

  lo_dbtr = lo_document->create_simple_element( name    = 'Dbtr'
                                     parent  = lo_pmtinf ).

  CLEAR lv_value.
  lv_value = g_dtadit20-tag03.
  lo_document->create_simple_element( name    = 'Nm'
                                    parent  = lo_dbtr
                                    value   = lv_value  ).

  lo_pstladr =  lo_document->create_simple_element( name    = 'PstlAdr'
                                        parent  = lo_dbtr ).

  lo_document->create_simple_element( name    = 'Ctry'
                                    parent  = lo_pstladr
                                    value   = lv_land1c ).

  lo_document->create_simple_element( name    = 'AdrLine'
                                    parent  = lo_pstladr
                                    value   = lv_ort01 ).

  lo_dbtracct = lo_document->create_simple_element( name    = 'DbtrAcct'
                                      parent  = lo_pmtinf ).

  lo_dbtrid = lo_document->create_simple_element( name    = 'Id'
                                        parent  =   lo_dbtracct ).

  CLEAR lv_value.
  lv_value = lv_iban.
  lo_document->create_simple_element( name    = 'IBAN'
                                    parent  = lo_dbtrid
                                    value   = lv_value ).

  lo_dbtragt =  lo_document->create_simple_element( name    = 'DbtrAgt'
                                    parent  = lo_pmtinf ).

  lo_fininstid =  lo_document->create_simple_element( name    = 'FinInstnId'
                                    parent  = lo_dbtragt ).

  lo_clrsysmmbid = lo_document->create_simple_element( name    = 'ClrSysMmbId'
                                      parent  = lo_fininstid  ).

  PERFORM convert_iban USING lv_iban
                       CHANGING lv_bank_account
                                lv_bank_country
                                lv_banknr.

*lv_land1c =  t001-land1
*lv_land1 = lfa1-land1
  CLEAR lv_swift.
  IF NOT lv_land1c = lv_land1.
    PERFORM get_swift USING lv_bank_account
                            lv_bank_country
                      CHANGING lv_swift.
  ENDIF.

  CLEAR lv_value.
  lv_value = lv_banknr(5).
  lo_document->create_simple_element( name    = 'MmbId'
                                      parent  = lo_clrsysmmbid
                                      value   = lv_value ).


  lo_document->create_simple_element( name    = 'ChrgBr'
                                      parent  = lo_pmtinf
                                      value   = 'SLEV' ).

  lo_cdttrftxinf =  lo_document->create_simple_element( name    = 'CdtTrfTxInf'
                                        parent  = lo_pmtinf ).

  lo_cdtid = lo_document->create_simple_element( name    = 'PmtId'
                                        parent  = lo_cdttrftxinf ).

  CLEAR lv_value.
  CONCATENATE i_bukrs i_belnr i_gjahr INTO lv_value.

  lo_document->create_simple_element( name    = 'InstrId'
                                    parent  = lo_cdtid
                                    value   = lv_value ).

  CLEAR lv_value.
  lv_value = i_belnr.

  lo_document->create_simple_element( name    = 'EndToEndId'
                                      parent  = lo_cdtid
                                      value   =  lv_value ).

  lo_cdtpmtinf  = lo_document->create_simple_element( name    = 'PmtTpInf'
                                        parent  = lo_cdttrftxinf ).

  lo_ctgypurp = lo_document->create_simple_element( name    = 'CtgyPurp'
                                        parent  = lo_cdtpmtinf ).

  lo_document->create_simple_element( name    = 'Cd'
                                      parent  = lo_ctgypurp
                                      value   =  'SUPP' ).

  lo_amt =  lo_document->create_simple_element( name    = 'Amt'
                                      parent  = lo_cdttrftxinf ).


  IF lv_swift IS NOT INITIAL.

    lo_cdtragt = lo_document->create_simple_element( name    = 'CdtrAgt'
                                          parent  = lo_cdttrftxinf ).

    lo_fininstnidcdt_cdtragt = lo_document->create_simple_element( name    = 'FinInstnId'
                                          parent  = lo_cdtragt ).

    CLEAR lv_value .
    lv_value = lv_swift.
    lo_document->create_simple_element( name    = 'BICIF'
                                     parent  = lo_fininstnidcdt_cdtragt
                                     value   = lv_value ).
  ENDIF.


  lo_instdamt = lo_document->create_simple_element( name    = 'InstdAmt'
                                       parent  = lo_amt
                                       value   =  lv_wrbtrx ).

  lo_instdamt->set_attribute( name  = 'Ccy'
                                   value = 'EUR' ).


  lo_cdtr = lo_document->create_simple_element( name    = 'Cdtr'
                                      parent  = lo_cdttrftxinf ).

  CLEAR lv_value.
*  lv_value = l_dtadit30-tag03.
  lv_value = lv_lifnr_name1.
  lo_document->create_simple_element( name    = 'Nm'
                                      parent  = lo_cdtr
                                      value   =  lv_value ).

  lo_cdtr_pstladr =   lo_document->create_simple_element( name    = 'PstlAdr'
                                      parent  = lo_cdtr ).


  lo_document->create_simple_element( name    = 'Ctry'
                                    parent  =  lo_cdtr_pstladr
                                    value   = lv_land1 ).



  CLEAR lv_value .
  lv_value = l_dtadit40-tag03. "lfa1-stras

  lo_document->create_simple_element( name    = 'AdrLine'
                                    parent  = lo_cdtr_pstladr
                                    value   = lv_value ).
  CLEAR lv_value .
  CONCATENATE l_dtadit40-tag06 l_dtadit40-tag05 INTO lv_value.
  CONDENSE lv_value NO-GAPS.

  lo_document->create_simple_element( name    = 'AdrLine'
                                  parent  = lo_cdtr_pstladr
                                  value   = lv_value ).


  lo_cdtracct =  lo_document->create_simple_element( name    = 'CdtrAcct'
                                     parent  = lo_cdttrftxinf ).

  lo_cdtracctid  = lo_document->create_simple_element( name    = 'Id'
                                     parent  = lo_cdtracct ).

  CLEAR lv_value .
**********MK ADD TC13487A
  IF i_agkoa EQ 'K' AND i_zlsch = '$' AND gv_empfb IS NOT INITIAL.
    IF gs_tiban IS NOT INITIAL.
      lv_value = gs_tiban-iban.
    ENDIF.
  ELSE.
**********MK ADD TC13487A
    lv_value = i_koinh.
  ENDIF.
  lo_document->create_simple_element( name    = 'IBAN'
                                  parent  = lo_cdtracctid
                                  value   = lv_value ).



  lo_rmtinf = lo_document->create_simple_element( name    = 'RmtInf'
                                    parent  = lo_cdttrftxinf ).

  PERFORM get_paym_txt USING i_bukrs          "CR-003
                             i_belnr
                             i_gjahr
                    CHANGING lv_paym_txt_str.
  CLEAR lv_value.


  CLEAR lv_value .
* CONCATENATE '/' 'INV' '/' sy-datum+4(2) '.' sy-datum+2(2) "CR-001
* '.' * sy-datum(4)  INTO lv_value.                         "CR-001

  MOVE i_xblnr TO lv_value.                                 "CR-001
  IF lv_paym_txt_str IS NOT INITIAL.                        "CR-003
    lv_value = lv_paym_txt_str.                         "CR-003
  ENDIF.                                                    "CR-003

  lo_document->create_simple_element( name    = 'Ustrd'
                                  parent  = lo_rmtinf
                                  value   = lv_value ).


*  PERFORM save_xml TABLES g_t_dta_record.

* Scarico dati
*****  PERFORM download TABLES g_t_dta_record.


**********************************************************************
*** Create Output Stream and Render **********************************
**********************************************************************

  DATA:
    lo_streamfactory TYPE REF TO if_ixml_stream_factory,
    lo_ostream       TYPE REF TO if_ixml_ostream,
    lo_renderer      TYPE REF TO if_ixml_renderer,
    lv_rc            TYPE i,
    lv_xml_size      TYPE i,
    lo_encoding      TYPE REF TO if_ixml_encoding.

  DATA lv_string TYPE xstring.
  DATA : lt_file LIKE STANDARD TABLE OF lv_string.


  DATA: lv_filename    TYPE string,
        lv_filenameapp TYPE string,
        lv_append      TYPE c,
        lv_useraction  TYPE i,
        lv_file        TYPE string.

  CLEAR lv_file .
  CONCATENATE sy-uname sy-uzeit INTO lv_file SEPARATED BY '_'.

*  CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
*    IMPORTING
*      fullpath    = lv_filename
*      user_action = lv_useraction.
*  IF lv_useraction NE 9. "Abort
*    IF lv_useraction = '2'.
*      lv_append = 'X'.
*    ELSE.
*      CLEAR lv_append.
*    ENDIF.

  CHECK lo_ixml IS NOT INITIAL. "DS@DB01
  CLEAR lv_filenameapp.
  CONCATENATE '.' lv_filename INTO lv_filenameapp .

* Create Stream Factory
  lo_streamfactory = lo_ixml->create_stream_factory( ).

  CHECK lo_streamfactory IS NOT INITIAL. "DS@DB01

*Create Output Stream
  lo_ostream  = lo_streamfactory->create_ostream_uri( system_id = lv_file ).

*Craete renderer
  lo_renderer = lo_ixml->create_renderer( ostream  = lo_ostream
                                          document = lo_document ).

  CHECK lo_renderer IS NOT INITIAL. "DS@DB01
*Set Pretty Print

  CHECK lo_ostream IS NOT INITIAL. "DS@DB01
  lo_ostream->set_pretty_print( 'X' ).

* Render
  lv_rc = lo_renderer->render( ).

*lo_ostream  = lo_streamfactory->create_istream_uri( system_id = lv_filenameapp ). "DS@DB01

  CHECK lo_ostream IS NOT INITIAL.
*    lo_encoding = lo_ostream->get_encoding( ).        "DS@DB01


*    CLEAR lv_string.
*    lv_string = lo_encoding->get_character_set( ).
*
*    cl_salv_data_services=>download_xml_to_file(
*    filename = lv_filename
*    xcontent = lv_string ).


*Get XML file size
  lv_xml_size = lo_ostream->get_num_written_raw( ).

  CLOSE DATASET lv_file.

  COMMIT WORK AND WAIT.

  CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
    IMPORTING
      fullpath    = lv_filename
      user_action = lv_useraction.
  IF lv_useraction NE 9. "Abort
    IF lv_useraction = '2'.
      lv_append = 'X'.
    ELSE.
      CLEAR lv_append.
    ENDIF.

    DATA : lv_targ TYPE sapb-sappfad.
    DATA : lv_file_set TYPE sapb-sappfad.

    CLEAR lv_targ .
    lv_targ = lv_filename.

    CLEAR lv_file_set.
    lv_file_set = lv_file.


    CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
      EXPORTING
        path             = lv_file_set
        targetpath       = lv_targ
      EXCEPTIONS
        error_file       = 1
        no_authorization = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    DELETE DATASET lv_file.

  ENDIF.


ENDFUNCTION.


PROGRAMI
*&---------------------------------------------------------------------*
*& Report  ZBANK_PAYMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zbank_payment.

TABLES: zpaym_file, epsf.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_txt  RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X', "Read txt response
            r_disp RADIOBUTTON GROUP rb. " Alv display
SELECTION-SCREEN END OF BLOCK b1.

*ALV display
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_bukrs FOR zpaym_file-bukrs MODIF ID bl2,
                 s_budat FOR zpaym_file-budat MODIF ID bl2,
*                 s_xblnr FOR zpaym_file-xblnr MODIF ID bl2,
                 s_usnam FOR zpaym_file-usnam MODIF ID bl2.

PARAMETERS: p_stat  TYPE zesito MODIF ID bl2,
            p_inbiz TYPE zstatus_inbiz MODIF ID bl2.

SELECTION-SCREEN END OF BLOCK b2.

*RFC parameters
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.

PARAMETERS : p_fdir TYPE eps2filnam DEFAULT '\\DSTEST.aquafil.com\InBiz_LINKSFG\Esiti' MODIF ID bl1.
SELECT-OPTIONS: s_mask FOR epsf-epsfilnam DEFAULT '*.txt' MODIF ID bl1.

SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
*       CLASS lcl_bank_payment DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bank_payment DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS: execute,
      set_first_display.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_alv_display,
              bukrs       TYPE bukrs,
              budat       TYPE budat,
*              xblnr       TYPE xblnr1,
              usnam       TYPE usnam,
              zlsch       TYPE schzw_bseg,
              dtkey       TYPE dtkey_d,
              wrbtr       TYPE wrbtr,
              waers       TYPE waers,
              esito       TYPE zesito, " status
              stat_light  TYPE icon_d, " status light
              stato_inbiz TYPE zstatus_inbiz, " esito inbiz
            END OF ty_alv_display,
            tt_alv_display TYPE STANDARD TABLE OF ty_alv_display.

    DATA: mt_alv_display      TYPE tt_alv_display,
          mo_alv_grid         TYPE REF TO cl_gui_alv_grid,
          mo_cust_grid        TYPE REF TO cl_gui_custom_container,
          mt_gridfcat         TYPE lvc_t_fcat,
          mt_update_paym_file TYPE STANDARD TABLE OF zpaym_file.

    METHODS: extract_data,
      get_fcat_grid,
      read_txt_file.

ENDCLASS.                    "

DATA go_bank_payment TYPE REF TO lcl_bank_payment.

DATA: BEGIN OF gs_screen100,
        ok_code TYPE syucomm,
      END OF gs_screen100.
*----------------------------------------------------------------------*
*       CLASS lcl_bank_payment IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bank_payment IMPLEMENTATION.

  METHOD execute.

    CASE abap_true.

      WHEN r_txt.

        IF p_fdir IS INITIAL OR s_mask IS INITIAL.
          MESSAGE 'Please fill obligatory fields!'(004) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
* read txt file from AL11
        read_txt_file( ).

      WHEN r_disp.
        extract_data( ).

    ENDCASE.

  ENDMETHOD.                    "execute

  METHOD extract_data.

    IF p_inbiz IS NOT INITIAL.
      DATA(lv_inbiz) = 'zpaym_file~stato_inbiz = @p_inbiz'.
    ENDIF.

    IF p_stat IS NOT INITIAL.
      DATA(lv_esito) = 'zpaym_msg~esito = @p_stat'.
    ENDIF.

    SELECT  zpaym_file~bukrs,
            zpaym_file~budat,
*            zpaym_file~xblnr,
            zpaym_file~usnam,
            zpaym_file~zlsch,
            zpaym_file~dtkey,
            zpaym_file~wrbtr,
            zpaym_file~waers,
            zpaym_msg~esito, " status
            zpaym_file~stato_inbiz "esito inbiz
       FROM zpaym_file
       JOIN zpaym_msg
       ON zpaym_file~stato_inbiz = zpaym_msg~stato_inbiz
       INTO CORRESPONDING FIELDS OF TABLE @mt_alv_display
       WHERE zpaym_file~bukrs IN @s_bukrs
       AND zpaym_file~budat IN @s_budat
*       AND zpaym_file~xblnr IN @s_xblnr
       AND zpaym_file~usnam IN @s_usnam
       AND (lv_esito)
       AND (lv_inbiz).

    IF mt_alv_display IS INITIAL.
      MESSAGE 'No data found!'(005) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CLEAR: lv_inbiz, lv_esito.

*    status light
    LOOP AT mt_alv_display ASSIGNING FIELD-SYMBOL(<ls_alv_display>).

      CASE <ls_alv_display>-esito.
        WHEN 'OK'.
          <ls_alv_display>-stat_light = icon_led_green.
        WHEN 'KO'.
          <ls_alv_display>-stat_light = icon_led_red.
      ENDCASE.
    ENDLOOP.

    set_first_display( ).

  ENDMETHOD.                    "extract_data
  METHOD read_txt_file.

    DATA : lv_dir       TYPE eps2filnam,
           lt_files     TYPE TABLE OF eps2fili,
           lv_line      TYPE string,
           lt_filetable TYPE TABLE OF string.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = p_fdir
*       file_mask              =
*      IMPORTING
*       dir_name               =
*       FILE_COUNTER           =
*       ERROR_COUNTER          =
      TABLES
        dir_list               = lt_files
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_files>) WHERE name IN s_mask.

      DATA(lv_filename) = p_fdir && '\' && <ls_files>-name.

      OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc = 0.
        DO.
          READ DATASET lv_filename INTO lv_line.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          APPEND lv_line TO lt_filetable.
        ENDDO.
      ENDIF.
      CLOSE DATASET lv_filename.

    ENDLOOP.

    READ TABLE lt_filetable ASSIGNING FIELD-SYMBOL(<ls_line>) INDEX 9.
    IF sy-subrc = 0.
      DATA(lv_status) = <ls_line>+12(1).
      DATA(lv_description) = <ls_line>+16.
    ENDIF.

* UPDATE THE STATUS AND DESCRIPTION OF THE SPECIFIC LINE - TABLE( ZPAYM_FILE )



  ENDMETHOD.

  METHOD set_first_display.

    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CONT' ).
    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = mo_cust_grid ).

    get_fcat_grid( ).

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = abap_true ).

    TRY.
        mo_alv_grid->set_table_for_first_display( EXPORTING is_layout       = ls_layout
                                                  CHANGING  it_outtab       = mt_alv_display
                                                            it_fieldcatalog = mt_gridfcat ).
        CALL SCREEN 100.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root TYPE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD get_fcat_grid.

    mt_gridfcat = VALUE #(
              ( fieldname = 'BUKRS'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'BUDAT'       ref_table = 'ZPAYM_FILE' )
*              ( fieldname = 'XBLNR'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'USNAM'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'ZLSCH'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'DTKEY'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'WRBTR'       ref_table = 'ZPAYM_FILE' )
              ( fieldname = 'WAERS'       ref_table = 'ZPAYM_FILE' )
*              ( fieldname = 'ESITO'       ref_table = 'ZPAYM_MSG'  coltext = 'Status' )
              ( fieldname = 'STAT_LIGHT'   coltext = 'Stato' )
              ( fieldname = 'STATO_INBIZ' ref_table = 'ZPAYM_FILE' coltext = 'Esito INBIZ' )
              ).
  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    CASE abap_true.

      WHEN r_txt.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 1.
          WHEN 'BL2'.
            screen-active = 0.
        ENDCASE.
      WHEN r_disp.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 0.
          WHEN 'BL2'.
            screen-active = 1.
        ENDCASE.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

START-OF-SELECTION.
  CREATE OBJECT go_bank_payment.
  go_bank_payment->execute( ).
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZPF_PAYMENT'.
  SET TITLEBAR 'ZTB_PAYMENT'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE gs_screen100-ok_code.
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
SCREEN 100

SCREEN 1000

PROCESS BEFORE OUTPUT.

MODULE %_INIT_PBO.

MODULE %_PBO_REPORT.

MODULE %_PF_STATUS.

MODULE %_S_BUKRS.

MODULE %_S_BUDAT.

MODULE %_S_USNAM.

MODULE %_S_MASK.

MODULE %_END_OF_PBO.

PROCESS AFTER INPUT.

  MODULE %_BACK AT EXIT-COMMAND.

  MODULE %_INIT_PAI.

CHAIN.
  FIELD R_TXT   .
  FIELD R_DISP  .
    MODULE %_RADIOBUTTON_GROUP_RB                            .
ENDCHAIN.


CHAIN.
  FIELD R_TXT   .
  FIELD R_DISP  .
    MODULE %_BLOCK_1000000.
ENDCHAIN.

CHAIN.
  FIELD  S_BUKRS-LOW.
  FIELD  S_BUKRS-HIGH.
  MODULE %_S_BUKRS.
ENDCHAIN.

CHAIN.
  FIELD  S_BUDAT-LOW.
  FIELD  S_BUDAT-HIGH.
  MODULE %_S_BUDAT.
ENDCHAIN.

CHAIN.
  FIELD  S_USNAM-LOW.
  FIELD  S_USNAM-HIGH.
  MODULE %_S_USNAM.
ENDCHAIN.

FIELD !P_STAT MODULE %_P_STAT .

FIELD !P_INBIZ MODULE %_P_INBIZ .


CHAIN.
  FIELD  S_BUKRS-LOW.
  FIELD  S_BUKRS-HIGH.
  FIELD  S_BUDAT-LOW.
  FIELD  S_BUDAT-HIGH.
  FIELD  S_USNAM-LOW.
  FIELD  S_USNAM-HIGH.
  FIELD P_STAT .
  FIELD P_INBIZ .
    MODULE %_BLOCK_1000004.
ENDCHAIN.

FIELD !P_FDIR MODULE %_P_FDIR .

CHAIN.
  FIELD  S_MASK-LOW.
  FIELD  S_MASK-HIGH.
  MODULE %_S_MASK.
ENDCHAIN.


CHAIN.
  FIELD P_FDIR .
  FIELD  S_MASK-LOW.
  FIELD  S_MASK-HIGH.
    MODULE %_BLOCK_1000011.
ENDCHAIN.

CHAIN.
  FIELD R_TXT   .
  FIELD R_DISP  .
  FIELD  S_BUKRS-LOW.
  FIELD  S_BUKRS-HIGH.
  FIELD  S_BUDAT-LOW.
  FIELD  S_BUDAT-HIGH.
  FIELD  S_USNAM-LOW.
  FIELD  S_USNAM-HIGH.
  FIELD P_STAT .
  FIELD P_INBIZ .
  FIELD P_FDIR .
  FIELD  S_MASK-LOW.
  FIELD  S_MASK-HIGH.
  MODULE %_END_OF_SCREEN.
  MODULE %_OK_CODE_1000.
ENDCHAIN.
