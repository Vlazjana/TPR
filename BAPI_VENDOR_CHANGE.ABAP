*&---------------------------------------------------------------------*
*& Report ZKD_BAPI_VENDOR_EDIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkd_bapi_vendor_edit.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN END OF BLOCK b1.
*----------------------------------------------------------------------*
*       CLASS lcl_vendor_edit DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_vendor_edit DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.
  PRIVATE SECTION.
    METHODS change_data.
ENDCLASS.
DATA go_vendor_edit TYPE REF TO lcl_vendor_edit.
*----------------------------------------------------------------------*
*       CLASS lcl_vendor_edit IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_vendor_edit IMPLEMENTATION.
  METHOD execute.
    change_data( ).
  ENDMETHOD.                    "execute
  METHOD change_data.
    DATA: lv_businesspartner TYPE bapibus1006_head-bpartner VALUE 'PG11-PED',
          lt_return          TYPE STANDARD TABLE OF bapiret2.
    DATA(ls_CENTRALDATA) = VALUE bapibus1006_central(
    searchterm1         = ' PG11-PED1'
    searchterm2         = ' PG11-PED1'
*    partnertype         =
*    authorizationgroup  =
*    partnerlanguage     =
*    partnerlanguageiso  =
*    dataorigintype      =
*    centralarchivingflag=
*    centralblock        =
*    title_key           =
*    contactallowance    =
*    partnerexternal     =
*    titleletter         =
*    notreleased         =
*    comm_type           =
*    print_mode          =
    ).
*    DATA(ls_CENTRALDATAPERSON) = VALUE bapibus1006_central_person(
*firstname               = 'deda'
*lastname                = 'deda'
*birthname               =
*middlename              =
*secondname              =
*title_aca1              =
*title_aca2              =
*title_sppl              =
*prefix1                 =
*prefix2                 =
*nickname                =
*initials                =
*nameformat              =
*namcountry              =
*namcountryiso           =
*sex                     =
*birthplace              =
*birthdate               =
*deathdate               =
*maritalstatus           =
*correspondlanguage      =
*correspondlanguageiso   =
*fullname                =
*employer                =
*occupation              =
*nationality             =
*nationalityiso          =
*countryorigin           =
*birthdt_status          =
*gender                  =
*    ).

    DATA(ls_CENTRALDATAORGANIZATION) = VALUE bapibus1006_central_organ(
    name1            = 'deda'
    name2            = 'deda'
*    name3            =
*    name4            =
*    legalform        =
*    industrysector   =
*    foundationdate   =
*    liquidationdate  =
*    loc_no_1         =
*    loc_no_2         =
*    chk_digit        =
*    legalorg         =
    ).
*
*    DATA(ls_CENTRALDATAGROUP) = VALUE bapibus1006_central_group(
*    namegroup1 =
*    namegroup2 =
*    grouptype  =
*
*     ).
    DATA(ls_CENTRALDATA_X) = VALUE bapibus1006_central_x(
    searchterm1          = abap_true
    searchterm2          = abap_true
*    partnertype          =
*    authorizationgroup   =
*    partnerlanguage      =
*    partnerlanguageiso   =
*    dataorigintype       =
*    centralarchivingflag =
*    centralblock         =
*    title_key            =
*    contactallowance     =
*    partnerexternal      =
*    titleletter          =
*    notreleased          =
*    comm_type            =
*    print_mode           =
    ).
*    DATA(ls_CENTRALDATAPERSON_X) = VALUE bapibus1006_central_person_x(
*    firstname            = abap_true
*    lastname             = abap_true
*    birthname            =
*    middlename           =
*    secondname           =
*    title_aca1           =
*    title_aca2           =
*    title_sppl           =
*    prefix1              =
*    prefix2              =
*    nickname             =
*    initials             =
*    nameformat           =
*    namcountry           =
*    namcountryiso        =
*    sex                  =
*    birthplace           =
*    birthdate            =
*    deathdate            =
*    maritalstatus        =
*    correspondlanguage   =
*    correspondlanguageiso =
*    fullname             =
*    employer             =
*    occupation           =
*    nationality          =
*    nationalityiso       =
*    countryorigin        =
*    birthdt_status       =
*    gender               =
*    ).
    DATA(ls_CENTRALDATAORGANIZATION_X) = VALUE bapibus1006_central_organ_x(
    name1           = abap_true
    name2           = abap_true
*    name3           =
*    name4           =
*    legalform       =
*    industrysector  =
*    foundationdate  =
*    liquidationdate =
*    loc_no_1        =
*    loc_no_2        =
*    chk_digit       =
*    legalorg        =
    ).
*
*    DATA(ls_CENTRALDATAGROUP_X) = VALUE bapibus1006_central_group_x(
*    namegroup1 =
*    namegroup2 =
*    grouptype  =
*    ).
    CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
      EXPORTING
        businesspartner           = lv_businesspartner
        centraldata               = ls_CENTRALDATA
*       centraldataperson         = ls_CENTRALDATAPERSON
        centraldataorganization   = ls_CENTRALDATAORGANIZATION
*       centraldatagroup          = ls_CENTRALDATAGROUP
        centraldata_x             = ls_CENTRALDATA_X
*       centraldataperson_x       = ls_CENTRALDATAPERSON_X
        centraldataorganization_x = ls_CENTRALDATAORGANIZATION_X
*       centraldatagroup_x        = ls_CENTRALDATAGROUP_X
*       VALID_DATE                = SY-DATLO
*       DUPLICATE_MESSAGE_TYPE    =
*       DUPLICATE_CHECK_ADDRESS   =
      TABLES
*       TELEFONDATANONADDRESS     =
*       FAXDATANONADDRESS         =
*       TELETEXDATANONADDRESS     =
*       TELEXDATANONADDRESS       =
*       E_MAILDATANONADDRESS      =
*       RMLADDRESSDATANONADDRESS  =
*       X400ADDRESSDATANONADDRESS =
*       RFCADDRESSDATANONADDRESS  =
*       PRTADDRESSDATANONADDRESS  =
*       SSFADDRESSDATANONADDRESS  =
*       URIADDRESSDATANONADDRESS  =
*       PAGADDRESSDATANONADDRESS  =
*       COMMUNICATIONNOTESNONADDRESS        =
*       COMMUNICATIONUSAGENONADDRESS        =
*       TELEFONDATANONADDRESSX    =
*       FAXDATANONADDRESSX        =
*       TELETEXDATANONADDRESSX    =
*       TELEXDATANONADDRESSX      =
*       E_MAILDATANONADDRESSX     =
*       RMLADDRESSDATANONADDRESSX =
*       X400ADDRESSDATANONADDRESSX          =
*       RFCADDRESSDATANONADDRESSX =
*       PRTADDRESSDATANONADDRESSX =
*       SSFADDRESSDATANONADDRESSX =
*       URIADDRESSDATANONADDRESSX =
*       PAGADDRESSDATANONADDRESSX =
*       COMMUNICATIONNOTESNONADDRESSX       =
*       COMMUNICATIONUSAGENONADDRESSX       =
        return                    = lt_return
*       ADDRESSDUPLICATES         =
      .
    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDMETHOD.                    "change_data
ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT go_vendor_edit.
  go_vendor_edit->execute( ).
