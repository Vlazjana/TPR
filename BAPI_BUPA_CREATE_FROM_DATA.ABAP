*&---------------------------------------------------------------------*
*& Report ZED_HIERARCHY_TREE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvq_test1.

DATA: lv_businesspartner TYPE bapibus1006_head-bpartner,
      ls_centralperson   TYPE bapibus1006_central_person,
      lv_partnercategory TYPE bapibus1006_head-partn_cat,
      lv_partnergroup    TYPE bapibus1006_head-partn_grp,
      ls_centraldata     TYPE bapibus1006_central,
      ls_centralorg      TYPE bapibus1006_central_organ,
      ls_addressdata     TYPE bapibus1006_address,
      lt_return          TYPE TABLE OF bapiret2,
      ls_return          TYPE bapiret2.


* Set partner category (1 = Person, 2 = Organization, 3 = Group)
lv_partnercategory = '1'.
lv_partnergroup = 'ZBPL'.

* Central data
ls_centraldata = VALUE #(
  searchterm1    = 'JOHNDOE'
  searchterm2    = 'TESTBP'
).

" Organization data (example, not used for person)
*ls_centralorg = VALUE #(
* name1            = 'ABC Corporation'
* name2            = 'Technology Division'
* name3            = 'Software Development'
* name4            = 'ERP Solutions'
* foundationdate   = '20000101'
* liquidationdate  = '00000000'
* legalform        = 'AG'
* legalorg         = '0001'
* industrysector   = '0001'
*).

ls_addressdata = VALUE #(
  standardaddress = 'X'
  country         = 'US'
  city            = 'New York'
  postl_cod1      = '10001'       " City postal code (street)
  street          = 'Main Street'
  house_no        = '123'
).


" Person-specific data
ls_centralperson = VALUE #(
  firstname           = 'John'
  lastname            = 'Doe'
  middlename          = 'Michael'
  birthdate           = '19850515'
  birthplace          = 'New York'
  sex                 = '1'
  maritalstatus       = '1'
  nationality         = 'US'
  correspondlanguage  = 'EN'
).


CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
  EXPORTING
*   BUSINESSPARTNEREXTERN              =
    partnercategory                    = lv_partnercategory
    PARTNERGROUP                       = lv_partnergroup
    centraldata                        = ls_centraldata
   CENTRALDATAPERSON                   = ls_centralperson
*   CENTRALDATAORGANIZATION             = ls_centralorg
*   CENTRALDATAGROUP                   =
   ADDRESSDATA                         = ls_addressdata
*   DUPLICATE_MESSAGE_TYPE             =
*   ACCEPT_ERROR                       = ' '
*   ADDRESS_DEP_ATTR_DATA              =
 IMPORTING
   BUSINESSPARTNER                    =  lv_businesspartner
 TABLES
*   TELEFONDATA                        =
*   FAXDATA                            =
*   TELETEXDATA                        =
*   TELEXDATA                          =
*   E_MAILDATA                         =
*   RMLADDRESSDATA                     =
*   X400ADDRESSDATA                    =
*   RFCADDRESSDATA                     =
*   PRTADDRESSDATA                     =
*   SSFADDRESSDATA                     =
*   URIADDRESSDATA                     =
*   PAGADDRESSDATA                     =
*   ADDRESSNOTES                       =
*   COMMUNICATIONNOTES                 =
*   COMMUNICATIONUSAGE                 =
*   TELEFONDATANONADDRESS              =
*   FAXDATANONADDRESS                  =
*   TELETEXDATANONADDRESS              =
*   TELEXDATANONADDRESS                =
*   E_MAILDATANONADDRESS               =
*   RMLADDRESSDATANONADDRESS           =
*   X400ADDRESSDATANONADDRESS          =
*   RFCADDRESSDATANONADDRESS           =
*   PRTADDRESSDATANONADDRESS           =
*   SSFADDRESSDATANONADDRESS           =
*   URIADDRESSDATANONADDRESS           =
*   PAGADDRESSDATANONADDRESS           =
*   COMMUNICATIONNOTESNONADDRESS       =
*   COMMUNICATIONUSAGENONADDRESS       =
   RETURN                             = LT_RETURN
*   ADDRESSDUPLICATES                  =
          .



READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
IF sy-subrc = 0.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  WRITE: / lv_businesspartner.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDIF.




------------------------------------------------------
ORGANIZATION (2 )


REPORT zvq_test1.

DATA: lv_businesspartner TYPE bapibus1006_head-bpartner,
      ls_centralperson   TYPE bapibus1006_central_person,
      lv_partnercategory TYPE bapibus1006_head-partn_cat,
      lv_partnergroup    TYPE bapibus1006_head-partn_grp,
      ls_centraldata     TYPE bapibus1006_central,
      ls_centralorg      TYPE bapibus1006_central_organ,
      ls_addressdata     TYPE bapibus1006_address,
      lt_return          TYPE TABLE OF bapiret2,
      ls_return          TYPE bapiret2.


* Set partner category (1 = Person, 2 = Organization, 3 = Group)
lv_partnercategory = '2'.
lv_partnergroup = 'MMGR'.

* Central data
ls_centraldata = VALUE #(
  searchterm1    = 'JOHNDOE'
  searchterm2    = 'TESTBP'
).

" Organization data (example, not used for person)
ls_centralorg = VALUE #(
 name1            = 'ABC Corporation'
 name2            = 'Technology Division'
* name3            = 'Software Development'
* name4            = 'ERP Solutions'
* foundationdate   = '20000101'
* liquidationdate  = '00000000'
* legalform        = 'AG'
* legalorg         = '0001'
* industrysector   = '0001'
).

ls_addressdata = VALUE #(
  standardaddress = 'X'
  country         = 'US'
  city            = 'New York'
  postl_cod1      = '10001'       " City postal code (street)
  street          = 'Main Street'
  house_no        = '123'
).


*" Person-specific data
*ls_centralperson = VALUE #(
*  firstname           = 'John'
*  lastname            = 'Doe'
*  middlename          = 'Michael'
*  birthdate           = '19850515'
*  birthplace          = 'New York'
*  sex                 = '1'
*  maritalstatus       = '1'
*  nationality         = 'US'
*  correspondlanguage  = 'EN'
*).


CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
  EXPORTING
*   BUSINESSPARTNEREXTERN              =
    partnercategory                    = lv_partnercategory
    PARTNERGROUP                       = lv_partnergroup
    centraldata                        = ls_centraldata
*   CENTRALDATAPERSON                   = ls_centralperson
   CENTRALDATAORGANIZATION             = ls_centralorg
*   CENTRALDATAGROUP                   =
   ADDRESSDATA                         = ls_addressdata
*   DUPLICATE_MESSAGE_TYPE             =
*   ACCEPT_ERROR                       = ' '
*   ADDRESS_DEP_ATTR_DATA              =
 IMPORTING
   BUSINESSPARTNER                    =  lv_businesspartner
 TABLES
*   TELEFONDATA                        =
*   FAXDATA                            =
*   TELETEXDATA                        =
*   TELEXDATA                          =
*   E_MAILDATA                         =
*   RMLADDRESSDATA                     =
*   X400ADDRESSDATA                    =
*   RFCADDRESSDATA                     =
*   PRTADDRESSDATA                     =
*   SSFADDRESSDATA                     =
*   URIADDRESSDATA                     =
*   PAGADDRESSDATA                     =
*   ADDRESSNOTES                       =
*   COMMUNICATIONNOTES                 =
*   COMMUNICATIONUSAGE                 =
*   TELEFONDATANONADDRESS              =
*   FAXDATANONADDRESS                  =
*   TELETEXDATANONADDRESS              =
*   TELEXDATANONADDRESS                =
*   E_MAILDATANONADDRESS               =
*   RMLADDRESSDATANONADDRESS           =
*   X400ADDRESSDATANONADDRESS          =
*   RFCADDRESSDATANONADDRESS           =
*   PRTADDRESSDATANONADDRESS           =
*   SSFADDRESSDATANONADDRESS           =
*   URIADDRESSDATANONADDRESS           =
*   PAGADDRESSDATANONADDRESS           =
*   COMMUNICATIONNOTESNONADDRESS       =
*   COMMUNICATIONUSAGENONADDRESS       =
   RETURN                             = LT_RETURN
*   ADDRESSDUPLICATES                  =
          .

READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
IF sy-subrc = 0.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  WRITE: / lv_businesspartner.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDIF.


-----------------------
GROUP ( 3 )
REPORT zvq_test1.

DATA: lv_businesspartner TYPE bapibus1006_head-bpartner,
      ls_centralperson   TYPE bapibus1006_central_person,
      lv_partnercategory TYPE bapibus1006_head-partn_cat,
      lv_partnergroup    TYPE bapibus1006_head-partn_grp,
      ls_centraldata     TYPE bapibus1006_central,
      ls_centralorg      TYPE bapibus1006_central_organ,
      ls_centralgroup    TYPE   bapibus1006_central_group,
      ls_addressdata     TYPE bapibus1006_address,
      lt_return          TYPE TABLE OF bapiret2,
      ls_return          TYPE bapiret2.


* Set partner category (1 = Person, 2 = Organization, 3 = Group)
lv_partnercategory = '3'.
lv_partnergroup = '2222'.

* Central data
ls_centraldata = VALUE #(
  searchterm1    = 'JOHNDOE'
  searchterm2    = 'TESTBP'
).

" Organization data (example, not used for person)
ls_centralorg = VALUE #(
 name1            = 'ABC Corporation'
 name2            = 'Technology Division'
* name3            = 'Software Development'
* name4            = 'ERP Solutions'
* foundationdate   = '20000101'
* liquidationdate  = '00000000'
* legalform        = 'AG'
* legalorg         = '0001'
* industrysector   = '0001'
).

ls_addressdata = VALUE #(
  standardaddress = 'X'
  country         = 'US'
  city            = 'New York'
  postl_cod1      = '10001'       " City postal code (street)
  street          = 'Main Street'
  house_no        = '123'
).

ls_centralgroup = VALUE #(
  NAMEGROUP1 = 'ABC Group'
  NAMEGROUP2 = 'Technology Group'
  " Group fields only
).

*" Person-specific data
*ls_centralperson = VALUE #(
*  firstname           = 'John'
*  lastname            = 'Doe'
*  middlename          = 'Michael'
*  birthdate           = '19850515'
*  birthplace          = 'New York'
*  sex                 = '1'
*  maritalstatus       = '1'
*  nationality         = 'US'
*  correspondlanguage  = 'EN'
*).


CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
  EXPORTING
*   BUSINESSPARTNEREXTERN              =
    partnercategory                    = lv_partnercategory
    PARTNERGROUP                       = lv_partnergroup
    centraldata                        = ls_centraldata
*   CENTRALDATAPERSON                   = ls_centralperson
*   CENTRALDATAORGANIZATION             = ls_centralorg
   CENTRALDATAGROUP                   = ls_centralgroup
*   ADDRESSDATA                         = ls_addressdata
*   DUPLICATE_MESSAGE_TYPE             =
*   ACCEPT_ERROR                       = ' '
*   ADDRESS_DEP_ATTR_DATA              =
 IMPORTING
   BUSINESSPARTNER                    =  lv_businesspartner
 TABLES
*   TELEFONDATA                        =
*   FAXDATA                            =
*   TELETEXDATA                        =
*   TELEXDATA                          =
*   E_MAILDATA                         =
*   RMLADDRESSDATA                     =
*   X400ADDRESSDATA                    =
*   RFCADDRESSDATA                     =
*   PRTADDRESSDATA                     =
*   SSFADDRESSDATA                     =
*   URIADDRESSDATA                     =
*   PAGADDRESSDATA                     =
*   ADDRESSNOTES                       =
*   COMMUNICATIONNOTES                 =
*   COMMUNICATIONUSAGE                 =
*   TELEFONDATANONADDRESS              =
*   FAXDATANONADDRESS                  =
*   TELETEXDATANONADDRESS              =
*   TELEXDATANONADDRESS                =
*   E_MAILDATANONADDRESS               =
*   RMLADDRESSDATANONADDRESS           =
*   X400ADDRESSDATANONADDRESS          =
*   RFCADDRESSDATANONADDRESS           =
*   PRTADDRESSDATANONADDRESS           =
*   SSFADDRESSDATANONADDRESS           =
*   URIADDRESSDATANONADDRESS           =
*   PAGADDRESSDATANONADDRESS           =
*   COMMUNICATIONNOTESNONADDRESS       =
*   COMMUNICATIONUSAGENONADDRESS       =
   RETURN                             = LT_RETURN
*   ADDRESSDUPLICATES                  =
          .



READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
IF sy-subrc = 0.
  WRITE: / 'Error creating Business Partner:'.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-id, ls_return-number, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  WRITE: / 'Business Partner created successfully:', lv_businesspartner.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WRITE: / 'Transaction committed.'.
ENDIF."Error creating Business Partner:













READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
IF sy-subrc = 0.
  WRITE: / 'Error creating Business Partner:'.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-id, ls_return-number, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  WRITE: / 'Business Partner created successfully:', lv_businesspartner.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WRITE: / 'Transaction committed.'.
ENDIF."Error creating Business Partner:


-------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Report ZED_HIERARCHY_TREE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvq_test1.

DATA: lv_businesspartner TYPE bapibus1006_head-bpartner,
      ls_centralperson   TYPE bapibus1006_central_person,
      lv_partnercategory TYPE bapibus1006_head-partn_cat,
      lv_partnergroup    TYPE bapibus1006_head-partn_grp,
      ls_centraldata     TYPE bapibus1006_central,
      ls_centralorg      TYPE bapibus1006_central_organ,
      ls_centralgroup    TYPE   bapibus1006_central_group,
      ls_addressdata     TYPE bapibus1006_address,
      lt_return          TYPE TABLE OF bapiret2,
      lt_telefondata     TYPE TABLE OF bapiadtel,
      ls_return          TYPE bapiret2.

DATA : LS_BANKDETAIL TYPE BAPIBUS1006_BANKDETAIL.
DATA : LT_TEL  TYPE TABLE OF BAPIADTEL.
*REPORT zvq_test1.
*
*DATA: lv_businesspartner TYPE bapibus1006_head-bpartner,
*      ls_centralperson   TYPE bapibus1006_central_person,
*      lv_partnercategory TYPE bapibus1006_head-partn_cat,
*      lv_partnergroup    TYPE bapibus1006_head-partn_grp,
*      ls_centraldata     TYPE bapibus1006_central,
*      ls_centralorg      TYPE bapibus1006_central_organ,
*      ls_addressdata     TYPE bapibus1006_address,
*      lt_return          TYPE TABLE OF bapiret2,
*      ls_return          TYPE bapiret2.


* Set partner category (1 = Person, 2 = Organization, 3 = Group)
lv_partnercategory = '2'.
lv_partnergroup = 'MMGR'.

* Central data
ls_centraldata = VALUE #(
  searchterm1    = 'JOHNDOE'
  searchterm2    = 'TESTBP'
).

" Organization data (example, not used for person)
ls_centralorg = VALUE #(
 name1            = 'ABC Corporation'
 name2            = 'Technology Division'
* name3            = 'Software Development'
* name4            = 'ERP Solutions'
* foundationdate   = '20000101'
* liquidationdate  = '00000000'
* legalform        = 'AG'
* legalorg         = '0001'
* industrysector   = '0001'
).

ls_addressdata = VALUE #(
  standardaddress = 'X'
  country         = 'US'
  city            = 'New York'
  postl_cod1      = '10001'       " City postal code (street)
  street          = 'Main Street'
  house_no        = '123'
).


*" Person-specific data
*ls_centralperson = VALUE #(
*  firstname           = 'John'
*  lastname            = 'Doe'
*  middlename          = 'Michael'
*  birthdate           = '19850515'
*  birthplace          = 'New York'
*  sex                 = '1'
*  maritalstatus       = '1'
*  nationality         = 'US'
*  correspondlanguage  = 'EN'
*).
*lt_telefondata = VALUE #(
*  ( country     = 'US'           " Country code
*    countryiso  = 'US'
*    std_no      = 'X'            " Standard phone
*    telephone   = '2125550123'   " Phone number
*    extension   = '101'          " Extension
*    tel_no      = '1'            " Sequence #
*    caller_no   = 'OFFICE'
*    std_recip   = 'X'
*    r_3_user    = '1'            " 0=Landline
*    home_flag   = ''             " Blank=Business
*  )        " Communication #
*).

CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
  EXPORTING
*   BUSINESSPARTNEREXTERN              =
    partnercategory                    = lv_partnercategory
    PARTNERGROUP                       = lv_partnergroup
    centraldata                        = ls_centraldata
*   CENTRALDATAPERSON                   = ls_centralperson
   CENTRALDATAORGANIZATION             = ls_centralorg
*   CENTRALDATAGROUP                   =
   ADDRESSDATA                         = ls_addressdata
*   DUPLICATE_MESSAGE_TYPE             =
*   ACCEPT_ERROR                       = ' '
*   ADDRESS_DEP_ATTR_DATA              =
 IMPORTING
   BUSINESSPARTNER                    =  lv_businesspartner
 TABLES
*   TELEFONDATA                        = lt_telefondata
*   FAXDATA                            =
*   TELETEXDATA                        =
*   TELEXDATA                          =
*   E_MAILDATA                         =
*   RMLADDRESSDATA                     =
*   X400ADDRESSDATA                    =
*   RFCADDRESSDATA                     =
*   PRTADDRESSDATA                     =
*   SSFADDRESSDATA                     =
*   URIADDRESSDATA                     =
*   PAGADDRESSDATA                     =
*   ADDRESSNOTES                       =
*   COMMUNICATIONNOTES                 =
*   COMMUNICATIONUSAGE                 =
*   TELEFONDATANONADDRESS              =
*   FAXDATANONADDRESS                  =
*   TELETEXDATANONADDRESS              =
*   TELEXDATANONADDRESS                =
*   E_MAILDATANONADDRESS               =
*   RMLADDRESSDATANONADDRESS           =
*   X400ADDRESSDATANONADDRESS          =
*   RFCADDRESSDATANONADDRESS           =
*   PRTADDRESSDATANONADDRESS           =
*   SSFADDRESSDATANONADDRESS           =
*   URIADDRESSDATANONADDRESS           =
*   PAGADDRESSDATANONADDRESS           =
*   COMMUNICATIONNOTESNONADDRESS       =
*   COMMUNICATIONUSAGENONADDRESS       =
   RETURN                             = LT_RETURN
*   ADDRESSDUPLICATES                  =
          .



READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
IF sy-subrc = 0.
  WRITE: / 'Error creating Business Partner:'.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-id, ls_return-number, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  WRITE: / 'Business Partner created successfully:', lv_businesspartner.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WRITE: / 'Transaction committed.'.
ENDIF."Error creating Business Partner:

LS_BANKDETAIL = VALUE #(
BANK_CTRY = 'IN'
*BANK_CTRYISO
BANK_KEY = 'SBIN000'
BANK_ACCT = '1234567890'
CTRL_KEY = 'SB'
*BANK_REF
*ACCOUNTHOLDER
*COLL_AUTH
*EXTERNALBANKID
*BANKACCOUNTNAME
*IBAN
*IBAN_FROM_DATE
*BANKDETAILVALIDFROM
*BANKDETAILVALIDTO
*BANKDETAILMOVEDATE
*BANKDETAILMOVEID
*BANK_ACCOUNT_TYPE
*BP_BANK_GUID
*PAYMENT_SYSTEM
*ALIAS_TYPE
*BANK_ALIAS
 ).



CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
  EXPORTING
    businesspartner       = lv_businesspartner
*   BANKDETAILID          =
    bankdetaildata        = LS_BANKDETAIL
* IMPORTING
*   BANKDETAILIDOUT       =
 TABLES
   RETURN                 = lt_return
          .



READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
IF sy-subrc = 0.
  WRITE: / 'Error creating Business Partner:'.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-id, ls_return-number, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  WRITE: / 'Business Partner created successfully:', lv_businesspartner.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WRITE: / 'Transaction committed.'.
ENDIF."Error creating Business Partner:


LT_TEL = VALUE #( (

COUNTRY = 'IN'
*COUNTRYISO =
STD_NO = 'X'
TELEPHONE = '9865321478'
EXTENSION = ''
TEL_NO = '+919865321478'
CALLER_NO = '9865321478'
STD_RECIP = 'X'
R_3_USER = '3'
*HOME_FLAG
*CONSNUMBER
*ERRORFLAG
*FLG_NOUSE
VALID_FROM = ''
VALID_TO = ''

) ).

CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
  EXPORTING
    businesspartner              = lv_businesspartner
    addressdata                  = ls_addressdata
*   DUPLICATE_MESSAGE_TYPE       =
*   ACCEPT_ERROR                 = ' '
*   ADDRESS_DEP_ATTR_DATA        =
* IMPORTING
*   ADDRESSGUID                  =
 TABLES
   BAPIADTEL                    = LT_TEL
*   BAPIADFAX                    =
*   BAPIADTTX                    =
*   BAPIADTLX                    =
*   BAPIADSMTP                   =
*   BAPIADRML                    =
*   BAPIADX400                   =
*   BAPIADRFC                    =
*   BAPIADPRT                    =
*   BAPIADSSF                    =
*   BAPIADURI                    =
*   BAPIADPAG                    =
*   BAPIAD_REM                   =
*   BAPICOMREM                   =
*   ADDRESSUSAGE                 =
*   BAPIADUSE                    =
   RETURN                        = lt_return
*   ADDRESSDUPLICATES            =
          .





READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
IF sy-subrc = 0.
  WRITE: / 'Error creating Business Partner:'.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-id, ls_return-number, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  WRITE: / 'Business Partner created successfully:', lv_businesspartner.
  LOOP AT lt_return INTO ls_return.
    WRITE: / ls_return-type, ls_return-message.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WRITE: / 'Transaction committed.'.
ENDIF."Error creating Business Partner:




















*
*
*
*
*
*
*
*
*
**
**" Organization data (example, not used for person)
**ls_centralorg = VALUE #(
*** name1            = 'ABC Corporation'
*** name2            = 'Technology Division'
*** name3            = 'Software Development'
*** name4            = 'ERP Solutions'
*** searchterm1      = 'ABCCORP'
*** searchterm2      = 'ABC'
*** foundationdate   = '20000101'
*** liquidationdate  = '00000000'
*** legalform        = 'AG'
*** legalorg         = '0001'
*** industrysector   = '0001'
**).
**
**" Address data
**ls_addressdata = VALUE #(
**  standardaddress = 'X'
**  street          = 'Main Street'
**  house_no        = '123'
**  house_no2       = 'Apt 4B'
**  postl_cod1      = '10001'
**  postl_cod2      = '1234'
**  postl_cod3      = '10001'
**  po_box          = '5678'
**  city            = 'New York'
**  district        = 'Manhattan'
**  country         = 'US'
**  region          = 'NY'
**  time_zone       = 'EST'
**  langu           = 'EN'
**  building        = 'Tower A'
**  floor           = '15'
**  department      = 'IT'
**).
**
**" Telephone data
**lt_telefondata = VALUE #(
**  ( country    = 'US'
**    telephone = '2125551234'
**    extension = '100'
**    tel_no = '1'
**    caller_no = 'OFFICE'
**    std_no = 'X'
**    r_3_user = '0'
**    home_flag = ' '
**    consnumber = '001' )
**  ( country    = 'US'
**  telephone = '9175559876'
**  extension = ''
**  tel_no = '2'
**  caller_no = 'MOBILE'
**  std_no = ' '
**  r_3_user = '1'
**  home_flag = ' '
**  consnumber = '002' )
**).
**
**"---------------------------
**" Fax data
**"---------------------------
**lt_faxdata = VALUE #(
**  ( country = 'US'
**    fax = '2125555678'
**    extension = '200'
**    fax_no = '1'
**    sender_no = 'FAX1'
**    std_no = 'X'
**    consnumber = '001' )
**).
**
**"---------------------------
**" Email data
**"---------------------------
**lt_emaildata = VALUE #(
**  ( e_mail = 'john.doe@example.com' std_no = 'X' std_recip = 'X' home_flag = ' ' consnumber = '001' uri_type = 'INT' uri_addr = 'john.doe@example.com' )
**  ( e_mail = 'johndoe.personal@gmail.com' std_no = ' ' std_recip = ' ' home_flag = 'X' consnumber = '002' uri_type = 'INT' uri_addr = 'johndoe.personal@gmail.com' )
**).
**
**"---------------------------
**" Address notes
**"---------------------------
**lt_addressnotes = VALUE #(
**  ( langu = 'EN' langu_iso = 'EN' adr_notes = 'Delivery only between 9 AM and 5 PM' consnumber = '001' )
**).
*
*
*"---------------------------
*" Call BAPI to create Business Partner
*"---------------------------
*CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
*  EXPORTING
*    businesspartnerextern  = ''  " Internal numbering
*    partnercategory        = lv_partnercategory
*    partnergroup           = lv_partnergroup
*    centraldata            = ls_centraldata
*    centraldataperson      = ls_centralperson
**   centraldataorganization = ls_centralorg  " For organization
*    addressdata            = ls_addressdata
*    duplicate_message_type = 'E'
*    accept_error           = ' '
*  IMPORTING
*    businesspartner        = lv_businesspartner
*  TABLES
*    telefondata            = lt_telefondata
*    faxdata                = lt_faxdata
*    e_maildata             = lt_emaildata
*    addressnotes           = lt_addressnotes
*    return                 = lt_return.
*
*"---------------------------
*" Error check
*"---------------------------
*READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
*IF sy-subrc = 0.
*  WRITE: / 'Error creating Business Partner:'.
*  LOOP AT lt_return INTO ls_return.
*    WRITE: / ls_return-type, ls_return-id, ls_return-number, ls_return-message.
*  ENDLOOP.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*ELSE.
*  WRITE: / 'Business Partner created successfully:', lv_businesspartner.
*  LOOP AT lt_return INTO ls_return.
*    WRITE: / ls_return-type, ls_return-message.
*  ENDLOOP.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
*
*  WRITE: / 'Transaction committed.'.
*ENDIF.



