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


