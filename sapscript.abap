Sapscript

REPORT zvq_script.


DATA:  gs_top TYPE zvq_tabb,
       gt_top TYPE zvq_tab1,
       gt_hd  TYPE zvq_thd,
       gs_hd  TYPE zvq_hd.

DATA: lv_ebeln TYPE ekko-ebeln,
      gv_count TYPE i.

CONSTANTS: lc_id       TYPE thead-tdid     VALUE 'F01',
           lc_langu    TYPE thead-tdspras  VALUE 'D',
           lc_tdobject TYPE thead-tdobject VALUE 'EKKO'.

DATA: lt_tlines  TYPE  TABLE OF tline,
      ls_theads  TYPE  stxh,
      lv_angebot TYPE string.

DATA lv_name     TYPE thead-tdname.
DATA ls_lines    TYPE tline.


SELECT-OPTIONS s_ebeln FOR lv_ebeln DEFAULT '4500004823' TO '4500004840'.

START-OF-SELECTION.
  "get data from address and order
  SELECT   ekko~aedat,
           ekko~zterm,
           ekko~verkf,
           ekko~telf1,
           ekko~ernam,
           ekko~waers,
           ekko~wkurs,
           ekko~lifnr,
           ekko~ebeln,
           ekko~bukrs,
           lfa1~name1,
           lfa1~ort01,
           lfa1~ort02,
           t001~butxt,
           t001~land1
     FROM ekko
     JOIN lfa1
       ON ekko~lifnr = lfa1~lifnr
     JOIN t001
       ON ekko~bukrs = t001~bukrs
     INTO TABLE @gt_top
     WHERE ekko~ebeln IN @s_ebeln
      ORDER BY ekko~ebeln ASCENDING.



  IF sy-subrc = 0.
    SELECT
         ekpo~ebeln,
         ekpo~ebelp,
         ekpo~txz01,
         makt~maktx,
         ekpo~matnr,
         ekpo~menge,
         ekpo~netpr,
*        ekpo~net_value,
         ekpo~zzxx_comment
       FROM ekpo
       LEFT JOIN makt
              ON ekpo~matnr = makt~matnr
             AND makt~spras = @sy-langu
       INTO CORRESPONDING FIELDS OF TABLE @gt_hd
      FOR ALL ENTRIES IN @gt_top
     WHERE ekpo~ebeln = @gt_top-ebeln.

    SORT gt_hd BY ebeln ebelp.
  ENDIF.

*****************************************************

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      form                        = 'ZVQ_SCRIPTS'
      language                    = sy-langu
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      codepage                    = 11
      OTHERS                      = 12.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

LOOP AT gt_top INTO gs_top .

CALL FUNCTION 'START_FORM'
    EXPORTING
      form        = 'ZVQ_SCRIPTS'
      language    = sy-langu
      startpage   = 'PAGE1'
    EXCEPTIONS
      form        = 1
      format      = 2
      unended     = 3
      unopened    = 4
      unused      = 5
      spool_error = 6
      codepage    = 7
      OTHERS      = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

IF sy-tabix > 1.
      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command   = 'NEW-PAGE PAGE1'
        EXCEPTIONS
          unopened  = 1
          unstarted = 2
          OTHERS    = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.


  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'ADDRESS'
*     FUNCTION                 = 'SET'
*     TYPE                     = 'BODY'
      window                   = 'ADDRESS'
* IMPORTING
*     PENDING_LINES            =
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'LOGO'
      window                   = 'LOGO'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'ORDER'
      window                   = 'ORDER'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  DATA  gv_total TYPE i.

    READ TABLE gt_hd TRANSPORTING NO FIELDS WITH KEY ebeln = gs_top-ebeln BINARY SEARCH.
    IF sy-subrc = 0.

      LOOP AT gt_hd INTO gs_hd FROM sy-tabix.

        IF gs_hd-ebeln <> gs_top-ebeln.
          EXIT.
        ENDIF.

        gs_hd-net_value = gs_hd-menge * gs_hd-netpr.
        gv_total = gv_total + gs_hd-net_value.
        MODIFY gt_hd FROM gs_hd.


        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element                  = 'MAIN'
            window                   = 'MAIN'
          EXCEPTIONS
            element                  = 1
            function                 = 2
            type                     = 3
            unopened                 = 4
            unstarted                = 5
            window                   = 6
            bad_pageformat_for_print = 7
            spool_error              = 8
            codepage                 = 9
            OTHERS                   = 10.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDLOOP.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'TOTAL'
          window                   = 'MAIN'
        EXCEPTIONS
          element                  = 1
          function                 = 2
          type                     = 3
          unopened                 = 4
          unstarted                = 5
          window                   = 6
          bad_pageformat_for_print = 7
          spool_error              = 8
          codepage                 = 9
          OTHERS                   = 10.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CLEAR gv_total.
    ENDIF.


    lv_name = gs_top-ebeln.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = lc_id
        language                = lc_langu
        name                    = lv_name
        object                  = lc_tdobject
      TABLES
        lines                   = lt_tlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT lt_tlines INTO ls_lines.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'READ'
          window                   = 'MAIN'
        EXCEPTIONS
          element                  = 1
          function                 = 2
          type                     = 3
          unopened                 = 4
          unstarted                = 5
          window                   = 6
          bad_pageformat_for_print = 7
          spool_error              = 8
          codepage                 = 9
          OTHERS                   = 10.
      IF sy-subrc <> 0.
        " Implement suitable error handling here
      ENDIF.

    ENDLOOP.
    CLEAR lt_tlines.
*

  CALL FUNCTION 'END_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      spool_error              = 3
      codepage                 = 4
      OTHERS                   = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  ENDLOOP.


  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      codepage                 = 5
      OTHERS                   = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
