Badi


    DATA: gt_set   TYPE STANDARD TABLE OF rgsb4, "Set Values
          gs_set   TYPE rgsb4,
          v_setid  TYPE sethier-setid,
          ra_range TYPE RANGE OF kna1-ktokd,
          r_range  LIKE LINE OF ra_range.


    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname                = 'ZVQ_ZEZIS_BP_WF_KTOKD' "External name of set (not the ID!)
      IMPORTING
        new_setid                = v_setid  "ID of set found
      EXCEPTIONS
        no_set_found             = 1
        no_set_picked_from_popup = 2
        wrong_class              = 3
        wrong_subclass           = 4
        table_field_not_found    = 5
        fields_dont_match        = 6
        set_is_empty             = 7
        formula_in_set           = 8
        set_is_dynamic           = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = v_setid
      TABLES
        set_values    = gt_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF sy-subrc EQ 0.
      IF gs_set-from EQ gs_set-to.
        LOOP AT gt_set INTO gs_set.
          MOVE 'I' TO r_range-sign.
          MOVE 'EQ' TO r_range-option.
          MOVE gs_set-from TO r_range-low.
          APPEND r_range TO ra_range.
        ENDLOOP.
      ENDIF.
    ENDIF.



    IF ( cs_customer-kna1-old_data-confs = 1 OR cs_customer-knb1-old_data-confs = 1 ) and  cs_customer-kna1-new_data-ktokd NOT IN ra_range.


      CLEAR :cs_customer-kna1-new_data-confs,
             cs_customer-kna1-new_data-updat,
             cs_customer-kna1-new_data-uptim,
             cs_customer-knb1-new_data-confs,
             cs_customer-knb1-new_data-updat,
             cs_customer-knb1-new_data-uptim.
             cs_customer-kna1-upd = abap_true.
             cs_customer-knb1-upd = abap_true.
    ENDIF.



*MERR VLERAT E SETIT
*KERKON NEPERMJET KNB1-BUKRS TEK SETI
*NESE EKZISTION=> CLEAR FUSHAT

*IF cs_customer-kna1-new_data-confs = 1

*******************************************************************
    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname                = 'ZEZIS_BP_WF_EXCL_BUKRS' "External name of set (not the ID!)
      IMPORTING
        new_setid                = v_setid  "ID of set found
      EXCEPTIONS
        no_set_found             = 1
        no_set_picked_from_popup = 2
        wrong_class              = 3
        wrong_subclass           = 4
        table_field_not_found    = 5
        fields_dont_match        = 6
        set_is_empty             = 7
        formula_in_set           = 8
        set_is_dynamic           = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = v_setid
      TABLES
        set_values    = gt_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF sy-subrc EQ 0.
      IF gs_set-from EQ gs_set-to.
        LOOP AT gt_set INTO gs_set.
          MOVE 'I' TO r_range-sign.
          MOVE 'EQ' TO r_range-option.
          MOVE gs_set-from TO r_range-low.
          APPEND r_range TO ra_range.
        ENDLOOP.
      ENDIF.
    ENDIF.



    IF cs_customer-kna1-old_data-confs = 1 AND cs_customer-knb1-old_data-bukrs IN ra_range.
      CLEAR : cs_customer-knb1-new_data-confs,
              cs_customer-knb1-new_data-updat,
              cs_customer-knb1-new_data-uptim.
              cs_customer-knb1-upd = abap_true.
    ENDIF.


—-----------
Vendor

    DATA: gt_set   TYPE STANDARD TABLE OF rgsb4, "Set Values
          gs_set   TYPE rgsb4,
          v_setid  TYPE sethier-setid,
          ra_range TYPE RANGE OF kna1-ktokd,
          r_range  LIKE LINE OF ra_range.


    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname                = 'ZEZIS_BP_WF_KTOKK' "External name of set (not the ID!)
      IMPORTING
        new_setid                = v_setid  "ID of set found
      EXCEPTIONS
        no_set_found             = 1
        no_set_picked_from_popup = 2
        wrong_class              = 3
        wrong_subclass           = 4
        table_field_not_found    = 5
        fields_dont_match        = 6
        set_is_empty             = 7
        formula_in_set           = 8
        set_is_dynamic           = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = v_setid
      TABLES
        set_values    = gt_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF sy-subrc EQ 0.
      IF gs_set-from EQ gs_set-to.
        LOOP AT gt_set INTO gs_set.
          MOVE 'I' TO r_range-sign.
          MOVE 'EQ' TO r_range-option.
          MOVE gs_set-from TO r_range-low.
          APPEND r_range TO ra_range.
        ENDLOOP.
      ENDIF.
    ENDIF.
    IF ( cs_vendor-lfa1-old_data-confs = 1 OR cs_vendor-lfb1-old_data-confs = 1 ) AND cs_vendor-lfa1-old_data-ktokk NOT IN ra_range.
      CLEAR :cs_vendor-lfa1-new_data-confs,
             cs_vendor-lfa1-new_data-updat,
             cs_vendor-lfa1-new_data-uptim,
             cs_vendor-lfb1-new_data-confs,
             cs_vendor-lfb1-new_data-updat,
             cs_vendor-lfb1-new_data-uptim.
             cs_vendor-lfa1-upd = abap_true.
             cs_vendor-lfb1-upd = abap_true.
    ENDIF.

*************************************************************

    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname                = 'ZEZIS_BP_WF_EXCL_BUKRS' "External name of set (not the ID!)
      IMPORTING
        new_setid                = v_setid  "ID of set found
      EXCEPTIONS
        no_set_found             = 1
        no_set_picked_from_popup = 2
        wrong_class              = 3
        wrong_subclass           = 4
        table_field_not_found    = 5
        fields_dont_match        = 6
        set_is_empty             = 7
        formula_in_set           = 8
        set_is_dynamic           = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = v_setid
      TABLES
        set_values    = gt_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

      IF sy-subrc EQ 0.
      IF gs_set-from EQ gs_set-to.
        LOOP AT gt_set INTO gs_set.
          MOVE 'I' TO r_range-sign.
          MOVE 'EQ' TO r_range-option.
          MOVE gs_set-from TO r_range-low.
          APPEND r_range TO ra_range.
        ENDLOOP.
      ENDIF.
    ENDIF.

        IF cs_vendor-lfb1-old_data-confs = 1 AND cs_vendor-lfb1-old_data-bukrs IN ra_range.
      CLEAR : cs_vendor-lfb1-new_data-confs,
              cs_vendor-lfb1-new_data-updat,
              cs_vendor-lfb1-new_data-uptim.
              cs_vendor-lfb1-upd = abap_true.
    ENDIF.





  ENDMETHOD.
