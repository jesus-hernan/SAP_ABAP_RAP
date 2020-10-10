CLASS lhc_Supplement DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS : calculateTotalSupplimPrice FOR DETERMINE ON MODIFY IMPORTING keys FOR Supplement~calculateTotalSupplimPrice.

ENDCLASS.

CLASS lhc_Supplement IMPLEMENTATION.

  METHOD calculateTotalSupplimPrice.
    IF keys IS NOT INITIAL.
      zcl_travel_auxiliary_mng=>calculate_price(
      it_travel_id = VALUE #( FOR GROUPS <booking_suppl> OF booksuppl_key IN keys
                              GROUP BY booksuppl_key-travel_id WITHOUT MEMBERS ( <booking_suppl> ) ) ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_save DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PUBLIC SECTION.
    CONSTANTS: create TYPE string VALUE 'C',
               update TYPE string VALUE 'U',
               delete TYPE string VALUE 'D'.
  PROTECTED SECTION.
    METHODS save_modified REDEFINITION.
ENDCLASS.

CLASS lcl_save IMPLEMENTATION.
  METHOD save_modified.

    DATA: lt_supplements TYPE STANDARD TABLE OF ztb_booksuppl_m,
          lv_op_type     TYPE zde_flag,
          lv_updated     TYPE zde_flag.

    IF NOT create-supplement IS INITIAL.
      lt_supplements = CORRESPONDING #( create-supplement ).
      lv_op_type = lcl_save=>create.
    ENDIF.

    IF NOT update-supplement IS INITIAL.
      lt_supplements = CORRESPONDING #( update-supplement ).
      lv_op_type = lcl_save=>update.
    ENDIF.

    IF NOT delete-supplement IS INITIAL.
      lt_supplements = CORRESPONDING #( delete-supplement ).
      lv_op_type = lcl_save=>delete.
    ENDIF.

    IF NOT lt_supplements IS INITIAL.

      CALL FUNCTION 'ZFM_UNMANAGED'
        EXPORTING
          values     = lt_supplements
          operation  = lv_op_type
        IMPORTING
          ev_updated = lv_updated.

      IF lv_updated EQ abap_true.
*       reported-supplement
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
