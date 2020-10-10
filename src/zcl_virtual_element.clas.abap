CLASS zcl_virtual_element DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.
ENDCLASS.

CLASS zcl_virtual_element IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

    IF iv_entity = 'ZCD_C_TRAVEL_P'.
      LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<fs_calc_element>).
        IF <fs_calc_element> = 'VIRTUALFIELD'.
          APPEND 'TOTALPRICE' TO et_requested_orig_elements.
        ENDIF.
      ENDLOOP.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA lt_original_data TYPE STANDARD TABLE OF zcd_c_travel_p WITH DEFAULT KEY.

    lt_original_data = CORRESPONDING #( it_original_data ).

    LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_data>).
      <fs_original_data>-VirtualField = <fs_original_data>-TotalPrice - ( <fs_original_data>-TotalPrice * ( 1 / 10 ) ).
    ENDLOOP.

    ct_calculated_data = CORRESPONDING #( lt_original_data ).

  ENDMETHOD.

ENDCLASS.
