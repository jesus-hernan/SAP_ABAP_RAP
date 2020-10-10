CLASS lhc_Booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS:
      calculateTotalFlightPrice FOR DETERMINE ON MODIFY IMPORTING keys FOR Booking~calculateTotalFlightPrice,
      validateStatus            FOR VALIDATE ON SAVE IMPORTING    keys FOR Booking~validateStatus,
      get_features              FOR FEATURES IMPORTING keys REQUEST requested_features FOR Booking  RESULT result. " Agregado

*   METHODS get_authorizations FOR AUTHORIZATION
*      IMPORTING keys REQUEST requested_authorizations FOR Booking RESULT result.

ENDCLASS.

CLASS lhc_Booking IMPLEMENTATION.
  METHOD calculateTotalFlightPrice .
    IF keys IS NOT INITIAL.
      zcl_travel_auxiliary_mng=>calculate_price(
      it_travel_id = VALUE #( FOR GROUPS <booking> OF booking_key IN keys
                              GROUP BY booking_key-travel_id WITHOUT MEMBERS ( <booking> ) ) ).
    ENDIF.

    " read changed data for result
*    READ ENTITIES OF zcd_i_travel_m IN LOCAL MODE
*     ENTITY travel
*       FIELDS ( agency_id customer_id begin_date end_date booking_fee
*                total_price currency_code overall_status description
*                created_by created_at last_changed_at last_changed_by )
*         WITH VALUE #( FOR key IN keys ( travel_id = key-travel_id ) )
*     RESULT DATA(lt_travel).
*
*    result = VALUE #( FOR travel IN lt_travel ( travel_id = travel-travel_id
*                                                %param    = travel ) ).

  ENDMETHOD.

  METHOD validateStatus.
    READ ENTITY zcd_i_travel_m\\Booking
    FIELDS ( booking_status )
    WITH VALUE #( FOR <root_key> IN keys ( %key = <root_key> ) )
    RESULT DATA(lt_booking_result).
    LOOP AT lt_booking_result INTO DATA(ls_booking_result).
      CASE ls_booking_result-booking_status.
        WHEN 'N'. " New
        WHEN 'X'. " Canceled
        WHEN 'B'. " Booked
        WHEN OTHERS.
          APPEND VALUE #( %key = ls_booking_result-%key ) TO failed-booking.
          APPEND VALUE #( %key = ls_booking_result-%key
                          %msg = new_message( id = /dmo/cx_flight_legacy=>status_is_not_valid-msgid
                                          number = /dmo/cx_flight_legacy=>status_is_not_valid-msgno
                                              v1 = ls_booking_result-booking_status
                                        severity = if_abap_behv_message=>severity-error )
                         %element-booking_status = if_abap_behv=>mk-on ) TO reported-booking.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  " Agregado
  METHOD get_features.
  ENDMETHOD.
  " Agregado

ENDCLASS.
