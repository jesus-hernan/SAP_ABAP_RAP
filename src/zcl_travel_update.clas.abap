CLASS zcl_travel_update DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.
CLASS zcl_travel_update IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(lv_travel_id) = '00000011'. " Valid travel ID
    DATA(lv_description) = 'Changed Travel Agency'.
    DATA(lv_new_agency_id) = '070017'. " Valid agency ID
    " UPDATE travel data
    MODIFY ENTITY zcd_i_travel_m
    UPDATE FIELDS ( agency_id description )
    WITH VALUE #( ( travel_id = lv_travel_id
                    agency_id = lv_new_agency_id
                  description = lv_description ) )
    FAILED DATA(ls_failed)
    REPORTED DATA(ls_reported).
    " Read travel data from transactional buffer
    CLEAR: ls_reported, ls_failed.
    READ ENTITY zcd_i_travel_m
    FIELDS ( agency_id description )
    WITH VALUE #( ( travel_id = lv_travel_id ) )
    RESULT DATA(lt_received_travel_data)
    FAILED ls_failed.
    " Output result data on the console
    out->write( lt_received_travel_data ).
    " Persist changed travel data in the database
    COMMIT ENTITIES.
    " Check criteria of success
    IF sy-subrc = 0.
      out->write( 'Successful COMMIT!' ).
    ELSE.
      out->write( 'COMMIT failed!' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
