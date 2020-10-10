CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      " Actions (Dynamic Feature Control)
      createTravelByTemplate FOR MODIFY IMPORTING keys FOR ACTION Travel~createTravelByTemplate RESULT result,
      acceptTravel           FOR MODIFY IMPORTING keys FOR ACTION Travel~acceptTravel           RESULT result,
      rejectTravel           FOR MODIFY IMPORTING keys FOR ACTION Travel~rejectTravel           RESULT result,
      get_features           FOR FEATURES IMPORTING keys REQUEST requested_features FOR Travel  RESULT result,
      " Fields (Dynamic Feature Control)
*      feature_ctrl          FOR FEATURES IMPORTING keys REQUEST requested_features FOR Travel RESULT result, No se permite multiples features.
      " Validations
      validateCustomer  FOR VALIDATE ON SAVE IMPORTING keys FOR Travel~validateCustomer,
      validateDates     FOR VALIDATE ON SAVE IMPORTING keys FOR Travel~validateDates,
      validateStatus    FOR VALIDATE ON SAVE IMPORTING keys FOR Travel~validateStatus,
      " Authorizations
      get_authorizations FOR AUTHORIZATION IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.
  " Unique
  METHOD createTravelByTemplate.
    SELECT MAX( travel_id ) FROM ztb_travel_m INTO @DATA(lv_travel_id).

    READ ENTITY zcd_i_travel_m
    FIELDS ( travel_id agency_id customer_id booking_fee total_price currency_code )
    WITH VALUE #( FOR travel IN keys ( %key = travel-%key ) )
    RESULT DATA(lt_read_result)
    FAILED failed
    REPORTED reported.

    DATA(lv_today) = cl_abap_context_info=>get_system_date( ).

    DATA lt_create TYPE TABLE FOR CREATE zcd_i_travel_m\\Travel.

    lt_create = VALUE #( FOR row IN lt_read_result INDEX INTO idx
    ( travel_id      = lv_travel_id + idx
      agency_id      = row-agency_id
      customer_id    = row-customer_id
      begin_date     = lv_today
      end_date       = lv_today + 30
      booking_fee    = row-booking_fee
      total_price    = row-total_price
      currency_code  = row-currency_code
      description    = 'Comments here'
      overall_status = 'O') ).

    MODIFY ENTITIES OF zcd_i_travel_m IN LOCAL MODE
    ENTITY travel
    CREATE FIELDS ( travel_id agency_id customer_id begin_date
                    end_date booking_fee total_price currency_code
                    description overall_status )
                    WITH lt_create
                    MAPPED mapped
                    FAILED failed
                    REPORTED reported.

    result = VALUE #( FOR create IN lt_create INDEX INTO idx
                    ( %cid_ref = keys[ idx ]-%cid_ref
                      %key     = keys[ idx ]-travel_id
                      %param   = CORRESPONDING #( create ) ) ).
  ENDMETHOD.

  METHOD acceptTravel.
    " Modify in local mode: BO-related updates that are not relevant for authorization checks
    MODIFY ENTITIES OF zcd_i_travel_m IN LOCAL MODE
    ENTITY travel
    UPDATE FIELDS ( overall_status )
    WITH VALUE #( FOR key IN keys ( travel_id = key-travel_id
    overall_status = 'A' ) ) " Accepted
    FAILED failed
    REPORTED reported.

    " Read changed data for action result
    READ ENTITIES OF zcd_i_travel_m IN LOCAL MODE
    ENTITY travel
    FIELDS ( agency_id customer_id begin_date end_date
             booking_fee total_price currency_code overall_status
             description created_by created_at last_changed_at last_changed_by )
    WITH VALUE #( FOR key IN keys ( travel_id = key-travel_id ) )
    RESULT DATA(lt_travel).
    result = VALUE #( FOR travel IN lt_travel ( travel_id = travel-travel_id
    %param = travel ) ).

    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).
      APPEND VALUE #( travel_id = <ls_travel>-travel_id
                       %msg = new_message( id    = 'ZCM_MESSAGE'
                                       number    = '001'
                                       v1        = <ls_travel>-overall_status
                                       severity  = if_abap_behv_message=>severity-success )
                         %element-overall_status = if_abap_behv=>mk-on ) TO reported-travel.
    ENDLOOP.
  ENDMETHOD.

  METHOD rejectTravel.
    MODIFY ENTITIES OF zcd_i_travel_m IN LOCAL MODE
             ENTITY travel
                UPDATE FROM VALUE #( FOR key IN keys ( travel_id = key-travel_id
                                                       overall_status = 'X'   " Canceled
                                                       %control-overall_status = if_abap_behv=>mk-on ) )
             FAILED   failed
             REPORTED reported.

    " read changed data for result
    READ ENTITIES OF zcd_i_travel_m IN LOCAL MODE
     ENTITY travel
       FIELDS ( agency_id customer_id begin_date end_date booking_fee
                total_price currency_code overall_status description
                created_by created_at last_changed_at last_changed_by )
         WITH VALUE #( FOR key IN keys ( travel_id = key-travel_id ) )
     RESULT DATA(lt_travel).

    result = VALUE #( FOR travel IN lt_travel ( travel_id = travel-travel_id
                                                %param    = travel ) ).

    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).
      APPEND VALUE #( travel_id = <ls_travel>-travel_id
                       %msg = new_message( id    = 'ZCM_MESSAGE'
                                       number    = '002'
                                       v1        = <ls_travel>-overall_status
                                       severity  = if_abap_behv_message=>severity-success )
                         %element-overall_status = if_abap_behv=>mk-on ) TO reported-travel.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_features.
    READ ENTITY zcd_i_travel_m
*     FIELDS ( travel_id overall_status description )
     FROM VALUE #( FOR keyval IN keys ( %key = keyval-%key ) )
     RESULT DATA(lt_travel_result).

    result = VALUE #( FOR ls_travel IN lt_travel_result (
    %key = ls_travel-%key
    %field-travel_id = if_abap_behv=>fc-f-read_only
*    %field-agency_id = if_abap_behv=>fc-f-mandatory
    %features-%action-rejectTravel = COND #( WHEN ls_travel-overall_status = 'X'
                                                THEN if_abap_behv=>fc-o-disabled
                                                ELSE if_abap_behv=>fc-o-enabled )

    %features-%action-acceptTravel = COND #( WHEN ls_travel-overall_status = 'A'
                                                THEN if_abap_behv=>fc-o-disabled
                                                ELSE if_abap_behv=>fc-o-enabled ) ) ).
  ENDMETHOD.

  METHOD validateCustomer.
    " Read relevant travel instance data
    READ ENTITIES OF zcd_i_travel_m IN LOCAL MODE
    ENTITY Travel
     FIELDS ( customer_id )
     WITH CORRESPONDING #(  keys )
    RESULT DATA(lt_travel).

    DATA lt_customer TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_customer = CORRESPONDING #( lt_travel DISCARDING DUPLICATES MAPPING customer_id = customer_id EXCEPT * ).
    DELETE lt_customer WHERE customer_id IS INITIAL.
    IF lt_customer IS NOT INITIAL.

      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
        FOR ALL ENTRIES IN @lt_customer
        WHERE customer_id = @lt_customer-customer_id
        INTO TABLE @DATA(lt_customer_db).
    ENDIF.
    " Raise msg for non existing and initial customer id
    LOOP AT lt_travel INTO DATA(ls_travel).
      IF ls_travel-customer_id IS INITIAL
         OR NOT line_exists( lt_customer_db[ customer_id = ls_travel-customer_id ] ).

        APPEND VALUE #(  travel_id = ls_travel-travel_id ) TO failed-travel.
        APPEND VALUE #(  travel_id = ls_travel-travel_id
                         %msg = new_message( id        = '/DMO/CM_FLIGHT_LEGAC'
                                             number    = '002'
                                             v1        = ls_travel-customer_id
                                             severity  = if_abap_behv_message=>severity-error )
                         %element-customer_id = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateDates.
    READ ENTITY zcd_i_travel_m\\Travel FIELDS ( begin_date end_date )  WITH
          VALUE #( FOR <root_key> IN keys ( %key = <root_key> ) )
          RESULT DATA(lt_travel_result).

    LOOP AT lt_travel_result INTO DATA(ls_travel_result).

      IF ls_travel_result-end_date < ls_travel_result-begin_date.  "end_date before begin_date

        APPEND VALUE #( %key        = ls_travel_result-%key
                        travel_id   = ls_travel_result-travel_id ) TO failed-travel.

        APPEND VALUE #( %key     = ls_travel_result-%key
                        %msg     = new_message( id       = /dmo/cx_flight_legacy=>end_date_before_begin_date-msgid
                                                number   = /dmo/cx_flight_legacy=>end_date_before_begin_date-msgno
                                                v1       = ls_travel_result-begin_date
                                                v2       = ls_travel_result-end_date
                                                v3       = ls_travel_result-travel_id
                                                severity = if_abap_behv_message=>severity-error )
                        %element-begin_date = if_abap_behv=>mk-on
                        %element-end_date   = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF ls_travel_result-begin_date < cl_abap_context_info=>get_system_date( ).  "begin_date must be in the future

        APPEND VALUE #( %key        = ls_travel_result-%key
                        travel_id   = ls_travel_result-travel_id ) TO failed-travel.

        APPEND VALUE #( %key = ls_travel_result-%key
                        %msg = new_message( id       = /dmo/cx_flight_legacy=>begin_date_before_system_date-msgid
                                            number   = /dmo/cx_flight_legacy=>begin_date_before_system_date-msgno
                                            severity = if_abap_behv_message=>severity-error )
                        %element-begin_date = if_abap_behv=>mk-on
                        %element-end_date   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateStatus.
    READ ENTITY zcd_i_travel_m\\Travel FIELDS ( overall_status ) WITH
              VALUE #( FOR <root_key> IN keys ( %key = <root_key> ) )
              RESULT DATA(lt_travel_result).

    LOOP AT lt_travel_result INTO DATA(ls_travel_result).
      CASE ls_travel_result-overall_status.
        WHEN 'O'.  " Open
        WHEN 'X'.  " Cancelled
        WHEN 'A'.  " Accepted

        WHEN OTHERS.
          APPEND VALUE #( %key = ls_travel_result-%key ) TO failed-travel.

          APPEND VALUE #( %key = ls_travel_result-%key
                          %msg = new_message( id       = /dmo/cx_flight_legacy=>status_is_not_valid-msgid
                                              number   = /dmo/cx_flight_legacy=>status_is_not_valid-msgno
                                              v1       = ls_travel_result-overall_status
                                              severity = if_abap_behv_message=>severity-error )
                          %element-overall_status = if_abap_behv=>mk-on ) TO reported-travel.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_authorizations.
    DATA(lv_auth) = COND #( WHEN cl_abap_context_info=>get_user_technical_name(  ) EQ 'CB0000000025'
                                    THEN if_abap_behv=>auth-allowed
                                    ELSE if_abap_behv=>auth-unauthorized ).

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_keys>).
      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<ls_result>).
      <ls_result> = VALUE #(  %key = <ls_keys>-%key
                              %op-%update                    = lv_auth
                              %delete                        = lv_auth
                              %action-acceptTravel           = lv_auth
                              %action-rejectTravel           = lv_auth
                              %action-createTravelByTemplate = lv_auth
                              %assoc-_Booking                = lv_auth ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lsc_ZCD_I_TRAVEL_M DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS save_modified REDEFINITION.
ENDCLASS.

CLASS lsc_ZCD_I_TRAVEL_M IMPLEMENTATION.
  METHOD save_modified.
    DATA: lt_travel_log   TYPE STANDARD TABLE OF ztb_log_m,
          lt_travel_log_u TYPE STANDARD TABLE OF ztb_log_m.
    DATA(lv_flag)     = cl_abap_behv=>flag_changed.
    DATA(lv_mk_on)    = if_abap_behv=>mk-on.
    DATA(lv_user_mod) = cl_abap_context_info=>get_user_technical_name( ).

    " (1) Get instance data of all instances that have been created
    IF create-travel IS NOT INITIAL.
      " Creates internal table with instance data
      lt_travel_log = CORRESPONDING #( create-travel ).
      LOOP AT lt_travel_log ASSIGNING FIELD-SYMBOL(<fs_travel_log_c>).
        GET TIME STAMP FIELD <fs_travel_log_c>-created_at.
        <fs_travel_log_c>-changing_operation = 'CREATE'.
        " Read travel instance data into ls_travel that includes %control structure
        READ TABLE create-travel WITH TABLE KEY entity COMPONENTS travel_id = <fs_travel_log_c>-travel_id INTO DATA(ls_travel).
        IF sy-subrc = 0.
          IF ls_travel-%control-booking_fee      = lv_flag.
            <fs_travel_log_c>-changed_field_name = 'booking_fee'.
            <fs_travel_log_c>-changed_value      = ls_travel-booking_fee.
            <fs_travel_log_c>-user_mod           = lv_user_mod.
            TRY.
                <fs_travel_log_c>-change_id      = cl_system_uuid=>create_uuid_x16_static( ) .
              CATCH cx_uuid_error.
                "handle exception
            ENDTRY.
            APPEND <fs_travel_log_c> TO lt_travel_log_u.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " (2) Get instance data of all instances that have been updated during the transaction
    IF update-travel IS NOT INITIAL.
      lt_travel_log = CORRESPONDING #( update-travel ).
      LOOP AT update-travel ASSIGNING FIELD-SYMBOL(<fs_travel_log_u>).
        ASSIGN lt_travel_log[ travel_id = <fs_travel_log_u>-travel_id ] TO FIELD-SYMBOL(<fs_travel_db>).
        GET TIME STAMP FIELD <fs_travel_db>-created_at.
        <fs_travel_db>-changing_operation = 'UPDATE'.
        IF <fs_travel_log_u>-%control-customer_id = lv_mk_on.
          <fs_travel_db>-changed_value      = <fs_travel_log_u>-customer_id.
          <fs_travel_db>-changed_field_name = 'customer_id'.
          <fs_travel_db>-user_mod           = lv_user_mod.
          TRY.
              <fs_travel_db>-change_id      = cl_system_uuid=>create_uuid_x16_static( ).
            CATCH cx_uuid_error.
              "handle exception
          ENDTRY.
          APPEND <fs_travel_db> TO lt_travel_log_u.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " (3) Get keys of all travel instances that have been deleted during the transaction
    IF delete-travel IS NOT INITIAL.
      lt_travel_log = CORRESPONDING #( delete-travel ).
      LOOP AT lt_travel_log ASSIGNING FIELD-SYMBOL(<fs_travel_log_d>).
        GET TIME STAMP FIELD <fs_travel_log_d>-created_at.
        <fs_travel_log_d>-changing_operation = 'DELETE'.
        <fs_travel_log_d>-user_mod           = lv_user_mod.
        TRY.
            <fs_travel_log_d>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
          CATCH cx_uuid_error.
            "handle exception
        ENDTRY.
        APPEND <fs_travel_log_d> TO lt_travel_log_u.
      ENDLOOP.
    ENDIF.
    INSERT ztb_log_m FROM TABLE @lt_travel_log_u.
  ENDMETHOD.

ENDCLASS.
