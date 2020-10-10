@EndUserText.label: 'Approver Consumption Travel'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZCD_C_TRAVEL_A
  as projection on ZCD_I_TRAVEL_M
{
      //ZCDS_I_TRAVEL_M
  key travel_id          as TravelID,
      @ObjectModel.text.element: ['AgencyName']
      agency_id          as AgencyID,
      _Agency.Name       as AgencyName,
      @ObjectModel.text.element: ['CustomerName']
      customer_id        as CustomerID,
      _Customer.LastName as CustomerName,
      begin_date         as BeginDate,
      end_date           as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      booking_fee        as BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price        as TotalPrice,
      currency_code      as CurrencyCode,
      overall_status     as TravelStatus,
      description        as Description,
      last_changed_at    as LastChangedAt,
      /* Associations */
      _Booking : redirected to composition child zcd_c_booking_a,
      _Agency,
      _Customer
}
