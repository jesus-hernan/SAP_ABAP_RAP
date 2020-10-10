@EndUserText.label: 'Booking Supplement Consumption View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity zcd_c_booksuppl_p
  as projection on ZCD_I_BOOKSUPPL_M
{
  key travel_id                   as TravelID,
  key booking_id                  as BookingID,
  key booking_supplement_id       as BookingSupplementID,
      @ObjectModel.text.element: ['SupplementDescription']
      supplement_id               as SupplementID,
      _SupplementText.Description as SupplementDescription : localized,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                       as Price,
      currency_code               as CurrencyCode,
      last_changed_at             as LastChangedAt,
      /* Associations */
      _Travel  : redirected to ZCD_C_TRAVEL_P, 
      _Booking : redirected to parent ZCD_C_BOOKING_P,
      _SupplementText
}
