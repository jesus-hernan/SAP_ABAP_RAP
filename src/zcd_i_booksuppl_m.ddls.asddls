@AbapCatalog.sqlViewName: 'ZVW_BOOKSUPPL_M'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Supplement Consumption View'
define view ZCD_I_BOOKSUPPL_M
  as select from ztb_booksuppl_m as BookingSupplement
  association        to parent ZCD_I_BOOKING_M as _Booking        on  $projection.travel_id  = _Booking.travel_id
                                                                  and $projection.booking_id = _Booking.booking_id
  association [1..1] to ZCD_I_TRAVEL_M         as _Travel         on  $projection.travel_id = _Travel.travel_id
  association [1..1] to /DMO/I_Supplement      as _Product        on  $projection.supplement_id = _Product.SupplementID
  association [1..*] to /DMO/I_SupplementText  as _SupplementText on  $projection.supplement_id = _SupplementText.SupplementID
{
      //BookingSupplement
  key travel_id,
  key booking_id,
  key booking_supplement_id,
      supplement_id,
      @Semantics.amount.currencyCode: 'currency_code'
      price,
      @Semantics.currencyCode: true
      currency_code,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at,
      /* Associations */
      _Travel,
      _Booking,
      _Product,
      _SupplementText
}
