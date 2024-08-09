trait DomainIdProvider[T] {
  type Id = List[T]
}
object Country extends DomainIdProvider[Country]
case class Country(
    id: Country.Id,
)
