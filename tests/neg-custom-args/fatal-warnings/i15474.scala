import scala.language.implicitConversions

object Test1:
  given c: Conversion[ String, Int ] with
    def apply(from: String): Int = from.toInt   // error

object Test2:
  given c: Conversion[ String, Int ] = _.toInt   // loop not detected, could be used as a fallback to avoid the warning.

object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // error
  }
}