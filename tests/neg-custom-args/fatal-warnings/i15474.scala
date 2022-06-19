import scala.language.implicitConversions

given Conversion[ String, Int ] with
  def apply(from: String): Int = from.toInt   // error

object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // error
  }
}