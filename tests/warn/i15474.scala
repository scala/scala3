

import scala.language.implicitConversions

object Test2:
  given c: Conversion[ String, Int ] = _.toInt   // warn

object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // warn
  }
}


