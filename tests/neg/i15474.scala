//> using options -Xfatal-warnings

import scala.language.implicitConversions

object Test1:
  given c: Conversion[ String, Int ] with
    def apply(from: String): Int = from.toInt   // error

object Test2:
  given c: Conversion[ String, Int ] = _.toInt   // error

object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // error
  }
}


