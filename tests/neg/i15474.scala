//> using options -Xfatal-warnings

import scala.language.implicitConversions

object Test1:
  given c: Conversion[ String, Int ] with
    def apply(from: String): Int = from.toInt   // warn

object Test2:
  given c: Conversion[ String, Int ] = _.toInt   // loop not detected, could be used as a fallback to avoid the warning.

object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // warn
  }
}
// nopos-error: No warnings can be incurred under -Werror.