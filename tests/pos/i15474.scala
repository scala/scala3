//> using options -Xfatal-warnings
import scala.language.implicitConversions
import scala.language.future

object Test2:
  given c: Conversion[ String, Int ] = _.toInt   // now avoided, was loop not detected, could be used as a fallback to avoid the warning.

object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // was error, now avoided
  }
}


