// Another minimisation (after tests/run-macros/i15485.fallout-monocle)
// of monocle's GenIsoSpec.scala
// which broke when fixing soundness in infering GADT constraints on refined types
class Can[T]
object Can:
  import scala.deriving.*, scala.quoted.*
  inline given derived[T](using inline m: Mirror.Of[T]): Can[T] = ${ impl('m) }
  private def impl[T](m: Expr[Mirror.Of[T]])(using Quotes, Type[T]): Expr[Can[T]] = m match
    case '{ $_ : Mirror.Sum     { type MirroredElemTypes = met } } => '{ new Can[T] }
    case '{ $_ : Mirror.Product { type MirroredElemTypes = met } } => '{ new Can[T] }
