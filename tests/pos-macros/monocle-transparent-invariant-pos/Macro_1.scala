import scala.quoted.*

// Invariant type constructor
final class Inv[A](val value: A)

object Inv:
  transparent inline def make: Inv[? <: Tuple] =
    ${ makeImpl }

  def makeImpl(using Quotes): Expr[Inv[? <: Tuple]] =
    '{ new Inv[EmptyTuple](EmptyTuple) }
