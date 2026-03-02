
final abstract class PLet

sealed trait Expr[+P]
case class ELet[+A](name: String, expr: Expr[A]) extends Expr[A | PLet]

def go[P](e: Expr[P]): P = e match
  case ELet(_, _) =>
    val x: Expr[P] | ELet[P] = ???
    val y: Expr[P] = x // conforms iff using gadt constraints
    // error before changes: cast from gadt reasoning was not inserted because
    // `Expr[P]` was erronously cached as a baseType of `Expr[P] | ELet[P]` (only true with gadt constraints)
    ???
