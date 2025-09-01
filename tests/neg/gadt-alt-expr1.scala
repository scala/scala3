enum Expr[+T]:
  case I1() extends Expr[Int]
  case I2() extends Expr[Int]
  case B() extends Expr[Boolean]
import Expr.*
def foo[T](e: Expr[T]): T =
  e match
    case I1() | I2() => 42  // ok
    case B() => true
def bar[T](e: Expr[T]): T =
  e match
    case I1() | B() => 42  // error
    case I2() => 0
