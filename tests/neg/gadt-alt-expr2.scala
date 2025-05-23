enum Expr[+T]:
  case I1() extends Expr[Int]
  case I2() extends Expr[Int]
  case I3() extends Expr[Int]
  case I4() extends Expr[Int]
  case I5() extends Expr[Int]
  case B() extends Expr[Boolean]
import Expr.*
def test1[T](e: Expr[T]): T =
  e match
    case I1() | I2() | I3() | I4() | I5() => 42  // ok
    case B() => true
def test2[T](e: Expr[T]): T =
  e match
    case I1() | I2() | I3() | I4() | I5() | B() => 42  // error
