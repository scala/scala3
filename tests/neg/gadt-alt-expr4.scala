trait A
trait B extends A
trait C extends B
enum Expr[T]:
  case IsA() extends Expr[A]
  case IsB() extends Expr[B]
  case IsC() extends Expr[C]
import Expr.*
def test1[T](e: Expr[T]): T = e match
  case IsA() => new A {}
  case IsB() => new B {}
  case IsC() => new C {}
def test2[T](e: Expr[T]): T = e match
  case IsA() | IsB() =>
    // IsA() implies T =:= A
    // IsB() implies T =:= B
    // No necessary constraint can be found
    new B {}  // error
  case IsC() => new C {}
