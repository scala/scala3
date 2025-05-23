trait A
trait B extends A
trait C extends B
trait D
enum Expr[+T]:
  case IsA() extends Expr[A]
  case IsB() extends Expr[B]
  case IsC() extends Expr[C]
  case IsD() extends Expr[D]
import Expr.*
def test1[T](e: Expr[T]): T = e match
  case IsA() => new A {}
  case IsB() => new B {}
  case IsC() => new C {}
def test2[T](e: Expr[T]): T = e match
  case IsA() | IsB() =>
    // IsA() implies T >: A
    // IsB() implies T >: B
    // So T >: B is chosen
    new B {}
  case IsC() => new C {}
def test3[T](e: Expr[T]): T = e match
  case IsA() | IsB() | IsC() =>
    // T >: C is chosen
    new C {}
def test4[T](e: Expr[T]): T = e match
  case IsA() | IsB() | IsC() =>
    new B {}  // error
def test5[T](e: Expr[T]): T = e match
  case IsA() | IsB() =>
    new A {}  // error
def test6[T](e: Expr[T]): T = e match
  case IsA() | IsC() | IsD() =>
    new C {}  // error
