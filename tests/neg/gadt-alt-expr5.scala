trait A
trait B extends A
trait C extends B
enum Expr[-T]:
  case IsA() extends Expr[A]
  case IsB() extends Expr[B]
  case IsC() extends Expr[C]
import Expr.*
def test1[T](e: Expr[T]): Unit = e match
  case IsA() | IsB() =>
    val t1: T = ???
    val t2: A = t1
    val t3: B = t1  // error
  case IsC() =>
