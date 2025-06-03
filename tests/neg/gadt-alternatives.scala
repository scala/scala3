enum Expr[+T]:
  case StringVal(x: String) extends Expr[String]
  case IntVal(x: Int) extends Expr[Int]
  case IntValAlt(x: Int) extends Expr[Int]
import Expr.*
def eval[T](e: Expr[T]): T = e match
  case StringVal(_) | IntVal(_) => "42"  // error
def eval1[T](e: Expr[T]): T = e match
  case IntValAlt(_) | IntVal(_) => 42  // error // limitation
