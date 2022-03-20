type Const = [X] =>> Int

trait Expr { type F[_] }
case class ConstExprHi() extends Expr { type F[a] <: Const[a] }
case class ConstExprLo() extends Expr { type F[a] >: Const[a] }

def foo[A](e: Expr) = e match
  case _: ConstExprHi =>
    val i: Int = (??? : e.F[A]) : Const[A]
  case _: ConstExprLo =>
    val i: Const[A] = ??? : Int
