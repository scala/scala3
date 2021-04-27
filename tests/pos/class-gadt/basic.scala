object basic {
  enum Expr[A] {
    case IntExpr(value: Int) extends Expr[Int]
    case Other[T](value: T) extends Expr[T]
  }

  class C[A] {
    def eval(e: Expr[A]): A =
      e match {
        case Expr.IntExpr(i) => i + 2
        case Expr.Other(v) => v
      }
  }
}
