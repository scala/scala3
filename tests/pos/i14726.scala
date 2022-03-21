def test[X, A >: X <: X, B <: Int] = {
  enum Expr[+T]:
    case TagA() extends Expr[A]
    case TagB() extends Expr[B]

  import Expr._

  def foo(e1: Expr[A], e2: Expr[B]) = e1 match {
    case TagB() => // add GADT constr: B <: A
      e2 match {
        case TagA() =>
          // add GADT constr: A <: B
          //   should propagate bound X (<: A <: B) <: Int for X.
          val t0: X = ???
          val t1: Int = t0   // should work too
          val t2: Int = t0 : A  // works
        case _ =>
      }
    case _ =>
  }
}
