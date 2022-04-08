def test0[A >: Int] = {
  def test1[X, B >: X <: X] = {
    enum Expr[+T]:
      case TagA() extends Expr[A]
      case TagB() extends Expr[B]

    import Expr._

    def foo(e1: Expr[A], e2: Expr[B]) = e1 match {
      case TagB() => // add GADT constr: B <: A
        e2 match {
          case TagA() =>
            // add GADT constr: A <: B
            //   should propagate bound Int <: (A <: B <:) X for X.
            val t0: X = 0
          case _ =>
        }
      case _ =>
    }
  }
}
