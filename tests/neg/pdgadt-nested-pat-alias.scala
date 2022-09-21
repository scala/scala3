trait Expr { type T }
case class B(b: Int) extends Expr { type T = Int }
case class C(c: Boolean) extends Expr { type T = Boolean }
case class A(a: Expr, b: Expr) extends Expr { type T = (a.T, b.T) }

def foo(e: Expr) = e match
  case e1 @ A(b1 @ B(b), c1 @ C(c)) =>
    val t1: e1.type = e
    val t2: e.type = e1

    val t3: b1.type = e  // error
    val t4: c1.type = e  // error
