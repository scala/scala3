trait Expr
case class IntLit(b: Int) extends Expr

def foo[M <: Expr](e: M) = e match {
  case e1: IntLit =>
    val t0: e1.type = e
    val t1: e.type = e1
}
