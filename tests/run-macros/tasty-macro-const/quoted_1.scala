import scala.quoted._

object Macros {

  inline def natConst(x: Int): Int = ${ natConstImpl('x) }

  def natConstImpl(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = {
    import s.tasty._
    val xTree: Term = x
    xTree match {
      case Inlined(_, _, Literal(Constant(n: Int))) =>
        if (n <= 0) {
          report.error("Parameter must be natural number")
          '{0}
        } else {
          xTree.seal.cast[Int]
        }
      case _ =>
        report.error("Parameter must be a known constant")
        '{0}
    }
  }

}
