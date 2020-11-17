import scala.quoted._

object Macros {

  inline def natConst(x: Int): Int = ${ natConstImpl('x) }

  def natConstImpl(x: Expr[Int])(using qctx: QuoteContext) : Expr[Int] = {
    import qctx.reflect._
    val xTree: Term = Term.of(x)
    xTree match {
      case Inlined(_, _, Literal(Constant.Int(n))) =>
        if (n <= 0) {
          report.error("Parameter must be natural number")
          '{0}
        } else {
          xTree.asExprOf[Int]
        }
      case _ =>
        report.error("Parameter must be a known constant")
        '{0}
    }
  }

}
