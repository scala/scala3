import scala.quoted.*

object Macros {

  inline def natConst(x: Int): Int = ${ natConstImpl('x) }

  def natConstImpl(x: Expr[Int])(using Quotes) : Expr[Int] = {
    import quotes.reflect.*
    val xTree: Term = x.asTerm
    xTree match {
      case Inlined(_, _, Literal(IntConstant(n))) =>
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
