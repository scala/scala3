import scala.quoted._

object Macros {

  inline def natConst(x: Int): Int = ${ natConstImpl('x) }

  def natConstImpl(x: Expr[Int]) given (qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty._
    val xTree: Term = x.unseal
    xTree match {
      case Inlined(_, _, Literal(Constant(n: Int))) =>
        if (n <= 0)
          QuoteError("Parameter must be natural number")
        xTree.seal.cast[Int]
      case _ =>
        QuoteError("Parameter must be a known constant")
    }
  }

}
