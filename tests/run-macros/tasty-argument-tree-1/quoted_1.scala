import scala.quoted._

object Macros {

  inline def inspect[T](x: T): Unit = ${ impl('x) }

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._
    val tree = x.asTerm
    '{
      println()
      println("tree: " + ${Expr(tree.showExtractors)})
      println("tree deref. vals: " + ${Expr(tree.underlying.showExtractors)})
    }
  }
}
