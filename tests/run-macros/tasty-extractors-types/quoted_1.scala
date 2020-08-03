import scala.quoted._

object Macros {

  implicit inline def printType[T]: Unit = ${ impl('[T]) }

  def impl[T](x: Staged[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val tree = x.unseal
    '{
      println(${Expr(tree.showExtractors)})
      println(${Expr(tree.tpe.showExtractors)})
      println()
    }
  }
}
