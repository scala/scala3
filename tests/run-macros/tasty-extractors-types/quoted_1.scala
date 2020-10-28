import scala.quoted._

object Macros {

  implicit inline def printType[T]: Unit = ${ impl(Type[T]) }

  def impl[T](x: Type[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val tree = x.unseal
    '{
      println(${Expr(tree.showExtractors)})
      println(${Expr(tree.tpe.showExtractors)})
      println()
    }
  }
}
