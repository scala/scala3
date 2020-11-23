import scala.quoted._

object Macros {

  implicit inline def printType[T]: Unit = ${ impl[T] }

  def impl[T: Type](using Quotes) : Expr[Unit] = {
    import qctx.reflect._
    '{
      println(${Expr(TypeTree.of[T].showExtractors)})
      println(${Expr(TypeRepr.of[T].showExtractors)})
      println()
    }
  }
}
