import scala.quoted._

object Macros {

  implicit inline def printType[T]: Unit = ${ impl('[T]) }

  def impl[T](using s: Scope)(x: s.Type[T]): s.Expr[Unit] = {
    val tree = x
    '{
      println(${Expr(tree.showExtractors)})
      println(${Expr(tree.tpe.showExtractors)})
      println()
    }
  }
}
