import scala.quoted._

object Macros {

  implicit inline def printType[T]: Unit = ${ impl[T] }

  def impl[T: Type](using Quotes) : Expr[Unit] = {
    import quotes.reflect._
    '{
      println(${Expr(TypeTree.of[T].show(using Printer.TreeStructure))})
      println(${Expr(TypeRepr.of[T].show(using Printer.TypeReprStructure))})
      println()
    }
  }
}
