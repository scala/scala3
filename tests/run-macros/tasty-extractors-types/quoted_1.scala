import scala.quoted._

object Macros {

  implicit inline def printType[T]: Unit = ${ impl[T] }

  def impl[T: Type](using Quotes) : Expr[Unit] = {
    import quotes.reflect._
    '{
      println(${Value(TypeTree.of[T].showExtractors)})
      println(${Value(TypeRepr.of[T].showExtractors)})
      println()
    }
  }
}
