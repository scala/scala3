import scala.quoted._
import scala.annotation.StaticAnnotation

object Test {
  class Annot extends StaticAnnotation

  class Quoted[T]

  inline def quote[T]: Quoted[T] = ${ quoteImpl[T] }
  def quoteImpl[T: Type](given qctx: QuoteContext): Expr[Quoted[T]] = '{
    new Quoted[T @Annot]
  }
}
