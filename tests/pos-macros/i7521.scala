import scala.quoted._
import scala.annotation.StaticAnnotation

object Test {
  inline def quote[T]: Unit = ${ quoteImpl[T] }
  def quoteImpl[T: Staged](using qctx: QuoteContext): Expr[Unit] = '{
     class Annot extends StaticAnnotation
     var test: T @Annot = ???
  }
}
