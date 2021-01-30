import scala.quoted.*
import scala.annotation.StaticAnnotation

object Test {
  inline def quote[T]: Unit = ${ quoteImpl[T] }
  def quoteImpl[T: Type](using Quotes): Expr[Unit] = '{
     class Annot extends StaticAnnotation
     var test: T @Annot = ???
  }
}
