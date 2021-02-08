import scala.quoted.*
import scala.annotation.StaticAnnotation

object Test {
  class Annot extends StaticAnnotation

  class Quoted[T]

  inline def quote[T]: Quoted[T] = ${ quoteImpl[T] }
  def quoteImpl[T: Type](using Quotes): Expr[Quoted[T]] = '{
    new Quoted[T @Annot]
  }
}
