import scala.quoted._
import scala.annotation.StaticAnnotation

object Test {
  class Annot extends StaticAnnotation

  class Quoted[T]

  inline def quote[T]: Quoted[T] = ${ quoteImpl[T] }
  def quoteImpl[T](using s: Scope)(using s.Type[T]): s.Expr[Quoted[T]] = '{
    new Quoted[T @Annot]
  }
}
