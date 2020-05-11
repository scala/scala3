import scala.quoted._
import scala.annotation.StaticAnnotation

class Annot(in: Int) extends StaticAnnotation

class Quoted[T]

inline def quote[T]: String = ${ quoteImpl[T] }

def quoteImpl[T](using s: Scope)(using s.Type[T]): s.Expr[String] = {
  val value: s.Expr[Int] = '{ 42 }
  Expr(('{ new Quoted[T @Annot($value)] }).show)
}
