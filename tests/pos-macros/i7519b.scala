import scala.quoted._
import scala.annotation.StaticAnnotation

class Annot(in: Int) extends StaticAnnotation

class Quoted[T]

inline def quote[T]: Quoted[T] = ${ quoteImpl[T] }

def quoteImpl[T](using s: Scope)(using s.Type[T]): s.Expr[Quoted[T]] = {
  val value: s.Expr[Int] = '{ 42 }
  '{ new Quoted[T @Annot($value)] }
}
