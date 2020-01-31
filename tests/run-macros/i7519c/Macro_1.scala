import scala.quoted._
import scala.annotation.StaticAnnotation

class Annot(in: Int) extends StaticAnnotation

class Quoted[T]

inline def quote[T]: String = ${ quoteImpl[T] }

def quoteImpl[T: Type](given qctx: QuoteContext): Expr[String] = {
  val value: Expr[Int] = '{ 42 }
  Expr(('{ new Quoted[T @Annot($value)] }).show)
}
