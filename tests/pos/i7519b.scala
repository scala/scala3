import scala.quoted._
import scala.annotation.StaticAnnotation

class Annot(in: Int) extends StaticAnnotation

class Quoted[T]

inline def quote[T]: Quoted[T] = ${ quoteImpl[T] }

def quoteImpl[T: Type](given qctx: QuoteContext): Expr[Quoted[T]] = {
  val value: Expr[Int] = '{ 42 }
  '{ new Quoted[T @Annot($value)] }
}
