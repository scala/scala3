import scala.quoted.*

inline def mac[T](inline expr: T): T =
  ${ impl('expr) }

class MyMap() extends ExprMap {
  var expressions: List[Expr[Any]] = Nil

  override def transform[T](e: Expr[T])(using Type[T])(using q: Quotes): Expr[T] =
    expressions ::= e
    transformChildren(e)

}

def impl[T: Type](expr: Expr[T])(using Quotes): Expr[T] = {

  val List(es1, es2) = List(MyMap(), MyMap()).map { m =>
    m.transform(expr)
    m.expressions
  }

  assert(es1 == es2)
  assert(es1.map(_.hashCode()) == es2.map(_.hashCode()), s"hash codes not equal:\n${es1.map(_.hashCode())}\n${es2.map(_.hashCode())}")

  expr

}
