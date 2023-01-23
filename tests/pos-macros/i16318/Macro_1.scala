import scala.quoted.*

final case class Record(a: String, b: Int)

transparent inline def ann[T]: List[Any] = ${ annsImpl[T] }

def annsImpl[T: Type](using Quotes): Expr[List[Any]] = {
  import quotes.reflect.*
  val annExpr = TypeRepr.of[T].typeSymbol.annotations.head.asExpr
  '{ List($annExpr) }
}