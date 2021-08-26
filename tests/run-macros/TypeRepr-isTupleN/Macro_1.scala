import scala.quoted.*

inline def isTupleN[T]: Boolean = ${ isTupleNImpl[T] }

private def isTupleNImpl[T: Type](using Quotes): Expr[Boolean] = {
  import quotes.reflect.*
  Expr(TypeRepr.of[T].isTupleN)
}
