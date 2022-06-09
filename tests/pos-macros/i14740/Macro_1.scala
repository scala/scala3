import scala.quoted.*

type Foo[T]

transparent inline def emptyList[T]: Any = ${ impl[T] }

private def impl[T: Type](using Quotes): Expr[Any] = {
  import quotes.reflect._
  val tpe = AppliedType(TypeRepr.of[Foo], List(TypeRepr.of[T])) // test AppliedType constructor
  Typed('{???}.asTerm, Inferred(tpe)).asExpr // '{ ??? : Foo[T] }
}
