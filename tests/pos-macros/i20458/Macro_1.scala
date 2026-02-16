import scala.quoted._

inline def matchCustom[F](): Unit = ${ matchCustomImpl[F] }

private def matchCustomImpl[F: Type](using q: Quotes): Expr[Unit] = {
  import q.reflect.*
  val any = TypeRepr.of[Any].typeSymbol
  assert(!any.termRef.widenTermRefByName.toString.contains("ClassInfo"))
  any.termRef.widenTermRefByName.asType match
    case '[t] => ()
  '{ () }
}
