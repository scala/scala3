import scala.quoted.*

inline def baseTypes[T]: List[String] =
  ${ baseTypesExpr[T] }

private def baseTypesExpr[T: Type](using Quotes): Expr[List[String]] =
  import quotes.reflect.*
  TypeRepr.of[T].typeSymbol.tree match
    case cdef: ClassDef => Expr(cdef.baseTypes.map(_.show))
    case _ => Expr(Nil)
