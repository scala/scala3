import scala.quoted.*

inline def typeMembers[A]: List[String] = ${ typeMembersExpr[A] }
inline def declaredTypes[A]: List[String] = ${ declaredTypesExpr[A] }

private def typeMembersExpr[A: Type](using Quotes): Expr[List[String]] = {
  import quotes.reflect.*
  val members = TypeRepr.of[A].typeSymbol.typeMembers
  Expr(members.map(_.name))
}

private def declaredTypesExpr[A: Type](using Quotes): Expr[List[String]] = {
  import quotes.reflect.*
  val decls = TypeRepr.of[A].typeSymbol.declaredTypes
  Expr(decls.map(_.name))
}
