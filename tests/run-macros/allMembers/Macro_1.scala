import scala.quoted.*

inline def allMembers[T]: List[String] =
  ${ allMembersExpr[T] }

private def allMembersExpr[T: Type](using Quotes): Expr[List[String]] =
  import quotes.reflect.*
  Expr(TypeRepr.of[T].typeSymbol.allMembers.map(_.fullName).toList)
