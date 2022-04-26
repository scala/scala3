import scala.quoted.*

inline def inspect[A]: String =
  ${ inspect2[A] }

def inspect2[A: Type](using Quotes): Expr[String] = {
  import quotes.reflect.*

  val ps =
    TypeRepr.of[A].typeSymbol.primaryConstructor.tree match
      case DefDef(_, List(Nil, ps: TermParamClause), _, _) => ps
      case DefDef(_, List(ps: TermParamClause, Nil), _, _) => ps
      case DefDef(_, List(ps: TermParamClause), _, _) => ps

  val names = ps.params.map(p => s"${p.name}: ${p.tpt.show}").mkString("(", ", ", ")")

  Expr(s"${Type.show[A]}: $names isImplicit=${ps.isImplicit}, isGiven=${ps.isGiven}, isErased=${ps.isErased}")
}
