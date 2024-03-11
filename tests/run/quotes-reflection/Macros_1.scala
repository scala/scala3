//> using options -experimental -Yno-experimental

import scala.quoted.*

inline def inspect[A]: String =
  ${ inspect2[A] }

def inspect2[A: Type](using Quotes): Expr[String] = {
  import quotes.reflect.*

  val methods = TypeRepr.of[A].typeSymbol.declarations
  val names = methods.map { m =>
    m.tree match
      case dd @ DefDef(name, params, r, body) =>
        val paramStr =
          params.map {
            case ps: TermParamClause =>
              val params = ps.params.map(p => s"${p.name}: ${p.tpt.show}").mkString("(", ", ", ")")
              s"$params isGiven=${ps.isGiven} isImplicit=${ps.isImplicit} erasedArgs=${ps.erasedArgs}"
            case ps: TypeParamClause => ps.params.map(_.show).mkString("[", ", ", "]")
          }.mkString("")
        s"method $name: $paramStr"
      case vd @ ValDef(name, tpt, body) =>
        tpt.tpe match
          case Refinement(parent, "apply", tpe: MethodType) if parent == defn.PolyFunctionClass.typeRef =>
            assert(tpt.tpe.isErasedFunctionType)

            val params = tpe.paramNames.zip(tpe.paramTypes).map((n, t) => s"$n: ${t.show}").mkString("(", ", ", ")")
            s"val $name: ${parent.show} with apply: ${params} isImplicit=${tpe.isImplicit} erasedParams=${tpe.erasedParams}"
          case _ =>
            s"val $name: ${tpt.show}"
      case td @ TypeDef(name, tpt) => s"type $name: ${tpt.show}"
      case _ => s"something else: $m"
  }

  Expr(names.mkString("\n"))
}
