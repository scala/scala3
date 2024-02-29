import scala.quoted.*

inline def classVariances[A <: AnyKind]: String =
  ${variancesImpl[A]}

def variancesImpl[A <: AnyKind: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  val variances = TypeRepr.of[A].typeSymbol.typeMembers.filter(_.isTypeParam).map { sym =>
    if sym.paramVariance == Flags.Covariant then "+" + sym.name
    else if sym.paramVariance == Flags.Contravariant then "-" + sym.name
    else sym.name
  }
  val res = variances.mkString(TypeRepr.of[A].typeSymbol.toString + "\n", ", ", "\n")
  Expr(res)
