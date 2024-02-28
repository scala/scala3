//> using options -experimental -Yno-experimental

import scala.quoted.*

inline def typeVariances[A <: AnyKind]: String =
  ${variancesImpl[A]}

def variancesImpl[A <: AnyKind: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  val TypeBounds(_, tl: TypeLambda) = TypeRepr.of[A].typeSymbol.info: @unchecked
  val variances = tl.paramNames.zip(tl.paramVariances).map { (name, variance) =>
    if variance == Flags.Covariant then "+" + name
    else if variance == Flags.Contravariant then "-" + name
    else name
  }.mkString(", ")
  Expr(TypeRepr.of[A].typeSymbol.toString() + "\n" + variances + "\n")
