import scala.quoted.*

inline def variances[A <: AnyKind]: String =
  ${variancesImpl[A]}

def variancesImpl[A <: AnyKind: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  def loop(tpe: TypeRepr): List[String] =
    tpe match
      case tpe: TypeLambda =>
        tpe.paramNames.zip(tpe.paramVariances).map { (name, variance) =>
          if variance == Flags.Covariant then "+" + name
          else if variance == Flags.Contravariant then "-" + name
          else name
        }.mkString(", ") :: tpe.paramTypes.flatMap(loop)
      case tpe: TypeBounds =>
        loop(tpe.low) ++ loop(tpe.hi)
      case _ =>
        Nil
  val res = (Type.show[A] :: loop(TypeRepr.of[A])).mkString("", "\n", "\n")
  Expr(res)
