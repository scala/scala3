import scala.deriving.Mirror
import scala.quoted.*

object MirroredExpr:
  extension (mirror: Expr[Mirror])
    def mirroredMonoType(using Quotes): Option[Type[?]] =
      mirror match
        case '{ $_ : Mirror { type MirroredMonoType = t } } => Some(Type.of[t])
        case _ => None

    def mirroredType(using Quotes): Option[Type[?]] =
      mirror match
        case '{ $_ : Mirror { type MirroredType = t } } => Some(Type.of[t])
        case _ => None

    def mirroredLabel(using Quotes): Option[String] =
      mirror match
        case '{ type label <: String; $_ : Mirror { type MirroredLabel = label } } =>
          Type.valueOfConstant[label]
        case _ => None

    def mirroredElemTypes(using Quotes): Option[List[Type[?]]] =
      mirror match
        case '{ type labels <: Tuple; $_ : Mirror { type MirroredElemTypes = labels } } =>
          tupleTypes[labels]
        case _ => None

    def mirroredElemLabels(using Quotes): Option[List[String]] =
      mirror match
        case '{ type labels <: Tuple; $_ : Mirror { type MirroredElemLabels = labels } } =>
          Type.valueOfTuple[labels].map(_.toList.asInstanceOf[List[String]])
        case _ => None

  private def tupleTypes[T <: Tuple : Type](using Quotes): Option[List[Type[?]]] =
    import quotes.reflect.*
    val cons = Symbol.classSymbol("scala.*:")
    def rec(tpe: TypeRepr): Option[List[Type[?]]] =
      tpe.widenTermRefByName.dealias match
        case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
          tpes.foldRight(Option(List.empty[Type[?]])) {
            case (_, None) => None
            case (tpe, Some(acc)) => Some(tpe.asType :: acc)
            case _ => None
          }
        case AppliedType(tp, List(headType, tail)) if tp.derivesFrom(cons) =>
          rec(tail) match
            case Some(tailTypes) => Some(headType.asType :: tailTypes)
            case None => None
        case tpe =>
          if tpe.derivesFrom(Symbol.classSymbol("scala.EmptyTuple")) then Some(Nil)
          else None
    rec(TypeRepr.of[T])
