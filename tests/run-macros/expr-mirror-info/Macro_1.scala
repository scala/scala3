import scala.deriving.Mirror
import scala.quoted.*

inline def reflectMirrorInfo[T](using mirror: Mirror.Of[T]): Any = ${ reflectMirrorInfoExpr[T]('mirror) }

private def reflectMirrorInfoExpr[T: Type](mirror: Expr[Mirror.Of[T]])(using Quotes): Expr[Any] =
  val mirroredLabel: String = MirroredExpr.mirroredLabel(mirror).getOrElse(quotes.reflect.report.errorAndAbort("MirroredLabel not found"))
  val mirroredElemLabels = MirroredExpr.mirroredElemLabels(mirror).getOrElse(quotes.reflect.report.errorAndAbort("MirroredElemLabels not found"))
  val mirroredMonoType: Type[?] = MirroredExpr.mirroredMonoType(mirror).getOrElse(quotes.reflect.report.errorAndAbort("MirroredMonoType not found"))
  val mirroredType: Type[?] = MirroredExpr.mirroredType(mirror).getOrElse(quotes.reflect.report.errorAndAbort("MirroredType not found"))
  val mirroredElemTypes: List[Type[?]] = MirroredExpr.mirroredElemTypes(mirror).getOrElse(quotes.reflect.report.errorAndAbort("MirroredElemTypes not found"))

  val mirroredMonoTypeString = mirroredMonoType match
    case '[t] => Type.show[t]
  val mirroredTypeString = mirroredType match
    case '[t] => Type.show[t]
  val mirroredElemTypesStrings = mirroredElemTypes.map {
    case '[t] => Type.show[t]
  }

  Expr((mirroredLabel, mirroredElemLabels, mirroredMonoTypeString, mirroredTypeString, mirroredElemTypesStrings))

inline def reflectMirrorInfo2[T](using mirror: Mirror.Of[T]): Any = ${ reflectMirrorInfoExpr2[T]('mirror) }

private def reflectMirrorInfoExpr2[T: Type](mirror: Expr[Mirror.Of[T]])(using Quotes): Expr[Any] =
  import MirroredExpr.*
  val mirroredLabel: String = mirror.mirroredLabel.getOrElse(quotes.reflect.report.errorAndAbort("MirroredLabel not found"))
  val mirroredElemLabels = mirror.mirroredElemLabels.getOrElse(quotes.reflect.report.errorAndAbort("MirroredElemLabels not found"))
  val mirroredMonoType: Type[?] = mirror.mirroredMonoType.getOrElse(quotes.reflect.report.errorAndAbort("MirroredMonoType not found"))
  val mirroredType: Type[?] = mirror.mirroredType.getOrElse(quotes.reflect.report.errorAndAbort("MirroredType not found"))
  val mirroredElemTypes: List[Type[?]] = mirror.mirroredElemTypes.getOrElse(quotes.reflect.report.errorAndAbort("MirroredElemTypes not found"))

  val mirroredMonoTypeString = mirroredMonoType match
    case '[t] => Type.show[t]
  val mirroredTypeString = mirroredType match
    case '[t] => Type.show[t]
  val mirroredElemTypesStrings = mirroredElemTypes.map {
    case '[t] => Type.show[t]
  }

  Expr((mirroredLabel, mirroredElemLabels, mirroredMonoTypeString, mirroredTypeString, mirroredElemTypesStrings))
