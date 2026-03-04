package dotty.tools.pc.completions

import scala.meta.internal.metals.Fuzzy

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Types.AndType
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.ConstantType
import dotty.tools.dotc.core.Types.OrType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.pc.completions.CompletionValue.SingletonValue
import dotty.tools.pc.utils.InteractiveEnrichments.*

object SingletonCompletions:
  def contribute(
      path: List[Tree],
      tpe0: Type,
      completionPos: CompletionPos
  )(using ctx: Context): List[CompletionValue] =
    for
      (name, span) <-
        path match
          case (i @ Ident(name)) :: _ => List(name.toString() -> i.span)
          case (l @ Literal(const)) :: _ => List(const.show -> l.span)
          case _ => Nil
      query = name.replace(Cursor.value, "").nn
      tpe = tpe0 match
        // for Tuple 2 we want to suggest first arg completion
        case AppliedType(t: TypeRef, args) if t.classSymbol == Symbols.defn.Tuple2 && args.nonEmpty =>
          args.head
        case t => t
      singletonValues = collectSingletons(tpe).map(_.show)
      range = completionPos.originalCursorPosition.withStart(span.start).withEnd(span.start + query.length).toLsp
      value <- singletonValues.collect {
        case name if Fuzzy.matches(query, name) =>
          SingletonValue(name, tpe, Some(range))
      }
    yield value

  private def collectSingletons(tpe: Type)(using Context): List[Constant] =
    tpe.deepDealiasAndSimplify match
      case ConstantType(value) => List(value)
      case OrType(tpe1, tpe2) =>
        collectSingletons(tpe1) ++ collectSingletons(tpe2)
      case AndType(tpe1, tpe2) =>
        collectSingletons(tpe1).intersect(collectSingletons(tpe2))
      case _ => Nil
