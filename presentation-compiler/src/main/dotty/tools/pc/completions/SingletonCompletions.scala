package dotty.tools.pc.completions

import scala.meta.internal.metals.Fuzzy
import dotty.tools.pc.utils.InteractiveEnrichments.*
import dotty.tools.pc.completions.CompletionValue.SingletonValue

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Types.AndType
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.ConstantType
import dotty.tools.dotc.core.Types.OrType
import dotty.tools.dotc.core.Types.TermRef
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.core.Symbols.defn

object SingletonCompletions:
  def contribute(
    path: List[Tree],
    tpe0: Type,
    completionPos: CompletionPos
  )(using ctx: Context): List[CompletionValue] =
    for {
      (name, span) <-
        path match
          case (i @ Ident(name)) :: _ => List(name.toString() -> i.span)
          case (l @ Literal(const)) :: _ => List(const.show -> l.span)
          case _ => Nil
      query = name.replace(Cursor.value, "")
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
    } yield value

  private def collectSingletons(tpe: Type)(using Context): List[Constant] =
    tpe.deepDealias match
      case ConstantType(value) => List(value)
      case OrType(tpe1, tpe2) =>
        collectSingletons(tpe1) ++ collectSingletons(tpe2)
      case AndType(tpe1, tpe2) =>
        collectSingletons(tpe1).intersect(collectSingletons(tpe2))
      case _ => Nil

object InterCompletionType:
  def inferType(path: List[Tree])(using Context): Option[Type] =
    path match
      case (lit: Literal) :: Select(Literal(_), _) :: Apply(Select(Literal(_), _), List(s: Select)) :: rest if s.symbol == defn.Predef_undefined =>
        inferType(rest, lit.span)
      case ident :: rest => inferType(rest, ident.span)
      case _ => None

  def inferType(path: List[Tree], span: Span)(using Context): Option[Type] =
    path match
      case Apply(head, List(p : Select)) :: rest if p.name == StdNames.nme.??? && p.qualifier.symbol.name == StdNames.nme.Predef && p.span.isSynthetic =>
        inferType(rest, span)
      case Block(_, expr) :: rest if expr.span.contains(span) =>
        inferType(rest, span)
      case If(cond, _, _) :: rest if !cond.span.contains(span) =>
        inferType(rest, span)
      case Typed(expr, tpt) :: _ if expr.span.contains(span) && !tpt.tpe.isErroneous => Some(tpt.tpe)
      case Block(_, expr) :: rest if expr.span.contains(span) =>
        inferType(rest, span)
      case Bind(_, body) :: rest if body.span.contains(span) => inferType(rest, span)
      case Alternative(_) :: rest => inferType(rest, span)
      case Try(block, _, _) :: rest if block.span.contains(span) => inferType(rest, span)
      case CaseDef(_, _, body) :: Try(_, cases, _) :: rest if body.span.contains(span) && cases.exists(_.span.contains(span)) => inferType(rest, span)
      case If(cond, _, _) :: rest if !cond.span.contains(span) => inferType(rest, span)
      case CaseDef(_, _, body) :: Match(_, cases) :: rest if body.span.contains(span) && cases.exists(_.span.contains(span)) =>
        inferType(rest, span)
      case NamedArg(_, arg) :: rest if arg.span.contains(span) => inferType(rest, span)
      // x match
      //  case @@
      case CaseDef(pat, _, _) :: Match(sel, cases) :: rest if pat.span.contains(span) && cases.exists(_.span.contains(span)) && !sel.tpe.isErroneous =>
        sel.tpe match
          case tpe: TermRef => Some(tpe.symbol.info).filterNot(_.isErroneous)
          case tpe => Some(tpe)
      // List(@@)
      case SeqLiteral(_, tpe) :: _ if !tpe.tpe.isErroneous =>
        Some(tpe.tpe)
      // val _: T = @@
      // def _: T = @@
      case (defn: ValOrDefDef) :: rest if !defn.tpt.tpe.isErroneous => Some(defn.tpt.tpe)
      // f(@@)
      case (app: Apply) :: rest =>
        val param =
          for {
            ind <- app.args.zipWithIndex.collectFirst {
              case (arg, id) if arg.span.contains(span) => id
            }
            params <- app.symbol.paramSymss.find(!_.exists(_.isTypeParam))
            param <- params.get(ind)
          } yield param.info
        param match
          // def f[T](a: T): T = ???
          // f[Int](@@)
          // val _: Int = f(@@)
          case Some(t : TypeRef) if t.symbol.is(Flags.TypeParam) =>
            for {
              (typeParams, args) <-
                app match
                  case Apply(TypeApply(fun, args), _) =>
                    val typeParams = fun.symbol.paramSymss.headOption.filter(_.forall(_.isTypeParam))
                    typeParams.map((_, args.map(_.tpe)))
                  // val f: (j: "a") => Int
                  // f(@@)
                  case Apply(Select(v, StdNames.nme.apply), _) =>
                      v.symbol.info match
                        case AppliedType(des, args) =>
                          Some((des.typeSymbol.typeParams, args))
                        case _ => None
                  case _ => None
              ind = typeParams.indexOf(t.symbol)
              tpe <- args.get(ind)
              if !tpe.isErroneous
            } yield tpe
          case Some(tpe) => Some(tpe)
          case _ => None
      case _ => None

