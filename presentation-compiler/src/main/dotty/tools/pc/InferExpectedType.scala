package dotty.tools.pc

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.typer.Applications.UnapplyArgs
import dotty.tools.dotc.util.NoSourcePosition
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.pc.IndexedContext
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.InteractiveEnrichments.*

import scala.meta.internal.metals.ReportContext
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolSearch

class InferExpectedType(
    search: SymbolSearch,
    driver: InteractiveDriver,
    params: OffsetParams
)(implicit rc: ReportContext):
  val uri = params.uri().nn
  val code = params.text().nn

  val sourceFile = SourceFile.virtual(uri, code)
  driver.run(uri, sourceFile)

  val ctx = driver.currentCtx
  val pos = driver.sourcePosition(params)

  def infer() =
    driver.compilationUnits.get(uri) match
      case Some(unit) =>
        val path =
          Interactive.pathTo(driver.openedTrees(uri), pos)(using ctx)
        val newctx = ctx.fresh.setCompilationUnit(unit)
        val tpdPath =
          Interactive.pathTo(newctx.compilationUnit.tpdTree, pos.span)(using
            newctx
          )
        val locatedCtx =
          Interactive.contextOfPath(tpdPath)(using newctx)
        val indexedCtx = IndexedContext(locatedCtx)
        val printer =
          ShortenedTypePrinter(search, IncludeDefaultParam.ResolveLater)(using indexedCtx)
        InterCompletionType.inferType(path)(using newctx).map{
          tpe => printer.tpe(tpe)
        }
      case None => None

object InterCompletionType:
  def inferType(path: List[Tree])(using Context): Option[Type] =
    path match
      case (lit: Literal) :: Select(Literal(_), _) :: Apply(Select(Literal(_), _), List(s: Select)) :: rest if s.symbol == defn.Predef_undefined => inferType(rest, lit.span)
      case ident :: rest => inferType(rest, ident.span)
      case _ => None

  def inferType(path: List[Tree], span: Span)(using Context): Option[Type] =
    path match
      case Typed(expr, tpt) :: _ if expr.span.contains(span) && !tpt.tpe.isErroneous => Some(tpt.tpe)
      case Block(_, expr) :: rest if expr.span.contains(span) =>
        inferType(rest, span)
      case Bind(_, body) :: rest if body.span.contains(span) => inferType(rest, span)
      case Alternative(_) :: rest => inferType(rest, span)
      case Try(block, _, _) :: rest if block.span.contains(span) => inferType(rest, span)
      case CaseDef(_, _, body) :: Try(_, cases, _) :: rest if body.span.contains(span) && cases.exists(_.span.contains(span)) => inferType(rest, span)
      case If(cond, _, _) :: rest if !cond.span.contains(span) => inferType(rest, span)
      case If(cond, _, _) :: rest if cond.span.contains(span) => Some(Symbols.defn.BooleanType)
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
      case UnApply(fun, _, pats) :: _ =>
        val ind = pats.indexWhere(_.span.contains(span))
        if ind < 0 then None
        else Some(UnapplyArgs(fun.tpe.finalResultType, fun, pats, NoSourcePosition).argTypes(ind))
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

