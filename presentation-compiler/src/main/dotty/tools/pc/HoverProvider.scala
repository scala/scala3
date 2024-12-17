package dotty.tools.pc

import java.util as ju

import scala.meta.internal.metals.Report
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.pc.ScalaHover
import scala.meta.pc.ContentType
import scala.meta.pc.HoverSignature
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.InteractiveEnrichments.*
import dotty.tools.dotc.ast.untpd.InferredTypeTree
import dotty.tools.dotc.core.StdNames

object HoverProvider:

  def hover(
      params: OffsetParams,
      driver: InteractiveDriver,
      search: SymbolSearch,
      contentType: ContentType
  )(implicit reportContext: ReportContext): ju.Optional[HoverSignature] =
    val uri = params.uri().nn
    val text = params.text().nn
    val sourceFile = SourceFile.virtual(uri, text)
    driver.run(uri, sourceFile)
    val unit = driver.compilationUnits.get(uri)

    given ctx: Context =
      val ctx = driver.currentCtx
      unit.map(ctx.fresh.setCompilationUnit).getOrElse(ctx)
    val pos = driver.sourcePosition(params)
    val path = unit
      .map(unit => Interactive.pathTo(unit.tpdTree, pos.span))
      .getOrElse(Interactive.pathTo(driver.openedTrees(uri), pos))
    val indexedContext = IndexedContext(ctx)

    def typeFromPath(path: List[Tree]) =
      if path.isEmpty then NoType else path.head.typeOpt

    val tp = typeFromPath(path)
    val tpw = tp.widenTermRefExpr
    // For expression we need to find all enclosing applies to get the exact generic type
    val enclosing = path.expandRangeToEnclosingApply(pos)

    if tp.isError || tpw == NoType || tpw.isError || path.isEmpty
    then
      def report =
        val posId =
          if path.isEmpty || !path.head.sourcePos.exists
          then pos.start
          else path.head.sourcePos.start
        Report(
          "empty-hover-scala3",
          s"""|$uri
              |pos: ${pos.toLsp}
              |
              |tp: $tp
              |has error: ${tp.isError}
              |
              |tpw: $tpw
              |has error: ${tpw.isError}
              |
              |path:
              |- ${path.map(_.toString()).mkString("\n- ")}
              |trees:
              |- ${unit
               .map(u => List(u.tpdTree))
               .getOrElse(driver.openedTrees(uri).map(_.tree))
               .map(_.toString()).mkString("\n- ")}
              |""".stripMargin,
          s"$uri::$posId"
        )
      end report
      reportContext.unsanitized.create(report, ifVerbose = true)
      ju.Optional.empty().nn
    else
      val skipCheckOnName =
        !pos.isPoint // don't check isHoveringOnName for RangeHover

      val printerCtx = Interactive.contextOfPath(path)
      val printer = ShortenedTypePrinter(search, IncludeDefaultParam.Include)(
        using IndexedContext(printerCtx)
      )
      MetalsInteractive.enclosingSymbolsWithExpressionType(
        enclosing,
        pos,
        indexedContext,
        skipCheckOnName
      ) match
        case Nil =>
          fallbackToDynamics(path, printer, contentType)
        case (symbol, tpe) :: _
            if symbol.name == nme.selectDynamic || symbol.name == nme.applyDynamic =>
          fallbackToDynamics(path, printer, contentType)
        case symbolTpes @ ((symbol, tpe) :: _) =>
          val exprTpw = tpe.widenTermRefExpr.deepDealias
          val hoverString =
            tpw match
              // https://github.com/scala/scala3/issues/8891
              case tpw: ImportType =>
                printer.hoverSymbol(symbol, symbol.paramRef)
              case _ =>
                val (tpe, sym) =
                  if symbol.isType then (symbol.typeRef, symbol)
                  else enclosing.head.seenFrom(symbol)

                val finalTpe =
                  if tpe != NoType then tpe
                  else tpw

                printer.hoverSymbol(sym, finalTpe.deepDealias)
            end match
          end hoverString

          val docString = symbolTpes
            .flatMap(symTpe => search.symbolDocumentation(symTpe._1, contentType))
            .map(_.docstring())
            .mkString("\n")

          val expresionTypeOpt = 
            if symbol.name == StdNames.nme.??? then
              InferExpectedType(search, driver, params).infer()
            else printer.expressionType(exprTpw)
          expresionTypeOpt match
            case Some(expressionType) =>
              val forceExpressionType =
                !pos.span.isZeroExtent || (
                  !hoverString.endsWith(expressionType) &&
                    !symbol.isType &&
                    !symbol.is(Module) &&
                    !symbol.flags.isAllOf(EnumCase)
                )
              ju.Optional.of(
                new ScalaHover(
                  expressionType = Some(expressionType),
                  symbolSignature = Some(hoverString),
                  docstring = Some(docString),
                  forceExpressionType = forceExpressionType,
                  contextInfo = printer.getUsedRenamesInfo,
                  contentType = contentType
                )
              ).nn
            case _ =>
              ju.Optional.empty().nn
          end match
      end match
    end if
  end hover

  extension (pos: SourcePosition)
    private def isPoint: Boolean = pos.start == pos.end

  private def fallbackToDynamics(
      path: List[Tree],
      printer: ShortenedTypePrinter,
      contentType: ContentType
  )(using Context): ju.Optional[HoverSignature] = path match
    case SelectDynamicExtractor(sel, n, name) =>
      def findRefinement(tp: Type): Option[HoverSignature] =
        tp match
          case RefinedType(_, refName, tpe) if name == refName.toString() =>
            val tpeString =
              if n == nme.selectDynamic then s": ${printer.tpe(tpe.resultType)}"
              else printer.tpe(tpe)

            val valOrDef =
              if n == nme.selectDynamic && !tpe.isInstanceOf[ExprType]
              then "val"
              else "def"

            Some(
              new ScalaHover(
                expressionType = Some(tpeString),
                symbolSignature = Some(s"$valOrDef $name$tpeString"),
                contextInfo = printer.getUsedRenamesInfo,
                contentType = contentType
              )
            )
          case RefinedType(parent, _, _) =>
            findRefinement(parent)
          case _ => None

      val refTpe = sel.typeOpt.widen.deepDealias match
        case r: RefinedType => Some(r)
        case t: (TermRef | TypeProxy) => Some(t.termSymbol.info.deepDealias)
        case _ => None

      refTpe.flatMap(findRefinement).asJava
    case _ =>
      ju.Optional.empty().nn

end HoverProvider

object SelectDynamicExtractor:
  def unapply(path: List[Tree])(using Context) =
    path match
      // tests `structural-types` and `structural-types1` in HoverScala3TypeSuite
      case Select(_, _) :: Apply(
            Select(Apply(reflSel, List(sel)), n),
            List(Literal(Constant(name: String)))
          ) :: _
          if (n == nme.selectDynamic || n == nme.applyDynamic) &&
            nme.reflectiveSelectable == reflSel.symbol.name =>
        Some(sel, n, name)
      // tests `selectable`,  `selectable2` and `selectable-full` in HoverScala3TypeSuite
      case Select(_, _) :: Apply(
            Select(sel, n),
            List(Literal(Constant(name: String)))
          ) :: _ if n == nme.selectDynamic || n == nme.applyDynamic =>
        Some(sel, n, name)
      case _ => None
    end match
  end unapply
end SelectDynamicExtractor
