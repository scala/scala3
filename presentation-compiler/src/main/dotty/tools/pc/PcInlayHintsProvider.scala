package dotty.tools.pc


import java.nio.file.Paths

import scala.annotation.tailrec

import scala.meta.internal.metals.ReportContext
import dotty.tools.pc.utils.InteractiveEnrichments.*
import dotty.tools.pc.printer.ShortenedTypePrinter
import scala.meta.internal.pc.InlayHints
import scala.meta.internal.pc.LabelPart
import scala.meta.internal.pc.LabelPart.*
import scala.meta.pc.InlayHintsParams
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span

import org.eclipse.lsp4j.InlayHint
import org.eclipse.lsp4j.InlayHintKind
import org.eclipse.{lsp4j as l}

class PcInlayHintsProvider(
    driver: InteractiveDriver,
    params: InlayHintsParams,
    symbolSearch: SymbolSearch,
)(using ReportContext):

  val uri = params.uri().nn
  val filePath = Paths.get(uri).nn
  val sourceText = params.text().nn
  val text = sourceText.toCharArray().nn
  val source =
    SourceFile.virtual(filePath.toString, sourceText)
  driver.run(uri, source)
  given InlayHintsParams = params

  given InferredType.Text = InferredType.Text(text)
  given ctx: Context = driver.currentCtx
  val unit = driver.currentCtx.run.nn.units.head
  val pos = driver.sourcePosition(params)

  def provide(): List[InlayHint] =
    val deepFolder = DeepFolder[InlayHints](collectDecorations)
    Interactive
      .pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)
      .headOption
      .getOrElse(unit.tpdTree)
      .enclosedChildren(pos.span)
      .flatMap(tpdTree => deepFolder(InlayHints.empty(params.uri().nn), tpdTree).result())

  private def adjustPos(pos: SourcePosition): SourcePosition =
    pos.adjust(text)._1

  def collectDecorations(
      inlayHints: InlayHints,
      tree: Tree,
  ): InlayHints =
    tree match
      case ImplicitConversion(symbol, range) =>
        val adjusted = adjustPos(range)
        inlayHints
          .add(
            adjusted.startPos.toLsp,
            labelPart(symbol, symbol.decodedName) :: LabelPart("(") :: Nil,
            InlayHintKind.Parameter,
          )
          .add(
            adjusted.endPos.toLsp,
            LabelPart(")") :: Nil,
            InlayHintKind.Parameter,
          )
      case ImplicitParameters(trees, pos) =>
        inlayHints.add(
          adjustPos(pos).toLsp,
          ImplicitParameters.partsFromImplicitArgs(trees).map((label, maybeSymbol) =>
             maybeSymbol match
               case Some(symbol) => labelPart(symbol, label)
               case None => LabelPart(label)
           ),
           InlayHintKind.Parameter
        )
      case ValueOf(label, pos) =>
        inlayHints.add(
          adjustPos(pos).toLsp,
          LabelPart("(") :: LabelPart(label) :: List(LabelPart(")")),
          InlayHintKind.Parameter,
        )
      case TypeParameters(tpes, pos, sel)
          if !syntheticTupleApply(sel) =>
        val label = tpes.map(toLabelParts(_, pos)).separated("[", ", ", "]")
        inlayHints.add(
          adjustPos(pos).endPos.toLsp,
          label,
          InlayHintKind.Type,
        )
      case InferredType(tpe, pos, defTree)
          if !isErrorTpe(tpe) =>
        val adjustedPos = adjustPos(pos).endPos
        if inlayHints.containsDef(adjustedPos.start) then inlayHints
        else
          inlayHints
            .add(
              adjustedPos.toLsp,
              LabelPart(": ") :: toLabelParts(tpe, pos),
              InlayHintKind.Type,
            )
            .addDefinition(adjustedPos.start)
      case _ => inlayHints

  private def toLabelParts(
      tpe: Type,
      pos: SourcePosition,
  ): List[LabelPart] =
    val tpdPath =
      Interactive.pathTo(unit.tpdTree, pos.span)

    val indexedCtx = IndexedContext(Interactive.contextOfPath(tpdPath))
    val printer = ShortenedTypePrinter(
      symbolSearch
    )(using indexedCtx)
    def optDealias(tpe: Type): Type =
      def isInScope(tpe: Type): Boolean =
        tpe match
          case tref: TypeRef =>
            indexedCtx.lookupSym(
              tref.currentSymbol
            ) == IndexedContext.Result.InScope
          case AppliedType(tycon, args) =>
            isInScope(tycon) && args.forall(isInScope)
          case _ => true
      if isInScope(tpe) then tpe
      else tpe.deepDealias(using indexedCtx.ctx)

    val dealiased = optDealias(tpe)
    val tpeStr = printer.tpe(dealiased)
    val usedRenames = printer.getUsedRenames
    val parts = partsFromType(dealiased, usedRenames)
    InlayHints.makeLabelParts(parts, tpeStr)
  end toLabelParts

  private val definitions = IndexedContext(ctx).ctx.definitions
  private def syntheticTupleApply(tree: Tree): Boolean =
    tree match
      case sel: Select =>
        if definitions.isTupleNType(sel.symbol.info.finalResultType) then
          sel match
            case Select(tupleClass: Ident, _)
                if !tupleClass.span.isZeroExtent &&
                  tupleClass.span.exists &&
                  tupleClass.name.startsWith("Tuple") =>
              val pos = tupleClass.sourcePos
              !sourceText.slice(pos.start, pos.end).mkString.startsWith("Tuple")
            case _ => true
        else false
      case _ => false

  private def labelPart(symbol: Symbol, label: String) =
    if symbol.source == pos.source then
      LabelPart(
        label,
        pos = Some(symbol.sourcePos.toLsp.getStart().nn),
      )
    else
      LabelPart(
        label,
        symbol = SemanticdbSymbols.symbolName(symbol),
      )

  private def partsFromType(
      tpe: Type,
      usedRenames: Map[Symbol, String],
  ): List[LabelPart] =
    NamedPartsAccumulator(_ => true)(Nil, tpe)
      .filter(_.symbol != NoSymbol)
      .map { t =>
        val label = usedRenames.get(t.symbol).getOrElse(t.symbol.decodedName)
        labelPart(t.symbol, label)
      }

  private def isErrorTpe(tpe: Type): Boolean = tpe.isError
end PcInlayHintsProvider

object ImplicitConversion:
  def unapply(tree: Tree)(using params: InlayHintsParams, ctx: Context) =
    if (params.implicitConversions()) {
      tree match
        case Apply(fun: Ident, args) if isSynthetic(fun) && args.exists(!_.span.isZeroExtent) =>
          implicitConversion(fun, args)
        case Apply(Select(fun, name), args)
            if name == nme.apply && isSynthetic(fun) && args.exists(!_.span.isZeroExtent) =>
          implicitConversion(fun, args)
        case _ => None
    } else None
  private def isSynthetic(tree: Tree)(using Context) =
    tree.span.isSynthetic && tree.symbol.isOneOf(Flags.GivenOrImplicit)

  private def implicitConversion(fun: Tree, args: List[Tree])(using Context) =
    val lastArgPos =
      args.lastOption.map(_.sourcePos).getOrElse(fun.sourcePos)
    Some(
      fun.symbol,
      lastArgPos.withStart(fun.sourcePos.start),
    )
end ImplicitConversion

object ImplicitParameters:
  def unapply(tree: Tree)(using params: InlayHintsParams, ctx: Context) =
    if (params.implicitParameters()) {
      tree match
        case Apply(fun, args)
            if args.exists(isSyntheticArg) && !tree.sourcePos.span.isZeroExtent && !args.exists(isQuotes(_)) =>
          val (implicitArgs, providedArgs) = args.partition(isSyntheticArg)
          val pos = implicitArgs.head.sourcePos
          Some(implicitArgs, pos)
        case _ => None
    } else None

  @tailrec
  def isSyntheticArg(tree: Tree)(using Context): Boolean = tree match
    case tree: Ident =>
      tree.span.isSynthetic && tree.symbol.isOneOf(Flags.GivenOrImplicit)
    case Apply(fun, _ ) if tree.span.isZeroExtent => isSyntheticArg(fun)
    case TypeApply(fun, _ ) if tree.span.isZeroExtent => isSyntheticArg(fun)
    case _ => false

  // Decorations for Quotes are rarely useful
  private def isQuotes(tree: Tree)(using Context) =
    tree.tpe.typeSymbol == defn.QuotesClass

  def partsFromImplicitArgs(trees: List[Tree])(using Context): List[(String, Option[Symbol])] = {
    @tailrec
    def recurseImplicitArgs(
        currentArgs: List[Tree],
        remainingArgsLists: List[List[Tree]],
        parts: List[(String, Option[Symbol])]
    ): List[(String, Option[Symbol])] =
      (currentArgs, remainingArgsLists) match {
        case (Nil, Nil) => parts
        case (Nil, headArgsList :: tailArgsList) =>
          if (headArgsList.isEmpty) {
            recurseImplicitArgs(
              headArgsList,
              tailArgsList,
              (")", None) :: parts
            )
          } else {
            recurseImplicitArgs(
              headArgsList,
              tailArgsList,
              (", ", None) :: (")", None) :: parts
            )
          }
        case (arg :: remainingArgs, remainingArgsLists) =>
          arg match {
            case Apply(fun, args) =>
              val applyLabel = (fun.symbol.decodedName, Some(fun.symbol))
              recurseImplicitArgs(
                args,
                remainingArgs :: remainingArgsLists,
                ("(", None) :: applyLabel :: parts
              )
            case t if t.isTerm =>
              val termLabel = (t.symbol.decodedName, Some(t.symbol))
              if (remainingArgs.isEmpty)
                recurseImplicitArgs(
                  remainingArgs,
                  remainingArgsLists,
                  termLabel :: parts
                )
              else
                recurseImplicitArgs(
                  remainingArgs,
                  remainingArgsLists,
                  (", ", None) :: termLabel :: parts
                )
            case _ =>
              recurseImplicitArgs(
                remainingArgs,
                remainingArgsLists,
                parts
              )
          }
      }
    ((")", None) :: recurseImplicitArgs(
      trees,
      Nil,
      List(("(using ", None))
    )).reverse
  }

end ImplicitParameters

object ValueOf:
  def unapply(tree: Tree)(using params: InlayHintsParams, ctx: Context) =
    if (params.implicitParameters()) {
      tree match
        case Apply(ta @ TypeApply(fun, _), _)
            if fun.span.isSynthetic && isValueOf(fun) =>
          Some(
            "new " + tpnme.valueOf.decoded.capitalize + "(...)",
            fun.sourcePos,
          )
        case _ => None
    } else None
  private def isValueOf(tree: Tree)(using Context) =
    val symbol = tree.symbol.maybeOwner
    symbol.name.decoded == tpnme.valueOf.decoded.capitalize
end ValueOf

object TypeParameters:
  def unapply(tree: Tree)(using params: InlayHintsParams, ctx: Context) =
    if (params.typeParameters()) {
      tree match
        case TypeApply(sel: Select, _)
            if sel.isForComprehensionMethod || sel.isInfix ||
            sel.symbol.name == nme.unapply =>
          None
        case TypeApply(fun, args) if inferredTypeArgs(args) =>
          val tpes = args.map(_.tpe.stripTypeVar.widen.finalResultType)
          Some((tpes, fun.sourcePos.endPos, fun))
        case _ => None
    } else None

  private def inferredTypeArgs(args: List[Tree]): Boolean =
    args.forall {
      case tt: TypeTree if tt.span.exists && !tt.span.isZeroExtent => true
      case _ => false
    }
end TypeParameters

object InferredType:
  opaque type Text = Array[Char]
  object Text:
    def apply(text: Array[Char]): Text = text

  def unapply(tree: Tree)(using params: InlayHintsParams, text: Text, ctx: Context) =
    if (params.inferredTypes()) {
      tree match
        case vd @ ValDef(_, tpe, _)
            if isValidSpan(tpe.span, vd.nameSpan) &&
              !vd.symbol.is(Flags.Enum) &&
              (isNotInUnapply(vd) || params.hintsInPatternMatch()) &&
              !isValDefBind(text, vd) =>
          if vd.symbol == vd.symbol.sourceSymbol then
            Some(tpe.tpe, tpe.sourcePos.withSpan(vd.nameSpan), vd)
          else None
        case vd @ DefDef(_, _, tpe, _)
            if isValidSpan(tpe.span, vd.nameSpan) &&
              tpe.span.start >= vd.nameSpan.end &&
              !vd.symbol.isConstructor &&
              !vd.symbol.is(Flags.Mutable) =>
          if vd.symbol == vd.symbol.sourceSymbol then
            Some(tpe.tpe, tpe.sourcePos, vd)
          else None
        case bd @ Bind(
              name,
              Ident(nme.WILDCARD),
            ) if !bd.span.isZeroExtent && bd.symbol.isTerm && params.hintsInPatternMatch() =>
          Some(bd.symbol.info, bd.namePos, bd)
        case _ => None
    } else None

  private def isNotInUnapply(vd: ValDef)(using Context) =
    vd.rhs.span.exists && vd.rhs.span.start > vd.nameSpan.end

  private def isValidSpan(tpeSpan: Span, nameSpan: Span): Boolean =
    tpeSpan.isZeroExtent &&
      nameSpan.exists &&
      !nameSpan.isZeroExtent

  /* If is left part of val definition bind:
   * val <<t>> @ ... =
   */
  def isValDefBind(text: Text, vd: ValDef)(using Context) =
    val afterDef = text.drop(vd.nameSpan.end)
    val index = indexAfterSpacesAndComments(afterDef)
    index >= 0 && index < afterDef.size && afterDef(index) == '@'

end InferredType
