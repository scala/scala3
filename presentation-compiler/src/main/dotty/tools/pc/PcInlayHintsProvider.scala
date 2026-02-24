package dotty.tools.pc

import java.nio.file.Paths

import scala.annotation.tailrec
import scala.meta.internal.pc.InlayHintOrigin
import scala.meta.internal.pc.InlayHints
import scala.meta.internal.pc.LabelPart
import scala.meta.internal.pc.LabelPart.*
import scala.meta.pc.InlayHintsParams
import scala.meta.pc.SymbolSearch
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.NameOps.fieldName
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.InlayHint
import org.eclipse.lsp4j.InlayHintKind
import org.eclipse.lsp4j as l

class PcInlayHintsProvider(
    driver: InteractiveDriver,
    params: InlayHintsParams,
    symbolSearch: SymbolSearch
)(using ReportContext):

  val uri: java.net.URI = params.uri()
  val filePath: java.nio.file.Path = Paths.get(uri)
  val sourceText: String = params.text()
  val text: Array[Char] = sourceText.toCharArray()
  val source: SourceFile =
    SourceFile.virtual(filePath.toString, sourceText)
  driver.run(uri, source)
  given InlayHintsParams = params

  given InferredType.Text = InferredType.Text(text)
  given ctx: Context = driver.currentCtx
  val unit = driver.currentCtx.run.nn.units.head
  val pos = driver.sourcePosition(params)

  def provide(): List[InlayHint] =
    val deepFolder = PcCollector.DeepFolderWithParent[InlayHints](collectDecorations)
    Interactive
      .pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)
      .headOption
      .getOrElse(unit.tpdTree)
      .enclosedChildren(pos.span)
      .flatMap(tpdTree => deepFolder(InlayHints.empty(params.uri()), tpdTree).result())

  private def adjustPos(pos: SourcePosition): SourcePosition =
    pos.adjust(text)._1

  def collectDecorations(
      inlayHints: InlayHints,
      tree: Tree,
      parent: Option[Tree]
  ): InlayHints =
    /*
      XRay hints and closing labels are not mutually exclusive with other hints,
      so they must be matched separately
     */
    val firstPassHints = (tree, parent) match
      case XRayModeHint(tpe, pos) =>
        inlayHints.addToBlock(
          adjustPos(pos).toLsp,
          LabelPart(": ") :: toLabelParts(tpe, pos),
          InlayHintKind.Type
        )
      case _ => inlayHints

    val withClosingLabels = tree match
      case ClosingLabel(name, pos) =>
        firstPassHints.add(
          adjustPos(pos).endPos.toLsp,
          List(LabelPart(name)),
          InlayHintKind.Parameter,
          InlayHintOrigin.ClosingLabel
        )
      case _ => firstPassHints

    tree match
      case ImplicitConversion(symbol, range) =>
        val adjusted = adjustPos(range)
        withClosingLabels
          .add(
            adjusted.startPos.toLsp,
            labelPart(symbol, symbol.decodedName) :: LabelPart("(") :: Nil,
            InlayHintKind.Parameter,
            InlayHintOrigin.ImplicitConversion
          )
          .add(
            adjusted.endPos.toLsp,
            LabelPart(")") :: Nil,
            InlayHintKind.Parameter,
            InlayHintOrigin.ImplicitConversion
          )
      case ImplicitParameters(trees, pos) =>
        withClosingLabels.add(
          adjustPos(pos).toLsp,
          ImplicitParameters.partsFromImplicitArgs(trees).map((label, maybeSymbol) =>
            maybeSymbol match
              case Some(symbol) => labelPart(symbol, label)
              case None => LabelPart(label)
          ),
          InlayHintKind.Parameter,
          InlayHintOrigin.ImplicitParameters
        )
      case ValueOf(label, pos) =>
        withClosingLabels.add(
          adjustPos(pos).toLsp,
          LabelPart("(") :: LabelPart(label) :: List(LabelPart(")")),
          InlayHintKind.Parameter,
          InlayHintOrigin.ImplicitParameters
        )
      case TypeParameters(tpes, pos, sel)
          if !syntheticTupleApply(sel) =>
        val label = tpes.map(toLabelParts(_, pos)).separated("[", ", ", "]")
        withClosingLabels.add(
          adjustPos(pos).endPos.toLsp,
          label,
          InlayHintKind.Type,
          InlayHintOrigin.TypeParameters
        )
      case InferredType(tpe, pos, defTree)
          if !isErrorTpe(tpe) =>
        val adjustedPos = adjustPos(pos).endPos
        withClosingLabels
          .add(
            adjustedPos.toLsp,
            LabelPart(": ") :: toLabelParts(tpe, pos),
            InlayHintKind.Type,
            InlayHintOrigin.InferredType
          )
      case Parameters(isInfixFun, args) =>
        def isNamedParam(pos: SourcePosition): Boolean =
          val start = text.indexWhere(!_.isWhitespace, pos.start)
          val end = text.lastIndexWhere(!_.isWhitespace, pos.end - 1)

          text.slice(start, end).contains('=')

        def isBlockParam(pos: SourcePosition): Boolean =
          val start = text.indexWhere(!_.isWhitespace, pos.start)
          val end = text.lastIndexWhere(!_.isWhitespace, pos.end - 1)
          val startsWithBrace = text.lift(start).contains('{')
          val endsWithBrace = text.lift(end).contains('}')

          startsWithBrace && endsWithBrace

        def adjustBlockParamPos(pos: SourcePosition): SourcePosition =
          pos.withStart(pos.start + 1)

        args.foldLeft(withClosingLabels) {
          case (ih, (name, pos0, isByName)) =>
            val pos = adjustPos(pos0)
            val isBlock = isBlockParam(pos)
            val namedLabel =
              if params.namedParameters() && !isInfixFun && !isBlock && !isNamedParam(pos) then s"${name} = " else ""
            val byNameLabel =
              if params.byNameParameters() && isByName && (!isInfixFun || isBlock) then "=> " else ""

            val labelStr = s"${namedLabel}${byNameLabel}"
            val hintPos = if isBlock then adjustBlockParamPos(pos) else pos

            if labelStr.nonEmpty then
              ih.add(
                hintPos.startPos.toLsp,
                List(LabelPart(labelStr)),
                InlayHintKind.Parameter,
                if params.byNameParameters then InlayHintOrigin.ByNameParameters else InlayHintOrigin.NamedParameters
              )
            else ih
        }
      case _ => withClosingLabels

  private def toLabelParts(
      tpe: Type,
      pos: SourcePosition
  ): List[LabelPart] =
    val tpdPath =
      Interactive.pathTo(unit.tpdTree, pos.span)

    val indexedCtx = IndexedContext(pos)(using Interactive.contextOfPath(tpdPath))
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
      else tpe.deepDealiasAndSimplify(using indexedCtx.ctx)

    val dealiased = optDealias(tpe)
    val tpeStr = printer.tpe(dealiased)
    val usedRenames = printer.getUsedRenames
    val parts = partsFromType(dealiased, usedRenames)
    InlayHints.makeLabelParts(parts, tpeStr)
  end toLabelParts

  private val definitions = IndexedContext(pos)(using ctx).ctx.definitions
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
        pos = Some(symbol.sourcePos.toLsp.getStart().nn)
      )
    else
      LabelPart(
        label,
        symbol = SemanticdbSymbols.symbolName(symbol)
      )

  private def partsFromType(
      tpe: Type,
      usedRenames: Map[Symbol, String]
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
    if params.implicitConversions() then
      tree match
        case Apply(fun: Ident, args) if isSynthetic(fun) && args.exists(!_.span.isZeroExtent) =>
          implicitConversion(fun, args)
        case Apply(Select(fun, name), args)
            if name == nme.apply && isSynthetic(fun) && args.exists(!_.span.isZeroExtent) =>
          implicitConversion(fun, args)
        case _ => None
    else None
  private def isSynthetic(tree: Tree)(using Context) =
    tree.span.isSynthetic && tree.symbol.isOneOf(Flags.GivenOrImplicit)

  private def implicitConversion(fun: Tree, args: List[Tree])(using Context) =
    val lastArgPos =
      args.lastOption.map(_.sourcePos).getOrElse(fun.sourcePos)
    Some(
      fun.symbol,
      lastArgPos.withStart(fun.sourcePos.start)
    )

object ImplicitParameters:
  def unapply(tree: Tree)(using params: InlayHintsParams, ctx: Context) =
    if params.implicitParameters() then
      tree match
        case Apply(_, args) if hasImplicitArgs(tree, args) => implicitArgs(args)
        case Inlined(Apply(_, args), _, _) if hasImplicitArgs(tree, args) => implicitArgs(args)
        case _ => None
    else None

  private def hasImplicitArgs(tree: Tree, args: List[Tree])(using Context): Boolean =
    args.exists(isSyntheticArg) && !tree.sourcePos.span.isZeroExtent && !args.exists(isQuotes)

  private def implicitArgs(args: List[Tree])(using Context): Option[(List[Tree], SourcePosition)] =
    val found = args.filter(isSyntheticArg)
    Option.when(found.nonEmpty)(found, found.head.sourcePos)

  @tailrec
  def isSyntheticArg(tree: Tree)(using Context): Boolean = tree match
    case tree: Ident =>
      tree.span.isSynthetic && tree.symbol.isOneOf(Flags.GivenOrImplicit)
    case Apply(fun, _) if tree.span.isZeroExtent => isSyntheticArg(fun)
    case TypeApply(fun, _) if tree.span.isZeroExtent => isSyntheticArg(fun)
    case _ => false

  // Decorations for Quotes are rarely useful
  private def isQuotes(tree: Tree)(using Context) =
    tree.tpe.typeSymbol == defn.QuotesClass

  def partsFromImplicitArgs(trees: List[Tree])(using Context): List[(String, Option[Symbol])] =
    @tailrec
    def recurseImplicitArgs(
        currentArgs: List[Tree],
        remainingArgsLists: List[List[Tree]],
        parts: List[(String, Option[Symbol])]
    ): List[(String, Option[Symbol])] =
      (currentArgs, remainingArgsLists) match
        case (Nil, Nil) => parts
        case (Nil, headArgsList :: tailArgsList) =>
          if headArgsList.isEmpty then
            recurseImplicitArgs(
              headArgsList,
              tailArgsList,
              (")", None) :: parts
            )
          else
            recurseImplicitArgs(
              headArgsList,
              tailArgsList,
              (", ", None) :: (")", None) :: parts
            )
        case (arg :: remainingArgs, remainingArgsLists) =>
          arg match
            case Apply(fun, args) =>
              val applyLabel = (fun.symbol.decodedName, Some(fun.symbol))
              recurseImplicitArgs(
                args,
                remainingArgs :: remainingArgsLists,
                ("(", None) :: applyLabel :: parts
              )
            case t if t.isTerm =>
              val termLabel = (t.symbol.decodedName, Some(t.symbol))
              if remainingArgs.isEmpty then
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
    ((")", None) :: recurseImplicitArgs(
      trees,
      Nil,
      List(("(using ", None))
    )).reverse

end ImplicitParameters

object ValueOf:
  def unapply(tree: Tree)(using params: InlayHintsParams, ctx: Context) =
    if params.implicitParameters() then
      tree match
        case Apply(ta @ TypeApply(fun, _), _)
            if fun.span.isSynthetic && isValueOf(fun) =>
          Some(
            "new " + tpnme.valueOf.decoded.capitalize + "(...)",
            fun.sourcePos
          )
        case _ => None
    else None
  private def isValueOf(tree: Tree)(using Context) =
    val symbol = tree.symbol.maybeOwner
    symbol.name.decoded == tpnme.valueOf.decoded.capitalize

object TypeParameters:
  def unapply(tree: Tree)(using params: InlayHintsParams, ctx: Context) =
    if params.typeParameters() then
      tree match
        case TypeApply(sel: Select, _)
            if sel.isForComprehensionMethod || sel.isInfix ||
              sel.symbol.name == nme.unapply =>
          None
        case TypeApply(fun, args) if inferredTypeArgs(args) =>
          val tpes = args.map(_.tpe.stripTypeVar.widen.finalResultType)
          Some((tpes, fun.sourcePos.endPos, fun))
        case _ => None
    else None

  private def inferredTypeArgs(args: List[Tree]): Boolean =
    args.forall {
      case tt: TypeTree if tt.span.exists && !tt.span.isZeroExtent => true
      case _ => false
    }

object InferredType:
  opaque type Text = Array[Char]
  object Text:
    def apply(text: Array[Char]): Text = text

  def unapply(tree: Tree)(using params: InlayHintsParams, text: Text, ctx: Context) =
    if params.inferredTypes() then
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
              Ident(nme.WILDCARD)
            ) if !bd.span.isZeroExtent && bd.symbol.isTerm && params.hintsInPatternMatch() =>
          Some(bd.symbol.info, bd.namePos, bd)
        case _ => None
    else None

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

object Parameters:
  def unapply(tree: Tree)(
      using params: InlayHintsParams,
      ctx: Context
  ): Option[(Boolean, List[(Name, SourcePosition, Boolean)])] =
    def shouldSkipFun(fun: Tree)(using Context): Boolean =
      fun match
        case sel: Select =>
          isForComprehensionMethod(sel) || sel.symbol.name == nme.unapply || sel.symbol.is(Flags.JavaDefined)
        case _ => false

    def isInfixFun(fun: Tree, args: List[Tree])(using Context): Boolean =
      val isInfixSelect = fun match
        case Select(sel, _) => sel.isInfix
        case _ => false
      val source = fun.source
      if args.isEmpty then isInfixSelect
      else
        (!(fun.span.end until args.head.span.start)
          .map(source.apply)
          .contains('.') && fun.symbol.is(Flags.ExtensionMethod)) || isInfixSelect

    def isRealApply(tree: Tree) =
      !tree.symbol.isOneOf(Flags.GivenOrImplicit) && !tree.span.isZeroExtent

    def getUnderlyingFun(tree: Tree): Tree =
      tree match
        case Apply(fun, _) => getUnderlyingFun(fun)
        case TypeApply(fun, _) => getUnderlyingFun(fun)
        case t => t

    @tailrec
    def isDefaultArg(arg: Tree): Boolean = arg match
      case Ident(name) => name.is(DefaultGetterName)
      case Select(_, name) => name.is(DefaultGetterName)
      case Apply(fun, _) => isDefaultArg(fun)
      case _ => false

    if params.namedParameters() || params.byNameParameters() then
      tree match
        case Apply(fun, args) if isRealApply(fun) =>
          val underlyingFun = getUnderlyingFun(fun)
          if shouldSkipFun(underlyingFun) then
            None
          else
            val funTp = fun.typeOpt.widenTermRefExpr
            val paramNames = funTp.paramNamess.flatten
            val paramInfos = funTp.paramInfoss.flatten

            Some(
              isInfixFun(fun, args) || underlyingFun.isInfix,
              args
                .zip(paramNames)
                .zip(paramInfos)
                .collect {
                  case ((arg, paramName), paramInfo) if !arg.span.isZeroExtent && !isDefaultArg(arg) =>
                    (paramName.fieldName, arg.sourcePos, paramInfo.isByName)
                }
            )
        case _ => None
    else None
end Parameters

object XRayModeHint:
  def unapply(trees: (Tree, Option[Tree]))(
      using params: InlayHintsParams,
      ctx: Context
  ): Option[(Type, SourcePosition)] =
    if params.hintsXRayMode() then
      val (tree, parent) = trees
      val isParentApply = parent match
        case Some(_: Apply) => true
        case _ => false
      val isParentOnSameLine = parent match
        case Some(sel: Select) if sel.isForComprehensionMethod => false
        case Some(par) if par.sourcePos.exists && par.sourcePos.line == tree.sourcePos.line => true
        case _ => false

      tree match
        /*
        anotherTree
         .innerSelect()
         */
        case apply @ Apply(inner, _)
            if !apply.span.isZeroExtent && inner.sourcePos.exists && !isParentOnSameLine &&
              !isParentApply && endsInSimpleSelect(apply) && isEndOfLine(tree.sourcePos) =>
          Some((apply.tpe.widen.finalResultType.deepDealiasAndSimplify, tree.sourcePos))
        /*
        innerTree
         .select
         */
        case select @ Select(innerTree, _)
            if !select.span.isZeroExtent && innerTree.sourcePos.exists &&
              !isParentOnSameLine && !isParentApply && isEndOfLine(tree.sourcePos) =>
          val tpe = parent match
            case Some(ta: TypeApply) => ta.tpe
            case _ => select.tpe
          Some((tpe.widen.finalResultType.deepDealiasAndSimplify, tree.sourcePos))
        case _ => None
    else None

  @tailrec
  private def endsInSimpleSelect(ap: Tree)(using ctx: Context): Boolean =
    ap match
      case Apply(sel: Select, _) =>
        sel.name != nme.apply && !isInfix(sel)
      case Apply(TypeApply(sel: Select, _), _) =>
        sel.name != nme.apply && !isInfix(sel)
      case Apply(innerTree @ Apply(_, _), _) =>
        endsInSimpleSelect(innerTree)
      case _ => false

  private def isEndOfLine(pos: SourcePosition): Boolean =
    if pos.exists then
      val source = pos.source
      val end = pos.end
      end >= source.length || source(end) == '\n' || source(end) == '\r'
    else false

end XRayModeHint

object ClosingLabel:
  def unapply(tree: Tree)(
      using params: InlayHintsParams,
      ctx: Context
  ): Option[(String, SourcePosition)] =
    if params.closingLabels() then
      tree match
        case tree: DefDef if !isIgnored(tree.symbol) && endsWithBrace(tree) =>
          Some((tree.symbol.decodedName, tree.sourcePos))
        case tree: TypeDef if tree.isClassDef && endsWithBrace(tree) =>
          Some((tree.symbol.decodedName, tree.sourcePos))
        case _ => None
    else None

  private def isIgnored(sym: Symbol)(using Context): Boolean =
    sym.is(Flags.Synthetic) || sym.isConstructor || samePosAsOwner(sym)

  private def samePosAsOwner(sym: Symbol)(using Context): Boolean =
    val owner = sym.owner
    sym.span == owner.span

  private def endsWithBrace(tree: Tree)(using Context): Boolean =
    val pos = tree.sourcePos
    pos.exists && !pos.span.isZeroExtent && pos.source(pos.end - 1) == '}'
end ClosingLabel
