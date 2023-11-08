package dotty.tools.pc

import java.nio.file.Paths

import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.OffsetParams
import scala.meta.pc.VirtualFileParams
import scala.meta as m

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.ExtMethods
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.pc.utils.MtagsEnrichments.*

abstract class PcCollector[T](
    driver: InteractiveDriver,
    params: VirtualFileParams
):
  private val caseClassSynthetics: Set[Name] = Set(nme.apply, nme.copy)
  val uri = params.uri().nn
  val filePath = Paths.get(uri).nn
  val sourceText = params.text().nn
  val source =
    SourceFile.virtual(filePath.toString(), sourceText)
  driver.run(uri, source)
  given ctx: Context = driver.currentCtx

  val unit = driver.currentCtx.run.nn.units.head
  val compilatonUnitContext = ctx.fresh.setCompilationUnit(unit)
  val offset = params match
    case op: OffsetParams => op.offset()
    case _ => 0
  val offsetParams =
    params match
      case op: OffsetParams => op
      case _ => CompilerOffsetParams(uri, sourceText, 0, params.token().nn)
  val pos = driver.sourcePosition(offsetParams)
  val rawPath =
    Interactive
      .pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)
      .dropWhile(t => // NamedArg anyway doesn't have symbol
        t.symbol == NoSymbol && !t.isInstanceOf[NamedArg] ||
          // same issue https://github.com/lampepfl/dotty/issues/15937 as below
          t.isInstanceOf[TypeTree]
      )

  val path = rawPath match
    // For type it will sometimes go into the wrong tree since TypeTree also contains the same span
    // https://github.com/lampepfl/dotty/issues/15937
    case TypeApply(sel: Select, _) :: tail if sel.span.contains(pos.span) =>
      Interactive.pathTo(sel, pos.span) ::: rawPath
    case _ => rawPath
  def collect(
      parent: Option[Tree]
  )(tree: Tree| EndMarker, pos: SourcePosition, symbol: Option[Symbol]): T

  /**
   * @return (adjusted position, should strip backticks)
   */
  def adjust(
      pos1: SourcePosition,
      forRename: Boolean = false
  ): (SourcePosition, Boolean) =
    if !pos1.span.isCorrect then (pos1, false)
    else
      val pos0 =
        val span = pos1.span
        if span.exists && span.point > span.end then
          pos1.withSpan(
            span
              .withStart(span.point)
              .withEnd(span.point + (span.end - span.start))
          )
        else pos1

      val pos =
        if pos0.end > 0 && sourceText(pos0.end - 1) == ',' then
          pos0.withEnd(pos0.end - 1)
        else pos0
      val isBackticked =
        sourceText(pos.start) == '`' &&
          pos.end > 0 &&
          sourceText(pos.end - 1) == '`'
      // when the old name contains backticks, the position is incorrect
      val isOldNameBackticked = sourceText(pos.start) != '`' &&
        pos.start > 0 &&
        sourceText(pos.start - 1) == '`' &&
        sourceText(pos.end) == '`'
      if isBackticked && forRename then
        (pos.withStart(pos.start + 1).withEnd(pos.end - 1), true)
      else if isOldNameBackticked then
        (pos.withStart(pos.start - 1).withEnd(pos.end + 1), false)
      else (pos, false)
  end adjust

  def symbolAlternatives(sym: Symbol) =
    def member(parent: Symbol) = parent.info.member(sym.name).symbol
    def primaryConstructorTypeParam(owner: Symbol) =
      for
        typeParams <- owner.primaryConstructor.paramSymss.headOption
        param <- typeParams.find(_.name == sym.name)
        if (param.isType)
      yield param
    def additionalForEnumTypeParam(enumClass: Symbol) =
      if enumClass.is(Flags.Enum) then
        val enumOwner =
          if enumClass.is(Flags.Case)
          then
            Option.when(member(enumClass).is(Flags.Synthetic))(
              enumClass.maybeOwner.companionClass
            )
          else Some(enumClass)
        enumOwner.toSet.flatMap { enumOwner =>
          val symsInEnumCases = enumOwner.children.toSet.flatMap(enumCase =>
            if member(enumCase).is(Flags.Synthetic)
            then primaryConstructorTypeParam(enumCase)
            else None
          )
          val symsInEnumOwner =
            primaryConstructorTypeParam(enumOwner).toSet + member(enumOwner)
          symsInEnumCases ++ symsInEnumOwner
        }
      else Set.empty
    val all =
      if sym.is(Flags.ModuleClass) then
        Set(sym, sym.companionModule, sym.companionModule.companion)
      else if sym.isClass then
        Set(sym, sym.companionModule, sym.companion.moduleClass)
      else if sym.is(Flags.Module) then
        Set(sym, sym.companionClass, sym.moduleClass)
      else if sym.isTerm && (sym.owner.isClass || sym.owner.isConstructor)
      then
        val info =
          if sym.owner.isClass then sym.owner.info else sym.owner.owner.info
        Set(
          sym,
          info.member(sym.asTerm.name.setterName).symbol,
          info.member(sym.asTerm.name.getterName).symbol
        ) ++ sym.allOverriddenSymbols.toSet
      // type used in primary constructor will not match the one used in the class
      else if sym.isTypeParam && sym.owner.isPrimaryConstructor then
        Set(sym, member(sym.maybeOwner.maybeOwner))
          ++ additionalForEnumTypeParam(sym.maybeOwner.maybeOwner)
      else if sym.isTypeParam then
        primaryConstructorTypeParam(sym.maybeOwner).toSet
          ++ additionalForEnumTypeParam(sym.maybeOwner) + sym
      else Set(sym)
    all.filter(s => s != NoSymbol && !s.isError)
  end symbolAlternatives

  private def isGeneratedGiven(df: NamedDefTree)(using Context) =
    val nameSpan = df.nameSpan
    df.symbol.is(Flags.Given) && sourceText.substring(
      nameSpan.start,
      nameSpan.end
    ) != df.name.toString()

  // First identify the symbol we are at, comments identify @@ as current cursor position
  def soughtSymbols(path: List[Tree]): Option[(Set[Symbol], SourcePosition)] =
    val sought = path match
      /* reference of an extension paramter
       * extension [EF](<<xs>>: List[EF])
       *   def double(ys: List[EF]) = <<x@@s>> ++ ys
       */
      case (id: Ident) :: _
          if id.symbol
            .is(Flags.Param) && id.symbol.owner.is(Flags.ExtensionMethod) =>
        Some(findAllExtensionParamSymbols(id.sourcePos, id.name, id.symbol))
      /* simple identifier:
       * val a = val@@ue + value
       */
      case (id: Ident) :: _ =>
        Some(symbolAlternatives(id.symbol), id.sourcePos)
      /* simple selector:
       * object.val@@ue
       */
      case (sel: Select) :: _ if selectNameSpan(sel).contains(pos.span) =>
        Some(symbolAlternatives(sel.symbol), pos.withSpan(sel.nameSpan))
      /* named argument:
       * foo(nam@@e = "123")
       */
      case (arg: NamedArg) :: (appl: Apply) :: _ =>
        val realName = arg.name.stripModuleClassSuffix.lastPart
        if pos.span.start > arg.span.start && pos.span.end < arg.span.point + realName.length
        then
          val length = realName.toString.backticked.length()
          val pos = arg.sourcePos.withSpan(
            arg.span
              .withEnd(arg.span.start + length)
              .withPoint(arg.span.start)
          )
          appl.symbol.paramSymss.flatten.find(_.name == arg.name).map { s =>
            // if it's a case class we need to look for parameters also
            if caseClassSynthetics(s.owner.name) && s.owner.is(Flags.Synthetic)
            then
              (
                Set(
                  s,
                  s.owner.owner.companion.info.member(s.name).symbol,
                  s.owner.owner.info.member(s.name).symbol
                )
                  .filter(_ != NoSymbol),
                pos,
              )
            else (Set(s), pos)
          }
        else None
        end if
      /* all definitions:
       * def fo@@o = ???
       * class Fo@@o = ???
       * etc.
       */
      case (df: NamedDefTree) :: _
          if df.nameSpan.contains(pos.span) && !isGeneratedGiven(df) =>
        Some(symbolAlternatives(df.symbol), pos.withSpan(df.nameSpan))
      /* enum cases with params
       * enum Foo:
       *  case B@@ar[A](i: A)
       */
      case (df: NamedDefTree) :: Template(_, _, self, _) :: _
          if (df.name == nme.apply || df.name == nme.unapply) && df.nameSpan.isZeroExtent =>
        Some(symbolAlternatives(self.tpt.symbol), self.sourcePos)
      /**
       * For traversing annotations:
       * @JsonNo@@tification("")
       * def params() = ???
       */
      case (df: MemberDef) :: _ if df.span.contains(pos.span) =>
        val annotTree = df.mods.annotations.find { t =>
          t.span.contains(pos.span)
        }
        collectTrees(annotTree).flatMap { t =>
          soughtSymbols(
            Interactive.pathTo(t, pos.span)
          )
        }.headOption

      /* Import selectors:
       * import scala.util.Tr@@y
       */
      case (imp: Import) :: _ if imp.span.contains(pos.span) =>
        imp
          .selector(pos.span)
          .map(sym => (symbolAlternatives(sym), sym.sourcePos))

      case _ => None

    sought match
      case None => seekInExtensionParameters()
      case _ => sought

  end soughtSymbols

  lazy val extensionMethods =
    NavigateAST
      .untypedPath(pos.span)(using compilatonUnitContext)
      .collectFirst { case em @ ExtMethods(_, _) => em }

  private def findAllExtensionParamSymbols(
      pos: SourcePosition,
      name: Name,
      sym: Symbol
  ) =
    val symbols =
      for
        methods <- extensionMethods.map(_.methods)
        symbols <- collectAllExtensionParamSymbols(
          unit.tpdTree,
          ExtensionParamOccurence(name, pos, sym, methods)
        )
      yield symbols
    symbols.getOrElse((symbolAlternatives(sym), pos))
  end findAllExtensionParamSymbols

  private def seekInExtensionParameters() =
    def collectParams(
        extMethods: ExtMethods
    ): Option[ExtensionParamOccurence] =
      NavigateAST
        .pathTo(pos.span, extMethods.paramss.flatten)(using
          compilatonUnitContext
        )
        .collectFirst {
          case v: untpd.ValOrTypeDef =>
            ExtensionParamOccurence(
              v.name,
              v.namePos,
              v.symbol,
              extMethods.methods
            )
          case i: untpd.Ident =>
            ExtensionParamOccurence(
              i.name,
              i.sourcePos,
              i.symbol,
              extMethods.methods
            )
        }

    for
      extensionMethodScope <- extensionMethods
      occurrence <- collectParams(extensionMethodScope)
      symbols <- collectAllExtensionParamSymbols(
        path.headOption.getOrElse(unit.tpdTree),
        occurrence
      )
    yield symbols
  end seekInExtensionParameters

  private def collectAllExtensionParamSymbols(
      tree: tpd.Tree,
      occurrence: ExtensionParamOccurence
  ): Option[(Set[Symbol], SourcePosition)] =
    occurrence match
      case ExtensionParamOccurence(_, namePos, symbol, _)
          if symbol != NoSymbol && !symbol.isError && !symbol.owner.is(
            Flags.ExtensionMethod
          ) =>
        Some((symbolAlternatives(symbol), namePos))
      case ExtensionParamOccurence(name, namePos, _, methods) =>
        val symbols =
          for
            method <- methods.toSet
            symbol <-
              Interactive.pathTo(tree, method.span) match
                case (d: DefDef) :: _ =>
                  d.paramss.flatten.collect {
                    case param if param.name.decoded == name.decoded =>
                      param.symbol
                  }
                case _ => Set.empty[Symbol]
            if (symbol != NoSymbol && !symbol.isError)
            withAlt <- symbolAlternatives(symbol)
          yield withAlt
        if symbols.nonEmpty then Some((symbols, namePos)) else None
  end collectAllExtensionParamSymbols

  def result(): List[T] =
    params match
      case _: OffsetParams => resultWithSought()
      case _ => resultAllOccurences().toList

  def resultAllOccurences(): Set[T] =
    def noTreeFilter = (_: Tree) => true
    def noSoughtFilter = (_: Symbol => Boolean) => true

    traverseSought(noTreeFilter, noSoughtFilter)

  def resultWithSought(): List[T] =
    soughtSymbols(path) match
      case Some((sought, _)) =>
        lazy val owners = sought
          .flatMap { s => Set(s.owner, s.owner.companionModule) }
          .filter(_ != NoSymbol)
        lazy val soughtNames: Set[Name] = sought.map(_.name)

        /*
         * For comprehensions have two owners, one for the enumerators and one for
         * yield. This is a heuristic to find that out.
         */
        def isForComprehensionOwner(named: NameTree) =
          soughtNames(named.name) &&
            scala.util
              .Try(named.symbol.owner)
              .toOption
              .exists(_.isAnonymousFunction) &&
            owners.exists(o =>
              o.span.exists && o.span.point == named.symbol.owner.span.point
            )

        def soughtOrOverride(sym: Symbol) =
          sought(sym) || sym.allOverriddenSymbols.exists(sought(_))

        def soughtTreeFilter(tree: Tree): Boolean =
          tree match
            case ident: Ident
                if soughtOrOverride(ident.symbol) ||
                  isForComprehensionOwner(ident) =>
              true
            case sel: Select if soughtOrOverride(sel.symbol) => true
            case df: NamedDefTree
                if soughtOrOverride(df.symbol) && !df.symbol.isSetter =>
              true
            case imp: Import if owners(imp.expr.symbol) => true
            case _ => false

        def soughtFilter(f: Symbol => Boolean): Boolean =
          sought.exists(f)

        traverseSought(soughtTreeFilter, soughtFilter).toList

      case None => Nil

  extension (span: Span)
    def isCorrect =
      !span.isZeroExtent && span.exists && span.start < sourceText.size && span.end <= sourceText.size

  def traverseSought(
      filter: Tree => Boolean,
      soughtFilter: (Symbol => Boolean) => Boolean
  ): Set[T] =
    def collectNamesWithParent(
        occurrences: Set[T],
        tree: Tree,
        parent: Option[Tree]
    ): Set[T] =
      def collect(
          tree: Tree | EndMarker,
          pos: SourcePosition,
          symbol: Option[Symbol] = None
      ) =
        this.collect(parent)(tree, pos, symbol)
      tree match
        /**
         * All indentifiers such as:
         * val a = <<b>>
         */
        case ident: Ident if ident.span.isCorrect && filter(ident) =>
          // symbols will differ for params in different ext methods, but source pos will be the same
          if soughtFilter(_.sourcePos == ident.symbol.sourcePos)
          then
            occurrences + collect(
              ident,
              ident.sourcePos
            )
          else occurrences
        /**
         * All select statements such as:
         * val a = hello.<<b>>
         */
        case sel: Select
          if sel.span.isCorrect && filter(sel) &&
            !isForComprehensionMethod(sel) =>
          occurrences + collect(
            sel,
            pos.withSpan(selectNameSpan(sel))
          )
        /* all definitions:
         * def <<foo>> = ???
         * class <<Foo>> = ???
         * etc.
         */
        case df: NamedDefTree
            if df.span.isCorrect && df.nameSpan.isCorrect &&
              filter(df) && !isGeneratedGiven(df) =>
          def collectEndMarker =
            EndMarker.getPosition(df, pos, sourceText).map:
              collect(EndMarker(df.symbol), _)
          val annots = collectTrees(df.mods.annotations)
          val traverser =
            new PcCollector.DeepFolderWithParent[Set[T]](
              collectNamesWithParent
            )
          annots.foldLeft(
            occurrences + collect(
              df,
              pos.withSpan(df.nameSpan)
            ) ++ collectEndMarker
          ) { case (set, tree) =>
            traverser(set, tree)
          }

        /* Named parameters don't have symbol so we need to check the owner
         * foo(<<name>> = "abc")
         * User(<<name>> = "abc")
         * etc.
         */
        case apply: Apply =>
          val args: List[NamedArg] = apply.args.collect {
            case arg: NamedArg
                if soughtFilter(sym =>
                  sym.name == arg.name &&
                    // foo(name = "123") for normal params
                    (sym.owner == apply.symbol ||
                      // Bar(name = "123") for case class, copy and apply methods
                      apply.symbol.is(Flags.Synthetic) &&
                      (sym.owner == apply.symbol.owner.companion || sym.owner == apply.symbol.owner))
                ) =>
              arg
          }
          val named = args.map { arg =>
            val realName = arg.name.stripModuleClassSuffix.lastPart
            val length = realName.toString.backticked.length()
            val sym = apply.symbol.paramSymss.flatten
              .find(_.name == realName)
            collect(
              arg,
              pos
                .withSpan(
                  arg.span
                    .withEnd(arg.span.start + length)
                    .withPoint(arg.span.start)
                ),
              sym
            )
          }
          occurrences ++ named

        /**
         * For traversing annotations:
         * @<<JsonNotification>>("")
         * def params() = ???
         */
        case mdf: MemberDef if mdf.mods.annotations.nonEmpty =>
          val trees = collectTrees(mdf.mods.annotations)
          val traverser =
            new PcCollector.DeepFolderWithParent[Set[T]](
              collectNamesWithParent
            )
          trees.foldLeft(occurrences) { case (set, tree) =>
            traverser(set, tree)
          }
        /**
         * For traversing import selectors:
         * import scala.util.<<Try>>
         */
        case imp: Import if filter(imp) =>
          imp.selectors
            .collect {
              case sel: ImportSelector
                  if soughtFilter(_.decodedName == sel.name.decoded) =>
                // Show both rename and main together
                val spans =
                  if !sel.renamed.isEmpty then
                    Set(sel.renamed.span, sel.imported.span)
                  else Set(sel.imported.span)
                // See https://github.com/scalameta/metals/pull/5100
                val symbol = imp.expr.symbol.info.member(sel.name).symbol match
                  // We can get NoSymbol when we import "_", "*"", "given" or when the names don't match
                  // eg. "@@" doesn't match "$at$at".
                  // Then we try to find member based on decodedName
                  case NoSymbol =>
                    imp.expr.symbol.info.allMembers
                      .find(_.name.decoded == sel.name.decoded)
                      .map(_.symbol)
                      .getOrElse(NoSymbol)
                  case sym => sym
                spans.filter(_.isCorrect).map { span =>
                  collect(
                    imp,
                    pos.withSpan(span),
                    Some(symbol)
                  )
                }
            }
            .flatten
            .toSet ++ occurrences
        case inl: Inlined =>
          val traverser =
            new PcCollector.DeepFolderWithParent[Set[T]](
              collectNamesWithParent
            )
          val trees = inl.call :: inl.bindings
          trees.foldLeft(occurrences) { case (set, tree) =>
            traverser(set, tree)
          }
        case o =>
          occurrences
      end match
    end collectNamesWithParent

    val traverser =
      new PcCollector.DeepFolderWithParent[Set[T]](collectNamesWithParent)
    val all = traverser(Set.empty[T], unit.tpdTree)
    all
  end traverseSought

  // @note (tgodzik) Not sure currently how to get rid of the warning, but looks to correctly
  // @nowarn
  private def collectTrees(trees: Iterable[Positioned]): Iterable[Tree] =
    trees.collect { case t: Tree =>
      t
    }

  // NOTE: Connected to https://github.com/lampepfl/dotty/issues/16771
  // `sel.nameSpan` is calculated incorrectly in (1 + 2).toString
  // See test DocumentHighlightSuite.select-parentheses
  private def selectNameSpan(sel: Select): Span =
    val span = sel.span
    if span.exists then
      val point = span.point
      if sel.name.toTermName == nme.ERROR then Span(point)
      else if sel.qualifier.span.start > span.point then // right associative
        val realName = sel.name.stripModuleClassSuffix.lastPart
        Span(span.start, span.start + realName.length, point)
      else Span(point, span.end, point)
    else span

  private val forCompMethods =
    Set(nme.map, nme.flatMap, nme.withFilter, nme.foreach)

  // We don't want to collect synthethic `map`, `withFilter`, `foreach` and `flatMap` in for-comprenhensions
  private def isForComprehensionMethod(sel: Select): Boolean =
    val syntheticName = sel.name match
      case name: TermName => forCompMethods(name)
      case _ => false
    val wrongSpan = sel.qualifier.span.contains(sel.nameSpan)
    syntheticName && wrongSpan
end PcCollector

object PcCollector:
  private class WithParentTraverser[X](f: (X, Tree, Option[Tree]) => X)
      extends TreeAccumulator[List[Tree]]:
    def apply(x: List[Tree], tree: Tree)(using Context): List[Tree] = tree :: x
    def traverse(acc: X, tree: Tree, parent: Option[Tree])(using Context): X =
      val res = f(acc, tree, parent)
      val children = foldOver(Nil, tree).reverse
      children.foldLeft(res)((a, c) => traverse(a, c, Some(tree)))

  // Folds over the tree as `DeepFolder` but `f` takes also the parent.
  class DeepFolderWithParent[X](f: (X, Tree, Option[Tree]) => X):
    private val traverser = WithParentTraverser[X](f)
    def apply(x: X, tree: Tree)(using Context) =
      traverser.traverse(x, tree, None)
end PcCollector

case class ExtensionParamOccurence(
    name: Name,
    pos: SourcePosition,
    sym: Symbol,
    methods: List[untpd.Tree]
)

case class EndMarker(symbol: Symbol)

object EndMarker:
  /**
    * Matches end marker line from start to the name's beginning.
    * E.g.
    *    end /* some comment */
    */
  private val endMarkerRegex = """.*end(/\*.*\*/|\s)+""".r
  def getPosition(df: NamedDefTree, pos: SourcePosition, sourceText: String)(
      implicit ct: Context
  ): Option[SourcePosition] =
    val name = df.name.toString()
    val endMarkerLine =
      sourceText.slice(df.span.start, df.span.end).split('\n').last
    val index = endMarkerLine.length() - name.length()
    if index < 0 then None
    else
      val (possiblyEndMarker, possiblyEndMarkerName) =
        endMarkerLine.splitAt(index)
      Option.when(
        possiblyEndMarkerName == name &&
          endMarkerRegex.matches(possiblyEndMarker)
      )(
        pos
          .withStart(df.span.end - name.length())
          .withEnd(df.span.end)
      )
  end getPosition
end EndMarker
