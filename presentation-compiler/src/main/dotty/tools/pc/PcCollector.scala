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
import dotty.tools.pc.PcSymbolSearch.*
import dotty.tools.pc.utils.InteractiveEnrichments.*

trait PcCollector[T]:
  self: WithCompilationUnit =>
  def collect(
      parent: Option[Tree]
  )(tree: Tree | EndMarker, pos: SourcePosition, symbol: Option[Symbol]): T

  def allowZeroExtentImplicits: Boolean = false

  def resultAllOccurences(): Set[T] =
    def noTreeFilter = (_: Tree) => true
    def noSoughtFilter = (_: Symbol => Boolean) => true

    traverseSought(noTreeFilter, noSoughtFilter)

  def resultWithSought(sought: Set[Symbol]): List[T] =
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

    def soughtOrOverride(sym0: Symbol) =
      val sym = if sym0.is(Flags.Exported) then sym0.sourceSymbol else sym0
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
        case imp: ImportOrExport if owners(imp.expr.symbol) => true
        case _ => false

    def soughtFilter(f: Symbol => Boolean): Boolean =
      sought.exists(f)

    traverseSought(soughtTreeFilter, soughtFilter).toList
  end resultWithSought

  extension (span: Span)
    def isCorrect =
      !span.isZeroExtent && span.exists && span.start < sourceText.size && span.end <= sourceText.size

  extension (tree: Tree)
    def isCorrectSpan =
      tree.span.isCorrect || (allowZeroExtentImplicits && tree.symbol.is(Flags.Implicit))

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
        /** All indentifiers such as:
         *  ```
         *  val a = <<b>>
         *  ```
         */
        case ident: Ident if ident.isCorrectSpan && filter(ident) =>
          // symbols will differ for params in different ext methods, but source pos will be the same
          val symbol = if ident.symbol.is(Flags.Exported) then ident.symbol.sourceSymbol else ident.symbol
          if soughtFilter(_.sourcePos == symbol.sourcePos)
          then
            occurrences + collect(
              ident,
              ident.sourcePos,
              Some(symbol)
            )
          else occurrences

        /** Workaround for missing symbol in:
         *  ```
         *  class A[T](a: T) val x = new <<A>>(1)
         *  ```
         */
        case sel @ Select(New(t), _)
            if sel.isCorrectSpan &&
              sel.symbol.isConstructor &&
              t.symbol == NoSymbol =>
          if soughtFilter(_ == sel.symbol.owner) then
            occurrences + collect(
              sel,
              namePos(t),
              Some(sel.symbol.owner)
            )
          else occurrences

        /** All select statements such as:
         *  ```
         *  val a = hello.<<b>>
         *  ```
         */
        case sel: Select
            if sel.isCorrectSpan && filter(sel) &&
              !sel.isForComprehensionMethod =>
          occurrences + collect(
            sel,
            pos.withSpan(selectNameSpan(sel))
          )
        /* all definitions:
         *  ```
         *  def <<foo>> = ???
         *  class <<Foo>> = ???
         *  ```
         * etc.
         */
        case df: NamedDefTree
            if df.span.isCorrect && df.nameSpan.isCorrect &&
              filter(df) && !isGeneratedGiven(df, sourceText) =>
          def collectEndMarker =
            EndMarker.getPosition(df, pos, sourceText).map:
              collect(EndMarker(df.symbol), _)
          val annots = collectTrees(df.symbol.annotations.map(_.tree))
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
         *  ```
         *  foo(<<name>> = "abc")
         *  User(<<name>> = "abc")
         *  ```
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

        /** ```
         *  @<<JsonNotification>>("")
         *  def params() = ???
         *  ```
         */
        case mdf: MemberDef if mdf.symbol.annotations.nonEmpty =>
          val trees = collectTrees(mdf.symbol.annotations.map(_.tree))
          val traverser =
            new PcCollector.DeepFolderWithParent[Set[T]](
              collectNamesWithParent
            )
          trees.foldLeft(occurrences) { case (set, tree) =>
            traverser(set, tree)
          }

        /** For traversing import selectors: import scala.util.<<Try>>
         */
        case imp: ImportOrExport if filter(imp) =>
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
    traverser(Set.empty[T], unit.tpdTree)
  end traverseSought

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

case class ExtensionParamOccurence(
    name: Name,
    pos: SourcePosition,
    sym: Symbol,
    methods: List[untpd.Tree]
)

case class EndMarker(symbol: Symbol)

object EndMarker:
  /** Matches end marker line from start to the name's beginning. E.g. end
   *  ```
   *  end /*
   *  some comment */
   *  ```
   */
  private val endMarkerRegex = """.*end(/\*.*\*/|\s)+""".r
  def getPosition(df: NamedDefTree, pos: SourcePosition, sourceText: String)(
      implicit ct: Context
  ): Option[SourcePosition] =
    val name = df.name.toString().stripSuffix("$")
    val lines = sourceText.slice(df.span.start, df.span.end).split('\n')

    if lines.nonEmpty then
      val endMarkerLine = lines.last
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
    else None

abstract class WithSymbolSearchCollector[T](
    driver: InteractiveDriver,
    params: OffsetParams
) extends WithCompilationUnit(driver, params)
    with PcSymbolSearch
    with PcCollector[T]:
  def result(): List[T] =
    soughtSymbols.toList.flatMap { case (sought, _) =>
      resultWithSought(sought)
    }

abstract class SimpleCollector[T](
    driver: InteractiveDriver,
    params: VirtualFileParams
) extends WithCompilationUnit(driver, params)
    with PcCollector[T]:
  def result(): List[T] = resultAllOccurences().toList
