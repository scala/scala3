package dotty.tools.pc

import scala.meta.internal.pc.Definition
import scala.meta.internal.pc.InlineValueProvider
import scala.meta.internal.pc.InlineValueProvider.Errors
import scala.meta.internal.pc.RangeOffset
import scala.meta.internal.pc.Reference
import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*
import dotty.tools.pc.IndexedContext.Result

import org.eclipse.lsp4j as l

final class PcInlineValueProvider(
    driver: InteractiveDriver,
    val params: OffsetParams
) extends WithSymbolSearchCollector[Option[Occurence]](driver, params):

  // We return a result or an error
  def getInlineTextEdits(): Either[String, List[l.TextEdit]] =
    defAndRefs() match {
      case Right((defn, refs)) =>
        val edits =
          if (defn.shouldBeRemoved) {
            val defEdit = definitionTextEdit(defn)
            val refsEdits = refs.map(referenceTextEdit(defn))
            defEdit :: refsEdits
          } else refs.map(referenceTextEdit(defn))
        Right(edits)
      case Left(error) => Left(error)
    }

  private def referenceTextEdit(
      definition: Definition
  )(ref: Reference): l.TextEdit =
    if (definition.requiresBrackets && ref.requiresBrackets)
      new l.TextEdit(
        ref.range,
        s"""(${ref.rhs})"""
      )
    else new l.TextEdit(ref.range, ref.rhs)

  private def definitionTextEdit(definition: Definition): l.TextEdit =
    new l.TextEdit(
      extend(
        definition.rangeOffsets.start,
        definition.rangeOffsets.end,
        definition.range
      ),
      ""
    )

  private def extend(
      startOffset: Int,
      endOffset: Int,
      range: l.Range
  ): l.Range = {
    val (startWithSpace, endWithSpace): (Int, Int) =
      extendRangeToIncludeWhiteCharsAndTheFollowingNewLine(
        text
      )(startOffset, endOffset)
    val startPos = new l.Position(
      range.getStart.getLine,
      range.getStart.getCharacter - (startOffset - startWithSpace)
    )
    val endPos =
      if (endWithSpace - 1 >= 0 && text(endWithSpace - 1) == '\n')
        new l.Position(range.getEnd.getLine + 1, 0)
      else
        new l.Position(
          range.getEnd.getLine,
          range.getEnd.getCharacter + endWithSpace - endOffset
        )

    new l.Range(startPos, endPos)
  }

  val position: l.Position = pos.toLsp.getStart().nn

  override def collect(parent: Option[Tree])(
      tree: Tree | EndMarker,
      pos: SourcePosition,
      sym: Option[Symbol]
  ): Option[Occurence] =
    tree match
      case tree: Tree =>
        val (adjustedPos, _) = pos.adjust(text)
        Some(Occurence(tree, parent, adjustedPos))
      case _ => None

  def defAndRefs(): Either[String, (Definition, List[Reference])] =
    val newctx = driver.currentCtx.fresh.setCompilationUnit(unit)
    val allOccurences = result().flatten
    for
      definition <- allOccurences
        .collectFirst { case Occurence(defn: ValDef, _, pos) =>
          DefinitionTree(defn, pos)
        }
        .toRight(Errors.didNotFindDefinition)
      path = Interactive.pathTo(unit.tpdTree, definition.tree.rhs.span)(using newctx)
      indexedContext = IndexedContext(Interactive.contextOfPath(path)(using newctx))
      symbols = symbolsUsedInDefn(definition.tree.rhs).filter(indexedContext.lookupSym(_) == Result.InScope)
      references <- getReferencesToInline(definition, allOccurences, symbols)
    yield
      val (deleteDefinition, refsEdits) = references

      val defPos = definition.tree.sourcePos
      val defEdit = Definition(
        defPos.toLsp,
        RangeOffset(defPos.start, defPos.end),
        definitionRequiresBrackets(definition.tree.rhs)(using newctx),
        deleteDefinition
      )

      (defEdit, refsEdits)
    end for
  end defAndRefs

  private def stripIndentPrefix(rhs: String, refIndent: String, defIndent: String): String =
    val rhsLines = rhs.split("\n").toList
    rhsLines match
      case h :: Nil => rhs
      case h :: t =>
        val header = if h.startsWith("{") then h else "\n" ++ refIndent ++ "  " ++ h
        header ++ t.map(refIndent ++ _.stripPrefix(defIndent)).mkString("\n", "\n", "")
      case Nil => rhs

  private def definitionRequiresBrackets(tree: Tree)(using Context): Boolean =
    NavigateAST
      .untypedPath(tree.span)
      .headOption
      .map {
        case _: untpd.If => true
        case _: untpd.Function => true
        case _: untpd.Match => true
        case _: untpd.ForYield => true
        case _: untpd.InfixOp => true
        case _: untpd.ParsedTry => true
        case _: untpd.Try => true
        case _: untpd.Block => true
        case _: untpd.Typed => true
        case _ => false
      }
      .getOrElse(false)

  end definitionRequiresBrackets

  private def referenceRequiresBrackets(tree: Tree)(using Context): Boolean =
    NavigateAST.untypedPath(tree.span) match
      case (_: untpd.InfixOp) :: _ => true
      case _ =>
        tree match
          case _: Apply => StdNames.nme.raw.isUnary(tree.symbol.name)
          case _: Select => true
          case _: Ident => true
          case _ => false

  end referenceRequiresBrackets

  private def extendWithSurroundingParens(pos: SourcePosition) =
    /** Move `point` by `step` as long as the character at `point` is `acceptedChar` */
    def extend(point: Int, acceptedChar: Char, step: Int): Int =
      val newPoint = point + step
      if newPoint > 0 && newPoint < text.length &&
        text(newPoint) == acceptedChar
      then extend(newPoint, acceptedChar, step)
      else point
    val adjustedStart = extend(pos.start, '(', -1)
    val adjustedEnd = extend(pos.end - 1, ')', 1) + 1
    text.slice(adjustedStart, adjustedEnd).mkString

  private def symbolsUsedInDefn(rhs: Tree): Set[Symbol] =
    def collectNames(
        symbols: Set[Symbol],
        tree: Tree
    ): Set[Symbol] =
      tree match
        case id: (Ident | Select)
            if !id.symbol.is(Synthetic) && !id.symbol.is(Implicit) =>
          symbols + tree.symbol
        case _ => symbols

    val traverser = new DeepFolder[Set[Symbol]](collectNames)
    traverser(Set(), rhs)
  end symbolsUsedInDefn

  private def getReferencesToInline(
      definition: DefinitionTree,
      allOccurences: List[Occurence],
      symbols: Set[Symbol]
  ): Either[String, (Boolean, List[Reference])] =
    val defIsLocal = definition.tree.symbol.ownersIterator
      .drop(1)
      .exists(e => e.isTerm)
    def allreferences = allOccurences.filterNot(_.isDefn)
    def inlineAll() =
      makeRefsEdits(allreferences, symbols, definition).map((true, _))
    if definition.tree.sourcePos.toLsp.encloses(position)
    then if defIsLocal then inlineAll() else Left(Errors.notLocal)
    else
      allreferences match
        case ref :: Nil if defIsLocal => inlineAll()
        case list =>
          for
            ref <- list
              .find(_.pos.toLsp.encloses(position))
              .toRight(Errors.didNotFindReference)
            refEdits <- makeRefsEdits(List(ref), symbols, definition)
          yield (false, refEdits)
    end if
  end getReferencesToInline

  extension (pos: SourcePosition)
    def startColumnIndentPadding: String = {
      val source = pos.source
      val offset = pos.start
      var idx = source.startOfLine(offset)
      val pad = new StringBuilder
      while (idx != offset && idx < source.content().length && source.content()(idx).isWhitespace) {
        pad.append(source.content()(idx))
        idx += 1
      }
      pad.result()
    }

  private def makeRefsEdits(
      refs: List[Occurence],
      symbols: Set[Symbol],
      definition: DefinitionTree
  ): Either[String, List[Reference]] =
    val newctx = driver.currentCtx.fresh.setCompilationUnit(unit)
    def buildRef(occurrence: Occurence): Either[String, Reference] =
      val path =
        Interactive.pathTo(unit.tpdTree, occurrence.pos.span)(using newctx)
      val indexedContext = IndexedContext(
        Interactive.contextOfPath(path)(using newctx)
      )
      import indexedContext.ctx
      val conflictingSymbols = symbols
        .withFilter {
          indexedContext.lookupSym(_) match
            case IndexedContext.Result.Conflict => true
            case _ => false
        }
        .map(_.fullNameBackticked)
      if conflictingSymbols.isEmpty then
        Right(
          Reference(
            occurrence.pos.toLsp,
            stripIndentPrefix(
              extendWithSurroundingParens(definition.tree.rhs.sourcePos),
              occurrence.tree.startPos.startColumnIndentPadding,
              definition.tree.startPos.startColumnIndentPadding
            ),
            occurrence.parent.map(p =>
              RangeOffset(p.sourcePos.start, p.sourcePos.end)
            ),
            occurrence.parent
              .map(p => referenceRequiresBrackets(p)(using newctx))
              .getOrElse(false)
          )
        )
      else Left(Errors.variablesAreShadowed(conflictingSymbols.mkString(", ")))
    end buildRef
    refs.foldLeft((Right(List())): Either[String, List[Reference]])((acc, r) =>
      for
        collectedEdits <- acc
        currentEdit <- buildRef(r)
      yield currentEdit :: collectedEdits
    )
  end makeRefsEdits

end PcInlineValueProvider

case class Occurence(tree: Tree, parent: Option[Tree], pos: SourcePosition):
  def isDefn =
    tree match
      case _: ValDef => true
      case _ => false

case class DefinitionTree(tree: ValDef, pos: SourcePosition)

case class RangeOffset(start: Int, end: Int)

case class Definition(
    range: l.Range,
    rangeOffsets: RangeOffset,
    requiresBrackets: Boolean,
    shouldBeRemoved: Boolean
)

case class Reference(
    range: l.Range,
    rhs: String,
    parentOffsets: Option[RangeOffset],
    requiresBrackets: Boolean
)
