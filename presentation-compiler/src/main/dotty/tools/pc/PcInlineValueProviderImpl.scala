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
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l

final class PcInlineValueProviderImpl(
    driver: InteractiveDriver,
    val params: OffsetParams
) extends WithSymbolSearchCollector[Option[Occurence]](driver, params)
    with InlineValueProvider:

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

  override def defAndRefs(): Either[String, (Definition, List[Reference])] =
    val newctx = driver.currentCtx.fresh.setCompilationUnit(unit)
    val allOccurences = result().flatten
    for
      definition <- allOccurences
        .collectFirst { case Occurence(defn: ValDef, _, pos) =>
          DefinitionTree(defn, pos)
        }
        .toRight(Errors.didNotFindDefinition)
      symbols = symbolsUsedInDefn(definition.tree.rhs)
      references <- getReferencesToInline(definition, allOccurences, symbols)
    yield
      val (deleteDefinition, refsEdits) = references

      val defPos = definition.tree.sourcePos
      val defEdit = Definition(
        defPos.toLsp,
        adjustRhs(definition.tree.rhs.sourcePos),
        RangeOffset(defPos.start, defPos.end),
        definitionRequiresBrackets(definition.tree.rhs)(using newctx),
        deleteDefinition
      )

      (defEdit, refsEdits)
    end for
  end defAndRefs

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

  private def adjustRhs(pos: SourcePosition) =
    def extend(point: Int, acceptedChar: Char, step: Int): Int =
      val newPoint = point + step
      if newPoint > 0 && newPoint < text.length && text(
          newPoint
        ) == acceptedChar
      then extend(newPoint, acceptedChar, step)
      else point
    val adjustedStart = extend(pos.start, '(', -1)
    val adjustedEnd = extend(pos.end - 1, ')', 1) + 1
    text.slice(adjustedStart, adjustedEnd).mkString

  private def symbolsUsedInDefn(
      rhs: Tree
  ): List[Symbol] =
    def collectNames(
        symbols: List[Symbol],
        tree: Tree
    ): List[Symbol] =
      tree match
        case id: (Ident | Select)
            if !id.symbol.is(Synthetic) && !id.symbol.is(Implicit) =>
          tree.symbol :: symbols
        case _ => symbols

    val traverser = new DeepFolder[List[Symbol]](collectNames)
    traverser(List(), rhs)
  end symbolsUsedInDefn

  private def getReferencesToInline(
      definition: DefinitionTree,
      allOccurences: List[Occurence],
      symbols: List[Symbol]
  ): Either[String, (Boolean, List[Reference])] =
    val defIsLocal = definition.tree.symbol.ownersIterator
      .drop(1)
      .exists(e => e.isTerm)
    def allreferences = allOccurences.filterNot(_.isDefn)
    def inlineAll() =
      makeRefsEdits(allreferences, symbols).map((true, _))
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
            refEdits <- makeRefsEdits(List(ref), symbols)
          yield (false, refEdits)
    end if
  end getReferencesToInline

  private def makeRefsEdits(
      refs: List[Occurence],
      symbols: List[Symbol]
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

end PcInlineValueProviderImpl

case class Occurence(tree: Tree, parent: Option[Tree], pos: SourcePosition):
  def isDefn =
    tree match
      case _: ValDef => true
      case _ => false

case class DefinitionTree(tree: ValDef, pos: SourcePosition)
