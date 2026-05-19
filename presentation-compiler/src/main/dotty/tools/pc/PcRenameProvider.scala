package dotty.tools.pc

import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l

final class PcRenameProvider(
    driver: InteractiveDriver,
    params: OffsetParams,
    name: Option[String]
) extends WithSymbolSearchCollector[l.TextEdit](driver, params):
  private val forbiddenMethods =
    Set("equals", "hashCode", "unapply", "apply", "<init>", "unary_!", "!")

  private val soughtSymbolNames = soughtSymbols match
    case Some((symbols, _)) =>
      symbols.filterNot(_.isError).map(symbol => symbol.decodedName.toString)
    case None => Set.empty[String]

  def canRenameSymbol(sym: Symbol)(using Context): Boolean =
    val decodedName = sym.decodedName
    def isForbiddenMethod = sym.is(Method) && forbiddenMethods(decodedName)
    def local = sym.ownersIterator.drop(1).exists(ow => ow.is(Method))
    def isInWorksheet = sym.source.path.isWorksheet
    !isForbiddenMethod && (local || isInWorksheet) && soughtSymbolNames(decodedName)

  def prepareRename(): Option[l.Range] =
    soughtSymbols.flatMap((symbols, pos) =>
      if symbols.forall(canRenameSymbol) then Some(pos.toLsp)
      else None
    )

  val newName = name.map(_.stripBackticks.backticked).getOrElse("newName")

  def collect(
      parent: Option[Tree]
  )(tree: Tree | EndMarker, toAdjust: SourcePosition, sym: Option[Symbol]): l.TextEdit =
    val (pos, stripBackticks) = toAdjust.adjust(text, forRename = true)
    l.TextEdit(
      pos.toLsp,
      if stripBackticks then newName.stripBackticks else newName
    )

  def rename(): List[l.TextEdit] =
    val (symbols, _) = soughtSymbols.getOrElse(Set.empty, pos)
    if symbols.nonEmpty && symbols.forall(canRenameSymbol(_))
    then result()
    else Nil
end PcRenameProvider
