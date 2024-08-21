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
) extends PcCollector[l.TextEdit](driver, params):
  private val forbiddenMethods =
    Set("equals", "hashCode", "unapply", "unary_!", "!")
  def canRenameSymbol(sym: Symbol)(using Context): Boolean =
    (!sym.is(Method) || !forbiddenMethods(sym.decodedName))
      && (sym.ownersIterator.drop(1).exists(ow => ow.is(Method))
        || sym.source.path.isWorksheet)

  def prepareRename(): Option[l.Range] =
    soughtSymbols(path).flatMap((symbols, pos) =>
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
  end collect

  def rename(
  ): List[l.TextEdit] =
    val (symbols, _) = soughtSymbols(path).getOrElse(Set.empty, pos)
    if symbols.nonEmpty && symbols.forall(canRenameSymbol(_))
    then
      val res = result()
      res
    else Nil
  end rename
end PcRenameProvider
