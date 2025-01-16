package dotty.tools.dotc
package staging

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Property


object StagingLevel {

  /** A key to be used in a context property that tracks the staging level */
  private val LevelKey = new Property.Key[Int]

  /** A key to be used in a context property that caches the `levelOf` mapping */
  private val LevelOfKey = new Property.Key[Map[Symbol, Int]]

  /** All enclosing calls that are currently inlined, from innermost to outermost. */
  def level(using Context): Int =
    ctx.property(LevelKey).getOrElse(0)

  /** Context with an incremented staging level. */
  def quoteContext(using Context): FreshContext =
    ctx.fresh.setProperty(LevelKey, level + 1)

  /** Context with a decremented staging level. */
  def spliceContext(using Context): FreshContext =
    ctx.fresh.setProperty(LevelKey, level - 1)

  /** If we are inside a quote or a splice */
  def inQuoteOrSpliceScope(using Context): Boolean =
    ctx.property(LevelKey).isDefined

  /** The quotation level of the definition of the locally defined symbol */
  def levelOf(sym: Symbol)(using Context): Int =
    ctx.property(LevelOfKey) match
      case Some(map) => map.getOrElse(sym, 0)
      case None => 0

  /** Context with the current staging level set for the symbols */
  def symbolsInCurrentLevel(syms: List[Symbol])(using Context): Context =
    if level == 0 then ctx
    else
      val levelOfMap = ctx.property(LevelOfKey).getOrElse(Map.empty)
      val syms1 = syms//.filter(sym => !levelOfMap.contains(sym))
      val newMap = syms1.foldLeft(levelOfMap)((acc, sym) => acc.updated(sym, level))
      ctx.fresh.setProperty(LevelOfKey, newMap)
}
