package dotty.tools.dotc
package staging

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.SrcPos

import scala.collection.mutable

object StagingLevel {

  /** A key to be used in a context property that tracks the staging level */
  private val LevelKey = new Property.Key[Int]

  /** A key to be used in a context property that caches the `levelOf` mapping */
  private val LevelOfKey = new Property.Key[Map[Symbol, Int]]

  /** All enclosing calls that are currently inlined, from innermost to outermost. */
  def level(using Context): Int =
    val v = ctx.propertyRaw(LevelKey)
    if v == null then 0 else v

  /** Context with an incremented staging level. */
  def quoteContext(using Context): FreshContext =
    ctx.fresh.setProperty(LevelKey, level + 1)

  /** Context with a decremented staging level. */
  def spliceContext(using Context): FreshContext =
    ctx.fresh.setProperty(LevelKey, level - 1)

  /** If we are inside a quote or a splice */
  def inQuoteOrSpliceScope(using Context): Boolean =
    ctx.propertyRaw(LevelKey) != null

  /** The quotation level of the definition of the locally defined symbol */
  def levelOf(sym: Symbol)(using Context): Int =
    val map = ctx.propertyRaw(LevelOfKey)
    if map == null then 0 else map.getOrElse(sym, 0)

  /** Context with the current staging level set for the symbols */
  def symbolsInCurrentLevel(syms: List[Symbol])(using Context): Context =
    if level == 0 then ctx
    else
      val raw = ctx.propertyRaw(LevelOfKey)
      val levelOfMap: Map[Symbol, Int] = if raw == null then Map.empty else raw
      val syms1 = syms//.filter(sym => !levelOfMap.contains(sym))
      val newMap = syms1.foldLeft(levelOfMap)((acc, sym) => acc.updated(sym, level))
      ctx.fresh.setProperty(LevelOfKey, newMap)
}
