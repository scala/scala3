package dotty.tools.dotc
package staging

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.SrcPos

import scala.collection.mutable

object StagingLevel {

  /** A key to be used in a context property that caches the `levelOf` mapping */
  private val LevelOfKey = new Property.Key[Map[Symbol, Int]]

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
