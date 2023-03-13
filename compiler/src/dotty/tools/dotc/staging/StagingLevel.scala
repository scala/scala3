package dotty.tools.dotc.staging

import dotty.tools.dotc.ast.{TreeMapWithImplicits, tpd}
import dotty.tools.dotc.config.Printers.staging
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.util.Property

import scala.collection.mutable

object StagingLevel {

  /** A key to be used in a context property that caches the `levelOf` mapping */
  private val LevelOfKey = new Property.Key[mutable.HashMap[Symbol, Int]]

  /** Initial context for a StagingTransformer transformation. */
  def freshStagingLevelContext(using Context): Context =
    ctx.fresh.setProperty(LevelOfKey, new mutable.HashMap[Symbol, Int])

  /** The quotation level of the definition of the locally defined symbol */
  def levelOf(sym: Symbol)(using Context): Int =
    ctx.property(LevelOfKey).get.getOrElse(sym, 0)

  def removeLevelOf(sym: Symbol)(using Context): Unit =
    val levelOfMap = ctx.property(LevelOfKey).get
    levelOfMap -= sym

  /** Enter staging level of symbol defined by `tree` */
  def markSymbol(sym: Symbol)(using Context): Boolean =
    val levelOfMap = ctx.property(LevelOfKey).get
    if level != 0 && !levelOfMap.contains(sym) then
      levelOfMap(sym) = level
      true
    else
      false
}