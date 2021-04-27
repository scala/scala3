package dotty.tools.dotc
package transform
package init

import core._
import Contexts._
import Types._
import Symbols._
import Decorators._

import Effects._, Potentials._, Summary._

given theCtx(using Env): Context = summon[Env].ctx

case class Env(ctx: Context, cache: Cache) {
  private implicit def self: Env = this

  /** Can the method call be ignored? */
  def canIgnoreMethod(symbol: Symbol): Boolean =
    !symbol.exists || // possible with outer selection, tests/init/crash/i1990b.scala
    canIgnoreClass(symbol.owner)

  def canIgnoreClass(cls: Symbol): Boolean =
    cls == defn.AnyClass ||
    cls == defn.AnyValClass ||
    cls == defn.ObjectClass

  def withCtx(newCtx: Context): Env = this.copy(ctx = newCtx)

  def withOwner(owner: Symbol) = this.copy(ctx = this.ctx.withOwner(owner))

  /** Whether values of a given type is always fully initialized?
   *
   *  It's true for primitive values
   */
  def isAlwaysInitialized(tp: Type)(implicit env: Env): Boolean = {
    val sym = tp.widen.finalResultType.typeSymbol
    sym.isPrimitiveValueClass || sym == defn.StringClass
  }

  def summaryOf(cls: ClassSymbol): ClassSummary = cache.summaryOf(cls)

  def resolveOuter(warm: Warm, cls: ClassSymbol): Potentials = cache.resolveOuter(warm, cls)
}
