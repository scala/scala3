package dotty.tools
package dotc
package transform

import core.*
import Flags.*
import MegaPhase.*
import Symbols.*, Contexts.*, Types.*, Decorators.*
import StdNames.nme
import ast.TreeTypeMap
import Constants.Constant

import scala.collection.mutable.ListBuffer

/** Devirtualize method calls.
 *
 *  If we have a `x.m` for `x: X` where `X <: T` and `m` is a member of `T`
 *  that is overwritten in `X`, then we can replace `x.m` with `X.m`.
 */
class Devirtualize extends MiniPhase:
  import ast.tpd.*

  override def phaseName: String = Devirtualize.name

  override def description: String = Devirtualize.description

  override def transformSelect(tree: Select)(using Context): Tree =
    val sym = tree.symbol
    val qualTypeSym = tree.qualifier.tpe.widenDealias.typeSymbol
    if !(sym.isTerm && sym.owner.isClass) || sym.maybeOwner.eq(qualTypeSym) || !qualTypeSym.isClass then tree
    else
      val devirtualizedSym = sym.overriddenSymbol(qualTypeSym.asClass)
      if !devirtualizedSym.exists || sym.eq(devirtualizedSym) || devirtualizedSym.isAllOf(Mutable | JavaDefined) then tree
      else tree.withType(devirtualizedSym.termRef)

object Devirtualize:
  val name: String = "devirtualize"
  val description: String = "try to defirualize call to methods"
