package dotty.tools.dotc
package transform

import core._
import MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts._
import ast._
import Flags._
import Symbols._
import ExplicitOuter.isOuterParamAccessor

import collection.mutable

object CountOuterAccesses:
  val name: String = "countOuterAccesses"
  val description: String = "identify outer accessors that can be dropped"

  /** Characterizes outer accessors and outer fields that can be dropped
   *  if there are no references to them from within the toplevel class
   *  where they are defined.
   */
  def mightBeDropped(sym: Symbol)(using Context) =
    def isLocal(cls: Symbol) =
      cls.isAnonymousClass
      || cls.owner.isTerm
      || cls.accessBoundary(defn.RootClass).isContainedIn(cls.topLevelClass)
    (sym.is(OuterAccessor) || sym.isOuterParamAccessor) && isLocal(sym.owner)

/** Counts number of accesses to outer accessors and outer fields of
 *  classes that are visible only within one source file. The info
 *  is collected in `outerAccessCount` and used in the subsequent
 *  DropOuterAccessors phase
 */
class CountOuterAccesses extends MiniPhase:
  thisPhase =>
  import tpd._

  override def phaseName: String = CountOuterAccesses.name

  override def description: String = CountOuterAccesses.description

  override def runsAfter: Set[String] = Set(LambdaLift.name)
    // LambdaLift can create outer paths. These need to be known in this phase.

  /** The number of times an outer accessor that might be dropped is accessed */
  val outerAccessCount = new mutable.HashMap[Symbol, Int] {
    override def default(s: Symbol): Int = 0
  }

  private def markAccessed(tree: RefTree)(using Context): Tree =
    val sym = tree.symbol
    if CountOuterAccesses.mightBeDropped(sym) then outerAccessCount(sym) += 1
    tree

  override def transformIdent(tree: Ident)(using Context): Tree =
    markAccessed(tree)

  override def transformSelect(tree: Select)(using Context): Tree =
    markAccessed(tree)
