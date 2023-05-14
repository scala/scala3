package dotty.tools.dotc
package transform

import core._
import MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts._
import ast._
import Flags._
import Symbols._
import Contexts._
import Decorators._
import DenotTransformers._
import ExplicitOuter.isOuterParamAccessor
import CountOuterAccesses.mightBeDropped
import collection.mutable

object DropOuterAccessors:
  val name: String = "dropOuterAccessors"
  val description: String = "drop unused outer accessors"

/** Drops unused outer accessors of inner classes that are visible only in one
 *  toplevel class. For other classes, we can't tell whether an outer accessor
 *  is used or not. It could for instance be used in a type test in some other source.
 */
class DropOuterAccessors extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>
  import tpd._

  override def phaseName: String = DropOuterAccessors.name

  override def description: String = DropOuterAccessors.description

  override def runsAfterGroupsOf: Set[String] = Set(CountOuterAccesses.name)

  override def changesMembers: Boolean = true // the phase drops outer accessors

  override def transformTemplate(impl: Template)(using Context): Tree =
    val outerAccessCount = ctx.base.countOuterAccessesPhase
      .asInstanceOf[CountOuterAccesses]
      .outerAccessCount

    def dropOuterAccessor(stat: Tree): Boolean = stat match
      case stat: DefDef
      if stat.symbol.is(OuterAccessor)
          && mightBeDropped(stat.symbol)
          && outerAccessCount(stat.symbol) == 0 =>
        assert(stat.rhs.isInstanceOf[RefTree], stat)
        assert(outerAccessCount(stat.rhs.symbol) > 0)
        outerAccessCount(stat.rhs.symbol) -= 1
        stat.symbol.dropAfter(thisPhase)
        true
      case _ =>
        false

    val droppedParamAccessors = mutable.Set[Symbol]()

    def dropOuterParamAccessor(stat: Tree): Boolean = stat match
      case stat: ValDef
      if stat.symbol.isOuterParamAccessor
          && mightBeDropped(stat.symbol)
          && outerAccessCount(stat.symbol) == 1 =>
        droppedParamAccessors += stat.symbol
        stat.symbol.dropAfter(thisPhase)
        true
      case _ =>
        false

    def dropOuterInit(stat: Tree): Boolean = stat match
      case Assign(lhs, rhs) => droppedParamAccessors.remove(lhs.symbol)
      case _ => false

    val body1 = impl.body
      .filterNot(dropOuterAccessor)
      .filterNot(dropOuterParamAccessor)
    val constr1 =
      if droppedParamAccessors.isEmpty then impl.constr
      else cpy.DefDef(impl.constr)(
        rhs = impl.constr.rhs match {
          case rhs @ Block(inits, expr) =>
            cpy.Block(rhs)(inits.filterNot(dropOuterInit), expr)
        })
    assert(droppedParamAccessors.isEmpty,
      i"""Failed to eliminate: ${droppedParamAccessors.toList}
          when dropping outer accessors for ${ctx.owner} with
          $impl""")
    cpy.Template(impl)(constr = constr1, body = body1)
  end transformTemplate
