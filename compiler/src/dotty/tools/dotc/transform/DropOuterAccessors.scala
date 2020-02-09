package dotty.tools.dotc
package transform

import core._
import MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts.Context
import ast._
import Trees._
import Flags._
import Symbols._
import Decorators._
import DenotTransformers._
import StdNames.nme
import collection.mutable

object DropOuterAccessors:
  val name: String = "dropOuterAccessors"

/** Drops outer accessors of final classes that are unused */
class DropOuterAccessors extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>
  import tpd._

  override def phaseName: String = DropOuterAccessors.name

  override def runsAfter: Set[String] = Set(LambdaLift.name)
    // LambdaLift can create outer paths. These need to be known in this phase

  override def changesMembers: Boolean = true // the phase drops outer accessors

  def (sym: Symbol).isOuterParamAccessor(using Context) =
    sym.is(ParamAccessor) && sym.name == nme.OUTER

  private def mightBeDropped(sym: Symbol)(using Context) =
    (sym.is(OuterAccessor) || sym.isOuterParamAccessor)
    && !sym.owner.isExtensibleClass

  /** The number of times an outer accessor that might be dropped is accessed */
  private val accessCount = new mutable.HashMap[Symbol, Int]:
    override def default(key: Symbol): Int = 0

  private def markAccessed(tree: RefTree)(implicit ctx: Context): Tree =
    val sym = tree.symbol
    if mightBeDropped(sym) then accessCount(sym) += 1
    tree

  override def transformIdent(tree: Ident)(using Context): Tree =
    markAccessed(tree)

  override def transformSelect(tree: Select)(using Context): Tree =
    markAccessed(tree)

  override def transformTemplate(impl: Template)(using ctx: Context): Tree =

    def dropOuterAccessor(stat: Tree): Boolean = stat match
      case stat: DefDef
      if stat.symbol.is(OuterAccessor)
          && mightBeDropped(stat.symbol)
          && accessCount(stat.symbol) == 0 =>
        assert(stat.rhs.isInstanceOf[RefTree], stat)
        assert(accessCount(stat.rhs.symbol) > 0)
        accessCount(stat.rhs.symbol) -= 1
        stat.symbol.dropAfter(thisPhase)
        true
      case _ =>
        false

    val droppedParamAccessors = mutable.Set[Symbol]()

    def dropOuterParamAccessor(stat: Tree): Boolean = stat match
      case stat: ValDef
      if stat.symbol.isOuterParamAccessor
          && mightBeDropped(stat.symbol)
          && accessCount(stat.symbol) == 1 =>
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
      i"""Failed to eliminate: $droppedParamAccessors
          when dropping outer accessors for ${ctx.owner} with
          $impl""")
    cpy.Template(impl)(constr = constr1, body = body1)
  end transformTemplate