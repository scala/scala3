package dotty.tools.dotc
package transform

import core._
import Contexts._
import DenotTransformers.SymTransformer
import Flags._
import SymDenotations._
import Symbols._
import MegaPhase.MiniPhase
import ast.tpd

class DeferInlineTraits extends MiniPhase with SymTransformer:
  import tpd._
  import DeferInlineTraits._

  override def phaseName: String = DeferInlineTraits.name

  override def description: String = DeferInlineTraits.description

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if isEraseable(sym) then sym.copySymDenotation(initFlags = sym.flags &~ Final | Deferred)
    else sym

  override def transformValDef(tree: ValDef)(using Context): ValDef =
    if isEraseable(tree.symbol) then cpy.ValDef(tree)(rhs = EmptyTree)
    else tree

  override def transformDefDef(tree: DefDef)(using Context): DefDef =
    if isEraseable(tree.symbol) then cpy.DefDef(tree)(rhs = EmptyTree)
    else tree

  private def isEraseable(sym: SymDenotation)(using Context): Boolean =
    sym.isTerm
    && !sym.isConstructor
    && !sym.is(Param)
    && !sym.is(ParamAccessor)
    && !sym.is(Private)
    && !sym.isLocalDummy
    && sym.owner.isInlineTrait


object DeferInlineTraits:
  import tpd._

  val name: String = "deferInlineTraits"
  val description: String = "defer all members in inline traits"
