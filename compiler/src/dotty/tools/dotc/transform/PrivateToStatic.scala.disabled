package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Contexts.Context
import Symbols._
import Scopes._
import Flags._
import StdNames._
import SymDenotations._
import Types._
import collection.mutable
import TreeTransforms._
import Decorators._
import ast.Trees._
import TreeTransforms.TransformerInfo

/** Makes private methods static, provided they not deferred, accessors, or static,
 *  by rewriting a method `m` in class `C` as follows:
 *
 *     private def m(ps) = e
 *
 *     --> private static def($this: C, ps) = [this -> $this] e
 */
class PrivateToStatic extends MiniPhase with SymTransformer { thisTransform =>
  import ast.tpd._
  override def phaseName = "privateToStatic"
  override def relaxedTyping = true

  private val Immovable = Deferred | Accessor | JavaStatic

  def shouldBeStatic(sd: SymDenotation)(implicit ctx: Context) =
    sd.current(ctx.withPhase(thisTransform)).asSymDenotation
      .is(PrivateMethod, butNot = Immovable) &&
    sd.owner.is(Trait)

  override def transformSym(sd: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (shouldBeStatic(sd)) {
      val mt @ MethodType(pnames, ptypes) = sd.info
      sd.copySymDenotation(
        initFlags = sd.flags | JavaStatic,
        info = MethodType(nme.SELF :: pnames, sd.owner.thisType :: ptypes, mt.resultType))
    }
    else sd

  val treeTransform = new Transform(NoSymbol)

  class Transform(thisParam: Symbol) extends TreeTransform {
    def phase = thisTransform

    override def prepareForDefDef(tree: DefDef)(implicit ctx: Context) =
      if (shouldBeStatic(tree.symbol)) {
        val selfParam = ctx.newSymbol(tree.symbol, nme.SELF, Param, tree.symbol.owner.thisType, coord = tree.pos)
        new Transform(selfParam)
      }
      else this

    override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo) =
      if (shouldBeStatic(tree.symbol)) {
        val thisParamDef = ValDef(thisParam.asTerm)
        val vparams :: Nil = tree.vparamss
        cpy.DefDef(tree)(vparamss = (thisParamDef :: vparams) :: Nil)
      }
      else tree

    override def transformThis(tree: This)(implicit ctx: Context, info: TransformerInfo) =
      if (shouldBeStatic(ctx.owner.enclosingMethod)) ref(thisParam).withPos(tree.pos)
      else tree

    /** Rwrites a call to a method `m` which is made static as folows:
     *
     *    qual.m(args)  -->  m(qual, args)
     */
    override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo) =
      tree.fun match {
        case fun @ Select(qual, name) if shouldBeStatic(fun.symbol) =>
          ctx.debuglog(i"mapping $tree to ${cpy.Ident(fun)(name)} (${qual :: tree.args}%, %)")
          cpy.Apply(tree)(ref(fun.symbol).withPos(fun.pos), qual :: tree.args)
        case _ =>
          tree
      }

    override def transformClosure(tree: Closure)(implicit ctx: Context, info: TransformerInfo) =
      tree.meth match {
        case meth @ Select(qual, name) if shouldBeStatic(meth.symbol) =>
          cpy.Closure(tree)(
            env = qual :: tree.env,
            meth = ref(meth.symbol).withPos(meth.pos))
        case _ =>
          tree
    }
  }
}
