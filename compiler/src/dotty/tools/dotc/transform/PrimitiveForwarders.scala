package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import ast.Trees._
import util.Positions._
import Names._
import collection.mutable
import ResolveSuper._

/** This phase adds forwarder where mixedin generic and primitive typed methods have a missmatch.
  *  In particular for every method that is declared both as generic with a primitive type and with a primitive type
  *    `<mods> def f[Ts](ps1)...(psN): U` in trait M` and
  *    `<mods> def f[Ts](ps1)...(psN): V = ...` in implemented in N`
  *    where U is a primitive and V a polymorphic type (or vice versa) needs:
  *
  *    <mods> def f[Ts](ps1)...(psN): U = super[N].f[Ts](ps1)...(psN)
  *
  *  IMPORTANT: When\If Valhalla happens, we'll need to move mixin before erasure and than this code will need to be rewritten
  *  as it will instead change super-class.
  */
class PrimitiveForwarders extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "primitiveForwarders"

  override def runsAfter = Set(classOf[ResolveSuper])

  override def changesMembers = true   // the phase adds primitive forwarders

  override def transformTemplate(impl: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisTransform)
    import ops._

    def methodPrimitiveForwarders: List[Tree] =
      for (meth <- mixins.flatMap(_.info.decls.toList.flatMap(needsPrimitiveForwarderTo)).distinct)
        yield polyDefDef(implementation(meth.asTerm), forwarder(meth))

    cpy.Template(impl)(body = methodPrimitiveForwarders ::: impl.body)
  }
}
