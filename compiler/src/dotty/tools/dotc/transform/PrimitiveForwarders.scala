package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Symbols._
import DenotTransformers._

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
class PrimitiveForwarders extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "primitiveForwarders"

  override def runsAfter = Set(ResolveSuper.name)

  override def changesMembers = true   // the phase adds primitive forwarders

  override def transformTemplate(impl: Template)(implicit ctx: Context) = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisPhase)
    import ops._

    def methodPrimitiveForwarders: List[Tree] =
      for (meth <- mixins.flatMap(_.info.decls.toList.flatMap(needsPrimitiveForwarderTo)).distinct)
        yield polyDefDef(implementation(meth.asTerm), forwarder(meth))

    cpy.Template(impl)(body = methodPrimitiveForwarders ::: impl.body)
  }
}
