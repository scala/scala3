package dotty.tools
package dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import Denotations._, SymDenotations._, Scopes._, StdNames._, NameOps._, Names._

class DispatchToSpecializedApply extends MiniPhaseTransform {
  import ast.Trees._
  import ast.tpd

  val phaseName = "dispatchToSpecializedApply"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo) =
    tree match {
      case Apply(select @ Select(id, nme.apply), arg :: Nil) =>
        val params = List(arg.tpe, tree.tpe)
        val specializedApply = nme.apply.specializedFor(params, params.map(_.typeSymbol.name))
        val hasOverridenSpecializedApply = id.tpe.decls.iterator.exists { sym =>
          sym.is(Flags.Override) && (sym.name eq specializedApply)
        }

        if (hasOverridenSpecializedApply) tpd.Apply(tpd.Select(id, specializedApply), arg :: Nil)
        else tree
      case _ => tree
    }
}
