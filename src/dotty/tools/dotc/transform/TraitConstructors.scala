package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.{SymTransformer, DenotTransformer}
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/***
  * Renames constructors in traits so that backend will call them with invokeInterface
  * Also makes sure that renamed constructor bodies conforms to type of method
 */
class TraitConstructors extends MiniPhaseTransform with SymTransformer {
  import dotty.tools.dotc.ast.tpd._
  def phaseName: String = "traitConstructors"


  override def treeTransformPhase: Phase = this.phase

  def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation = {
    if (sym.isPrimaryConstructor && (sym.owner is Flags.Trait))
      // TODO: Someone needs to carefully check if name clashes are possible with this mangling scheme
      sym.copySymDenotation(name = nme.INITIALIZER_PREFIX ++ sym.owner.fullNameSeparated("$"))
    else sym
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    if (sym.isPrimaryConstructor && (sym.owner is Flags.Trait))
      cpy.DefDef(tree)(rhs = Block(List(tree.rhs), This(tree.symbol.enclosingClass.asClass)))
    else tree
  }

}
