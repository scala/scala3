package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** This phase extracts the arguments of phantom type before the application to avoid losing any
 *  effects in the argument tree. This trivializes the removal of parameter in the Erasure phase.
 *
 *  `f(x1,...)(y1,...)...(...)` with at least one phantom argument
 *
 *    -->
 *
 *  `val ev$f = f` // if `f` is some expression that needs evaluation
 *  `val ev$x1 = x1`
  *  ...
 *  `val ev$y1 = y1`
  *  ...
 *  `ev$f(ev$x1,...)(ev$y1,...)...(...)`
 *
 */
class PhantomArgumentEval extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "phantomArgumentEval"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: Apply =>
      tree.args.foreach { arg =>
        assert(
          !arg.tpe.isPhantom ||
          (arg.isInstanceOf[Ident] && !arg.symbol.is(Method) && !arg.symbol.is(Lazy))
        )
      }
    case _ =>
  }

  /* Tree transform */

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = tree.tpe match {
    case _: MethodType => tree // Do the transformation higher in the tree if needed
    case _ =>
      def mkNewBlock(newApply: Tree, synthVals: List[ValDef]) = Block(synthVals, newApply)
      if (!hasPhantomArgs(tree)) tree
      else transformApplication(tree, mkNewBlock)
  }

  override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!tree.rhs.tpe.isPhantom) super.transformAssign(tree)
    else {
      // Apply the same transformation to setters before their creation.
      val (synthVal, synthValRef) = mkSynthVal(tree.rhs)
      Block(List(synthVal), Assign(tree.lhs, synthValRef))
    }
  }

  /* private methods */

  /** Returns true if at least on of the arguments is a phantom.
   *  Inner applies are also checked in case of multiple parameter list.
   */
  private def hasPhantomArgs(tree: Apply)(implicit ctx: Context): Boolean = {
    tree.args.exists {
      case arg: Ident if !arg.symbol.is(Method) && !arg.symbol.is(Lazy) => false
      case arg => arg.tpe.isPhantom
    } || {
      tree.fun match {
        case fun: Apply => hasPhantomArgs(fun)
        case _ => false
      }
    }
  }

  /** Collects all args (and possibly the function) as synthetic vals and replaces them in the tree by the reference to
   *  the lifted its val.
   */
  private def transformApplication(tree: Tree, mkTree: (Tree, List[ValDef]) => Tree)(implicit ctx: Context): Tree = tree match {
    case tree: Apply =>
      def mkNewApply(newFun: Tree, accSynthVals: List[ValDef]) = {
        val (synthVals, synthValRefs) = tree.args.map(mkSynthVal).unzip
        mkTree(cpy.Apply(tree)(newFun, synthValRefs), accSynthVals ::: synthVals)
      }
      transformApplication(tree.fun, mkNewApply)
    case tree: TypeApply =>
      def mkNewTypeApply(newFun: Tree, accSynthVals: List[ValDef]) =
        mkTree(cpy.TypeApply(tree)(newFun, tree.args), accSynthVals)
      transformApplication(tree.fun, mkNewTypeApply)
    case tree: Select if !tree.qualifier.isInstanceOf[New] =>
      val (accSynthVal, synthValRef) = mkSynthVal(tree.qualifier)
      mkTree(Select(synthValRef, tree.name), List(accSynthVal))
    case _ => mkTree(tree, Nil)
  }

  /** Makes a synthetic val with the tree and creates a reference to it.
   *    `tree` --> (`val ev$x = tree`, `ev$x`)
   */
  private def mkSynthVal(tree: Tree)(implicit ctx: Context): (ValDef, Tree) = {
    val synthVal = SyntheticValDef(TempResultName.fresh(), tree)
    (synthVal, ref(synthVal.symbol))
  }
}
