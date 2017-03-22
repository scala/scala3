package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhantomTermEval extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "phantomTermEval"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    // TODO
  }

  /* Tree transform */

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = tree.tpe match {
    case _: MethodType => tree
    case _ if tree.args.forall(!_.tpe.isPhantom) => tree
    case _ =>
      val argsVals = tree.args.map(toSynthVal)

      def evalArgsInBlock() = Block(argsVals, cpy.Apply(tree)(tree.fun, refs(argsVals)))
      def evalFunAndArgsInBlock(fun: Select) = {
        val qualVal = toSynthVal(fun.qualifier)
        val app = cpy.Apply(tree)(Select(ref(qualVal.symbol), fun.name), refs(argsVals))
        Block(qualVal :: argsVals, app)
      }

      tree.fun match {
        case fun: Select =>
          if (fun.qualifier.isInstanceOf[New]) evalArgsInBlock()
          else evalFunAndArgsInBlock(fun)
        case _ => evalArgsInBlock()
      }
  }

  /* private methods */

  private def toSynthVal(t: Tree)(implicit ctx: Context) =
    SyntheticValDef(ctx.freshName("ev$").toTermName, t)

  private def refs(vals: List[Tree])(implicit ctx: Context) =
    vals.map(x => ref(x.symbol))

}
