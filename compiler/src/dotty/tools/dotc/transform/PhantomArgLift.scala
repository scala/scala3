package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.EtaExpansion

import scala.collection.mutable.ListBuffer

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
class PhantomArgLift extends MiniPhase {
  import tpd._

  override def phaseName: String = "phantomArgLift"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: Apply =>
      tree.args.foreach { arg =>
        assert(!arg.tpe.isPhantom || isPureExpr(arg))
      }
    case _ =>
  }

  /* Tree transform */

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = tree.tpe.widen match {
    case _: MethodType => tree // Do the transformation higher in the tree if needed
    case _ =>
      if (!hasImpurePhantomArgs(tree)) tree
      else {
        val buffer = ListBuffer.empty[Tree]
        val app = EtaExpansion.liftApp(buffer, tree)
        if (buffer.isEmpty) app
        else Block(buffer.result(), app)
      }
  }

  /* private methods */

  /** Returns true if at least on of the arguments is an impure phantom.
   *  Inner applies are also checked in case of multiple parameter list.
   */
  private def hasImpurePhantomArgs(tree: Apply)(implicit ctx: Context): Boolean = {
    tree.args.exists(arg => arg.tpe.isPhantom && !isPureExpr(arg)) || {
      tree.fun match {
        case fun: Apply => hasImpurePhantomArgs(fun)
        case _ => false
      }
    }
  }

}
