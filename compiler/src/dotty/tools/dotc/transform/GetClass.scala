package dotty.tools.dotc
package transform

import ast.tpd
import core.Contexts.Context
import core.StdNames.nme
import TypeUtils._
import MegaPhase.MiniPhase

/** Rewrite `getClass` calls as follow:
  *
  *  For every instance of primitive class C whose boxed class is called B:
  *    instanceC.getClass    -> B.TYPE
  *  For every instance of non-primitive class D:
  *    instanceD.getClass    -> instanceD.getClass
  */
class GetClass extends MiniPhase {
  import tpd._

  override def phaseName: String = "getClass"

  // getClass transformation should be applied to specialized methods
  override def runsAfter: Set[String] = Set(Erasure.name)

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    import ast.Trees._
    tree match {
      case Apply(Select(qual, nme.getClass_), Nil) if qual.tpe.widen.isPrimitiveValueType =>
        clsOf(qual.tpe.widen).withPos(tree.pos)
      case _ => tree
    }
  }
}
