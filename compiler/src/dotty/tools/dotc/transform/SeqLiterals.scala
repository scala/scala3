package dotty.tools.dotc
package transform

import core._
import Types._
import dotty.tools.dotc.transform.TreeTransforms._
import Contexts.Context
import Symbols._
import Phases._
import Decorators._

/** A transformer that eliminates SeqLiteral's, transforming `SeqLiteral(elems)` to an operation
 *  equivalent to
 *
 *      JavaSeqLiteral(elems).toSeq
 *
 *  Instead of `toSeq`, which takes an implicit, the appropriate "wrapArray" method
 *  is called directly. The reason for this step is that JavaSeqLiterals, being arrays
 *  keep a precise type after erasure, whereas SeqLiterals only get the erased type `Seq`,
 */
class SeqLiterals extends MiniPhaseTransform {
  import ast.tpd._

  override def phaseName = "seqLiterals"
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[PatternMatcher])

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tpd: SeqLiteral => assert(tpd.isInstanceOf[JavaSeqLiteral])
    case _ =>
  }

  override def transformSeqLiteral(tree: SeqLiteral)(implicit ctx: Context, info: TransformerInfo): Tree = tree match {
    case tree: JavaSeqLiteral => tree
    case _ =>
      val arr = JavaSeqLiteral(tree.elems, tree.elemtpt)
      //println(i"trans seq $tree, arr = $arr: ${arr.tpe} ${arr.tpe.elemType}")
      val elemtp = tree.elemtpt.tpe
      val elemCls = elemtp.classSymbol
      val (wrapMethStr, targs) =
        if (elemCls.isPrimitiveValueClass) (s"wrap${elemCls.name}Array", Nil)
        else if (elemtp derivesFrom defn.ObjectClass) ("wrapRefArray", elemtp :: Nil)
        else ("genericWrapArray", elemtp :: Nil)
      ref(defn.ScalaPredefModule)
        .select(wrapMethStr.toTermName)
        .appliedToTypes(targs)
        .appliedTo(arr)
  }
}
