package dotty.tools.dotc
package transform

import core._
import Contexts.Context
import Types._
import MegaPhase._
import ast.Trees._

/** Transform references of the form
 *
 *     C.this.m
 *
 *  where `C` is a class with explicit self type and `C` is not a
 *  subclass of the owner of `m` to
 *
 *     C.this.asInstanceOf[S & C.this.type].m
 *
 *  where `S` is the self type of `C`.
 *  See run/i789.scala for a test case why this is needed.
 *
 *  Also replaces idents referring to the self type with ThisTypes.
 */
class ExplicitSelf extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = "explicitSelf"

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree = tree.tpe match {
    case tp: ThisType =>
      ctx.debuglog(s"owner = ${ctx.owner}, context = ${ctx}")
      This(tp.cls).withSpan(tree.span)
    case _ => tree
  }

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree = tree match {
    case Select(thiz: This, name) if name.isTermName =>
      val clsd = thiz.symbol.classDenot
      if (clsd.givenSelfType.exists && !clsd.derivesFrom(tree.symbol.owner))
        cpy.Select(tree)(thiz.cast(AndType(clsd.classInfo.selfType, thiz.tpe)), name)
      else tree
    case _ => tree
  }
}
